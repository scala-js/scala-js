/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package org.scalajs.core.compiler

import scala.collection.mutable

import scala.tools.nsc._
import scala.math.PartialOrdering
import scala.reflect.internal.Flags

import org.scalajs.core.ir
import ir.{Trees => js, Types => jstpe}
import ir.Trees.OptimizerHints

import util.ScopedVar
import ScopedVar.withScopedVars

/** Generation of exports for JavaScript
 *
 *  @author Sébastien Doeraene
 */
trait GenJSExports extends SubComponent { self: GenJSCode =>
  import global._
  import jsAddons._
  import definitions._
  import jsDefinitions._
  import jsInterop.{jsNameOf, JSName}

  trait JSExportsPhase { this: JSCodePhase =>

    /** Generates exported methods and properties for a class.
     *
     *  @param classSym symbol of the class we export for
     */
    def genMemberExports(classSym: Symbol): List[js.Tree] = {
      val allExports = classSym.info.members.filter(jsInterop.isExport(_))

      val newlyDecldExports = if (classSym.superClass == NoSymbol) {
        allExports
      } else {
        allExports.filterNot { sym =>
          classSym.superClass.info.member(sym.name)
            .filter(_.tpe =:= sym.tpe).exists
        }
      }

      val newlyDecldExportNames =
        newlyDecldExports.map(_.name.toTermName).toList.distinct

      newlyDecldExportNames map { genMemberExport(classSym, _) }
    }

    def genJSClassDispatchers(classSym: Symbol,
        dispatchMethodsNames: List[JSName]): List[js.Tree] = {
      dispatchMethodsNames
        .map(genJSClassDispatcher(classSym, _))
    }

    def genConstructorExports(classSym: Symbol): List[js.ConstructorExportDef] = {
      val constructors = classSym.tpe.member(nme.CONSTRUCTOR).alternatives

      // Generate exports from constructors and their annotations
      val ctorExports = for {
        ctor <- constructors
        exp  <- jsInterop.registeredExportsOf(ctor)
      } yield (exp, ctor)

      if (ctorExports.isEmpty) {
        Nil
      } else {
        val exports = for {
          (jsName, specs) <- ctorExports.groupBy(_._1.jsName) // group by exported name
        } yield {
          val (namedExports, normalExports) = specs.partition(_._1.isNamed)

          val normalCtors = normalExports.map(s => ExportedSymbol(s._2))
          val namedCtors = for {
            (exp, ctor) <- namedExports
          } yield {
            implicit val pos = exp.pos
            ExportedBody(List(JSAnyTpe),
              genNamedExporterBody(ctor, genFormalArg(1).ref),
              nme.CONSTRUCTOR.toString, pos)
          }

          val ctors = normalCtors ++ namedCtors

          implicit val pos = ctors.head.pos

          val js.MethodDef(_, _, args, _, body) =
            withNewLocalNameScope(genExportMethod(ctors, JSName.Literal(jsName),
                static = false))

          js.ConstructorExportDef(jsName, args, body.get)
        }

        exports.toList
      }
    }

    def genJSClassExports(classSym: Symbol): List[js.JSClassExportDef] = {
      for {
        exp <- jsInterop.registeredExportsOf(classSym)
      } yield {
        implicit val pos = exp.pos
        assert(!exp.isNamed, "Class cannot be exported named")

        exp.destination match {
          case ExportDestination.Normal | ExportDestination.TopLevel =>
            js.JSClassExportDef(exp.jsName)
          case ExportDestination.Static =>
            throw new AssertionError(
                "Found a class export static for " + classSym.fullName)
        }
      }
    }

    def genModuleAccessorExports(classSym: Symbol): List[js.Tree] = {
      for {
        exp <- jsInterop.registeredExportsOf(classSym)
      } yield {
        implicit val pos = exp.pos
        assert(!exp.isNamed, "Module cannot be exported named")

        exp.destination match {
          case ExportDestination.Normal =>
            js.ModuleExportDef(exp.jsName)
          case ExportDestination.TopLevel =>
            js.TopLevelModuleExportDef(exp.jsName)
          case ExportDestination.Static =>
            throw new AssertionError(
                "Found a module export static for " + classSym.fullName)
        }
      }
    }

    def genTopLevelExports(classSym: Symbol): List[js.Tree] =
      genTopLevelOrStaticExports(classSym, ExportDestination.TopLevel)

    def genStaticExports(classSym: Symbol): List[js.Tree] =
      genTopLevelOrStaticExports(classSym, ExportDestination.Static)

    private def genTopLevelOrStaticExports(classSym: Symbol,
        destination: ExportDestination): List[js.Tree] = {
      require(
          destination == ExportDestination.TopLevel ||
          destination == ExportDestination.Static)

      val exportsNamesAndPositions = {
        genTopLevelOrStaticFieldExports(classSym, destination) ++
        genTopLevelOrStaticMethodExports(classSym, destination)
      }

      for {
        exportsWithSameName <- exportsNamesAndPositions.groupBy(_._2).values
        duplicate <- exportsWithSameName.tail
      } {
        val strKind =
          if (destination == ExportDestination.TopLevel) "top-level"
          else "static"
        reporter.error(duplicate._3,
            s"Duplicate $strKind export with name '${duplicate._2}': " +
            "a field may not share its exported name with another field or " +
            "method")
      }

      exportsNamesAndPositions.map(_._1)
    }

    private def genTopLevelOrStaticFieldExports(classSym: Symbol,
        destination: ExportDestination): List[(js.Tree, String, Position)] = {
      (for {
        fieldSym <- classSym.info.members
        if !fieldSym.isMethod && fieldSym.isTerm && !fieldSym.isModule
        export <- jsInterop.registeredExportsOf(fieldSym)
        if export.destination == destination
      } yield {
        implicit val pos = fieldSym.pos

        val tree = if (destination == ExportDestination.Static) {
          val mutable = true // static fields must always be mutable
          val name = js.StringLiteral(export.jsName)
          val irTpe = genExposedFieldIRType(fieldSym)
          js.FieldDef(static = true, name, irTpe, mutable)
        } else {
          js.TopLevelFieldExportDef(export.jsName, encodeFieldSym(fieldSym))
        }

        (tree, export.jsName, pos)
      }).toList
    }

    private def genTopLevelOrStaticMethodExports(classSym: Symbol,
        destination: ExportDestination): List[(js.Tree, String, Position)] = {
      val allRelevantExports = for {
        methodSym <- classSym.info.members
        if methodSym.isMethod && !methodSym.isConstructor
        export <- jsInterop.registeredExportsOf(methodSym)
        if export.destination == destination
      } yield {
        (export, methodSym)
      }

      for {
        (jsName, tups) <- allRelevantExports.groupBy(_._1.jsName).toList
      } yield {
        implicit val pos = tups.head._1.pos

        val alts = tups.map(_._2).toList
        val firstAlt = alts.head
        val isProp = jsInterop.isJSProperty(firstAlt)

        // Check for conflict between method vs property

        for {
          conflicting <- alts.tail
          if jsInterop.isJSProperty(conflicting) != isProp
        } {
          val kindStr = if (isProp) "method" else "property"
          reporter.error(conflicting.pos,
              s"Exported $kindStr $jsName conflicts with ${firstAlt.nameString}")
        }

        // Generate the export

        val exportedMember = genMemberExportOrDispatcher(classSym,
            JSName.Literal(jsName), isProp, alts, static = true)

        val exportDef =
          if (destination == ExportDestination.Static) exportedMember
          else js.TopLevelMethodExportDef(exportedMember.asInstanceOf[js.MethodDef])

        (exportDef, jsName, pos)
      }
    }

    /** Tests whether the given def a named exporter def that needs to be
     *  generated with `genNamedExporterDef`.
     */
    def isNamedExporterDef(dd: DefDef): Boolean = {
      jsInterop.isExport(dd.symbol) &&
      dd.symbol.annotations.exists(_.symbol == JSExportNamedAnnotation)
    }

    /** Generate the exporter proxy for a named export */
    def genNamedExporterDef(dd: DefDef): Option[js.MethodDef] = {
      implicit val pos = dd.pos

      if (isAbstractMethod(dd)) {
        None
      } else {
        val sym = dd.symbol

        val Block(Apply(fun, _) :: Nil, _) = dd.rhs
        val trgSym = fun.symbol

        val inArg =
          js.ParamDef(js.Ident("namedParams"), jstpe.AnyType,
              mutable = false, rest = false)
        val inArgRef = inArg.ref

        val methodIdent = encodeMethodSym(sym)

        Some(js.MethodDef(static = false, methodIdent,
            List(inArg), toIRType(sym.tpe.resultType),
            Some(genNamedExporterBody(trgSym, inArg.ref)))(
            OptimizerHints.empty, None))
      }
    }

    private def genNamedExporterBody(trgSym: Symbol, inArg: js.Tree)(
        implicit pos: Position) = {

      if (hasRepeatedParam(trgSym)) {
        reporter.error(pos,
            "You may not name-export a method with a *-parameter")
      }

      val jsArgs = for {
        (pSym, index) <- trgSym.info.params.zipWithIndex
      } yield {
        val rhs = js.JSBracketSelect(inArg,
            js.StringLiteral(pSym.name.decoded))
        js.VarDef(js.Ident("namedArg$" + index), jstpe.AnyType,
            mutable = false, rhs = rhs)
      }

      val jsArgRefs = jsArgs.map(_.ref)

      // Generate JS code to prepare arguments (default getters and unboxes)
      val jsArgPrep = genPrepareArgs(jsArgRefs, trgSym)
      val jsResult = genResult(trgSym, jsArgPrep.map(_.ref), static = false)

      js.Block(jsArgs ++ jsArgPrep :+ jsResult)
    }

    private def genMemberExport(classSym: Symbol, name: TermName): js.Tree = {
      val alts = classSym.info.member(name).alternatives

      assert(!alts.isEmpty,
          s"Ended up with no alternatives for ${classSym.fullName}::$name. " +
          s"Original set was ${alts} with types ${alts.map(_.tpe)}")

      val (jsName, isProp) = jsInterop.jsExportInfo(name)

      // Check if we have a conflicting export of the other kind
      val conflicting =
        classSym.info.member(jsInterop.scalaExportName(jsName, !isProp))

      if (conflicting != NoSymbol) {
        val kind = if (isProp) "property" else "method"
        val alts = conflicting.alternatives

        reporter.error(alts.head.pos,
            s"Exported $kind $jsName conflicts with ${alts.head.fullName}")
      }

      genMemberExportOrDispatcher(classSym, JSName.Literal(jsName), isProp,
          alts, static = false)
    }

    private def genJSClassDispatcher(classSym: Symbol, name: JSName): js.Tree = {
      var alts: List[Symbol] = Nil
      for {
        sym <- classSym.info.members
        if !sym.isBridge && jsNameOf(sym) == name
      } {
        val tpe = sym.tpe
        if (!alts.exists(alt => tpe.matches(alt.tpe)))
          alts ::= sym
      }

      assert(!alts.isEmpty,
          s"Ended up with no alternatives for ${classSym.fullName}::$name.")

      val (propSyms, methodSyms) = alts.partition(jsInterop.isJSProperty(_))
      val isProp = propSyms.nonEmpty

      if (isProp && methodSyms.nonEmpty) {
        reporter.error(alts.head.pos,
            s"Conflicting properties and methods for ${classSym.fullName}::$name.")
        js.Skip()(ir.Position.NoPosition)
      } else {
        genMemberExportOrDispatcher(classSym, name, isProp, alts,
            static = false)
      }
    }

    def genMemberExportOrDispatcher(classSym: Symbol, jsName: JSName,
        isProp: Boolean, alts: List[Symbol], static: Boolean): js.Tree = {
      withNewLocalNameScope {
        if (isProp)
          genExportProperty(alts, jsName, static)
        else
          genExportMethod(alts.map(ExportedSymbol), jsName, static)
      }
    }

    def genJSConstructorExport(alts: List[Symbol]): js.MethodDef = {
      genExportMethod(alts.map(ExportedSymbol), JSName.Literal("constructor"),
          static = false)
    }

    private def genExportProperty(alts: List[Symbol], jsName: JSName,
        static: Boolean): js.PropertyDef = {
      assert(!alts.isEmpty)
      implicit val pos = alts.head.pos

      // Separate getters and setters. Somehow isJSGetter doesn't work here. Hence
      // we just check the parameter list length.
      val (getter, setters) = alts.partition(_.tpe.params.isEmpty)

      // We can have at most one getter
      if (getter.size > 1) {
        /* Member export of properties should be caught earlier, so if we get
         * here with a non-static export, something went horribly wrong.
         */
        assert(static,
            s"Found more than one instance getter to export for name $jsName.")
        for (duplicate <- getter.tail) {
          reporter.error(duplicate.pos,
              s"Duplicate static getter export with name '${jsName.displayName}'")
        }
      }

      val getterBody = getter.headOption.map(genApplyForSym(minArgc = 0,
          hasRestParam = false, _, static))

      val setterArgAndBody = {
        if (setters.isEmpty) {
          None
        } else {
          val arg = genFormalArg(1)
          val body = genExportSameArgc(minArgc = 1, hasRestParam = false,
              alts = setters.map(ExportedSymbol), paramIndex = 0,
              static = static)
          Some((arg, body))
        }
      }

      js.PropertyDef(static, genPropertyName(jsName), getterBody,
          setterArgAndBody)
    }

    /** generates the exporter function (i.e. exporter for non-properties) for
     *  a given name */
    private def genExportMethod(alts0: List[Exported], jsName: JSName,
        static: Boolean): js.MethodDef = {
      assert(alts0.nonEmpty,
          "need at least one alternative to generate exporter method")

      implicit val pos = alts0.head.pos

      val alts = {
        // toString() is always exported. We might need to add it here
        // to get correct overloading.
        val needsToString =
          jsName == JSName.Literal("toString") && alts0.forall(_.params.nonEmpty)

        if (needsToString)
          ExportedSymbol(Object_toString) :: alts0
        else
          alts0
      }

      // Factor out methods with variable argument lists. Note that they can
      // only be at the end of the lists as enforced by PrepJSExports
      val (varArgMeths, normalMeths) = alts.partition(_.hasRepeatedParam)

      // Highest non-repeated argument count
      val maxArgc = (
          // We have argc - 1, since a repeated parameter list may also be empty
          // (unlike a normal parameter)
          varArgMeths.map(_.params.size - 1) ++
          normalMeths.map(_.params.size)
      ).max

      // Calculates possible arg counts for normal method
      def argCounts(ex: Exported) = ex match {
        case ExportedSymbol(sym) =>
          val params = sym.tpe.params
          // Find default param
          val dParam = params.indexWhere { _.hasFlag(Flags.DEFAULTPARAM) }
          if (dParam == -1) Seq(params.size)
          else dParam to params.size
        case ex: ExportedBody =>
          List(ex.params.size)
      }

      // Generate tuples (argc, method)
      val methodArgCounts = {
        // Normal methods
        for {
          method <- normalMeths
          argc   <- argCounts(method)
        } yield (argc, method)
      } ++ {
        // Repeated parameter methods
        for {
          method <- varArgMeths
          argc   <- method.params.size - 1 to maxArgc
        } yield (argc, method)
      }

      // Create a map: argCount -> methods (methods may appear multiple times)
      val methodByArgCount =
        methodArgCounts.groupBy(_._1).mapValues(_.map(_._2).toSet)

      // Minimum number of arguments that must be given
      val minArgc = methodByArgCount.keys.min

      val hasVarArg = varArgMeths.nonEmpty

      // List of formal parameters
      val needsRestParam = maxArgc != minArgc || hasVarArg
      val formalArgs = genFormalArgs(minArgc, needsRestParam)

      // Create tuples: (methods, argCounts). This will be the cases we generate
      val caseDefinitions =
        methodByArgCount.groupBy(_._2).mapValues(_.keySet)

      // Verify stuff about caseDefinitions
      assert({
        val argcs = caseDefinitions.values.flatten.toList
        argcs == argcs.distinct &&
        argcs.forall(_ <= maxArgc)
      }, "every argc should appear only once and be lower than max")

      // Generate a case block for each (methods, argCounts) tuple
      val cases = for {
        (methods, argcs) <- caseDefinitions
        if methods.nonEmpty && argcs.nonEmpty

        // exclude default case we're generating anyways for varargs
        if methods != varArgMeths.toSet

        // body of case to disambiguates methods with current count
        caseBody =
          genExportSameArgc(minArgc, needsRestParam,
              methods.toList, paramIndex = 0, static, Some(argcs.min))

        // argc in reverse order
        argcList = argcs.toList.sortBy(- _)
      } yield (argcList.map(argc => js.IntLiteral(argc - minArgc)), caseBody)

      def defaultCase = {
        if (!hasVarArg) {
          genThrowTypeError()
        } else {
          genExportSameArgc(minArgc, needsRestParam, varArgMeths,
              paramIndex = 0, static = static)
        }
      }

      val body = {
        if (cases.isEmpty)
          defaultCase
        else if (cases.size == 1 && !hasVarArg)
          cases.head._2
        else {
          assert(needsRestParam,
              "Trying to read rest param length but needsRestParam is false")
          js.Match(
              js.Unbox(js.JSBracketSelect(
                  genRestArgRef(),
                  js.StringLiteral("length")),
                  'I'),
              cases.toList, defaultCase)(jstpe.AnyType)
        }
      }

      js.MethodDef(static, genPropertyName(jsName),
          formalArgs, jstpe.AnyType, Some(body))(OptimizerHints.empty, None)
    }

    /**
     * Resolve method calls to [[alts]] while assuming they have the same
     * parameter count.
     * @param minArgc The minimum number of arguments that must be given
     * @param alts Alternative methods
     * @param paramIndex Index where to start disambiguation
     * @param maxArgc only use that many arguments
     */
    private def genExportSameArgc(minArgc: Int, hasRestParam: Boolean,
        alts: List[Exported], paramIndex: Int, static: Boolean,
        maxArgc: Option[Int] = None): js.Tree = {

      implicit val pos = alts.head.pos

      if (alts.size == 1)
        alts.head.genBody(minArgc, hasRestParam, static)
      else if (maxArgc.exists(_ <= paramIndex) ||
        !alts.exists(_.params.size > paramIndex)) {
        // We reach here in three cases:
        // 1. The parameter list has been exhausted
        // 2. The optional argument count restriction has triggered
        // 3. We only have (more than once) repeated parameters left
        // Therefore, we should fail
        reporter.error(pos,
            s"""Cannot disambiguate overloads for exported method ${alts.head.name} with types
               |  ${alts.map(_.typeInfo).mkString("\n  ")}""".stripMargin)
        js.Undefined()
      } else {

        val altsByTypeTest = groupByWithoutHashCode(alts) {
          case ExportedSymbol(alt) =>
            typeTestForTpe(computeExportArgType(alt, paramIndex))

          case ex: ExportedBody =>
            typeTestForTpe(ex.params(paramIndex))
        }

        if (altsByTypeTest.size == 1) {
          // Testing this parameter is not doing any us good
          genExportSameArgc(minArgc, hasRestParam, alts,
              paramIndex+1, static, maxArgc)
        } else {
          // Sort them so that, e.g., isInstanceOf[String]
          // comes before isInstanceOf[Object]
          val sortedAltsByTypeTest = topoSortDistinctsBy(
              altsByTypeTest)(_._1)(RTTypeTest.Ordering)

          val defaultCase = genThrowTypeError()

          sortedAltsByTypeTest.foldRight[js.Tree](defaultCase) { (elem, elsep) =>
            val (typeTest, subAlts) = elem
            implicit val pos = subAlts.head.pos

            val paramRef = genFormalArgRef(paramIndex+1, minArgc)
            val genSubAlts = genExportSameArgc(minArgc, hasRestParam,
                subAlts, paramIndex+1, static, maxArgc)

            def hasDefaultParam = subAlts.exists {
              case ExportedSymbol(p) =>
                val params = p.tpe.params
                params.size > paramIndex &&
                params(paramIndex).hasFlag(Flags.DEFAULTPARAM)
              case _: ExportedBody => false
            }

            val optCond = typeTest match {
              case HijackedTypeTest(boxedClassName, _) =>
                Some(js.IsInstanceOf(paramRef, jstpe.ClassType(boxedClassName)))

              case InstanceOfTypeTest(tpe) =>
                Some(genIsInstanceOf(paramRef, tpe))

              case NoTypeTest =>
                None
            }

            optCond.fold[js.Tree] {
              genSubAlts // note: elsep is discarded, obviously
            } { cond =>
              val condOrUndef = if (!hasDefaultParam) cond else {
                js.If(cond, js.BooleanLiteral(true),
                    js.BinaryOp(js.BinaryOp.===, paramRef, js.Undefined()))(
                    jstpe.BooleanType)
              }
              js.If(condOrUndef, genSubAlts, elsep)(jstpe.AnyType)
            }
          }
        }
      }
    }

    private def computeExportArgType(alt: Symbol, paramIndex: Int): Type = {
      // See the comment in genPrimitiveJSArgs for a rationale about this
      enteringPhase(currentRun.uncurryPhase) {

        lazy val paramsUncurry = alt.paramss.flatten
        lazy val paramsTypesUncurry = paramsUncurry.map(_.tpe)
        lazy val isRepeatedUncurry = paramsUncurry.map(isRepeated)

        lazy val paramsPosterasure = enteringPhase(currentRun.posterasurePhase) {
          alt.paramss.flatten
        }
        def paramTypePosterasure = enteringPhase(currentRun.posterasurePhase) {
          paramsPosterasure.apply(paramIndex).tpe
        }

        if (!alt.isClassConstructor) {
          // get parameter type while resolving repeated params
          if (paramsTypesUncurry.size <= paramIndex || isRepeatedUncurry(paramIndex)) {
            assert(isRepeatedUncurry.last)
            repeatedToSingle(paramsTypesUncurry.last)
          } else {
            paramTypePosterasure
          }
        } else {
          // Compute the number of captured parameters that are added to the front
          val paramsNamesUncurry = paramsUncurry.map(_.name)
          val numCapturesFront = enteringPhase(currentRun.posterasurePhase) {
            if (paramsNamesUncurry.isEmpty) paramsPosterasure.size
            else paramsPosterasure.map(_.name).indexOfSlice(paramsNamesUncurry)
          }
          // get parameter type while resolving repeated params
          if (paramIndex < numCapturesFront) {
            // Type of a parameter that represents a captured outer context
            paramTypePosterasure
          } else {
            val paramIndexNoCaptures = paramIndex - numCapturesFront
            if (paramsTypesUncurry.size <= paramIndexNoCaptures ||
                isRepeatedUncurry(paramIndexNoCaptures)) {
              assert(isRepeatedUncurry.last)
              repeatedToSingle(paramsTypesUncurry.last)
            } else {
              paramsTypesUncurry(paramIndexNoCaptures)
            }
          }
        }
      }
    }

    /**
     * Generate a call to the method [[sym]] while using the formalArguments
     * and potentially the argument array. Also inserts default parameters if
     * required.
     */
    private def genApplyForSym(minArgc: Int, hasRestParam: Boolean,
        sym: Symbol, static: Boolean): js.Tree = {
      if (isScalaJSDefinedJSClass(currentClassSym) &&
          sym.owner != currentClassSym.get) {
        assert(!static)
        genApplyForSymJSSuperCall(minArgc, hasRestParam, sym)
      } else {
        genApplyForSymNonJSSuperCall(minArgc, sym, static)
      }
    }

    private def genApplyForSymJSSuperCall(minArgc: Int, hasRestParam: Boolean,
        sym: Symbol): js.Tree = {
      implicit val pos = sym.pos

      val restArg =
        if (hasRestParam) js.JSSpread(genRestArgRef()) :: Nil
        else Nil

      val allArgs =
        (1 to minArgc).map(genFormalArgRef(_, minArgc)) ++: restArg

      val cls = jstpe.ClassType(encodeClassFullName(currentClassSym))
      val receiver = js.This()(jstpe.AnyType)
      val nameString = genExpr(jsNameOf(sym))

      if (jsInterop.isJSGetter(sym)) {
        assert(allArgs.isEmpty)
        js.JSSuperBracketSelect(cls, receiver, nameString)
      } else if (jsInterop.isJSSetter(sym)) {
        assert(allArgs.size == 1 && !allArgs.head.isInstanceOf[js.JSSpread])
        js.Assign(js.JSSuperBracketSelect(cls, receiver, nameString),
            allArgs.head)
      } else {
        js.JSSuperBracketCall(cls, receiver, nameString, allArgs)
      }
    }

    private def genApplyForSymNonJSSuperCall(minArgc: Int,
        sym: Symbol, static: Boolean): js.Tree = {
      implicit val pos = sym.pos

      // the (single) type of the repeated parameter if any
      val repeatedTpe = enteringPhase(currentRun.uncurryPhase) {
        for {
          param <- sym.paramss.flatten.lastOption
          if isRepeated(param)
        } yield repeatedToSingle(param.tpe)
      }

      val normalArgc = sym.tpe.params.size -
        (if (repeatedTpe.isDefined) 1 else 0)

      // optional repeated parameter list
      val jsVarArgPrep = repeatedTpe map { tpe =>
        // new WrappedArray(varargs)
        val rhs = genNew(WrappedArrayClass, WrappedArray_ctor, List(
            genVarargRef(normalArgc, minArgc)))
        js.VarDef(js.Ident("prep" + normalArgc), rhs.tpe, mutable = false, rhs)
      }

      // normal arguments
      val jsArgRefs = (1 to normalArgc).toList.map(
          i => genFormalArgRef(i, minArgc))

      // Generate JS code to prepare arguments (default getters and unboxes)
      val jsArgPrep = genPrepareArgs(jsArgRefs, sym) ++ jsVarArgPrep
      val jsResult = genResult(sym, jsArgPrep.map(_.ref), static)

      js.Block(jsArgPrep :+ jsResult)
    }

    /** Generate the necessary JavaScript code to prepare the arguments of an
     *  exported method (unboxing and default parameter handling)
     */
    private def genPrepareArgs(jsArgs: List[js.Tree], sym: Symbol)(
        implicit pos: Position): List[js.VarDef] = {

      val result = new mutable.ListBuffer[js.VarDef]

      val paramsPosterasure =
        enteringPhase(currentRun.posterasurePhase)(sym.tpe).params
      val paramsNow = sym.tpe.params

      /* The parameters that existed at posterasurePhase are taken from that
       * phase. The parameters that where added after posterasurePhase are taken
       * from the current parameter list.
       */
      val params = paramsPosterasure ++ paramsNow.drop(paramsPosterasure.size)

      for {
        (jsArg, (param, i)) <- jsArgs zip params.zipWithIndex
      } yield {
        // Unboxed argument (if it is defined)
        val unboxedArg = fromAny(jsArg,
            enteringPhase(currentRun.posterasurePhase)(param.tpe))

        // If argument is undefined and there is a default getter, call it
        val verifiedOrDefault = if (param.hasFlag(Flags.DEFAULTPARAM)) {
          js.If(js.BinaryOp(js.BinaryOp.===, jsArg, js.Undefined()), {
            val trgSym = {
              if (sym.isClassConstructor) {
                /* Get the companion module class.
                 * For inner classes the sym.owner.companionModule can be broken,
                 * therefore companionModule is fetched at uncurryPhase.
                 */
                val companionModule = enteringPhase(currentRun.namerPhase) {
                  sym.owner.companionModule
                }
                companionModule.moduleClass
              } else {
                sym.owner
              }
            }
            val defaultGetter = trgSym.tpe.member(
                nme.defaultGetterName(sym.name, i+1))

            assert(defaultGetter.exists,
                s"need default getter for method ${sym.fullName}")
            assert(!defaultGetter.isOverloaded)

            val trgTree = {
              if (sym.isClassConstructor) genLoadModule(trgSym)
              else js.This()(encodeClassType(trgSym))
            }

            // Pass previous arguments to defaultGetter
            val defaultGetterArgs =
              result.take(defaultGetter.tpe.params.size).toList.map(_.ref)

            if (isRawJSType(trgSym.toTypeConstructor)) {
              if (isScalaJSDefinedJSClass(defaultGetter.owner)) {
                genApplyJSClassMethod(trgTree, defaultGetter, defaultGetterArgs)
              } else {
                reporter.error(param.pos, "When overriding a native method " +
                    "with default arguments, the overriding method must " +
                    "explicitly repeat the default arguments.")
                js.Undefined()
              }
            } else {
              genApplyMethod(trgTree, defaultGetter, defaultGetterArgs)
            }
          }, {
            // Otherwise, unbox the argument
            unboxedArg
          })(unboxedArg.tpe)
        } else {
          // Otherwise, it is always the unboxed argument
          unboxedArg
        }

        result +=
          js.VarDef(js.Ident("prep"+i),
              verifiedOrDefault.tpe, mutable = false, verifiedOrDefault)
      }

      result.toList
    }

    /** Generate the final forwarding call to the exported method.
     *  Attention: This method casts the arguments to the right type. The IR
     *  checker will not detect if you pass in a wrongly typed argument.
     */
    private def genResult(sym: Symbol, args: List[js.Tree],
        static: Boolean)(implicit pos: Position) = {
      val thisType =
        if (sym.owner == ObjectClass) jstpe.ClassType(ir.Definitions.ObjectClass)
        else encodeClassType(sym.owner)
      val receiver =
        if (static) genLoadModule(sym.owner)
        else js.This()(thisType)
      val call = {
        if (isScalaJSDefinedJSClass(currentClassSym)) {
          assert(sym.owner == currentClassSym.get, sym.fullName)
          genApplyJSClassMethod(receiver, sym, args)
        } else {
          if (sym.isClassConstructor)
            genApplyMethodStatically(receiver, sym, args)
          else
            genApplyMethod(receiver, sym, args)
        }
      }
      ensureBoxed(call,
          enteringPhase(currentRun.posterasurePhase)(sym.tpe.resultType))
    }

    private sealed abstract class Exported {
      def pos: Position
      def params: List[Type]
      def genBody(minArgc: Int, hasRestParam: Boolean, static: Boolean): js.Tree
      def name: String
      def typeInfo: String
      def hasRepeatedParam: Boolean
    }

    private case class ExportedSymbol(sym: Symbol) extends Exported {
      def pos: Position = sym.pos
      def params: List[Type] = sym.tpe.params.map(_.tpe)
      def genBody(minArgc: Int, hasRestParam: Boolean, static: Boolean): js.Tree =
        genApplyForSym(minArgc, hasRestParam, sym, static)
      def name: String = sym.name.toString
      def typeInfo: String = sym.tpe.toString
      def hasRepeatedParam: Boolean = GenJSExports.this.hasRepeatedParam(sym)
    }

    private case class ExportedBody(params: List[Type], body: js.Tree,
        name: String, pos: Position) extends Exported {
      def genBody(minArgc: Int, hasRestParam: Boolean, static: Boolean): js.Tree = body
      def typeInfo: String = params.mkString("(", ", ", ")")
      val hasRepeatedParam: Boolean = false
    }
  }

  private sealed abstract class RTTypeTest

  private final case class HijackedTypeTest(
      boxedClassName: String, rank: Int) extends RTTypeTest

  // scalastyle:off equals.hash.code
  private final case class InstanceOfTypeTest(tpe: Type) extends RTTypeTest {
    override def equals(that: Any): Boolean = {
      that match {
        case InstanceOfTypeTest(thatTpe) => tpe =:= thatTpe
        case _ => false
      }
    }
  }
  // scalastyle:on equals.hash.code

  private case object NoTypeTest extends RTTypeTest

  private object RTTypeTest {
    implicit object Ordering extends PartialOrdering[RTTypeTest] {
      override def tryCompare(lhs: RTTypeTest, rhs: RTTypeTest): Option[Int] = {
        if (lteq(lhs, rhs)) if (lteq(rhs, lhs)) Some(0) else Some(-1)
        else                if (lteq(rhs, lhs)) Some(1) else None
      }

      override def lteq(lhs: RTTypeTest, rhs: RTTypeTest): Boolean = {
        (lhs, rhs) match {
          // NoTypeTest is always last
          case (_, NoTypeTest) => true
          case (NoTypeTest, _) => false

          case (HijackedTypeTest(_, rank1), HijackedTypeTest(_, rank2)) =>
            rank1 <= rank2

          case (InstanceOfTypeTest(t1), InstanceOfTypeTest(t2)) =>
            t1 <:< t2

          case (_: HijackedTypeTest, _: InstanceOfTypeTest) => true
          case (_: InstanceOfTypeTest, _: HijackedTypeTest) => false
        }
      }

      override def equiv(lhs: RTTypeTest, rhs: RTTypeTest): Boolean = {
        lhs == rhs
      }
    }
  }

  // Very simple O(n²) topological sort for elements assumed to be distinct
  private def topoSortDistinctsBy[A <: AnyRef, B](coll: List[A])(f: A => B)(
      implicit ord: PartialOrdering[B]): List[A] = {

    @scala.annotation.tailrec
    def loop(coll: List[A], acc: List[A]): List[A] = {
      if (coll.isEmpty) acc
      else if (coll.tail.isEmpty) coll.head :: acc
      else {
        val (lhs, rhs) = coll.span(x => !coll.forall(
            y => (x eq y) || !ord.lteq(f(x), f(y))))
        assert(!rhs.isEmpty, s"cycle while ordering $coll")
        loop(lhs ::: rhs.tail, rhs.head :: acc)
      }
    }

    loop(coll, Nil)
  }

  private def typeTestForTpe(tpe: Type): RTTypeTest = {
    tpe match {
      case tpe: ErasedValueType =>
        InstanceOfTypeTest(tpe.valueClazz.typeConstructor)

      case _ =>
        import ir.{Definitions => Defs}
        (toTypeKind(tpe): @unchecked) match {
          case VoidKind    => HijackedTypeTest(Defs.BoxedUnitClass,    0)
          case BooleanKind => HijackedTypeTest(Defs.BoxedBooleanClass, 1)
          case ByteKind    => HijackedTypeTest(Defs.BoxedByteClass,    2)
          case ShortKind   => HijackedTypeTest(Defs.BoxedShortClass,   3)
          case IntKind     => HijackedTypeTest(Defs.BoxedIntegerClass, 4)
          case FloatKind   => HijackedTypeTest(Defs.BoxedFloatClass,   5)
          case DoubleKind  => HijackedTypeTest(Defs.BoxedDoubleClass,  6)

          case CharKind => InstanceOfTypeTest(boxedClass(CharClass).tpe)
          case LongKind => InstanceOfTypeTest(boxedClass(LongClass).tpe)

          case REFERENCE(cls) =>
            cls match {
              case BoxedUnitClass => HijackedTypeTest(Defs.BoxedUnitClass, 0)
              case StringClass    => HijackedTypeTest(Defs.StringClass, 7)
              case ObjectClass    => NoTypeTest
              case _              =>
                if (isRawJSType(tpe)) NoTypeTest
                else InstanceOfTypeTest(tpe)
            }

          case ARRAY(_) => InstanceOfTypeTest(tpe)
        }
    }
  }

  // Group-by that does not rely on hashCode(), only equals() - O(n²)
  private def groupByWithoutHashCode[A, B](
      coll: List[A])(f: A => B): List[(B, List[A])] = {

    import scala.collection.mutable.ArrayBuffer
    val m = new ArrayBuffer[(B, List[A])]
    m.sizeHint(coll.length)

    for (elem <- coll) {
      val key = f(elem)
      val index = m.indexWhere(_._1 == key)
      if (index < 0) m += ((key, List(elem)))
      else m(index) = (key, elem :: m(index)._2)
    }

    m.toList
  }

  private def genThrowTypeError(msg: String = "No matching overload")(
      implicit pos: Position): js.Tree = {
    js.Throw(js.StringLiteral(msg))
  }

  private def genFormalArgs(minArgc: Int, needsRestParam: Boolean)(
      implicit pos: Position): List[js.ParamDef] = {
    val fixedParams = (1 to minArgc map genFormalArg).toList
    if (needsRestParam) fixedParams :+ genRestFormalArg()
    else fixedParams
  }

  private def genFormalArg(index: Int)(implicit pos: Position): js.ParamDef = {
    js.ParamDef(js.Ident("arg$" + index), jstpe.AnyType,
        mutable = false, rest = false)
  }

  private def genRestFormalArg()(implicit pos: Position): js.ParamDef = {
    js.ParamDef(js.Ident("arg$rest"), jstpe.AnyType,
        mutable = false, rest = true)
  }

  private def genFormalArgRef(index: Int, minArgc: Int)(
      implicit pos: Position): js.Tree = {
    if (index <= minArgc)
      js.VarRef(js.Ident("arg$" + index))(jstpe.AnyType)
    else
      js.JSBracketSelect(genRestArgRef(), js.IntLiteral(index - 1 - minArgc))
  }

  private def genVarargRef(fixedParamCount: Int, minArgc: Int)(
      implicit pos: Position): js.Tree = {
    val restParam = genRestArgRef()
    assert(fixedParamCount >= minArgc)
    if (fixedParamCount == minArgc) restParam
    else {
      js.JSBracketMethodApply(restParam, js.StringLiteral("slice"), List(
          js.IntLiteral(fixedParamCount - minArgc)))
    }
  }

  private def genRestArgRef()(implicit pos: Position): js.Tree =
    js.VarRef(js.Ident("arg$rest"))(jstpe.AnyType)

  private def hasRepeatedParam(sym: Symbol) =
    enteringPhase(currentRun.uncurryPhase) {
      sym.paramss.flatten.lastOption.exists(isRepeated _)
  }

}
