/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.nscplugin

import scala.collection.{immutable, mutable}

import scala.tools.nsc._
import scala.math.PartialOrdering
import scala.reflect.{ClassTag, classTag}
import scala.reflect.internal.Flags

import org.scalajs.ir
import org.scalajs.ir.{Trees => js, Types => jstpe}
import org.scalajs.ir.Names.LocalName
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees.OptimizerHints

import org.scalajs.nscplugin.util.ScopedVar
import ScopedVar.withScopedVars

/** Generation of exports for JavaScript
 *
 *  @author Sébastien Doeraene
 */
trait GenJSExports[G <: Global with Singleton] extends SubComponent {
  self: GenJSCode[G] =>

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
    def genMemberExports(classSym: Symbol): List[js.MemberDef] = {
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
        dispatchMethodsNames: List[JSName]): List[js.MemberDef] = {
      dispatchMethodsNames
        .map(genJSClassDispatcher(classSym, _))
    }

    def genConstructorExports(
        classSym: Symbol): List[js.TopLevelMethodExportDef] = {

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
          val ctors = specs.map(s => ExportedSymbol(s._2))

          implicit val pos = ctors.head.pos

          val methodDef = withNewLocalNameScope {
            genExportMethod(ctors, JSName.Literal(jsName), static = true)
          }

          js.TopLevelMethodExportDef(methodDef)
        }

        exports.toList
      }
    }

    def genJSClassExports(
        classSym: Symbol): List[js.TopLevelJSClassExportDef] = {
      for {
        exp <- jsInterop.registeredExportsOf(classSym)
      } yield {
        implicit val pos = exp.pos

        exp.destination match {
          case ExportDestination.Normal | ExportDestination.TopLevel =>
            js.TopLevelJSClassExportDef(exp.jsName)
          case ExportDestination.Static =>
            throw new AssertionError(
                "Found a class export static for " + classSym.fullName)
        }
      }
    }

    def genModuleAccessorExports(
        classSym: Symbol): List[js.TopLevelExportDef] = {

      for {
        exp <- jsInterop.registeredExportsOf(classSym)
      } yield {
        implicit val pos = exp.pos

        exp.destination match {
          case ExportDestination.Normal =>
            throw new AssertionError(
                "Found a non-top-level module export for " + classSym.fullName)
          case ExportDestination.TopLevel =>
            js.TopLevelModuleExportDef(exp.jsName)
          case ExportDestination.Static =>
            throw new AssertionError(
                "Found a module export static for " + classSym.fullName)
        }
      }
    }

    def genTopLevelExports(classSym: Symbol): List[js.TopLevelExportDef] =
      genTopLevelOrStaticExports[js.TopLevelExportDef](classSym, ExportDestination.TopLevel)

    def genStaticExports(classSym: Symbol): List[js.MemberDef] =
      genTopLevelOrStaticExports[js.MemberDef](classSym, ExportDestination.Static)

    private def genTopLevelOrStaticExports[A <: js.IRNode: ClassTag](
        classSym: Symbol, destination: ExportDestination): List[A] = {
      require(
          destination == ExportDestination.TopLevel ||
          destination == ExportDestination.Static,
          destination)

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

    private def genTopLevelOrStaticFieldExports[A <: js.IRNode: ClassTag](
        classSym: Symbol,
        destination: ExportDestination): List[(A, String, Position)] = {
      (for {
        fieldSym <- classSym.info.members
        if !fieldSym.isMethod && fieldSym.isTerm && !fieldSym.isModule
        export <- jsInterop.registeredExportsOf(fieldSym)
        if export.destination == destination
      } yield {
        implicit val pos = fieldSym.pos

        val tree = if (destination == ExportDestination.Static) {
          // static fields must always be mutable
          val flags = js.MemberFlags.empty
            .withNamespace(js.MemberNamespace.PublicStatic)
            .withMutable(true)
          val name = js.StringLiteral(export.jsName)
          val irTpe = genExposedFieldIRType(fieldSym)
          checkedCast[A](js.JSFieldDef(flags, name, irTpe))
        } else {
          checkedCast[A](
              js.TopLevelFieldExportDef(export.jsName, encodeFieldSym(fieldSym)))
        }

        (tree, export.jsName, pos)
      }).toList
    }

    private def genTopLevelOrStaticMethodExports[A <: js.IRNode: ClassTag](
        classSym: Symbol,
        destination: ExportDestination): List[(A, String, Position)] = {
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

        val exportDef = {
          if (destination == ExportDestination.Static)
            checkedCast[A](exportedMember)
          else
            checkedCast[A](js.TopLevelMethodExportDef(exportedMember.asInstanceOf[js.JSMethodDef]))
        }

        (exportDef, jsName, pos)
      }
    }

    private def checkedCast[A: ClassTag](x: js.IRNode): A =
      classTag[A].runtimeClass.asInstanceOf[Class[A]].cast(x)

    private def genMemberExport(classSym: Symbol, name: TermName): js.MemberDef = {
      /* This used to be `.member(name)`, but it caused #3538, since we were
       * sometimes selecting mixin forwarders, whose type history does not go
       * far enough back in time to see varargs. We now explicitly exclude
       * mixed-in members in addition to bridge methods (the latter are always
       * excluded by `.member(name)`).
       */
      val alts = classSym.info.memberBasedOnName(name,
          excludedFlags = Flags.BRIDGE | Flags.MIXEDIN).alternatives

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

    private def genJSClassDispatcher(classSym: Symbol, name: JSName): js.MemberDef = {
      val alts = classSym.info.members.toList.filter { sym =>
        sym.isMethod && !sym.isBridge && jsNameOf(sym) == name
      }

      assert(!alts.isEmpty,
          s"Ended up with no alternatives for ${classSym.fullName}::$name.")

      val (propSyms, methodSyms) = alts.partition(jsInterop.isJSProperty(_))
      val isProp = propSyms.nonEmpty

      if (isProp && methodSyms.nonEmpty) {
        reporter.error(alts.head.pos,
            s"Conflicting properties and methods for ${classSym.fullName}::$name.")
        implicit val pos = alts.head.pos
        js.JSPropertyDef(js.MemberFlags.empty, genExpr(name), None, None)
      } else {
        genMemberExportOrDispatcher(classSym, name, isProp, alts,
            static = false)
      }
    }

    def genMemberExportOrDispatcher(classSym: Symbol, jsName: JSName,
        isProp: Boolean, alts: List[Symbol], static: Boolean): js.MemberDef = {
      withNewLocalNameScope {
        if (isProp)
          genExportProperty(alts, jsName, static)
        else
          genExportMethod(alts.map(ExportedSymbol), jsName, static)
      }
    }

    def genJSConstructorExport(
        alts: List[Symbol]): (Option[List[js.ParamDef]], js.JSMethodDef) = {
      val exporteds = alts.map(ExportedSymbol)

      val isLiftedJSCtor = exporteds.head.isLiftedJSConstructor
      assert(exporteds.tail.forall(_.isLiftedJSConstructor == isLiftedJSCtor),
          s"Alternative constructors $alts do not agree on whether they are " +
          "lifted JS constructors or not")
      val captureParams = if (!isLiftedJSCtor) {
        None
      } else {
        Some(for {
          exported <- exporteds
          param <- exported.captureParamsFront ::: exported.captureParamsBack
        } yield {
          genParamDef(param.sym)
        })
      }

      val ctorDef = genExportMethod(exporteds, JSName.Literal("constructor"),
          static = false)

      (captureParams, ctorDef)
    }

    private def genExportProperty(alts: List[Symbol], jsName: JSName,
        static: Boolean): js.JSPropertyDef = {
      assert(!alts.isEmpty,
          s"genExportProperty with empty alternatives for $jsName")

      implicit val pos = alts.head.pos

      val namespace =
        if (static) js.MemberNamespace.PublicStatic
        else js.MemberNamespace.Public
      val flags = js.MemberFlags.empty.withNamespace(namespace)

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

      val getterBody = getter.headOption.map { getterSym =>
        genApplyForSym(new FormalArgsRegistry(0, false),
            ExportedSymbol(getterSym), static)
      }

      val setterArgAndBody = {
        if (setters.isEmpty) {
          None
        } else {
          val formalArgsRegistry = new FormalArgsRegistry(1, false)
          val List(arg) = formalArgsRegistry.genFormalArgs()
          val body = genExportSameArgc(jsName, formalArgsRegistry,
              alts = setters.map(ExportedSymbol),
              paramIndex = 0, static = static)
          Some((arg, body))
        }
      }

      js.JSPropertyDef(flags, genExpr(jsName), getterBody, setterArgAndBody)
    }

    /** generates the exporter function (i.e. exporter for non-properties) for
     *  a given name */
    private def genExportMethod(alts0: List[Exported], jsName: JSName,
        static: Boolean): js.JSMethodDef = {
      assert(alts0.nonEmpty,
          "need at least one alternative to generate exporter method")

      implicit val pos = alts0.head.pos

      val namespace =
        if (static) js.MemberNamespace.PublicStatic
        else js.MemberNamespace.Public
      val flags = js.MemberFlags.empty.withNamespace(namespace)

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
      def argCounts(ex: Exported) = {
        val params = ex.params
        // Find default param
        val dParam = params.indexWhere(_.hasDefault)
        if (dParam == -1) Seq(params.size)
        else dParam to params.size
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
        methodArgCounts.groupBy(_._1).map(kv => kv._1 -> kv._2.map(_._2).toSet)

      // Create the formal args registry
      val minArgc = methodByArgCount.keys.min
      val hasVarArg = varArgMeths.nonEmpty
      val needsRestParam = maxArgc != minArgc || hasVarArg
      val formalArgsRegistry = new FormalArgsRegistry(minArgc, needsRestParam)

      // List of formal parameters
      val formalArgs = formalArgsRegistry.genFormalArgs()

      // Create tuples: (methods, argCounts). This will be the cases we generate
      val caseDefinitions =
        methodByArgCount.groupBy(_._2).map(kv => kv._1 -> kv._2.keySet)

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
        caseBody = genExportSameArgc(jsName, formalArgsRegistry,
            methods.toList, paramIndex = 0, static, Some(argcs.min))

        // argc in reverse order
        argcList = argcs.toList.sortBy(- _)
      } yield (argcList.map(argc => js.IntLiteral(argc - minArgc)), caseBody)

      def defaultCase = {
        if (!hasVarArg) {
          genThrowTypeError()
        } else {
          genExportSameArgc(jsName, formalArgsRegistry, varArgMeths,
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
          val restArgRef = formalArgsRegistry.genRestArgRef()
          js.Match(
              js.AsInstanceOf(js.JSSelect(restArgRef, js.StringLiteral("length")), jstpe.IntType),
              cases.toList, defaultCase)(jstpe.AnyType)
        }
      }

      js.JSMethodDef(flags, genExpr(jsName), formalArgs, body)(
          OptimizerHints.empty, None)
    }

    /**
     * Resolve method calls to [[alts]] while assuming they have the same
     * parameter count.
     * @param minArgc The minimum number of arguments that must be given
     * @param alts Alternative methods
     * @param paramIndex Index where to start disambiguation
     * @param maxArgc only use that many arguments
     */
    private def genExportSameArgc(jsName: JSName,
        formalArgsRegistry: FormalArgsRegistry, alts: List[Exported],
        paramIndex: Int, static: Boolean,
        maxArgc: Option[Int] = None): js.Tree = {

      implicit val pos = alts.head.pos

      if (alts.size == 1) {
        alts.head.genBody(formalArgsRegistry, static)
      } else if (maxArgc.exists(_ <= paramIndex) ||
        !alts.exists(_.params.size > paramIndex)) {
        // We reach here in three cases:
        // 1. The parameter list has been exhausted
        // 2. The optional argument count restriction has triggered
        // 3. We only have (more than once) repeated parameters left
        // Therefore, we should fail
        reportCannotDisambiguateError(jsName, alts)
        js.Undefined()
      } else {
        val altsByTypeTest = groupByWithoutHashCode(alts) { exported =>
          typeTestForTpe(exported.exportArgTypeAt(paramIndex))
        }

        if (altsByTypeTest.size == 1) {
          // Testing this parameter is not doing any us good
          genExportSameArgc(jsName, formalArgsRegistry, alts, paramIndex + 1,
              static, maxArgc)
        } else {
          // Sort them so that, e.g., isInstanceOf[String]
          // comes before isInstanceOf[Object]
          val sortedAltsByTypeTest = topoSortDistinctsBy(
              altsByTypeTest)(_._1)(RTTypeTest.Ordering)

          val defaultCase = genThrowTypeError()

          sortedAltsByTypeTest.foldRight[js.Tree](defaultCase) { (elem, elsep) =>
            val (typeTest, subAlts) = elem
            implicit val pos = subAlts.head.pos

            val paramRef = formalArgsRegistry.genArgRef(paramIndex)
            val genSubAlts = genExportSameArgc(jsName, formalArgsRegistry,
                subAlts, paramIndex + 1, static, maxArgc)

            def hasDefaultParam = subAlts.exists { exported =>
              val params = exported.params
              params.size > paramIndex &&
              params(paramIndex).hasDefault
            }

            val optCond = typeTest match {
              case PrimitiveTypeTest(tpe, _) =>
                Some(js.IsInstanceOf(paramRef, tpe))

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

    private def reportCannotDisambiguateError(jsName: JSName,
        alts: List[Exported]): Unit = {
      val currentClass = currentClassSym.get

      /* Find a position that is in the current class for decent error reporting.
       * If there are more than one, always use the "highest" one (i.e., the
       * one coming last in the source text) so that we reliably display the
       * same error in all compilers.
       */
      val validPositions = alts.collect {
        case alt if alt.sym.owner == currentClass => alt.sym.pos
      }
      val pos =
        if (validPositions.isEmpty) currentClass.pos
        else validPositions.maxBy(_.point)

      val kind =
        if (isNonNativeJSClass(currentClass)) "method"
        else "exported method"

      val displayName = jsName.displayName
      val altsTypesInfo = alts.map(_.typeInfo).sorted.mkString("\n  ")

      reporter.error(pos,
          s"Cannot disambiguate overloads for $kind $displayName with types\n" +
          s"  $altsTypesInfo")
    }

    /**
     * Generate a call to the method [[sym]] while using the formalArguments
     * and potentially the argument array. Also inserts default parameters if
     * required.
     */
    private def genApplyForSym(formalArgsRegistry: FormalArgsRegistry,
        exported: Exported, static: Boolean): js.Tree = {
      if (isNonNativeJSClass(currentClassSym) &&
          exported.sym.owner != currentClassSym.get) {
        assert(!static,
            s"nonsensical JS super call in static export of ${exported.sym}")
        genApplyForSymJSSuperCall(formalArgsRegistry, exported)
      } else {
        genApplyForSymNonJSSuperCall(formalArgsRegistry, exported, static)
      }
    }

    private def genApplyForSymJSSuperCall(
        formalArgsRegistry: FormalArgsRegistry, exported: Exported): js.Tree = {
      implicit val pos = exported.pos

      val sym = exported.sym
      assert(!sym.isClassConstructor,
          "Trying to genApplyForSymJSSuperCall for the constructor " +
          sym.fullName)

      val allArgs = formalArgsRegistry.genAllArgsRefsForForwarder()

      val superClass = {
        val superClassSym = currentClassSym.superClass
        if (isNestedJSClass(superClassSym)) {
          js.VarRef(js.LocalIdent(JSSuperClassParamName))(jstpe.AnyType)
        } else {
          js.LoadJSConstructor(encodeClassName(superClassSym))
        }
      }

      val receiver = js.This()(jstpe.AnyType)
      val nameString = genExpr(jsNameOf(sym))

      if (jsInterop.isJSGetter(sym)) {
        assert(allArgs.isEmpty,
            s"getter symbol $sym does not have a getter signature")
        js.JSSuperSelect(superClass, receiver, nameString)
      } else if (jsInterop.isJSSetter(sym)) {
        assert(allArgs.size == 1 && allArgs.head.isInstanceOf[js.Tree],
            s"setter symbol $sym does not have a setter signature")
        js.Assign(js.JSSuperSelect(superClass, receiver, nameString),
            allArgs.head.asInstanceOf[js.Tree])
      } else {
        js.JSSuperMethodCall(superClass, receiver, nameString, allArgs)
      }
    }

    private def genApplyForSymNonJSSuperCall(
        formalArgsRegistry: FormalArgsRegistry, exported: Exported,
        static: Boolean): js.Tree = {
      implicit val pos = exported.pos

      // the (single) type of the repeated parameter if any
      val repeatedTpe =
        exported.params.lastOption.withFilter(_.isRepeated).map(_.tpe)

      val normalArgc = exported.params.size -
        (if (repeatedTpe.isDefined) 1 else 0)

      // optional repeated parameter list
      val jsVarArgPrep = repeatedTpe map { tpe =>
        val rhs = genJSArrayToVarArgs(formalArgsRegistry.genVarargRef(normalArgc))
        val ident = freshLocalIdent("prep" + normalArgc)
        js.VarDef(ident, NoOriginalName, rhs.tpe, mutable = false, rhs)
      }

      // normal arguments
      val jsArgRefs =
        (0 until normalArgc).toList.map(formalArgsRegistry.genArgRef(_))

      // Generate JS code to prepare arguments (default getters and unboxes)
      val jsArgPrep = genPrepareArgs(jsArgRefs, exported) ++ jsVarArgPrep
      val jsArgPrepRefs = jsArgPrep.map(_.ref)

      // Combine prep'ed formal arguments with captures
      def varRefForCaptureParam(param: ParamSpec): js.Tree =
        js.VarRef(encodeLocalSym(param.sym))(toIRType(param.sym.tpe))
      val allJSArgs = {
        exported.captureParamsFront.map(varRefForCaptureParam) :::
        jsArgPrepRefs :::
        exported.captureParamsBack.map(varRefForCaptureParam)
      }

      val jsResult = genResult(exported, allJSArgs, static)

      js.Block(jsArgPrep :+ jsResult)
    }

    /** Generate the necessary JavaScript code to prepare the arguments of an
     *  exported method (unboxing and default parameter handling)
     */
    private def genPrepareArgs(jsArgs: List[js.Tree], exported: Exported)(
        implicit pos: Position): List[js.VarDef] = {

      val result = new mutable.ListBuffer[js.VarDef]

      for {
        (jsArg, (param, i)) <- jsArgs.zip(exported.params.zipWithIndex)
      } yield {
        // Unboxed argument (if it is defined)
        val unboxedArg = fromAny(jsArg, param.tpe)

        // If argument is undefined and there is a default getter, call it
        val verifiedOrDefault = if (param.hasDefault) {
          js.If(js.BinaryOp(js.BinaryOp.===, jsArg, js.Undefined()), {
            genCallDefaultGetter(exported.sym, i, param.sym.pos) {
              prevArgsCount => result.take(prevArgsCount).toList.map(_.ref)
            }
          }, {
            // Otherwise, unbox the argument
            unboxedArg
          })(unboxedArg.tpe)
        } else {
          // Otherwise, it is always the unboxed argument
          unboxedArg
        }

        result += js.VarDef(freshLocalIdent("prep" + i), NoOriginalName,
            verifiedOrDefault.tpe, mutable = false, verifiedOrDefault)
      }

      result.toList
    }

    private def genCallDefaultGetter(sym: Symbol, paramIndex: Int,
        paramPos: Position)(
        previousArgsValues: Int => List[js.Tree])(
        implicit pos: Position): js.Tree = {

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
          nme.defaultGetterName(sym.name, paramIndex + 1))

      assert(defaultGetter.exists,
          s"need default getter for method ${sym.fullName}")
      assert(!defaultGetter.isOverloaded,
          s"found overloaded default getter $defaultGetter")

      val trgTree = {
        if (sym.isClassConstructor) genLoadModule(trgSym)
        else js.This()(encodeClassType(trgSym))
      }

      // Pass previous arguments to defaultGetter
      val defaultGetterArgs = previousArgsValues(defaultGetter.tpe.params.size)

      if (isJSType(trgSym)) {
        if (isNonNativeJSClass(defaultGetter.owner)) {
          genApplyJSClassMethod(trgTree, defaultGetter, defaultGetterArgs)
        } else {
          reporter.error(paramPos, "When overriding a native method " +
              "with default arguments, the overriding method must " +
              "explicitly repeat the default arguments.")
          js.Undefined()
        }
      } else {
        genApplyMethod(trgTree, defaultGetter, defaultGetterArgs)
      }
    }

    /** Generate the final forwarding call to the exported method. */
    private def genResult(exported: Exported, args: List[js.Tree],
        static: Boolean)(implicit pos: Position): js.Tree = {
      val sym = exported.sym

      def receiver = {
        if (static)
          genLoadModule(sym.owner)
        else if (sym.owner == ObjectClass)
          js.This()(jstpe.ClassType(ir.Names.ObjectClass))
        else
          js.This()(encodeClassType(sym.owner))
      }

      def boxIfNeeded(call: js.Tree): js.Tree = {
        ensureBoxed(call,
            enteringPhase(currentRun.posterasurePhase)(sym.tpe.resultType))
      }

      if (isNonNativeJSClass(currentClassSym)) {
        assert(sym.owner == currentClassSym.get, sym.fullName)
        boxIfNeeded(genApplyJSClassMethod(receiver, sym, args))
      } else {
        if (sym.isClassConstructor)
          genNew(currentClassSym, sym, args)
        else if (sym.isPrivate)
          boxIfNeeded(genApplyMethodStatically(receiver, sym, args))
        else
          boxIfNeeded(genApplyMethod(receiver, sym, args))
      }
    }

    private final class ParamSpec(val sym: Symbol, val tpe: Type,
        val isRepeated: Boolean, val hasDefault: Boolean) {
      override def toString(): String =
        s"ParamSpec(${sym.name}, $tpe, $isRepeated, $hasDefault)"
    }

    private object ParamSpec extends (Symbol => ParamSpec) {
      def apply(sym: Symbol): ParamSpec = {
        val hasDefault = sym.hasFlag(Flags.DEFAULTPARAM)
        val repeated = isRepeated(sym)
        val tpe = if (repeated) repeatedToSingle(sym.tpe) else sym.tpe
        new ParamSpec(sym, tpe, repeated, hasDefault)
      }
    }

    private sealed abstract class Exported {
      def sym: Symbol
      def pos: Position
      def isLiftedJSConstructor: Boolean
      def params: immutable.IndexedSeq[ParamSpec]
      def captureParamsFront: List[ParamSpec]
      def captureParamsBack: List[ParamSpec]
      def exportArgTypeAt(paramIndex: Int): Type
      def genBody(formalArgsRegistry: FormalArgsRegistry, static: Boolean): js.Tree
      def typeInfo: String
      def hasRepeatedParam: Boolean
    }

    private case class ExportedSymbol(sym: Symbol) extends Exported {
      private val isAnonJSClassConstructor =
        sym.isClassConstructor && sym.owner.isAnonymousClass && isJSType(sym.owner)

      val isLiftedJSConstructor =
        sym.isClassConstructor && isNestedJSClass(sym.owner)

      val (params, captureParamsFront, captureParamsBack) = {
        val allParamsUncurry =
          enteringPhase(currentRun.uncurryPhase)(sym.paramss.flatten.map(ParamSpec))
        val allParamsPosterasure =
          enteringPhase(currentRun.posterasurePhase)(sym.paramss.flatten.map(ParamSpec))
        val allParamsNow = sym.paramss.flatten.map(ParamSpec)

        def mergeUncurryPosterasure(paramsUncurry: List[ParamSpec],
            paramsPosterasure: List[ParamSpec]): List[ParamSpec] = {
          for {
            (paramUncurry, paramPosterasure) <- paramsUncurry.zip(paramsPosterasure)
          } yield {
            if (paramUncurry.isRepeated) paramUncurry
            else paramPosterasure
          }
        }

        if (!isLiftedJSConstructor && !isAnonJSClassConstructor) {
          /* Easy case: all params are formal params, and we only need to
           * travel back before uncurry to handle repeated params, or before
           * posterasure for other params.
           */
          assert(allParamsUncurry.size == allParamsPosterasure.size,
              s"Found ${allParamsUncurry.size} params entering uncurry but " +
              s"${allParamsPosterasure.size} params entering posterasure for " +
              s"non-lifted symbol ${sym.fullName}")
          val formalParams =
            mergeUncurryPosterasure(allParamsUncurry, allParamsPosterasure)
          (formalParams.toIndexedSeq, Nil, Nil)
        } else {
          /* The `arg$outer` param is added by explicitouter (between uncurry
           * and posterasure) while the other capture params are added by
           * lambdalift (between posterasure and now).
           *
           * Note that lambdalift creates new symbols even for parameters that
           * are not the result of lambda lifting, but it preserves their
           * `name`s.
           */

          val hasOuterParam = {
            allParamsPosterasure.size == allParamsUncurry.size + 1 &&
            allParamsPosterasure.head.sym.name == jsnme.arg_outer
          }
          assert(
              hasOuterParam ||
              allParamsPosterasure.size == allParamsUncurry.size,
              s"Found ${allParamsUncurry.size} params entering uncurry but " +
              s"${allParamsPosterasure.size} params entering posterasure for " +
              s"lifted constructor symbol ${sym.fullName}")

          val nonOuterParamsPosterasure =
            if (hasOuterParam) allParamsPosterasure.tail
            else allParamsPosterasure
          val formalParams =
            mergeUncurryPosterasure(allParamsUncurry, nonOuterParamsPosterasure)

          val startOfRealParams =
            allParamsNow.map(_.sym.name).indexOfSlice(allParamsUncurry.map(_.sym.name))
          val (captureParamsFront, restOfParamsNow) =
            allParamsNow.splitAt(startOfRealParams)
          val captureParamsBack = restOfParamsNow.drop(formalParams.size)

          if (isAnonJSClassConstructor) {
            /* For an anonymous JS class constructor, we put the capture
             * parameters back as formal parameters.
             */
            val allFormalParams =
              captureParamsFront ::: formalParams ::: captureParamsBack
            (allFormalParams.toIndexedSeq, Nil, Nil)
          } else {
            (formalParams.toIndexedSeq, captureParamsFront, captureParamsBack)
          }
        }
      }

      val hasRepeatedParam = params.nonEmpty && params.last.isRepeated

      def pos: Position = sym.pos

      def exportArgTypeAt(paramIndex: Int): Type = {
        if (paramIndex < params.length) {
          params(paramIndex).tpe
        } else {
          assert(hasRepeatedParam,
              s"$sym does not have varargs nor enough params for $paramIndex")
          params.last.tpe
        }
      }

      def genBody(formalArgsRegistry: FormalArgsRegistry, static: Boolean): js.Tree =
        genApplyForSym(formalArgsRegistry, this, static)

      def typeInfo: String = sym.tpe.toString
    }
  }

  private sealed abstract class RTTypeTest

  private case class PrimitiveTypeTest(tpe: jstpe.Type, rank: Int)
      extends RTTypeTest

  // scalastyle:off equals.hash.code
  private case class InstanceOfTypeTest(tpe: Type) extends RTTypeTest {
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

          case (PrimitiveTypeTest(_, rank1), PrimitiveTypeTest(_, rank2)) =>
            rank1 <= rank2

          case (InstanceOfTypeTest(t1), InstanceOfTypeTest(t2)) =>
            t1 <:< t2

          case (_: PrimitiveTypeTest, _: InstanceOfTypeTest) => true
          case (_: InstanceOfTypeTest, _: PrimitiveTypeTest) => false
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
        import org.scalajs.ir.Names

        (toIRType(tpe): @unchecked) match {
          case jstpe.AnyType => NoTypeTest

          case jstpe.NoType      => PrimitiveTypeTest(jstpe.UndefType, 0)
          case jstpe.BooleanType => PrimitiveTypeTest(jstpe.BooleanType, 1)
          case jstpe.CharType    => PrimitiveTypeTest(jstpe.CharType, 2)
          case jstpe.ByteType    => PrimitiveTypeTest(jstpe.ByteType, 3)
          case jstpe.ShortType   => PrimitiveTypeTest(jstpe.ShortType, 4)
          case jstpe.IntType     => PrimitiveTypeTest(jstpe.IntType, 5)
          case jstpe.LongType    => PrimitiveTypeTest(jstpe.LongType, 6)
          case jstpe.FloatType   => PrimitiveTypeTest(jstpe.FloatType, 7)
          case jstpe.DoubleType  => PrimitiveTypeTest(jstpe.DoubleType, 8)

          case jstpe.ClassType(Names.BoxedUnitClass)   => PrimitiveTypeTest(jstpe.UndefType, 0)
          case jstpe.ClassType(Names.BoxedStringClass) => PrimitiveTypeTest(jstpe.StringType, 9)
          case jstpe.ClassType(_)                      => InstanceOfTypeTest(tpe)

          case jstpe.ArrayType(_) => InstanceOfTypeTest(tpe)
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

  private class FormalArgsRegistry(minArgc: Int, needsRestParam: Boolean) {
    private val fixedParamNames: scala.collection.immutable.IndexedSeq[LocalName] =
      (0 until minArgc).toIndexedSeq.map(_ => freshLocalIdent("arg")(NoPosition).name)

    private val restParamName: LocalName =
      if (needsRestParam) freshLocalIdent("rest")(NoPosition).name
      else null

    def genFormalArgs()(implicit pos: Position): List[js.ParamDef] = {
      val fixedParamDefs = fixedParamNames.toList.map { paramName =>
        js.ParamDef(js.LocalIdent(paramName), NoOriginalName, jstpe.AnyType,
            mutable = false, rest = false)
      }

      if (needsRestParam) {
        val restParamDef = {
          js.ParamDef(js.LocalIdent(restParamName),
              NoOriginalName, jstpe.AnyType,
              mutable = false, rest = true)
        }
        fixedParamDefs :+ restParamDef
      } else {
        fixedParamDefs
      }
    }

    def genArgRef(index: Int)(implicit pos: Position): js.Tree = {
      if (index < minArgc)
        js.VarRef(js.LocalIdent(fixedParamNames(index)))(jstpe.AnyType)
      else
        js.JSSelect(genRestArgRef(), js.IntLiteral(index - minArgc))
    }

    def genVarargRef(fixedParamCount: Int)(implicit pos: Position): js.Tree = {
      val restParam = genRestArgRef()
      assert(fixedParamCount >= minArgc,
          s"genVarargRef($fixedParamCount) with minArgc = $minArgc at $pos")
      if (fixedParamCount == minArgc) {
        restParam
      } else {
        js.JSMethodApply(restParam, js.StringLiteral("slice"),
            List(js.IntLiteral(fixedParamCount - minArgc)))
      }
    }

    def genRestArgRef()(implicit pos: Position): js.Tree = {
      assert(needsRestParam,
          s"trying to generate a reference to non-existent rest param at $pos")
      js.VarRef(js.LocalIdent(restParamName))(jstpe.AnyType)
    }

    def genAllArgsRefsForForwarder()(implicit pos: Position): List[js.Tree] = {
      val fixedArgRefs = fixedParamNames.toList.map { paramName =>
        js.VarRef(js.LocalIdent(paramName))(jstpe.AnyType)
      }

      if (needsRestParam) {
        val restArgRef = js.VarRef(js.LocalIdent(restParamName))(jstpe.AnyType)
        fixedArgRefs :+ restArgRef
      } else {
        fixedArgRefs
      }
    }
  }

  private def hasRepeatedParam(sym: Symbol) = {
    enteringPhase(currentRun.uncurryPhase) {
      sym.paramss.flatten.lastOption.exists(isRepeated _)
    }
  }

}
