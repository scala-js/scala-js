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
import ir.Infos.MethodInfoBuilder

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

  trait JSExportsPhase { this: JSCodePhase =>

    /**
     * Generate exporter methods for a class
     * @param classSym symbol of class we export for
     * @param decldExports symbols exporter methods that have been encountered in
     *   the class' tree. This is not the same as classSym.info.delcs since
     *   inherited concrete methods from traits should be in this param, too
     */
    def genMemberExports(
        classSym: Symbol,
        decldExports: List[Symbol]): List[js.Tree] = {

      val newlyDecldExports = decldExports.filterNot { isOverridingExport _ }
      val newlyDecldExportNames =
        newlyDecldExports.map(_.name.toTermName).toList.distinct

      newlyDecldExportNames map { genMemberExport(classSym, _) }
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
        withScopedVars(
          currentMethodInfoBuilder := new MethodInfoBuilder
        ) {
          currentMethodInfoBuilder
            .setEncodedName(ir.Definitions.ExportedConstructorsName)
            .setIsExported(true)

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
              withNewLocalNameScope(genExportMethod(ctors, jsName))

            js.ConstructorExportDef(jsName, args, body)
          }

          currentClassInfoBuilder.addMethod(currentMethodInfoBuilder.result())

          exports.toList
        }
      }
    }

    def genModuleAccessorExports(classSym: Symbol): List[js.ModuleExportDef] = {
      for {
        exp <- jsInterop.registeredExportsOf(classSym)
      } yield {
        implicit val pos = exp.pos
        assert(!exp.isNamed, "Module cannot be exported named")
        js.ModuleExportDef(exp.jsName)
      }
    }

    /** Generate the exporter proxy for a named export */
    def genNamedExporterDef(dd: DefDef): js.MethodDef = {
      implicit val pos = dd.pos

      val sym = dd.symbol

      val Block(Apply(fun, _) :: Nil, _) = dd.rhs
      val trgSym = fun.symbol

      val inArg =
        js.ParamDef(js.Ident("namedParams"), jstpe.AnyType,
            mutable = false, rest = false)
      val inArgRef = inArg.ref

      val methodIdent = encodeMethodSym(sym)

      withScopedVars(
          currentMethodInfoBuilder := new MethodInfoBuilder
      ) {
        currentMethodInfoBuilder.setEncodedName(methodIdent.name)

        val result = js.MethodDef(static = false, methodIdent,
            List(inArg), toIRType(sym.tpe.resultType),
            genNamedExporterBody(trgSym, inArg.ref))(
            OptimizerHints.empty, None)

        currentClassInfoBuilder.addMethod(currentMethodInfoBuilder.result())
        result
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
      val jsResult = genResult(trgSym, jsArgPrep.map(_.ref))

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

      withScopedVars(
        currentMethodInfoBuilder := new MethodInfoBuilder
      ) {
        currentMethodInfoBuilder
          .setEncodedName(jsName)
          .setIsExported(true)

        val result = withNewLocalNameScope {
          if (isProp)
            genExportProperty(alts, jsName)
          else
            genExportMethod(alts.map(ExportedSymbol), jsName)
        }

        currentClassInfoBuilder.addMethod(currentMethodInfoBuilder.result())
        result
      }
    }

    private def genExportProperty(alts: List[Symbol], jsName: String) = {
      assert(!alts.isEmpty)
      implicit val pos = alts.head.pos

      // Separate getters and setters. Somehow isJSGetter doesn't work here. Hence
      // we just check the parameter list length.
      val (getter, setters) = alts.partition(_.tpe.params.isEmpty)

      // if we have more than one getter, something went horribly wrong
      assert(getter.size <= 1,
          s"Found more than one getter to export for name ${jsName}.")

      val getTree =
        if (getter.isEmpty) js.EmptyTree
        else genApplyForSym(0, getter.head)

      val setTree =
        if (setters.isEmpty) js.EmptyTree
        else genExportSameArgc(1, setters.map(ExportedSymbol), 0) // we only have 1 argument

      js.PropertyDef(js.StringLiteral(jsName), getTree, genFormalArg(1), setTree)
    }

    /** generates the exporter function (i.e. exporter for non-properties) for
     *  a given name */
    private def genExportMethod(alts0: List[Exported], jsName: String) = {
      assert(alts0.nonEmpty,
          "need at least one alternative to generate exporter method")

      implicit val pos = alts0.head.pos

      val alts = {
        // toString() is always exported. We might need to add it here
        // to get correct overloading.
        if (jsName == "toString" && alts0.forall(_.params.nonEmpty))
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
          genExportSameArgc(minArgc, methods.toList, 0, Some(argcs.min))

        // argc in reverse order
        argcList = argcs.toList.sortBy(- _)
      } yield (argcList.map(argc => js.IntLiteral(argc - minArgc)), caseBody)

      def defaultCase = {
        if (!hasVarArg)
          genThrowTypeError()
        else
          genExportSameArgc(minArgc, varArgMeths, 0)
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

      js.MethodDef(static = false, js.StringLiteral(jsName),
          formalArgs, jstpe.AnyType, body)(OptimizerHints.empty, None)
    }

    /**
     * Resolve method calls to [[alts]] while assuming they have the same
     * parameter count.
     * @param minArgc The minimum number of arguments that must be given
     * @param alts Alternative methods
     * @param paramIndex Index where to start disambiguation
     * @param maxArgc only use that many arguments
     */
    private def genExportSameArgc(minArgc: Int, alts: List[Exported],
        paramIndex: Int, maxArgc: Option[Int] = None): js.Tree = {

      implicit val pos = alts.head.pos

      if (alts.size == 1)
        alts.head.genBody(minArgc)
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
            // get parameter type while resolving repeated params
            val tpe = enteringPhase(currentRun.uncurryPhase) {
              val ps = alt.paramss.flatten
              if (ps.size <= paramIndex || isRepeated(ps(paramIndex))) {
                assert(isRepeated(ps.last))
                repeatedToSingle(ps.last.tpe)
              } else {
                enteringPhase(currentRun.posterasurePhase) {
                  ps(paramIndex).tpe
                }
              }
            }

            typeTestForTpe(tpe)

          case ex: ExportedBody =>
            typeTestForTpe(ex.params(paramIndex))
        }

        if (altsByTypeTest.size == 1) {
          // Testing this parameter is not doing any us good
          genExportSameArgc(minArgc, alts, paramIndex+1, maxArgc)
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
            val genSubAlts = genExportSameArgc(minArgc,
                subAlts, paramIndex+1, maxArgc)

            def hasDefaultParam = subAlts.exists {
              case ExportedSymbol(p) =>
                val params = p.tpe.params
                params.size > paramIndex &&
                params(paramIndex).hasFlag(Flags.DEFAULTPARAM)
              case _: ExportedBody => false
            }

            val optCond = typeTest match {
              case HijackedTypeTest(boxedClassName, _) =>
                val tpe = jstpe.ClassType(boxedClassName)
                currentMethodInfoBuilder.addUsedInstanceTest(tpe)
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

    /**
     * Generate a call to the method [[sym]] while using the formalArguments
     * and potentially the argument array. Also inserts default parameters if
     * required.
     */
    private def genApplyForSym(minArgc: Int, sym: Symbol): js.Tree = {
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
      val jsVarArg = repeatedTpe map { tpe =>
        // new WrappedArray(varargs)
        genNew(WrappedArrayClass, WrappedArray_ctor, List(
            genVarargRef(normalArgc, minArgc)))
      }

      // normal arguments
      val jsArgRefs = (1 to normalArgc).toList.map(
          i => genFormalArgRef(i, minArgc))

      // Generate JS code to prepare arguments (default getters and unboxes)
      val jsArgPrep = genPrepareArgs(jsArgRefs, sym)
      val jsResult = genResult(sym, jsArgPrep.map(_.ref) ++ jsVarArg)

      js.Block(jsArgPrep :+ jsResult)
    }

    /** Generate the necessary JavaScript code to prepare the arguments of an
     *  exported method (unboxing and default parameter handling)
     */
    private def genPrepareArgs(jsArgs: List[js.Tree], sym: Symbol)(
        implicit pos: Position): List[js.VarDef] = {

      val result = new mutable.ListBuffer[js.VarDef]

      val funTpe = enteringPhase(currentRun.posterasurePhase)(sym.tpe)
      for {
        (jsArg, (param, i)) <- jsArgs zip funTpe.params.zipWithIndex
      } yield {
        // Code to verify the type of the argument (if it is defined)
        val verifiedArg = {
          val tpePosterasure =
            enteringPhase(currentRun.posterasurePhase)(param.tpe)
          tpePosterasure match {
            case tpe if isPrimitiveValueType(tpe) =>
              val unboxed = makePrimitiveUnbox(jsArg, tpe)
              // Ensure we don't convert null to a primitive value type
              js.If(js.BinaryOp(js.BinaryOp.===, jsArg, js.Null()),
                genThrowTypeError(s"Found null, expected $tpe"),
                unboxed)(unboxed.tpe)
            case tpe: ErasedValueType =>
              val boxedClass = tpe.valueClazz
              val unboxMethod = boxedClass.derivedValueClassUnbox
              genApplyMethod(
                  genAsInstanceOf(jsArg, tpe), unboxMethod, Nil)
            case tpe =>
              genAsInstanceOf(jsArg, tpe)
          }
        }

        // If argument is undefined and there is a default getter, call it
        val verifiedOrDefault = if (param.hasFlag(Flags.DEFAULTPARAM)) {
          js.If(js.BinaryOp(js.BinaryOp.===, jsArg, js.Undefined()), {
            val trgSym = {
              if (sym.isClassConstructor) sym.owner.companionModule.moduleClass
              else sym.owner
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
            genApplyMethod(trgTree, defaultGetter,
                result.take(defaultGetter.tpe.params.size).toList.map(_.ref))
          }, {
            // Otherwise, unbox the argument
            verifiedArg
          })(verifiedArg.tpe)
        } else {
          // Otherwise, it is always the unboxed argument
          verifiedArg
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
    private def genResult(sym: Symbol,
        args: List[js.Tree])(implicit pos: Position) = {
      val thisType =
        if (sym.owner == ObjectClass) jstpe.ClassType(ir.Definitions.ObjectClass)
        else encodeClassType(sym.owner)
      val receiver = js.This()(thisType)
      val call =
        if (sym.isClassConstructor)
          genApplyMethodStatically(receiver, sym, args)
        else
          genApplyMethod(receiver, sym, args)
      ensureBoxed(call,
          enteringPhase(currentRun.posterasurePhase)(sym.tpe.resultType))
    }

    private sealed abstract class Exported {
      def pos: Position
      def params: List[Type]
      def genBody(minArgc: Int): js.Tree
      def name: String
      def typeInfo: String
      def hasRepeatedParam: Boolean
    }

    private case class ExportedSymbol(sym: Symbol) extends Exported {
      def pos: Position = sym.pos
      def params: List[Type] = sym.tpe.params.map(_.tpe)
      def genBody(minArgc: Int): js.Tree = genApplyForSym(minArgc, sym)
      def name: String = sym.name.toString
      def typeInfo: String = sym.tpe.toString
      def hasRepeatedParam: Boolean = GenJSExports.this.hasRepeatedParam(sym)
    }

    private case class ExportedBody(params: List[Type], body: js.Tree,
        name: String, pos: Position) extends Exported {
      def genBody(minArgc: Int): js.Tree = body
      def typeInfo: String = params.mkString("(", ", ", ")")
      val hasRepeatedParam: Boolean = false
    }
  }

  private def isOverridingExport(sym: Symbol): Boolean = {
    lazy val osym = sym.nextOverriddenSymbol
    sym.isOverridingSymbol && !osym.owner.isInterface
  }

  private sealed abstract class RTTypeTest

  private final case class HijackedTypeTest(
      boxedClassName: String, rank: Int) extends RTTypeTest

  private final case class InstanceOfTypeTest(tpe: Type) extends RTTypeTest {
    override def equals(that: Any): Boolean = {
      that match {
        case InstanceOfTypeTest(thatTpe) => tpe =:= thatTpe
        case _ => false
      }
    }
  }

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
