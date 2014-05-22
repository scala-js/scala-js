/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.collection.mutable

import scala.tools.nsc._
import scala.math.PartialOrdering
import scala.reflect.internal.Flags

import scala.scalajs.ir.{Trees => js, Types => jstpe}

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
        ctor          <- constructors
        (jsName, pos) <- jsInterop.exportsOf(ctor)
      } yield (jsName, ctor)

      val exports = for {
        (jsName, specs) <- ctorExports.groupBy(_._1) // group by exported name
      } yield {
        val ctors = specs.map(_._2)
        implicit val pos = ctors.head.pos

        val js.MethodDef(_, args, _, body) = genExportMethod(ctors, jsName)
        js.ConstructorExportDef(jsName, args, body)
      }

      exports.toList
    }

    def genModuleAccessorExports(classSym: Symbol): List[js.ModuleExportDef] = {
      for {
        (jsName, p) <- jsInterop.exportsOf(classSym)
      } yield {
        implicit val pos = p
        js.ModuleExportDef(jsName)
      }
    }

    private def genMemberExport(classSym: Symbol, name: TermName): js.Tree = {
      val alts = classSym.info.member(name).alternatives

      assert(!alts.isEmpty,
          s"Ended up with no alternatives for ${classSym.fullName}::$name. " +
          s"Original set was ${alts} with types ${alts.map(_.tpe)}")

      val (jsName, isProp) = jsInterop.jsExportInfo(name)

      if (isProp)
        genExportProperty(alts, jsName)
      else
        genExportMethod(alts, jsName)

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
        else genApplyForSym(getter.head)

      val setTree =
        if (setters.isEmpty) js.EmptyTree
        else genExportSameArgc(setters, 0) // we only have 1 argument

      js.PropertyDef(js.StringLiteral(jsName), getTree, genFormalArg(1), setTree)
    }

    /** generates the exporter function (i.e. exporter for non-properties) for
     *  a given name */
    private def genExportMethod(alts: List[Symbol], jsName: String) = {
      implicit val pos = alts.head.pos

      assert(alts.nonEmpty,
          "need at least one alternative to generate exporter method")

      // Factor out methods with variable argument lists. Note that they can
      // only be at the end of the lists as enforced by PrepJSExports
      val (varArgMeths, normalMeths) = alts.partition(hasRepeatedParam _)

      // Highest non-repeated argument count
      val maxArgc = (
          // We have argc - 1, since a repeated parameter list may also be empty
          // (unlike a normal parameter)
          varArgMeths.map(_.tpe.params.size - 1) ++
          normalMeths.map(_.tpe.params.size)
      ).max

      val formalArgs = genFormalArgs(maxArgc)

      // Calculates possible arg counts for normal method
      def argCounts(sym: Symbol) = {
        val params = sym.tpe.params
        // Find default param
        val dParam = params.indexWhere { _.hasFlag(Flags.DEFAULTPARAM) }
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
          argc   <- method.tpe.params.size - 1 to maxArgc
        } yield (argc, method)
      }

      // Create a map: argCount -> methods (methods may appear multiple times)
      val methodByArgCount =
        methodArgCounts.groupBy(_._1).mapValues(_.map(_._2).toSet)

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
          genExportSameArgc(methods.toList, 0, Some(argcs.min))

        // argc in reverse order
        argcList = argcs.toList.sortBy(- _)
      } yield (argcList.map(js.IntLiteral(_)), caseBody)

      val hasVarArg = varArgMeths.nonEmpty

      def defaultCase = {
        if (!hasVarArg)
          genThrowTypeError()
        else
          genExportSameArgc(varArgMeths, 0)
      }

      val body = {
        if (cases.isEmpty)
          defaultCase
        else if (cases.size == 1 && !hasVarArg)
          cases.head._2
        else {
          js.Match(js.JSDotSelect(
              js.VarRef(js.Ident("arguments"), false)(jstpe.DynType), js.Ident("length")),
              cases.toList, defaultCase)(jstpe.AnyType)
        }
      }

      js.MethodDef(js.StringLiteral(jsName), formalArgs, jstpe.AnyType, body)
    }

    /**
     * Resolve method calls to [[alts]] while assuming they have the same
     * parameter count.
     * @param alts Alternative methods
     * @param paramIndex Index where to start disambiguation
     * @param maxArgc only use that many arguments
     */
    private def genExportSameArgc(alts: List[Symbol],
        paramIndex: Int, maxArgc: Option[Int] = None): js.Tree = {

      implicit val pos = alts.head.pos

      if (alts.size == 1)
        genApplyForSym(alts.head)
      else if (maxArgc.exists(_ <= paramIndex) ||
        !alts.exists(_.tpe.params.size > paramIndex)) {
        // We reach here in three cases:
        // 1. The parameter list has been exhausted
        // 2. The optional argument count restriction has triggered
        // 3. We only have (more than once) repeated parameters left
        // Therefore, we should fail
        currentUnit.error(pos,
            s"""Cannot disambiguate overloads for exported method ${alts.head.name} with types
               |  ${alts.map(_.tpe).mkString("\n  ")}""".stripMargin)
        js.Undefined()
      } else {

        val altsByTypeTest = groupByWithoutHashCode(alts) { alt =>
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
        }

        if (altsByTypeTest.size == 1) {
          // Testing this parameter is not doing any us good
          genExportSameArgc(alts, paramIndex+1, maxArgc)
        } else {
          // Sort them so that, e.g., isInstanceOf[String]
          // comes before isInstanceOf[Object]
          val sortedAltsByTypeTest = topoSortDistinctsBy(
              altsByTypeTest)(_._1)(RTTypeTest.Ordering)

          val defaultCase = genThrowTypeError()

          sortedAltsByTypeTest.foldRight[js.Tree](defaultCase) { (elem, elsep) =>
            val (typeTest, subAlts) = elem
            implicit val pos = subAlts.head.pos

            val param = genFormalArg(paramIndex+1)
            val genSubAlts = genExportSameArgc(subAlts, paramIndex+1, maxArgc)

            def hasDefaultParam = subAlts.exists { p =>
              val params = p.tpe.params
              params.size > paramIndex &&
              params(paramIndex).hasFlag(Flags.DEFAULTPARAM)
            }

            def orUndef(cond: js.Tree) = if (!hasDefaultParam) cond else {
              js.JSBinaryOp("||", cond, js.JSBinaryOp("===", param.ref, js.Undefined()))
            }

            typeTest match {
              case HelperTypeTest(helperName, _) =>
                js.If(orUndef(js.CallHelper(helperName, param.ref)(jstpe.BooleanType)),
                    genSubAlts, elsep)(jstpe.AnyType)

              case TypeOfTypeTest(typeString) =>
                js.If(orUndef {
                  js.JSBinaryOp("===", js.JSUnaryOp("typeof", param.ref),
                      js.StringLiteral(typeString))
                }, genSubAlts, elsep)(jstpe.AnyType)

              case InstanceOfTypeTest(tpe) =>
                js.If(orUndef(genIsInstanceOf(param.ref, tpe)),
                    genSubAlts, elsep)(jstpe.AnyType)

              case NoTypeTest =>
                genSubAlts // note: elsep is discarded, obviously
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
    private def genApplyForSym(sym: Symbol): js.Tree = {
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
        // Construct a new JSArraySeq
        genNew(JSArraySeqClass, JSArraySeq_ctor,
          List(js.VarRef(js.Ident("arguments"), false)(jstpe.DynType),
            js.IntLiteral(normalArgc)))
      }

      // normal arguments
      val jsArgs = genFormalArgs(normalArgc)

      // Generate JS code to prepare arguments (default getters and unboxes)
      val funTpe = enteringPhase(currentRun.posterasurePhase)(sym.tpe)
      val jsArgPrep = for {
        (jsArg, (param, i)) <- jsArgs zip funTpe.params.zipWithIndex
      } yield {

        // Code to verify the type of the argument (if it is defined)
        val jsVerifyArg = {
          val tpePosterasure =
            enteringPhase(currentRun.posterasurePhase)(param.tpe)
          val verifiedArg = tpePosterasure match {
            case tpe if isPrimitiveValueType(tpe) =>
              val unboxed = makePrimitiveUnbox(jsArg.ref, tpe)
              // Ensure we don't convert null to a primitive value type
              js.If(js.BinaryOp(js.BinaryOp.===, jsArg.ref, js.Null()),
                genThrowTypeError(s"Found null, expected $tpe"),
                unboxed)(unboxed.tpe)
            case tpe: ErasedValueType =>
              val boxedClass = tpe.valueClazz
              val unboxMethod = boxedClass.derivedValueClassUnbox
              genApplyMethod(
                  genAsInstanceOf(jsArg.ref, tpe),
                  boxedClass, unboxMethod, Nil)
            case tpe =>
              genAsInstanceOf(jsArg.ref, tpe)
          }

          js.Assign(jsArg.ref, verifiedArg)
        }

        // If argument is undefined and there is a default getter, call it
        if (param.hasFlag(Flags.DEFAULTPARAM)) {
          js.If(js.BinaryOp(js.BinaryOp.===, jsArg.ref, js.Undefined()), {
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
            js.Assign(jsArg.ref, genApplyMethod(trgTree, trgSym, defaultGetter,
                jsArgs.take(defaultGetter.tpe.params.size).map(_.ref)))
          }, {
            // Otherwise, unbox the argument
            jsVerifyArg
          })(jstpe.AnyType)
        } else {
          // Otherwise, it is always the unboxed argument
          jsVerifyArg
        }
      }

      val jsResult = {
        val call = genApplyMethod(js.This()(encodeClassType(sym.owner)),
            sym.owner, sym, jsArgs.map(_.ref) ++ jsVarArg)
        ensureBoxed(call,
            enteringPhase(currentRun.posterasurePhase)(sym.tpe.resultType))
      }

      js.Block(jsArgPrep :+ jsResult)
    }

  }

  private def isOverridingExport(sym: Symbol): Boolean = {
    lazy val osym = sym.nextOverriddenSymbol
    sym.isOverridingSymbol && !osym.owner.isInterface
  }

  private sealed abstract class RTTypeTest

  private final case class HelperTypeTest(helperName: String,
      rank: Int) extends RTTypeTest

  private final case class TypeOfTypeTest(typeString: String) extends RTTypeTest

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

          // undefined test is always first
          case (TypeOfTypeTest("undefined"), _) => true
          case (_, TypeOfTypeTest("undefined")) => false

          case (HelperTypeTest(_, rank1), HelperTypeTest(_, rank2)) =>
            rank1 <= rank2
          case (_:HelperTypeTest, _) => true
          case (_, _:HelperTypeTest) => false

          case (TypeOfTypeTest(s1), TypeOfTypeTest(s2)) =>
            s1 <= s2

          case (InstanceOfTypeTest(t1), InstanceOfTypeTest(t2)) =>
            t1 <:< t2

          case (_:TypeOfTypeTest, _:InstanceOfTypeTest) => true
          case (_:InstanceOfTypeTest, _:TypeOfTypeTest) => false
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
        (toTypeKind(tpe): @unchecked) match {
          case VoidKind    => TypeOfTypeTest("undefined")
          case BooleanKind => TypeOfTypeTest("boolean")
          case CharKind    => InstanceOfTypeTest(boxedClass(CharClass).tpe)
          case ByteKind    => HelperTypeTest("isByte", 0)
          case ShortKind   => HelperTypeTest("isShort", 1)
          case IntKind     => HelperTypeTest("isInt", 2)
          case LongKind    => InstanceOfTypeTest(RuntimeLongClass.tpe)
          case _:DOUBLE    => TypeOfTypeTest("number")

          case REFERENCE(cls) =>
            if (cls == StringClass) TypeOfTypeTest("string")
            else if (isRawJSType(tpe)) {
              cls match {
                case JSNumberClass => TypeOfTypeTest("number")
                case JSBooleanClass => TypeOfTypeTest("boolean")
                case JSStringClass => TypeOfTypeTest("string")
                case JSUndefinedClass => TypeOfTypeTest("undefined")
                case _ => NoTypeTest
              }
            } else InstanceOfTypeTest(tpe)

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

  private def genFormalArgs(count: Int)(implicit pos: Position): List[js.ParamDef] =
    (1 to count map genFormalArg).toList

  private def genFormalArg(index: Int)(implicit pos: Position): js.ParamDef =
    js.ParamDef(js.Ident("arg$" + index), jstpe.AnyType)

  private def hasRepeatedParam(sym: Symbol) =
    enteringPhase(currentRun.uncurryPhase) {
      sym.paramss.flatten.lastOption.exists(isRepeated _)
  }

}
