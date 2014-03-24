/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.collection.mutable

import scala.tools.nsc._
import scala.math.PartialOrdering
import scala.reflect.internal.Flags

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

    def genConstructorExports(classSym: Symbol): List[js.Tree] = {
      val constructors = classSym.tpe.member(nme.CONSTRUCTOR).alternatives

      // Generate exports from constructors and their annotations
      val ctorExports = for {
        ctor          <- constructors
        (jsName, pos) <- jsInterop.exportsOf(ctor)
      } yield (jsName, ctor)

      val exports = for {
        (jsName, specs) <- ctorExports.groupBy(_._1) // group by exported name
      } yield {
        import js.TreeDSL._

        val ctors = specs.map(_._2)
        implicit val pos = ctors.head.pos

        val js.MethodDef(_, args, body) = genExportMethod(ctors, jsName)

        val jsCtor = envField("c") DOT encodeClassFullNameIdent(classSym)
        val (createNamespace, expCtorVar) = genCreateNamespaceInExports(jsName)

        js.Block(
          createNamespace,
          js.DocComment("@constructor"),
          expCtorVar := js.Function(args, js.Block(
            // Call the js constructor while passing the current this
            js.ApplyMethod(jsCtor, js.Ident("call"), List(js.This())),
            body
          )),
          expCtorVar DOT "prototype" := jsCtor DOT "prototype"
        )
      }

      exports.toList
    }

    def genModuleAccessorExports(classSym: Symbol): List[js.Tree] = {
      import js.TreeDSL._

      for {
        (jsName, p) <- jsInterop.exportsOf(classSym)
      } yield {
        implicit val pos = p

        val accessorVar =
          envField("modules") DOT encodeModuleFullNameIdent(classSym)
        val (createNamespace, expAccessorVar) = genCreateNamespaceInExports(jsName)

        js.Block(
          createNamespace,
          expAccessorVar := accessorVar
        )
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

      // Factor out methods with variable argument lists. Note that they can
      // only be at the end of the lists as enforced by PrepJSExports
      val (varArgMeths, normalMeths) = enteringPhase(currentRun.uncurryPhase) {
        alts.partition(_.paramss.flatten.lastOption.exists(isRepeated _))
      }

      val hasVarArg = varArgMeths.nonEmpty

      // Group normal methods by argument count
      val normalByArgCount = normalMeths.groupBy(_.tpe.params.size)

      // Argument counts (for varArgs, this is minimal count)
      val argcS = (normalByArgCount.toList.map(_._1)
          ++ varArgMeths.map(_.tpe.params.size - 1)).sorted

      val formalArgs = genFormalArgs(argcS.last)

      // Generate cases by argument count
      val nestedCases = for {
        (argc, nextArgc) <- argcS.zipAll(argcS.tail, -1, -1)
      } yield {
        // Fetch methods with this argc
        val methods = normalByArgCount.getOrElse(argc, Nil)

        if (nextArgc == -1 && methods.isEmpty) {
          // We are in the last argc case and we have only varargs. This is
          // the default case. Don't generate anything
          Nil
        } else {
          // Include varArg methods that apply to this arg count
          // We have argc + 1, since a parameter list may also be empty
          // (unlike a normal parameter)
          val vArgs = varArgMeths.filter(_.tpe.params.size <= argc + 1)

          // The full overload resolution
          val baseCaseBody = genExportSameArgc(methods ++ vArgs, 0)

          if (methods.nonEmpty && vArgs.nonEmpty)
            (js.IntLiteral(argc), baseCaseBody) ::
            genMultiValCase(argc + 1 until nextArgc,
              genExportSameArgc(vArgs, 0))
          else if (methods.nonEmpty)
            (js.IntLiteral(argc), baseCaseBody) :: Nil
          else
            genMultiValCase(argc until nextArgc, baseCaseBody)

        }
      }

      val cases = nestedCases.flatten

      val defaultCase = {
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
          js.Switch(js.DotSelect(js.Ident("arguments"), js.Ident("length")),
              cases, defaultCase)
        }
      }

      js.MethodDef(js.StringLiteral(jsName), formalArgs, body)
    }

    private def genExportSameArgc(alts: List[Symbol], paramIndex: Int): js.Tree = {
      implicit val pos = alts.head.pos

      if (alts.size == 1)
        genApplyForSym(alts.head)
      else if (!alts.exists(_.tpe.params.size > paramIndex)) {
        // We reach here in two cases:
        // 1. The parameter list has been exhausted
        // 2. We only have (more than once) repeated parameters left
        // Therefore, we should fail
        currentUnit.error(pos,
            s"""Cannot disambiguate overloads for exported method ${alts.head.name} with types
               |  ${alts.map(_.tpe).mkString("\n  ")}""".stripMargin)
        js.Return(js.Undefined())
      } else {

        val altsByTypeTest = groupByWithoutHashCode(alts) { alt =>
          // get parameter type while resolving repeated params
          val tpe = enteringPhase(currentRun.uncurryPhase) {
            val ps = alt.paramss.flatten
            if (ps.size <= paramIndex || isRepeated(ps(paramIndex))) {
              assert(isRepeated(ps.last))
              repeatedToSingle(ps.last.tpe)
            } else ps(paramIndex).tpe
          }

          typeTestForTpe(tpe)
        }

        if (altsByTypeTest.size == 1) {
          // Testing this parameter is not doing any us good
          genExportSameArgc(alts, paramIndex+1)
        } else {
          // Sort them so that, e.g., isInstanceOf[String]
          // comes before isInstanceOf[Object]
          val sortedAltsByTypeTest = topoSortDistinctsBy(
              altsByTypeTest)(_._1)(RTTypeTest.Ordering)

          val defaultCase = genThrowTypeError()

          sortedAltsByTypeTest.foldRight[js.Tree](defaultCase) { (elem, elsep) =>
            val (typeTest, subAlts) = elem
            implicit val pos = subAlts.head.pos

            def param = genFormalArg(paramIndex+1)
            val genSubAlts = genExportSameArgc(subAlts, paramIndex+1)

            typeTest match {
              case TypeOfTypeTest(typeString) =>
                js.If(
                    js.BinaryOp("===", js.UnaryOp("typeof", param),
                        js.StringLiteral(typeString)),
                    genSubAlts, elsep)

              case InstanceOfTypeTest(tpe) =>
                js.If(encodeIsInstanceOf(param, tpe)._1, genSubAlts, elsep)

              case NoTypeTest =>
                genSubAlts // note: elsep is discarded, obviously
            }
          }
        }
      }
    }

    private def genApplyForSym(sym: Symbol): js.Tree = {
      implicit val pos = sym.pos

      val repeatedTpe = enteringPhase(currentRun.uncurryPhase) {
        for {
          param <- sym.paramss.flatten.lastOption
          if isRepeated(param)
        } yield repeatedToSingle(param.tpe)
      }

      val normalArgc = sym.tpe.params.size -
        (if (repeatedTpe.isDefined) 1 else 0)

      val lastArg = repeatedTpe map { tpe =>
        // Construct a new JSArraySeq with optional boxing
        genNew(JSArraySeqClass, JSArraySeq_ctor,
          List(js.Ident("arguments"), js.IntLiteral(normalArgc),
              genBoxFunction(tpe)))
      }

      js.Return {
        js.ApplyMethod(js.This(), encodeMethodSym(sym),
            genFormalArgs(normalArgc) ++ lastArg)
      }
    }

    private def genBoxFunction(tpe: Type)(implicit pos: Position) = {
      toTypeKind(tpe) match {
        case kind: ValueTypeKind =>
          js.Select(environment, js.Ident("b" + kind.primitiveCharCode))
        case _ =>
          // No boxing. Identity function
          val arg = js.Ident("x", None)
          js.Function(arg :: Nil, js.Return(arg))
      }
    }

  }

  /** Gen JS code for assigning an rhs to a qualified name in the exports scope.
   *  For example, given the qualified name "foo.bar.Something", generates:
   *
   *  ScalaJS.e["foo"] = ScalaJS.e["foo"] || {};
   *  ScalaJS.e["foo"]["bar"] = ScalaJS.e["foo"]["bar"] || {};
   *
   *  Returns (statements, ScalaJS.e["foo"]["bar"]["Something"])
   */
  private def genCreateNamespaceInExports(qualName: String)(
      implicit pos: Position): (js.Tree, js.Tree) = {
    val parts = qualName.split("\\.")
    val statements = mutable.ListBuffer.empty[js.Tree]
    var namespace = envField("e")
    for (i <- 0 until parts.length-1) {
      namespace = js.BracketSelect(namespace, js.StringLiteral(parts(i)))
      statements +=
        js.Assign(namespace, js.BinaryOp("||", namespace, js.ObjectConstr(Nil)))
    }
    val lhs = js.BracketSelect(namespace, js.StringLiteral(parts.last))
    (js.Block(statements.result()), lhs)
  }

  private def isOverridingExport(sym: Symbol): Boolean = {
    lazy val osym = sym.nextOverriddenSymbol
    sym.isOverridingSymbol && !osym.owner.isInterface
  }

  private sealed abstract class RTTypeTest

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
          case (_, NoTypeTest) => true
          case (NoTypeTest, _) => false

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
    toTypeKind(tpe) match {
      case UNDEFINED => TypeOfTypeTest("undefined")
      case LongKind => InstanceOfTypeTest(RuntimeLongClass.tpe)
      case _:INT | _:FLOAT => TypeOfTypeTest("number")
      case BOOL => TypeOfTypeTest("boolean")

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

  private def genThrowTypeError()(
      implicit pos: Position): js.Tree = {
    js.Throw(js.StringLiteral("No matching overload"))
  }

  private def genFormalArgs(count: Int)(implicit pos: Position): List[js.Ident] =
    (1 to count map genFormalArg).toList

  private def genFormalArg(index: Int)(implicit pos: Position): js.Ident =
    js.Ident("arg$" + index)


  private def genMultiValCase(is: Seq[Int], body: => js.Tree)(
      implicit pos: Position): List[(js.Tree,js.Tree)] = {

    if (is.isEmpty) Nil
    else {
      val bodyCase = (js.IntLiteral(is.last), body)
      val emptyCases = is.init.map(i => (js.IntLiteral(i), js.Skip()))

      (emptyCases :+ bodyCase).toList
    }
  }

}
