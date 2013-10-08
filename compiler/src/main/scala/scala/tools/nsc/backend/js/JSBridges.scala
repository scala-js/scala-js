/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package js

import scala.math.PartialOrdering
import scala.reflect.internal.Flags

/** Generation of bridges for JavaScript
 *
 *  @author Sébastien Doeraene
 */
trait JSBridges extends SubComponent { self: GenJSCode =>
  import global._
  import jsAddons._
  import definitions._
  import jsDefinitions._
  import scalaPrimitives.isPrimitive

  private def isCandidateForBridge(sym: Symbol): Boolean =
    sym.isMethod && !sym.isBridge && sym.isPublic && !isPrimitive(sym)

  def genBridgesForClass(sym: Symbol): List[js.Tree] = {
    val declaredMethods = sym.info.decls.filter(isCandidateForBridge)
    val newlyDeclaredMethods = declaredMethods.filterNot(
        x => x.isOverridingSymbol && x.nextOverriddenSymbol.isPublic)
    val newlyDeclaredMethodNames =
      newlyDeclaredMethods.map(_.name).toList.distinct
    newlyDeclaredMethodNames map (genBridge(sym, _))
  }

  private def genBridge(classSym: Symbol, name: TermName): js.Tree = {
    val alts0 = classSym.info.member(name).alternatives
    val alts1 = alts0.filter(isCandidateForBridge)
    val alts = alts1.filterNot(
        x => alts1.exists(y => (y ne x) && (y.tpe <:< x.tpe)))
    assert(!alts.isEmpty,
        s"Ended up with no alternatives for ${classSym.fullName}::$name. " +
        s"Original set was ${alts0} with types ${alts0.map(_.tpe)}")

    implicit val pos = alts.head.pos

    val altsByArgCount = alts.groupBy(_.tpe.params.size).toList.sortBy(_._1)

    val maxArgCount = altsByArgCount.last._1
    val formalsArgs = genFormalArgs(maxArgCount)

    val cases = for {
      (argc, methods) <- altsByArgCount
    } yield {
      (js.IntLiteral(argc), genBridgeSameArgc(methods, 0))
    }

    val body = {
      if (cases.size == 1) cases.head._2
      else {
        js.Switch(js.DotSelect(js.Ident("arguments"), js.Ident("length")),
            cases, genThrowTypeError())
      }
    }

    val jsName = name.toString match {
      case "<init>" => "init\ufe33" // will be stolen by the JS constructor
      case "constructor" => "$constructor"
      case x if js.isKeyword(x) => "$" + x
      case x => x
    }
    js.MethodDef(js.Ident(jsName), formalsArgs, body)
  }

  private def genBridgeSameArgc(alts: List[Symbol], paramIndex: Int): js.Tree = {
    implicit val pos = alts.head.pos

    val remainingParamLists = alts map (_.tpe.params.drop(paramIndex))

    if (alts.size == 1) genApplyForSym(alts.head)
    if (remainingParamLists.head.isEmpty) genTieBreak(alts)
    else {
      val altsByTypeTest = groupByWithoutHashCode(alts) {
        alt => typeTestForTpe(alt.tpe.params(paramIndex).tpe)
      }

      if (altsByTypeTest.size == 1) {
        // Testing this parameter is not doing any us good
        genBridgeSameArgc(alts, paramIndex+1)
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
          val genSubAlts = genBridgeSameArgc(subAlts, paramIndex+1)

          typeTest match {
            case TypeOfTypeTest(typeString) =>
              js.If(
                  js.BinaryOp("===", js.UnaryOp("typeof", param),
                      js.StringLiteral(typeString)),
                  genSubAlts, elsep)

            case InstanceOfTypeTest(tpe) =>
              js.If(genIsInstance(param, tpe), genSubAlts, elsep)

            case NoTypeTest =>
              genSubAlts // note: elsep is discarded, obviously
          }
        }
      }
    }
  }

  private def genIsInstance(value: js.Tree, tpe: Type)(
      implicit pos: Position): js.Tree = {
    encodeIsInstanceOf(value, tpe)
  }

  private def genTieBreak(alts: List[Symbol]): js.Tree = {
    // TODO For now we just emit the first one
    implicit val pos = alts.head.pos
    js.Block(genApplyForSym(alts.head))
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

  private def genApplyForSym(sym: Symbol): js.Tree = {
    implicit val pos = sym.pos
    js.Return {
      js.ApplyMethod(js.This(), encodeMethodSym(sym),
          genFormalArgs(sym.tpe.params.size))
    }
  }

  private def genThrowTypeError()(
      implicit pos: Position): js.Tree = {
    js.Throw(js.StringLiteral("No matching overload"))
  }

  private def genFormalArgs(count: Int)(implicit pos: Position): List[js.Ident] =
    (1 to count map genFormalArg).toList

  private def genFormalArg(index: Int)(implicit pos: Position): js.Ident =
    js.Ident("arg$" + index)
}
