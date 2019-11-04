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

package org.scalajs.nscplugin.test

import util._

import org.junit.Test

import org.scalajs.ir.{Trees => js, Types => jstpe}
import org.scalajs.ir.Definitions._

class OptimizationTest extends JSASTTest {
  import OptimizationTest._

  @Test
  def testArrayApplyOptimization: Unit = {
    /* Make sure Array(...) is optimized away completely for several kinds
     * of data types, with both the generic overload and the ones specialized
     * for primitives.
     */
    """
    class A {
      val a = Array(5, 7, 9, -3)
      val b = Array("hello", "world")
      val c = Array('a', 'b')
      val d = Array(Nil)
      val e = Array(5.toByte, 7.toByte, 9.toByte, -3.toByte)
    }
    """.
    hasNot("any LoadModule of the scala.Array companion") {
      case js.LoadModule(jstpe.ClassRef(ArrayModuleClass)) =>
    }

    /* Using [] with primitives produces suboptimal trees, which cannot be
     * optimized. We should improve this in the future, if possible. This is
     * particularly annoying for Byte and Short, as it means that we need to
     * write `.toByte` for every single element if we want the optimization to
     * kick in.
     *
     * Scala/JVM has the same limitation.
     */
    """
    class A {
      val a = Array[Int](5, 7, 9, -3)
      val b = Array[Byte](5, 7, 9, -3)
    }
    """.
    hasExactly(2, "calls to Array.apply methods") {
      case js.Apply(_, js.LoadModule(jstpe.ClassRef(ArrayModuleClass)), js.MethodIdent(methodName, _), _)
          if methodName.simpleName == applySimpleMethodName =>
    }
  }

  @Test
  def testJSArrayApplyOptimization: Unit = {
    /* Make sure js.Array(...) is optimized away completely for several kinds
     * of data types.
     */
    """
    import scala.scalajs.js

    class VC(val x: Int) extends AnyVal

    class A {
      val a = js.Array(5, 7, 9, -3)
      val b = js.Array("hello", "world")
      val c = js.Array('a', 'b')
      val d = js.Array(Nil)
      val e = js.Array(new VC(151189))
    }
    """.
    hasNot("any of the wrapArray methods") {
      case WrapArrayCall() =>
    }
  }

  @Test
  def testVarArgsOptimization: Unit = {
    /* Make sure varargs are optimized to use js.WrappedArray instead of
     * scm.WrappedArray, for various data types.
     */
    """
    import scala.scalajs.js

    class VC(val x: Int) extends AnyVal

    class A {
      val a = List(5, 7, 9, -3)
      val b = List("hello", "world")
      val c = List('a', 'b')
      val d = List(Nil)
      val e = List(new VC(151189))
    }
    """.
    hasNot("any of the wrapArray methods") {
      case WrapArrayCall() =>
    }

    /* #2265 and #2741:
     * Make sure varargs are optimized to use js.WrappedArray instead of
     * scm.WrappedArray, for different species of target method (single arg
     * list, multiple arg list, in value class).
     */
    """
    import scala.scalajs.js

    class VC(val x: Int) extends AnyVal {
      def singleInVC(ys: Int*): Int = x + ys.size
    }

    class A {
      def test(): Int = {
        val a = single(5, 7, 9, -3)
        val b = multiple(5)(7, 9, -3)
        val c = new VC(5).singleInVC(7, 9, -3)
        a + b + c
      }

      def single(x: Int, ys: Int*): Int = x + ys.size
      def multiple(x: Int)(ys: Int*): Int = x + ys.size
    }
    """.
    hasNot("any of the wrapArray methods") {
      case WrapArrayCall() =>
    }

    /* Make sure our wrapper matcher has the right name.
     * With the new collections, only actual varargs will produce a call to the
     * methods we optimize, and we would always be able to optimize them in
     * that case. So we need to explicitly call the method that the codegen
     * would use.
     */
    val sanityCheckCode = if (hasOldCollections) {
      """
      class A {
        val a: Seq[Int] = new Array[Int](5)
      }
      """
    } else {
      """
      class A {
        runtime.ScalaRunTime.wrapIntArray(new Array[Int](5))
      }
      """
    }
    sanityCheckCode.has("one of the wrapArray methods") {
      case WrapArrayCall() =>
    }
  }

  @Test
  def testNewJSObjectAndJSArray: Unit = {
    // Verify the optimized emitted code for 'new js.Object' and 'new js.Array'
    """
    import scala.scalajs.js

    class A {
      val o = new js.Object
      val a = new js.Array
    }
    """.
    hasNot("any reference to the global scope") {
      case js.JSLinkingInfo() =>
    }
  }

  @Test
  def noLabeledBlockForPatmatWithToplevelCaseClassesOnlyAndNoGuards: Unit = {
    """
    sealed abstract class Foo
    final case class Foobar(x: Int) extends Foo
    final case class Foobaz(y: String) extends Foo

    class Test {
      def testWithListsStat(xs: List[Int]): Unit = {
        xs match {
          case head :: tail => println(head + " " + tail)
          case Nil          => println("nil")
        }
      }

      def testWithListsExpr(xs: List[Int]): Int = {
        xs match {
          case head :: tail => head + tail.size
          case Nil          => 0
        }
      }

      def testWithFooStat(foo: Foo): Unit = {
        foo match {
          case Foobar(x) => println("foobar: " + x)
          case Foobaz(y) => println(y)
        }
      }

      def testWithFooExpr(foo: Foo): String = {
        foo match {
          case Foobar(x) => "foobar: " + x
          case Foobaz(y) => "foobaz: " + y
        }
      }
    }
    """.hasNot("Labeled block") {
      case js.Labeled(_, _, _) =>
    }
  }

  @Test
  def switchWithoutGuards: Unit = {
    """
    class Test {
      def switchWithGuardsStat(x: Int, y: Int): Unit = {
        x match {
          case 1            => println("one")
          case 2            => println("two")
          case z if y > 100 => println("big " + z)
          case _            => println("None of those")
        }
      }
    }
    """.hasNot("Labeled block") {
      case js.Labeled(_, _, _) =>
    }.has("Match node") {
      case js.Match(_, _, _) =>
    }
  }

  @Test
  def switchWithGuards: Unit = {
    // Statement position
    """
    class Test {
      def switchWithGuardsStat(x: Int, y: Int): Unit = {
        x match {
          case 1            => println("one")
          case 2 if y < 10  => println("two special")
          case 2            => println("two")
          case 3 if y < 10  => println("three special")
          case 3 if y > 100 => println("three big special")
          case z if y > 100 => println("big " + z)
          case _            => println("None of those")
        }
      }
    }
    """.hasExactly(1, "default case (\"None of those\")") {
      case js.StringLiteral("None of those") =>
    }.has("Match node") {
      case js.Match(_, _, _) =>
    }

    // Expression position
    """
    class Test {
      def switchWithGuardsExpr(x: Int, y: Int): Unit = {
        val message = x match {
          case 1            => "one"
          case 2 if y < 10  => "two special"
          case 2            => "two"
          case 3 if y < 10  => "three special"
          case 3 if y > 100 => "three big special"
          case z if y > 100 => "big " + z
          case _            => "None of those"
        }
        println(message)
      }
    }
    """.hasExactly(1, "default case (\"None of those\")") {
      case js.StringLiteral("None of those") =>
    }.has("Match node") {
      case js.Match(_, _, _) =>
    }
  }

  @Test
  def newSJSDefinedTraitProducesObjectConstr: Unit = {
    """
    import scala.scalajs.js
    import scala.scalajs.js.annotation._

    trait Point extends js.Object {
      val x: Double
      val y: Double
    }

    class Test {
      def newSJSDefinedTraitProducesObjectConstr(): Any = {
        new Point {
          val x = 5.0
          val y = 6.5
        }
      }
    }
    """.hasNot("`new Object`") {
      case js.JSNew(_, _) =>
    }.has("object literal") {
      case js.JSObjectConstr(Nil) =>
    }

    """
    import scala.scalajs.js
    import scala.scalajs.js.annotation._

    trait Point extends js.Object {
      var x: js.UndefOr[Double] = js.undefined
      var y: js.UndefOr[Double] = js.undefined
    }

    class Test {
      def newSJSDefinedTraitProducesObjectConstr(): Any = {
        new Point {
          x = 5.0
          y = 6.5
        }
      }
    }
    """.hasNot("`new Object`") {
      case js.JSNew(_, _) =>
    }.has("object literal") {
      case js.JSObjectConstr(Nil) =>
    }
  }

}

object OptimizationTest {

  private val ArrayModuleClass = ClassName("scala.Array$")

  private val applySimpleMethodName = SimpleMethodName("apply")

  private val hasOldCollections = {
    val version = scala.util.Properties.versionNumberString

    version.startsWith("2.11.") ||
    version.startsWith("2.12.")
  }

  private object WrapArrayCall {
    private val WrappedArrayTypeRef = {
      val name =
        if (hasOldCollections) "scala.collection.mutable.WrappedArray"
        else "scala.collection.immutable.ArraySeq"
      jstpe.ClassRef(ClassName(name))
    }

    def unapply(tree: js.Apply): Boolean = {
      val methodName = tree.method.name
      methodName.simpleName.nameString.startsWith("wrap") &&
      methodName.resultTypeRef.exists(_ == WrappedArrayTypeRef)
    }
  }

}
