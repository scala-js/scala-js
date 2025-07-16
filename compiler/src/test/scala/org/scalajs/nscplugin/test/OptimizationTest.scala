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
import org.junit.Assert._

import org.scalajs.ir.{Trees => js, Types => jstpe}
import org.scalajs.ir.Names._

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

      // Also with exactly 1 element of a primitive type (#3938)
      val f = Array('a')
      val g = Array(5.toByte)
    }
    """.
    hasNot("any LoadModule of the scala.Array companion") {
      case js.LoadModule(ArrayModuleClass) =>
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
      val c = Array[Int](5)
      val d = Array[Byte](5)
    }
    """.
    hasExactly(4, "calls to Array.apply methods") {
      case js.Apply(_, js.LoadModule(ArrayModuleClass), js.MethodIdent(methodName), _)
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
    hasNot("any reference to the global scope nor loading JS constructor") {
      case js.JSGlobalRef(_) =>
      case js.LoadJSConstructor(_) =>
    }
  }

  @Test
  def noLabeledBlockForWhileLoops: Unit = {
    """
    class Test {
      def testWhileStatWithCond(): Unit = {
        var x: Int = 5
        while (x != 0) {
          x -= 1
        }
        println(x)
      }

      def testWhileExprWithCond(s: Any): Unit = {
        var x: Int = 5
        s match {
          case s: String =>
            while (x != 0) {
              x -= 1
            }
        }
      }

      def testWhileTrueStat(): Unit = {
        var x: Int = 5
        while (true) {
          x -= 1
          if (x == 0)
            return
          println(x)
        }
      }

      def testWhileTrueExpr(s: Any): Unit = {
        var x: Int = 5
        s match {
          case s: String =>
            while (true) {
              x -= 1
              if (x == 0)
                return
              println(x)
            }
        }
      }

      def testWhileFalseStat(): Unit = {
        var x: Int = 5
        while (false) {
          x -= 1
          if (x == 0)
            return
          println(x)
        }
      }

      def testWhileFalseExpr(s: Any): Unit = {
        var x: Int = 5
        s match {
          case s: String =>
            while (false) {
              x -= 1
              if (x == 0)
                return
              println(x)
            }
        }
      }
    }
    """.hasNot("non-return labeled block") {
      case js.Labeled(name, _, _) if !name.nameString.startsWith("_return") =>
    }
  }

  @Test
  def noLabeledBlockForDoWhileLoops: Unit = {
    """
    class Test {
      def testDoWhileStatWithCond(): Unit = {
        var x: Int = 5
        do {
          x -= 1
        } while (x != 0)
        println(x)
      }

      def testDoWhileExprWithCond(s: Any): Unit = {
        var x: Int = 5
        s match {
          case s: String =>
            do {
              x -= 1
            } while (x != 0)
        }
      }

      def testDoWhileTrueStat(): Unit = {
        var x: Int = 5
        do {
          x -= 1
          if (x == 0)
            return
          println(x)
        } while (true)
      }

      def testDoWhileTrueExpr(s: Any): Unit = {
        var x: Int = 5
        s match {
          case s: String =>
            do {
              x -= 1
              if (x == 0)
                return
              println(x)
            } while (true)
        }
      }

      def testDoWhileFalseStat(): Unit = {
        var x: Int = 5
        do {
          x -= 1
          if (x == 0)
            return
          println(x)
        } while (false)
      }

      def testDoWhileFalseExpr(s: Any): Unit = {
        var x: Int = 5
        s match {
          case s: String =>
            do {
              x -= 1
              if (x == 0)
                return
              println(x)
            } while (false)
        }
      }
    }
    """.hasNot("non-return labeled block") {
      case js.Labeled(name, _, _) if !name.nameString.startsWith("_return") =>
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

  @Test
  def optimizeScalaLambda: Unit = {
    val allowedNames = Set(ClassName("A$"), ClassName("A"))

    """
    object A {
      val x: Int => String = _.toString
    }
    """.hasNot("auxiliary/anonymous class") {
      case cl: js.ClassDef if !allowedNames.contains(cl.name.name) =>
    }
  }

  @Test
  def noWrapJavaScriptExceptionWhenCatchingWildcardThrowable: Unit = {
    """
    class Test {
      def foo(): Int = throw new IllegalArgumentException("foo")

      def testCatchFullWildcard(): Int = {
        try {
          foo()
        } catch {
          case _ => -1 // causes an expected Scala compile warning
        }
      }

      def testCatchWildcardOfTypeThrowable(): Int = {
        try {
          foo()
        } catch {
          case _: Throwable => -1
        }
      }
    }
    """.hasNot("WrapAsThrowable") {
      case js.UnaryOp(js.UnaryOp.WrapAsThrowable, _) =>
    }

    // Confidence check
    """
    class Test {
      def foo(): Int = throw new IllegalArgumentException("foo")

      def testCatchWildcardOfTypeRuntimeException(): Int = {
        try {
          foo()
        } catch {
          case _: RuntimeException => -1
        }
      }
    }
    """.hasExactly(1, "WrapAsThrowable") {
      case js.UnaryOp(js.UnaryOp.WrapAsThrowable, _) =>
    }
  }

  @Test
  def callSiteInlineSingleDispatchJSMethods: Unit = {
    val fooName = SimpleMethodName("foo")
    val aName = ClassName("A")

    val flags = {
      """
      import scala.scalajs.js

      class A extends js.Object {
        def foo(x: Int, y: Int = 2): Int = x + y
      }
      """.extractOne("foo dispatch call") {
        case js.ApplyStatic(flags, `aName`, SMN("foo"), _) =>
          flags
      }
    }

    assertTrue(flags.inline)
  }

  @Test
  def loadModuleAfterStoreModuleIsThis: Unit = {
    val testName = ClassName("Test$")

    """
    object Test {
      private val selfPair = (Test, Test)
    }
    """.hasNot("LoadModule") {
      case js.LoadModule(_) =>
    }

    // Confidence check
    """
    object Test {
      private def selfPair = (Test, Test)
    }
    """.hasExactly(2, "LoadModule") {
      case js.LoadModule(`testName`) =>
    }
  }

  @Test
  def unsignedComparisonsInt: Unit = {
    import js.BinaryOp._

    val comparisons = List(
      (Int_unsigned_<, "<"),
      (Int_unsigned_<=, "<="),
      (Int_unsigned_>, ">"),
      (Int_unsigned_>=, ">=")
    )

    for ((op, codeOp) <- comparisons) {
      s"""
      class Test {
        private final val SignBit = Int.MinValue

        def unsignedComparisonsInt(x: Int, y: Int): Unit = {
          (x ^ 0x80000000) $codeOp (y ^ 0x80000000)
          (SignBit ^ x) $codeOp (y ^ SignBit)
          (SignBit ^ x) $codeOp 0x80000010
          0x00000020 $codeOp (y ^ SignBit)
        }
      }
      """.hasExactly(4, "unsigned comparisons") {
        case js.BinaryOp(`op`, _, _) =>
      }.hasNot("any Int_^") {
        case js.BinaryOp(Int_^, _, _) =>
      }.hasNot("any signed comparison") {
        case js.BinaryOp(Int_< | Int_<= | Int_> | Int_>=, _, _) =>
      }
    }
  }

  @Test
  def unsignedComparisonsLong: Unit = {
    import js.BinaryOp._

    val comparisons = List(
      (Long_unsigned_<, "<"),
      (Long_unsigned_<=, "<="),
      (Long_unsigned_>, ">"),
      (Long_unsigned_>=, ">=")
    )

    for ((op, codeOp) <- comparisons) {
      s"""
      class Test {
        private final val SignBit = Long.MinValue

        def unsignedComparisonsInt(x: Long, y: Long): Unit = {
          (x ^ 0x8000000000000000L) $codeOp (y ^ 0x8000000000000000L)
          (SignBit ^ x) $codeOp (y ^ SignBit)
          (SignBit ^ x) $codeOp 0x8000000000000010L
          0x0000000000000020L $codeOp (y ^ SignBit)
        }
      }
      """.hasExactly(4, "unsigned comparisons") {
        case js.BinaryOp(`op`, _, _) =>
      }.hasNot("any Long_^") {
        case js.BinaryOp(Long_^, _, _) =>
      }.hasNot("any signed comparison") {
        case js.BinaryOp(Long_< | Long_<= | Long_> | Long_>=, _, _) =>
      }
    }
  }

  @Test
  def linkTimeIf: Unit = {
    /* Make sure the cast in
     * `linkTimeIf(cond)(thenp)(elsep).asInstanceOf[T]``
     * is optimized away if `thenp` and `elsep` are subtypes of `T`.
     */
    """
    import scala.scalajs.js
    import scala.scalajs.LinkingInfo._

    class LinkTimeIfTest {
      import LinkTimeIfTest._
      private val impl = linkTimeIf[ArrayImpl](productionMode) {
        JSArrayImpl
      } {
        ScalaArrayImpl
      }
      def implPattern(): Int = {
        impl.length(impl.empty())
      }
    }
    object LinkTimeIfTest {
      sealed private abstract class ArrayImpl {
        type Repr
        def empty(): Repr
        def length(v: Repr): Int
      }
      private object JSArrayImpl extends ArrayImpl {
        type Repr = js.Array[AnyRef]
        def empty(): Repr = js.Array[AnyRef]()
        def length(v: Repr): Int = v.length
      }
      private object ScalaArrayImpl extends ArrayImpl {
        type Repr = Array[AnyRef]
        def empty(): Repr = new Array[AnyRef](0)
        def length(v: Repr): Int = v.length
      }
    }
    """.
    hasNot("linkTimeIf[A](...).asInstanceOf[A]") {
      case js.AsInstanceOf(_:js.LinkTimeIf, _) =>
    }
  }
}

object OptimizationTest {

  private val ArrayModuleClass = ClassName("scala.Array$")

  private val applySimpleMethodName = SimpleMethodName("apply")

  private val hasOldCollections =
    scala.util.Properties.versionNumberString.startsWith("2.12.")

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
      methodName.resultTypeRef == WrappedArrayTypeRef
    }
  }

}
