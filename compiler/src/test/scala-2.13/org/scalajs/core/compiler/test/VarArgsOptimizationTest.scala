package org.scalajs.core.compiler.test

import util._

import org.junit.Test

import org.scalajs.core.ir.{Trees => js, Types => jstpe}

class VarArgsOptimizationTest extends JSASTTest {

  @Test
  def testVarArgsOptimization: Unit = {
    /* Make sure varargs are optimized to use js.ImmutableArray instead of
     * sci.ImmutableArray, for various data types.
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
      case js.Apply(_, js.Ident(name, _), _)
          if name.startsWith("wrap") && name.endsWith("__sci_ImmutableArray") =>
    }

    /* #2265 and #2741:
     * Make sure varargs are optimized to use js.ImmutableArray instead of
     * sci.ImmutableArray, for different species of target method (single arg
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
      case js.Apply(_, js.Ident(name, _), _)
          if name.startsWith("wrap") && name.endsWith("__sci_ImmutableArray") =>
    }

    /* Make sure our wrapper matcher has the right name.
     * We explicitly call one of the `ScalaRunTime` array wrapping
     * operation, exactly like it would have been called by the
     * compiler, internally.
     */
    """
    import scala.scalajs.js

    class A {
      runtime.ScalaRunTime.wrapIntArray(new Array[Int](5))
    }
    """.
    has("one of the wrapArray methods") {
      case js.Apply(_, js.Ident(name, _), _)
          if name.startsWith("wrap") && name.endsWith("__sci_ImmutableArray") =>
    }
  }

}
