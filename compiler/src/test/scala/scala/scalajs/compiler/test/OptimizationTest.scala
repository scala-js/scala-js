package scala.scalajs.compiler.test

import util._

import org.junit.Test

import scala.scalajs.ir.{Trees => js, Types => jstpe}

class OptimizationTest extends JSASTTest {

  @Test
  def unwrapScalaFunWrapper: Unit = {

    // Make sure we do not wrap and unwrap right away
    """
    import scala.scalajs.js

    class A {
      val jsFun: js.Function = (x: Int) => x * 2
    }
    """.
    hasNot("runtime.AnonFunction ctor") {
      case js.New(jstpe.ClassType("sjsr_AnonFunction1"), _, _) =>
    }

    // Make sure our wrapper matcher has the right name
    """
    import scala.scalajs.js

    class A {
      val scalaFun = (x: Int) => x * 2
      val jsFun: js.Function = scalaFun
    }
    """.
    has("runtime.AnonFunction ctor") {
      case js.New(jstpe.ClassType("sjsr_AnonFunction1"), _, _) =>
    }

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
      case js.Apply(_, js.Ident(name, _), _)
          if name.startsWith("wrap") && name.endsWith("__scm_WrappedArray") =>
    }

    // Make sure our wrapper matcher has the right name
    """
    import scala.scalajs.js

    class A {
      val a = Array(5, 7, 9, -3)
    }
    """.
    has("one of the wrapArray methods") {
      case js.Apply(_, js.Ident(name, _), _)
          if name.startsWith("wrap") && name.endsWith("__scm_WrappedArray") =>
    }

  }

}
