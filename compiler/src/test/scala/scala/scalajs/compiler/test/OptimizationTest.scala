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
      case js.New(jstpe.ClassType("scala_scalajs_runtime_AnonFunction1"), _, _) =>
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
      case js.New(jstpe.ClassType("scala_scalajs_runtime_AnonFunction1"), _, _) =>
    }

  }

}
