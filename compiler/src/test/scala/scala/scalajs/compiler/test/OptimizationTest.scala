package scala.scalajs.compiler.test

import util._

import org.junit.Test

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
    hasNot("runtime.AnonFunction ctor") { (js, jse) => {
      case js.New(jse.QualIdent(
          "ScalaJS.c.scala_scalajs_runtime_AnonFunction1"), _) =>
    }}

    // Make sure our wrapper matcher has the right name
    """
    import scala.scalajs.js

    class A {
      val scalaFun = (x: Int) => x * 2
      val jsFun: js.Function = scalaFun
    }
    """.
    has("runtime.AnonFunction ctor") { (js, jse) => {
      case js.New(jse.QualIdent(
          "ScalaJS.c.scala_scalajs_runtime_AnonFunction1"), _) =>
    }}

  }

}
