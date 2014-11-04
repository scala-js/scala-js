package scala.scalajs.sbtplugin.test.env

import scala.scalajs.sbtplugin.env.rhino._

import scala.scalajs.tools.sem._

class RhinoJSEnvTest extends ComTests {
  protected def newJSEnv = new RhinoJSEnv(Semantics.Defaults)
}
