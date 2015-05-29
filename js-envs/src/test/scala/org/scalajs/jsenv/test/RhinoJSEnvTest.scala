package org.scalajs.jsenv.test

import org.scalajs.jsenv.rhino._

import org.scalajs.core.tools.sem._

class RhinoJSEnvTest extends TimeoutComTests {
  protected def newJSEnv: RhinoJSEnv = new RhinoJSEnv(Semantics.Defaults)
}
