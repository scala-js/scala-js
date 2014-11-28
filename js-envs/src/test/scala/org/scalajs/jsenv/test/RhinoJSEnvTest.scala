package org.scalajs.jsenv.test

import org.scalajs.jsenv.rhino._

import org.scalajs.core.tools.sem._

class RhinoJSEnvTest extends ComTests {
  protected def newJSEnv = new RhinoJSEnv(Semantics.Defaults)
}
