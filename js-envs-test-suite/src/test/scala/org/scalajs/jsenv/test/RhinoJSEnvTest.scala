package org.scalajs.jsenv.test

import org.scalajs.core.tools.sem.Semantics

import org.scalajs.jsenv.rhino._

class RhinoJSEnvTest extends TimeoutComTests {
  protected def newJSEnv: RhinoJSEnv =
    new RhinoJSEnv(Semantics.Defaults, withDOM = false, internal = ())
}
