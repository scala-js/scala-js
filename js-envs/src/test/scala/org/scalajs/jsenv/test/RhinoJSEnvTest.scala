package org.scalajs.jsenv.test

import org.scalajs.jsenv.rhino._

class RhinoJSEnvTest extends TimeoutComTests {
  protected def newJSEnv: RhinoJSEnv = new RhinoJSEnv
}
