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

package org.scalajs.jsenv.test

import org.scalajs.core.tools.sem.Semantics

import org.scalajs.jsenv.rhino._

class RhinoJSEnvTest extends TimeoutComTests {
  protected def newJSEnv: RhinoJSEnv =
    new RhinoJSEnv(Semantics.Defaults, withDOM = false, internal = ())
}
