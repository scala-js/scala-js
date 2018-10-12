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

package org.scalajs.testsuite

private[testsuite] object Compat210 {
  object blackbox {
    type Context = scala.reflect.macros.Context
  }
}

import Compat210._

private[testsuite] trait Compat210Component {
  // Import macros only here, otherwise we collide with the above
  import scala.reflect.macros._
  import blackbox.Context

  val c: Context

  import c.universe._

  implicit final class ContextCompat(self: c.type) {
    def typecheck(tree: Tree): Tree = c.typeCheck(tree)
  }
}
