/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite

private[testsuite] object Compat210 {
  object blackbox { // scalastyle:ignore
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
