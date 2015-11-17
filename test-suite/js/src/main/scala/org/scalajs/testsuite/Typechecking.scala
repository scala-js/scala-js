/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite

import scala.language.experimental.macros

import Compat210._

object Typechecking {
  import scala.reflect.macros._
  import blackbox.Context

  def typeError(code: String): Unit = macro TypecheckingMacros.typeError
  def typeErrorWithMsg(code: String, msg: String): Unit =
    macro TypecheckingMacros.typeErrorWithMsg
}
