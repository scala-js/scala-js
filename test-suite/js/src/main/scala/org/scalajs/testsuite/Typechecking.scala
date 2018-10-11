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

import scala.language.experimental.macros

import Compat210._

object Typechecking {
  import scala.reflect.macros._
  import blackbox.Context

  def typeError(code: String): Unit = macro TypecheckingMacros.typeError
  def typeErrorWithMsg(code: String, msg: String): Unit =
    macro TypecheckingMacros.typeErrorWithMsg
}
