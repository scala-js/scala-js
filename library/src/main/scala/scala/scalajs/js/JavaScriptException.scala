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

package scala.scalajs.js

import scala.language.reflectiveCalls

import scala.scalajs.js

final case class JavaScriptException(exception: scala.Any) extends RuntimeException {

  override def getMessage(): String = exception.toString()
}
