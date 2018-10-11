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

import scala.language.experimental.macros

import scala.scalajs.macroimpls.UseAsMacros

/** Helper for syntactic sugar of [[js.use]]. Only use in `js.use(x).as[T]` */
final class Using[A] private[js] (val x: A) extends AnyVal {
  def as[B <: Any]: B = macro UseAsMacros.as_impl[A, B]
}
