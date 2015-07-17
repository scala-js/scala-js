/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js

import scala.language.experimental.macros

import scala.scalajs.macroimpls.UseAsMacros

/** Helper for syntactic sugar of [[js.use]]. Only use in `js.use(x).as[T]` */
final class Using[A] private[js] (val x: A) extends AnyVal {
  def as[B <: Any]: B = macro UseAsMacros.as_impl[A, B]
}
