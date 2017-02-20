/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.scalajs.js.annotation

import scala.annotation.meta._

/** Specifies the JavaScript name of an entity.
 *
 *  @see [[http://www.scala-js.org/doc/calling-javascript.html Calling JavaScript from Scala.js]]
 */
@field @getter @setter
class JSName private () extends scala.annotation.StaticAnnotation {
  def this(name: String) = this()
  def this(symbol: scala.scalajs.js.Symbol) = this()
}
