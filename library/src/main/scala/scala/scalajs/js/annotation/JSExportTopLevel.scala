/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.scalajs.js.annotation

import scala.annotation.meta._

/** Specifies that the given member should be exported to the top level of the module.
 *
 *  @see [[http://www.scala-js.org/doc/export-to-javascript.html Export Scala.js APIs to JavaScript]]
 */
@field @getter @setter
class JSExportTopLevel(name: String) extends scala.annotation.StaticAnnotation
