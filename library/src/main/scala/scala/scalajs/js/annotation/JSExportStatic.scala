/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js.annotation

import scala.annotation.meta._

// scalastyle:off line.size.limit

/** Specifies that the annotated member should be exported as a JavaScript
 *  static member of the companion class.
 *
 *  This annotation may only be used on members of a Scala `object` whose
 *  companion class is a Scala.js-defined JavaScript class. The annotated
 *  member will be available as a static member of the companion class.
 *
 *  @see [[https://www.scala-js.org/doc/export-to-javascript.html Export Scala.js APIs to JavaScript]]
 *
 *  @see [[https://www.scala-js.org/doc/interoperability/sjs-defined-js-classes.html Write JavaScript classes in Scala.js]]
 */
class JSExportStatic extends scala.annotation.StaticAnnotation {
  def this(name: String) = this()
}
