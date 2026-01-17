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

package scala.scalajs.js.annotation

import scala.annotation.meta._

/** Specifies that the annotated member should be exported as a JavaScript
 *  static member of the companion class.
 *
 *  This annotation may only be used on members of a Scala `object` whose
 *  companion class is a non-native JS class. The annotated
 *  member will be available as a static member of the companion class.
 *
 *  @see [[https://www.scala-js.org/doc/export-to-javascript.html Export Scala.js APIs to JavaScript]]
 *
 *  @see [[https://www.scala-js.org/doc/interoperability/sjs-defined-js-classes.html Write JavaScript classes in Scala.js]]
 */
@field @getter @setter
class JSExportStatic extends scala.annotation.StaticAnnotation {
  def this(name: String) = this()
}
