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

/** Specifies that all the objects extending the annotated class or trait
 *  should be exported for use in raw JS.
 *  Note that objects exported this way are exported under their fully
 *  qualified name.
 *
 *  @param ignoreInvalidDescendants If true, descendants that cannot be exported
 *      are silently ignored.
 *  @see [[http://www.scala-js.org/doc/export-to-javascript.html Export Scala.js APIs to JavaScript]]
 */
class JSExportDescendentObjects(ignoreInvalidDescendants: Boolean)
    extends scala.annotation.StaticAnnotation {
  /** Constructor that makes invalid descendants fail.
   *
   *  same as setting ingoreInvalidDescendants to false
   */
  def this() = this(false)
}
