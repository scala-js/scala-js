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

/** Exports all public members directly defined in a class / object.
 *
 *  Strictly equivalent to putting [[JSExport]] on every public member.
 *  Note: You are allowed to export protected members, but you'll have to do
 *  this explicitly on each member.
 *
 *  @see [[http://www.scala-js.org/doc/export-to-javascript.html Export Scala.js APIs to JavaScript]]
 */
class JSExportAll extends scala.annotation.StaticAnnotation
