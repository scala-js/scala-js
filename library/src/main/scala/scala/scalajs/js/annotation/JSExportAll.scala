/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



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
