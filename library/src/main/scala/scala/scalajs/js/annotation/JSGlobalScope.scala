/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js.annotation

/** Marks the annotated object as representing the JavaScript global scope.
 *
 *  This is particularly useful to model top-level functions and fields that
 *  are in the JavaScript global scope. They can be declared inside an object
 *  annotated with `@JSGlobalScope`.
 *
 *  @see [[http://www.scala-js.org/doc/calling-javascript.html Calling JavaScript from Scala.js]]
 */
class JSGlobalScope extends scala.annotation.StaticAnnotation
