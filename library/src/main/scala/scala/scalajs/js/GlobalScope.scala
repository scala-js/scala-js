/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */



package scala.scalajs.js

/** Marker trait for top-level objects representing the JS global scope.
 *
 *  When calling method on a top-level object or package object that is a
 *  subtype of GlobalScope, the receiver is dropped, and the JavaScript global
 *  scope is used instead.
 *
 *  @see [[http://www.scala-js.org/doc/calling-javascript.html Calling JavaScript from Scala.js]]
 */
@deprecated("Use the annotation @js.annotation.JSGlobalScope instead.", "0.6.13")
@native
trait GlobalScope extends Any
