/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.scalajs.js

/** Marker trait for top-level objects representing the JS global scope.
 *
 *  When calling method on a top-level object or package object that is a
 *  subtype of GlobalScope, the receiver is dropped, and the JavaScript global
 *  scope is used instead.
 */
trait GlobalScope extends Object
