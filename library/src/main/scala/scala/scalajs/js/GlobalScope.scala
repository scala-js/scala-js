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
