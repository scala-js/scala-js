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

/** Marks the annotated object as representing the JavaScript global scope.
 *
 *  This is particularly useful to model top-level functions and fields that
 *  are in the JavaScript global scope. They can be declared inside an object
 *  annotated with `@JSGlobalScope`.
 *
 *  @see [[http://www.scala-js.org/doc/calling-javascript.html Calling JavaScript from Scala.js]]
 */
class JSGlobalScope extends scala.annotation.StaticAnnotation
