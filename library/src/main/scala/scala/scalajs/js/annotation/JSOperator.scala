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

/** Specifies that the entity is a JavaScript operator.
 *
 *  Only members whose Scala name corresponds to one of the JavaScript
 *  operators can be marked with this annotation.
 *
 *  @see [[http://www.scala-js.org/doc/calling-javascript.html Calling JavaScript from Scala.js]]
 */
@field @getter @setter
class JSOperator() extends scala.annotation.StaticAnnotation
