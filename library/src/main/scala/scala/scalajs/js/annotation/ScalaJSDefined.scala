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

/** Marks the annotated class as a Scala.js-defined JavaScript class.
 *
 *  This annotation may only be used on a class extending
 *  [[scala.scalajs.js.Any js.Any]].
 */
class ScalaJSDefined extends scala.annotation.StaticAnnotation
