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

package scala.scalajs.reflect.annotation

/** Enables reflective instantiation for the annotated class, trait or object,
 *  and all its descendants.
 *
 *  Affected classes can be identified at run-time through methods of
 *  [[scala.scalajs.reflect.Reflect]].
 */
class EnableReflectiveInstantiation extends scala.annotation.StaticAnnotation
