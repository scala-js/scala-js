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

package scala.scalajs.js.annotation.internal

/** IMPLEMENTATION DETAIL: Marks the annotated class, trait or object as a
 *  JavaScript type.
 *
 *  This annotation is added automatically by the compiler to all classes,
 *  traits and objects inheriting directly or indirectly from
 *  [[scala.scalajs.js.Any]]. It marks the annotated entity as being a
 *  JavaScript type.
 *
 *  Do not use this annotation yourself.
 */
class JSType extends scala.annotation.StaticAnnotation
