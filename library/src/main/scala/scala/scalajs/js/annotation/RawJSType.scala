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

/** Marks the annotated class, trait or object as a raw JavaScript type.
 *
 *  This annotation is added automatically by the compiler to all classes,
 *  traits and objects inheriting directly or indirectly from
 *  [[scala.scalajs.js.Any]]. It marks the annotated entity as being a raw
 *  JavaScript type, i.e., one that represents type information for an entity
 *  defined in JavaScript code.
 *
 *  Do not use this annotation yourself.
 */
class RawJSType extends scala.annotation.StaticAnnotation
