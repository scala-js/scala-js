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

package scala.scalajs.annotation

/** Publicly marks the annotated method as being a link-time property.
 *
 *  When an entity is annotated with `@linkTimeProperty`, its body must be a
 *  link-time property with the same `name`. The annotation makes that body
 *  "public", and it can therefore be inlined at call site at compile-time.
 *
 *  From a user perspective, we can treat the presence of that annotation as if
 *  it were the `inline` keyword of Scala 3: it forces the inlining to happen
 *  at compile-time.
 *
 *  This is necessary for the target method to be used in the condition of a
 *  `LinkingInfo.linkTimeIf`.
 *
 *  @param name The name used to resolve the link-time value.
 *
 *  @see [[LinkingInfo.linkTimeIf]]
 */
private[scalajs] final class linkTimeProperty(name: String)
    extends scala.annotation.StaticAnnotation
