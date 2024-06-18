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

/** Specifies that the annotated entity can be replaced by a value known at linktime.
 *
 *  When an entity is annotated with `@linkTimeProperty`, it can be used in the
 *  condition of `LinkingInfo.linkTimeIf`. During linking, the annotated entity
 *  will be replaced by a value determined at link time.
 *
 *  The link-time value is resolved using the `name` parameter of the annotation
 *  by the `org.scalajs.linker.standard.LinkTimeProperties`.
 *
 *  @param name The name used to resolve the link-time value.
 *
 *  @see [[LinkingInfo.linkTimeIf]]
 *  @see [[LinkTimeProperties]]
 */
private[scalajs] class linkTimeProperty(name: String) extends scala.annotation.StaticAnnotation
