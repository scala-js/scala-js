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

/** IMPLEMENTATION DETAIL: Marks public members of anonymous classes before typer.
 *
 *  This annotation is added automatically by the compiler to all public
 *  members of anonymous classes.
 *
 *  Do not use this annotation yourself.
 */
class WasPublicBeforeTyper extends scala.annotation.Annotation
