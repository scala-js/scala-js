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

/** IMPLEMENTATION DETAIL: Saves the fully qualified JS name of a symbol.
 *
 *  This annotation was used prior to Scala.js 0.6.13. It is only kept for
 *  backwards binary compatibility, and should not be used anymore.
 *
 *  Do not use this annotation yourself.
 */
@deprecated("Replaced by internal.JSNativeLoadSpec.", "0.6.13")
class JSFullName(fullName: String) extends scala.annotation.StaticAnnotation
