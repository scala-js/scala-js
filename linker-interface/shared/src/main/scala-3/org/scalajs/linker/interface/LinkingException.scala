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

package org.scalajs.linker.interface

/** Thrown by the linker when linking cannot be performed. */
class LinkingException(message: String | Null, cause: Throwable | Null)
    extends Exception(message, cause) {

  def this(message: String | Null) = this(message, null)

  def this(cause: Throwable | Null) =
    this(if (cause == null) null else cause.toString(), cause)

  def this() = this(null, null)
}
