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

package org.scalajs.linker

/** Thrown by the linker when linking cannot be performed. */
class LinkingException(message: String, cause: Throwable)
    extends Exception(message, cause) {

  def this(message: String) = this(message, null)

  def this(cause: Throwable) =
    this(if (cause == null) null else cause.toString(), cause)

  def this() = this(null, null)
}
