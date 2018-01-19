/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.linker

/** Thrown by the linker when linking cannot be performed. */
class LinkingException(message: String, cause: Throwable)
    extends Exception(message, cause) {

  def this(message: String) = this(message, null)

  def this(cause: Throwable) =
    this(if (cause == null) null else cause.toString(), cause)

  def this() = this(null, null)
}
