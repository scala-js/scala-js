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

package org.scalajs.linker.runtime

/** Error thrown when an undefined behavior in Fatal mode has been detected.
 *  This error should never be caught. It indicates a severe programming bug.
 *  In Unchecked mode, the program may behave arbitrarily.
 *  The `cause` is set to the exception that would have been thrown if the
 *  given behavior was in Compliant mode.
 *  If your program relies on the proper kind of exception being thrown, as if
 *  running on the JVM, you should set the appropriate behavior to Compliant.
 *  Note that this will have (potentially major) performance impacts.
 */
class UndefinedBehaviorError(message: String, cause: Throwable)
    extends java.lang.VirtualMachineError(message, cause) {

  def this(message: String) = this(message, null)

  def this(cause: Throwable) =
    this(if (cause == null) null else cause.toString, cause)

  def this() = this(null, null)
}
