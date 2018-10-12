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

package java.util.concurrent

class ExecutionException(message: String, cause: Throwable)
    extends Exception(message, cause) {

  protected def this() = this(null, null)
  protected def this(message: String) = this(message, null)
  def this(cause: Throwable) =
    this(if (cause == null) null else cause.toString, cause)
}

class CancellationException(message: String)
    extends IllegalStateException(message) {

  def this() = this(null)
}

class TimeoutException(message: String) extends Exception(message) {
  def this() = this(null)
}

class RejectedExecutionException(message: String, cause: Throwable)
    extends RuntimeException(message, cause) {

  def this() = this(null, null)

  def this(message: String) = this(message, null)

  def this(cause: Throwable) =
    this(if (cause eq null) null else cause.toString, cause)
}
