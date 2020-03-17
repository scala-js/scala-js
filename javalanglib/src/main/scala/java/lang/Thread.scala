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

package java.lang

/* We need a constructor to create SingleThread in the companion object, but:
 * PLEASE DO NOT USE THIS CLASS IN USER CODE!
 */
class Thread extends Runnable {
  private var interruptedState = false
  private[this] var name: String = "main" // default name of the main thread

  def run(): Unit = ()

  def interrupt(): Unit =
    interruptedState = true

  def isInterrupted(): scala.Boolean =
    interruptedState

  final def setName(name: String): Unit =
    this.name = name

  final def getName(): String =
    this.name

  def getStackTrace(): Array[StackTraceElement] =
    StackTrace.getCurrentStackTrace()

  def getId(): scala.Long = 1

  // Javadocs:
  // Returns the handler invoked when this thread abruptly terminates due to an uncaught exception. If this thread has not had an uncaught exception handler explicitly set then this thread's ThreadGroup object is returned, unless this thread has terminated, in which case null is returned.
  // Therefore:
  // JS VMs are running on a single thread, if this thread has been terminated it has to be this one, so, since it's terminated, we return null
  def getUncaughtExceptionHandler(): Thread.UncaughtExceptionHandler = null

}

object Thread {
  private[this] val SingleThread = new Thread()

  def currentThread(): Thread = SingleThread

  def interrupted(): scala.Boolean = {
    val ret = currentThread.isInterrupted
    currentThread.interruptedState = false
    ret
  }

  trait UncaughtExceptionHandler {
    def uncaughtException(t: Thread, e: Throwable): Unit
  }
}
