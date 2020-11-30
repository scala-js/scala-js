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

import java.util.{Collection, Collections}

class Semaphore(private[this] var permits: Int, fairness: Boolean) extends java.io.Serializable {

  def this(permits: Int) = this(permits, false)

  // These methods can’t be implemented because they block
  // def acquire(): Unit
  // def acquire(permits: Int): Unit
  // def acquireUninterruptibly(): Unit
  // def acquireUninterruptibly(permits: Int): Unit
  // def tryAcquire(permits: Int, timeout: Long, unit: TimeUnit): Boolean
  // def tryAcquire(timeout: Long, unit: TimeUnit): Boolean

  def availablePermits(): Int = permits

  def drainPermits(): Int = {
    val old = permits
    permits = 0
    old
  }

  /* One would expect that the accessor methods delegate to `getQueuedThreads`,
   * but that is not the JDK behavior. In the absence of a specification, we
   * replicate the JDK behavior. Notably, because the documentation of
   * `getQueuedThreads` mentions that it is intended for extensive monitoring,
   * not overriding. The fact that the method is not final is hence likely an
   * oversight.
   */

  protected def getQueuedThreads(): Collection[Thread] = Collections.emptySet()

  final def getQueueLength(): Int = 0

  final def hasQueuedThreads(): Boolean = false

  def isFair(): Boolean = fairness

  protected def reducePermits(reduction: Int): Unit = {
    requireNonNegative(reduction)
    permits -= reduction
  }

  def release(): Unit = release(1)

  def release(permits: Int): Unit = {
    requireNonNegative(permits)
    this.permits += permits
  }

  override def toString: String =
    s"${super.toString}[Permits = ${permits}]"

  def tryAcquire(): Boolean = tryAcquire(1)

  def tryAcquire(permits: Int): Boolean = {
    requireNonNegative(permits)
    if (this.permits >= permits) {
      this.permits -= permits
      true
    } else {
      false
    }
  }

  @inline private def requireNonNegative(n: Int): Unit = {
    if (n < 0)
      throw new IllegalArgumentException
  }
}
