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

class Semaphore(private[this] var permits: Int, fairness: Boolean) extends Serializable {

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

  protected def getQueuedThreads(): Collection[Thread] = Collections.emptySet()

  @inline private def requireNonNegative(n: Int): Unit = {
    if (n < 0) {
      throw new IllegalArgumentException()
    }
  }

  def getQueueLength(): Int = 0

  def hasQueuedThreads(): Boolean = false

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
    if (this.permits >= permits) {
      reducePermits(permits)
      true
    } else {
      false
    }
  }
}
