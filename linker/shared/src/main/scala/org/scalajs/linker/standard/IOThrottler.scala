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

package org.scalajs.linker.standard

import scala.annotation.tailrec
import scala.concurrent._

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger

private[linker] final class IOThrottler(totalSlots: Int) {
  /* This is basically a java.util.concurrent.Semaphore, but it is not
   * implemented in the javalib.
   */
  private val slots = new AtomicInteger(totalSlots)
  private val queue = new ConcurrentLinkedQueue[() => Unit]()

  def throttle[T](future: => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    if (tryGetSlot()) {
      // Fast path.
      val result = future
      result.onComplete(onComplete)
      result
    } else {
      val promise = Promise[T]()

      queue.add { () =>
        val result = future
        promise.completeWith(result)
        result.onComplete(onComplete)
      }

      // Ensure our task is not victim of a race.
      process()

      promise.future
    }
  }

  private val onComplete: Any => Unit = { _ =>
    slots.incrementAndGet()
    process()
  }

  private def process(): Unit = {
    while (!queue.isEmpty() && tryGetSlot()) {
      val work = queue.poll()
      if (work == null) {
        // We raced. Release the slot and try again.
        slots.incrementAndGet()
      } else {
        work()
      }
    }
  }

  @tailrec
  private def tryGetSlot(): Boolean = {
    val s = slots.get()
    if (s > 0) {
      if (slots.compareAndSet(s, s - 1)) {
        true
      } else {
        tryGetSlot()
      }
    } else {
      false
    }
  }
}
