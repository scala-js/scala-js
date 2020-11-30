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

import java.util.concurrent.{ConcurrentLinkedQueue, Semaphore}

private[linker] final class IOThrottler(totalSlots: Int) {

  private val slots = new Semaphore(totalSlots)
  private val queue = new ConcurrentLinkedQueue[() => Unit]()

  def throttle[T](future: => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    if (slots.tryAcquire()) {
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
    slots.release()
    process()
  }

  private def process(): Unit = {
    while (!queue.isEmpty() && slots.tryAcquire()) {
      val work = queue.poll()
      if (work == null) {
        // We raced. Release the slot and try again.
        slots.release()
      } else {
        work()
      }
    }
  }
}
