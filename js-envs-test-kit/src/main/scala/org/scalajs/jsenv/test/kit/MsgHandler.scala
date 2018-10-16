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

package org.scalajs.jsenv.test.kit

import scala.annotation.tailrec

import scala.collection.immutable

import scala.concurrent.Promise
import scala.concurrent.duration.Deadline

import scala.util.Try

import java.util.concurrent.TimeoutException

private[kit] final class MsgHandler {
  private[this] var msgs: immutable.Queue[String] =
    immutable.Queue.empty[String]
  private[this] val run = Promise[Unit]

  def onMessage(msg: String): Unit = synchronized {
    if (run.isCompleted) {
      throw new IllegalStateException(
          "run already completed but still got a message")
    }

    msgs = msgs.enqueue(msg)
    notifyAll()
  }

  def onRunComplete(t: Try[Unit]): Unit = synchronized {
    run.complete(t)
    notifyAll()
  }

  @tailrec
  def waitOnMessage(deadline: Deadline): String = synchronized {
    if (msgs.nonEmpty) {
      val (msg, newMsgs) = msgs.dequeue
      msgs = newMsgs
      msg
    } else if (run.isCompleted) {
      val cause = run.future.value.get.failed.getOrElse(null)
      throw new AssertionError("no messages left and run has completed", cause)
    } else {
      val millis = deadline.timeLeft.toMillis

      if (millis <= 0) {
        throw new TimeoutException("timed out waiting for next message")
      }

      wait(millis)
      waitOnMessage(deadline)
    }
  }

  /** @note may only be called once the run is completed. */
  def remainingMessages(): List[String] = synchronized(msgs.toList)
}
