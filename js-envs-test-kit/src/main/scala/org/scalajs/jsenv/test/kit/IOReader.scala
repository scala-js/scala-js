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

import scala.concurrent.Promise
import scala.concurrent.duration.Deadline

import scala.util.Try

import java.nio.ByteBuffer
import java.nio.channels.{Channels, ReadableByteChannel}

import java.io.InputStream

import java.util.concurrent._

private[kit] final class IOReader {
  private val executor = Executors.newSingleThreadExecutor()

  private[this] var _closed = false
  private[this] var _channel: ReadableByteChannel = _
  private[this] val run = Promise[Unit]()

  def read(len: Int, deadline: Deadline): ByteBuffer = {
    val chan = try {
      waitOnChannel(deadline)
    } catch {
      case t: TimeoutException =>
        throw new TimeoutException("timed out waiting on run to call onOutputStream")
    }

    val task = executor.submit(
        new Callable[ByteBuffer] {
          def call(): ByteBuffer = readLoop(chan, ByteBuffer.allocate(len))
        }
    )

    try {
      task.get(millisLeft(deadline), TimeUnit.MILLISECONDS)
    } catch {
      case e: ExecutionException =>
        throw e.getCause()

      case e: CancellationException =>
        throw new AssertionError("unexpected exception while running read task", e)

      case e: InterruptedException =>
        throw new AssertionError("unexpected exception while running read task", e)

      case e: TimeoutException =>
        task.cancel(true)
        throw new TimeoutException("timed out reading from stream")
    }
  }

  def onInputStream(in: InputStream): Unit = synchronized {
    require(_channel == null, "onInputStream called twice")

    if (_closed) {
      in.close()
    } else {
      _channel = Channels.newChannel(in)
      notifyAll()
    }
  }

  def onRunComplete(t: Try[Unit]): Unit = synchronized {
    run.complete(t)
    notifyAll()
  }

  def close(): Unit = synchronized {
    if (_channel != null)
      _channel.close()
    _closed = true
  }

  private def waitOnChannel(deadline: Deadline) = synchronized {
    while (_channel == null && !run.isCompleted)
      wait(millisLeft(deadline))

    if (_channel == null) {
      throw new AssertionError(
          "run completed and did not call onOutputStream", runFailureCause())
    }

    _channel
  }

  private def runFailureCause() = {
    require(run.isCompleted)
    run.future.value.get.failed.getOrElse(null)
  }

  @tailrec
  private def readLoop(chan: ReadableByteChannel, buf: ByteBuffer): buf.type = {
    if (chan.read(buf) == -1) {
      // If we have reached the end of the stream, we wait for completion of the
      // run so we can report a potential failure as a cause.
      synchronized {
        while (!run.isCompleted)
          wait()
      }

      throw new AssertionError("reached end of stream", runFailureCause())
    } else if (buf.hasRemaining()) {
      readLoop(chan, buf)
    } else {
      buf.flip()
      buf
    }
  }

  private def millisLeft(deadline: Deadline): Long = {
    val millis = deadline.timeLeft.toMillis

    if (millis <= 0) {
      throw new TimeoutException
    }

    millis
  }
}
