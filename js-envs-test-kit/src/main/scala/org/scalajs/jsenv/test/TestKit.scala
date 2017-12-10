package org.scalajs.jsenv.test

import java.io._
import java.nio.CharBuffer
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeoutException

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.Deadline

import org.scalajs.io.{VirtualJSFile, MemVirtualJSFile}

import org.scalajs.jsenv._

import org.junit.Assert._
import org.junit.Assume._

private[test] final class TestKit(config: JSEnvSuiteConfig, withCom: Boolean) {
  assumeTrue("JSEnv needs com support", config.supportsCom || !withCom)

  def start(code: String, config: RunConfig): JSRun = {
    val vf = new MemVirtualJSFile("testScript.js").withContent(code)
    start(vf, config)
  }

  def start(vf: VirtualJSFile, runConfig: RunConfig): JSRun = {
    val input = Input.ScriptsToLoad(List(vf))
    if (withCom)
      config.jsEnv.startWithCom(input, runConfig, _ => ())
    else
      config.jsEnv.start(input, runConfig)
  }

  /** Await a run started with [[start]] after it got closed.
   *
   *  This expects successful termination depending on [[withCom]]: we are
   *  allowed to expect successful termination with a com.
   */
  def awaitAfterClose(run: JSRun): Unit = {
    if (withCom)
      Await.result(run.future, config.awaitTimeout)
    else
      Await.ready(run.future, config.awaitTimeout)
  }

  implicit final class RunMatcher private[TestKit] (codeStr: String) {
    def hasOutput(expectedOut: String): Unit = {
      val comparator = new OutputComparator(expectedOut)
      val config = comparator.configure(RunConfig())
      val run = start(codeStr, config)

      try {
        comparator.compare()
      } finally {
        run.close()
      }

      awaitAfterClose(run)
    }

    def fails(): Unit = {
      // We do not want to spam the console with error output, so we ignore it.
      def ignoreStreams(out: Option[InputStream], err: Option[InputStream]) = {
        out.foreach(_.close())
        err.foreach(_.close())
      }

      val runConfig = RunConfig()
        .withOnOutputStream(ignoreStreams)
        .withInheritOut(false)
        .withInheritErr(false)

      val run = start(codeStr, runConfig)
      try {
        Await.ready(run.future, config.awaitTimeout)
        assertTrue("Code snipped should fail", run.future.value.get.isFailure)
      } finally {
        run.close()
      }
    }
  }

  private class OutputComparator(expectedOut: String) {
    private val waiter = new StreamWaiter

    def configure(config: RunConfig): RunConfig = {
      config
        .withOnOutputStream(waiter.onOutputStream _)
        .withInheritOut(false)
        .withInheritErr(true)
    }

    def compare(): Unit = {
      val deadline = config.awaitTimeout.fromNow
      val stream = waiter.waitForStream(deadline)

      /* When reading, we use a CharBuffer for easy index tracking. However, we
       * back it by an array so we can easily read partial results.
       */
      val in = new InputStreamReader(stream, StandardCharsets.UTF_8)
      val arr = new Array[Char](expectedOut.length)
      val buf = CharBuffer.wrap(arr)
      while (buf.hasRemaining && tryRead(in, buf, deadline) != -1) {
        val len = buf.position
        assertEquals("Partial check",
            expectedOut.substring(0, len),
            new String(arr, 0, len))
      }

      buf.flip()
      assertEquals(expectedOut, buf.toString)
    }
  }

  @tailrec
  private final def tryRead(in: Reader, buf: CharBuffer, deadline: Deadline): Int = {
    if (deadline.isOverdue) {
      buf.flip()
      throw new TimeoutException("Timed out out waiting for output. Got so far: " + buf.toString)
    }

    if (in.ready()) {
      in.read(buf)
    } else {
      Thread.sleep(50)
      tryRead(in, buf, deadline)
    }
  }

  private class StreamWaiter {
    private[this] var stream: InputStream = _

    def waitForStream(deadline: Deadline): InputStream = synchronized {
      while (stream == null) {
        val m = deadline.timeLeft.toMillis
        if (m > 0)
          wait(m)
        else
          throw new TimeoutException("Timed out waiting for stdout")
      }

      stream
    }

    def onOutputStream(out: Option[InputStream],
        err: Option[InputStream]): Unit = synchronized {
      require(err.isEmpty, "Got error stream, did not request it.")
      require(stream == null, "Got called twice")

      stream = out.getOrElse(new ByteArrayInputStream(new Array[Byte](0)))
      notifyAll()
    }
  }
}

