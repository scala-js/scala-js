package org.scalajs.jsenv.test

import java.util.concurrent.TimeoutException

import org.scalajs.io.{VirtualBinaryFile, MemVirtualBinaryFile}

import org.scalajs.jsenv._

import org.junit.Assert.fail

import scala.collection.immutable
import scala.concurrent.Await
import scala.concurrent.duration.Duration

private[test] final class TestComKit(config: JSEnvSuiteConfig) {
  def start(code: String, runConfig: RunConfig): Run = {
    val vf = new MemVirtualBinaryFile("testScript.js").withStringUTF8(code)
    start(vf, runConfig)
  }

  def start(vf: VirtualBinaryFile, runConfig: RunConfig): Run = {
    val input = Input.ScriptsToLoad(List(vf))
    new Run(input, runConfig)
  }

  final class Run(input: Input, runConfig: RunConfig) {
    val run: JSComRun = config.jsEnv.startWithCom(input, runConfig, onMessage _)

    private var received = immutable.Queue.empty[String]

    def waitNextMessage(): String = synchronized {
      val deadline = config.awaitTimeout.fromNow

      while (received.isEmpty) {
        val m = deadline.timeLeft.toMillis
        if (m > 0)
          wait(m)
        else
          throw new TimeoutException("Timed out waiting for next message")
      }

      val (msg, newReceived) = received.dequeue
      received = newReceived
      msg
    }

    def closeAndWait(): Unit = {
      run.close()

      // Run must complete successfully.
      Await.result(run.future, config.awaitTimeout)

      synchronized {
        if (received.nonEmpty) {
          fail(s"There were unhandled messages: $received")
        }
      }
    }

    private def onMessage(msg: String) = synchronized {
      received = received.enqueue(msg)
      notifyAll()
    }
  }
}
