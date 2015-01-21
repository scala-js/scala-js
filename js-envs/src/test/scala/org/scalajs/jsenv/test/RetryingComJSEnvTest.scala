package org.scalajs.jsenv.test

import org.scalajs.jsenv.rhino.RhinoJSEnv
import org.scalajs.jsenv._

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.sem._

import scala.concurrent.Future
import scala.concurrent.duration.Duration

import org.junit.Test

class RetryingComJSEnvTest extends JSEnvTest with ComTests {

  private final val maxFails = 5

  override protected def logger: Logger = NullLogger

  protected def newJSEnv = {
    val baseEnv = new RhinoJSEnv(Semantics.Defaults)
    new RetryingComJSEnv(new FailingEnv(baseEnv), maxFails)
  }

  private final class FailingEnv(baseEnv: ComJSEnv) extends ComJSEnv {
    private[this] var fails = 0
    private[this] var failedReceive = false

    def jsRunner(classpath: CompleteClasspath, code: VirtualJSFile,
        logger: Logger, console: JSConsole): JSRunner = {
      baseEnv.jsRunner(classpath, code, logger, console)
    }

    def asyncRunner(classpath: CompleteClasspath, code: VirtualJSFile,
        logger: Logger, console: JSConsole): AsyncJSRunner = {
      baseEnv.asyncRunner(classpath, code, logger, console)
    }

    def comRunner(classpath: CompleteClasspath, code: VirtualJSFile,
        logger: Logger, console: JSConsole): ComJSRunner = {
      new FailingComJSRunner(
        baseEnv.comRunner(classpath, code, logger, console))
    }

    /** Hack to work around abstract override in ComJSRunner */
    private trait DummyJSRunner {
      def stop(): Unit = ()
    }

    private class FailingComJSRunner(baseRunner: ComJSRunner)
        extends DummyJSRunner with ComJSRunner {

      def future = baseRunner.future

      def send(msg: String): Unit = {
        maybeFail()
        baseRunner.send(msg)
      }

      def receive(timeout: Duration): String = {
        if (shouldFail) {
          failedReceive = true
          fail()
        }
        baseRunner.receive(timeout)
      }

      def start(): Future[Unit] = {
        maybeFail()
        baseRunner.start()
      }

      override def stop(): Unit = {
        maybeFail()
        baseRunner.stop()
      }

      def close(): Unit = {
        maybeFail()
        baseRunner.close()
      }

      private def shouldFail = !failedReceive && fails < maxFails

      private def maybeFail() = {
        if (shouldFail)
          fail()
      }

      private def fail() = {
        fails += 1
        sys.error("Dummy fail for testing purposes")
      }
    }
  }

}
