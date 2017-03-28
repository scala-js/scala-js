package org.scalajs.jsenv.phantomjs

import org.scalajs.core.tools.io.VirtualJSFile
import org.scalajs.core.tools.logging._

import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.jsenv.{ComJSRunner, JSConsole, _}
import org.scalajs.jsenv.test._

import scala.concurrent.Future
import scala.concurrent.duration.Duration

class RetryingComJSEnvTest extends JSEnvTest with ComTests {

  private final val maxFails = 5

  // Don't log anything here
  override protected def start(runner: AsyncJSRunner): Future[Unit] = {
    runner.start(NullLogger, ConsoleJSConsole)
  }

  protected def newJSEnv: RetryingComJSEnv =
    new RetryingComJSEnv(new FailingEnv(new NodeJSEnv), maxFails)

  private final class FailingEnv(baseEnv: ComJSEnv) extends ComJSEnv {
    def name: String = s"FailingJSEnv of ${baseEnv.name}"

    private[this] var fails = 0
    private[this] var failedReceive = false

    def jsRunner(libs: Seq[VirtualJSFile],
        code: VirtualJSFile): JSRunner = {
      baseEnv.jsRunner(libs, code)
    }

    def asyncRunner(libs: Seq[VirtualJSFile],
        code: VirtualJSFile): AsyncJSRunner = {
      baseEnv.asyncRunner(libs, code)
    }

    def comRunner(libs: Seq[VirtualJSFile],
        code: VirtualJSFile): ComJSRunner = {
      new FailingComJSRunner(baseEnv.comRunner(libs, code))
    }

    /** Hack to work around abstract override in ComJSRunner */
    private trait DummyJSRunner {
      def stop(): Unit = ()
    }

    private class FailingComJSRunner(baseRunner: ComJSRunner)
        extends DummyJSRunner with ComJSRunner {

      def future: Future[Unit] = baseRunner.future

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

      def start(logger: Logger, console: JSConsole): Future[Unit] = {
        maybeFail()
        baseRunner.start(logger, console)
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
