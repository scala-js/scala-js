package org.scalajs.jsenv.selenium

import org.scalajs.core.tools.io.VirtualJSFile
import org.scalajs.core.tools.jsdep.ResolvedJSDependency
import org.scalajs.core.tools.logging.Logger
import org.scalajs.jsenv._

import scala.concurrent.{Promise, Future}
import scala.util.Try

import scala.concurrent.ExecutionContext.Implicits.global

class SeleniumAsyncJSRunner(browserProvider: SeleniumBrowser,
    libs: Seq[ResolvedJSDependency], code: VirtualJSFile, keepAlive: Boolean)
    extends AbstractSeleniumJSRunner(browserProvider, libs, code)
    with AsyncJSRunner {

  private[this] var promise = Promise[Unit]

  def future: Future[Unit] = promise.future

  def start(logger: Logger, console: JSConsole): Future[Unit] = synchronized {
    setupLoggerAndConsole(logger, console)
    promise = Promise[Unit]
    (new SeleniumAsyncJSRunnerThread).start()
    future
  }

  override def stop(): Unit = synchronized {
    if (!keepAlive) {
      future.onComplete { _ =>
        browser.processConsoleLogs(console)
        browser.close()
      }
    }
  }

  private object Stopped extends Throwable("Stopped by SeleniumAsyncJSRunner.stop()")

  private class SeleniumAsyncJSRunnerThread extends Thread {
    override def run(): Unit = {
      // This thread should not be interrupted, so it is safe to use Trys
      val runnerInit = Try {
        browser.start()
        runAllScripts()
      }

      if (runnerInit.isFailure)
        browser.processConsoleLogs(console)

      promise.complete(runnerInit)
    }
  }
}
