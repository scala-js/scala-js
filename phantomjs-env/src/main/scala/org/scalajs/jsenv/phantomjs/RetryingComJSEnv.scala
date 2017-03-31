/*                     __                                                   *\
**     ________ ___   / /  ___      __ ____  PhantomJS support for Scala.js **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL       **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    https://www.scala-js.org/      **
** /____/\___/_/ |_/____/_/ | |__/ /____/                                   **
**                          |/____/                                         **
\*                                                                          */

package org.scalajs.jsenv.phantomjs

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.logging.Logger

import org.scalajs.jsenv._

import scala.concurrent.{Future, Promise, ExecutionContext}
import scala.concurrent.duration.Duration
import scala.collection.mutable
import scala.annotation.tailrec
import scala.util.control.NonFatal
import scala.util.{Try, Failure, Success}

/** A RetryingComJSEnv allows to automatically retry if a call to the underlying
 *  ComJSRunner fails.
 *
 *  While it protects the JVM side from observing state that differs inbetween
 *  runs that have been retried, it assumes that the executed JavaScript code
 *  does not have side-effects other than the ones visible through the channel
 *  (e.g. writing to a file). It is the users responsibility to ensure this
 *  property.
 *
 *  No retrying is performed for synchronous, or normal asynchronous runs.
 *
 *  Although `RetryingComJSEnv` is agnostic of the underlying JS env, and is
 *  therefore not tied to PhantomJS, it is most often used to compensate for
 *  flakiness effects of PhantomJS.
 */
final class RetryingComJSEnv(val baseEnv: ComJSEnv,
    val maxRetries: Int) extends ComJSEnv {

  def this(baseEnv: ComJSEnv) = this(baseEnv, 5)

  def name: String = s"Retrying ${baseEnv.name}"

  def jsRunner(libs: Seq[VirtualJSFile], code: VirtualJSFile): JSRunner =
    baseEnv.jsRunner(libs, code)

  def asyncRunner(libs: Seq[VirtualJSFile], code: VirtualJSFile): AsyncJSRunner =
    baseEnv.asyncRunner(libs, code)

  def comRunner(libs: Seq[VirtualJSFile], code: VirtualJSFile): ComJSRunner =
    new RetryingComJSRunner(libs, code)

  /** Hack to work around abstract override in ComJSRunner */
  private trait DummyJSRunner {
    def stop(): Unit = ()
  }

  private class RetryingComJSRunner(libs: Seq[VirtualJSFile],
      code: VirtualJSFile) extends DummyJSRunner with ComJSRunner {

    private[this] val promise = Promise[Unit]

    private[this] var curRunner = baseEnv.comRunner(libs, code)

    private[this] var hasReceived = false
    private[this] var retryCount = 0

    private[this] val log = mutable.Buffer.empty[LogItem]

    private[this] var _logger: Logger = _
    private[this] var _console: JSConsole = _

    def future: Future[Unit] = promise.future

    def start(logger: Logger, console: JSConsole): Future[Unit] = {
      require(log.isEmpty, "start() may only be called once")

      _logger = logger
      _console = console

      logAndDo(Start)
      future
    }

    override def stop(): Unit = {
      require(log.nonEmpty, "start() must have been called")
      close()
      logAndDo(Stop)
    }

    def send(msg: String): Unit = {
      require(log.nonEmpty, "start() must have been called")
      logAndDo(Send(msg))
    }

    def receive(timeout: Duration): String = {
      @tailrec
      def recLoop(): String = {
        // Need to use Try for tailrec
        Try {
          val result = curRunner.receive(timeout)
          // At this point, we are sending state to the JVM, we cannot retry
          // after this.
          hasReceived = true
          result
        } match {
          case Failure(t) =>
            retry(t)
            recLoop()
          case Success(v) => v
        }
      }

      recLoop()
    }

    def close(): Unit = {
      require(log.nonEmpty, "start() must have been called")
      logAndDo(Close)
    }

    @tailrec
    private final def retry(cause: Throwable): Unit = {
      retryCount += 1

      // Accesses to promise and swaps in the curRunner must be synchronized
      synchronized {
        if (hasReceived || retryCount > maxRetries || promise.isCompleted)
          throw cause

        _logger.warn("Retrying to launch a " + baseEnv.getClass.getName +
          " after " + cause.toString)

        val oldRunner = curRunner

        curRunner = try {
          baseEnv.comRunner(libs, code)
        } catch {
          case NonFatal(t) =>
            _logger.error("Could not retry: creating an new runner failed: " +
              t.toString)
            throw cause
        }

        try oldRunner.stop() // just in case
        catch {
          case NonFatal(t) => // ignore
        }
      }

      // Replay the whole log
      // Need to use Try for tailrec
      Try(log.foreach(executeTask)) match {
        case Failure(t) => retry(t)
        case _ =>
      }
    }

    private def logAndDo(task: LogItem) = {
      log += task
      try executeTask(task)
      catch {
        case NonFatal(t) => retry(t)
      }
    }

    private def executeTask(task: LogItem) = task match {
      case Start =>
        import ExecutionContext.Implicits.global
        val runner = curRunner
        runner.start(_logger, _console) onComplete { result =>
          // access to curRunner and promise must be synchronized
          synchronized {
            if (curRunner eq runner)
              promise.complete(result)
          }
        }
      case Send(msg) =>
        curRunner.send(msg)
      case Stop =>
        curRunner.stop()
      case Close =>
        curRunner.close()
    }

    private sealed trait LogItem
    private case object Start extends LogItem
    private case class Send(msg: String) extends LogItem
    private case object Stop extends LogItem
    private case object Close extends LogItem

  }

}
