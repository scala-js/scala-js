/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.logging._

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
 */
final class RetryingComJSEnv(val baseEnv: ComJSEnv,
    val maxRetries: Int) extends ComJSEnv {

  def this(baseEnv: ComJSEnv) = this(baseEnv, 5)

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
    new RetryingComJSRunner(classpath, code, logger, console)
  }

  /** Hack to work around abstract override in ComJSRunner */
  private trait DummyJSRunner {
    def stop(): Unit = ()
  }

  private class RetryingComJSRunner(classpath: CompleteClasspath,
      code: VirtualJSFile, logger: Logger,
      console: JSConsole) extends DummyJSRunner with ComJSRunner {

    private[this] val promise = Promise[Unit]

    private[this] var curRunner =
      baseEnv.comRunner(classpath, code, logger, console)

    private[this] var hasReceived = false
    private[this] var retryCount = 0

    private[this] val log = mutable.Buffer.empty[LogItem]

    def future: Future[Unit] = promise.future

    def start(): Future[Unit] = {
      require(log.isEmpty, "start() may only be called once")
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

        logger.warn("Retrying to launch a " + baseEnv.getClass.getName +
          " after " + cause.toString)

        val oldRunner = curRunner

        curRunner = try {
          baseEnv.comRunner(classpath, code, logger, console)
        } catch {
          case NonFatal(t) =>
            logger.error("Could not retry: creating an new runner failed: " +
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
        runner.start() onComplete { result =>
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
