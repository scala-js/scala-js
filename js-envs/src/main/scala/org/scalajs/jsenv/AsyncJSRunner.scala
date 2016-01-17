package org.scalajs.jsenv

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration

import org.scalajs.core.tools.logging.Logger

trait AsyncJSRunner {

  /** A future that completes when the associated run has terminated. */
  def future: Future[Unit]

  /**
   *  Start the associated run and returns a Future that completes
   *  when the run terminates. The returned Future is equivalent to
   *  the one returned by [[future]].
   */
  def start(logger: Logger, console: JSConsole): Future[Unit]

  /** Aborts the associated run.
   *
   *  There is no guarantee that the runner will be effectively terminated
   *  by the time this method returns. If necessary, this call can be followed
   *  by a call to `await()`.
   *
   *  If the run has already completed, this does nothing. Similarly,
   *  subsequent calls to `stop()` will do nothing.
   *
   *  This method cannot be called before `start()` has been called.
   */
  def stop(): Unit

  /**
   *  Checks whether this async runner is still running. Strictly
   *  equivalent to
   *
   *  {{{
   *  !future.isCompleted
   *  }}}
   */
  final def isRunning(): Boolean = !future.isCompleted

  /** Await completion of the started Run. Strictly equivalent to
   *
   *  {{{
   *  Await.result(future, Duration.Inf)
   *  }}}
   */
  final def await(): Unit = Await.result(future, Duration.Inf)

  /** Await completion of the started Run for the duration specified
   *  by `atMost`. Strictly equivalent to:
   *
   *  {{{
   *  Await.result(future, atMost)
   *  }}}
   *
   */
  final def await(atMost: Duration): Unit = Await.result(future, atMost)

  /** Awaits completion of the started Run for the duration specified by
   *  `atMost`, or force it to stop.
   *
   *  If any exception is thrown while awaiting completion (including a
   *  [[scala.concurrent.TimeoutException TimeoutException]]), forces the runner
   *  to stop by calling `stop()` before rethrowing the exception.
   *
   *  Strictly equivalent to:
   *
   *  {{{
   *  try await(atMost)
   *  finally stop()
   *  }}}
   */
  final def awaitOrStop(atMost: Duration): Unit = {
    try await(atMost)
    finally stop()
  }

}
