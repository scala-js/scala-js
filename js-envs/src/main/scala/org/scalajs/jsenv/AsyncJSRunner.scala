package org.scalajs.jsenv

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration

trait AsyncJSRunner {

  /** A future that completes when the associated run has terminated. */
  def future: Future[Unit]

  /**
   *  Start the associated run and returns a Future that completes
   *  when the run terminates. The returned Future is equivalent to
   *  the one returned by [[future]].
   */
  def start(): Future[Unit]

  /** Abort the associated run */
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
   *  by [[atMost]]. Strictly equivalent to:
   * 
   *  {{{
   *  Await.result(future, atMost)
   *  }}}
   * 
   */
  final def await(atMost: Duration): Unit = Await.result(future, atMost)

}
