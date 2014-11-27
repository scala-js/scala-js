package org.scalajs.jsenv

import scala.concurrent.Future

trait AsyncJSRunner {
  /** Start the associated run and returns a Future that completes when the run
   *  terminates.
   */
  def start(): Future[Unit]

  /** Abort the associated run */
  def stop(): Unit

  /** Checks whether this async runner is still running */
  def isRunning(): Boolean

  /** Await completion of the started Run. Throws if the run failed */
  def await(): Unit
}
