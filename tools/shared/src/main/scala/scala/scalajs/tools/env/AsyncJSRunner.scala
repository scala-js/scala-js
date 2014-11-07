package scala.scalajs.tools.env

trait AsyncJSRunner {
  /** Start the associated run and return. */
  def start(): Unit

  /** Abort the associated run */
  def stop(): Unit

  /** Checks whether this async runner is still running */
  def isRunning(): Boolean

  /** Await completion of the started Run. Throws if the run failed */
  def await(): Unit
}
