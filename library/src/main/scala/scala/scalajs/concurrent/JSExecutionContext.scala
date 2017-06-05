package scala.scalajs.concurrent

import scala.concurrent.ExecutionContextExecutor

/**
 * Execution contexts for use in JavaScript
 *
 * Enables the use of Futures/Promises
 * @author Tobias Schlatter
 */
object JSExecutionContext {

  /** Execution context that submits into the JavaScript runtime's task queue.
   *
   *  It uses JavaScript [[scala.scalajs.js.Promise Promises]] if available,
   *  or `setTimeout`s otherwise.
   */
  val queue: ExecutionContextExecutor = QueueExecutionContext()

  object Implicits {
    implicit val queue: ExecutionContextExecutor = JSExecutionContext.queue
  }

}
