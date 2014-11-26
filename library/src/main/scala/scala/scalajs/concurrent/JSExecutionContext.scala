package scala.scalajs.concurrent

import scala.concurrent.ExecutionContextExecutor

/**
 * Execution contexts for use in JavaScript
 *
 * Enables the use of Futures/Promises
 * @author Tobias Schlatter
 */
object JSExecutionContext {

  /** execution context that runs immediately. beware of stack growth! */
  val runNow: ExecutionContextExecutor = RunNowExecutionContext
  /** execution context that submits into the JavaScript runtime's
    * task queue */
  val queue: ExecutionContextExecutor = QueueExecutionContext

  object Implicits {
    implicit val runNow: ExecutionContextExecutor = JSExecutionContext.runNow
    implicit val queue: ExecutionContextExecutor = JSExecutionContext.queue
  }

}
