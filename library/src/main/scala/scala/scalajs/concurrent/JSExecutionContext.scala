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
  @deprecated("Not asynchronous. Use JSExecutionContext.queue instead.", "0.6.6")
  val runNow: ExecutionContextExecutor = RunNowExecutionContext
  /** execution context that submits into the JavaScript runtime's
    * task queue */
  val queue: ExecutionContextExecutor = QueueExecutionContext

  object Implicits {
    @deprecated("Not asynchronous. Use JSExecutionContext.Implicits.queue instead.", "0.6.6")
    implicit val runNow: ExecutionContextExecutor = RunNowExecutionContext
    implicit val queue: ExecutionContextExecutor = JSExecutionContext.queue
  }

}
