package scala.scalajs.concurrent

import scala.concurrent.ExecutionContext

/**
 * Execution contexts for use in JavaScript
 *
 * Enables the use of Futures/Promises
 * @author Tobias Schlatter
 */
object JSExecutionContext {

  /** execution context that runs immediately. beware of stack growth! */
  val runNow = RunNowExecutionContext
  /** execution context that submits into the JavaScript runtime's
    * task queue */
  val queue  = QueueExecutionContext

  object Implicits {
    implicit val runNow: ExecutionContext = JSExecutionContext.runNow
    implicit val queue:  ExecutionContext = JSExecutionContext.queue
  }

}
