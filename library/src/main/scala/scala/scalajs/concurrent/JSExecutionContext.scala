/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.scalajs.concurrent

import scala.concurrent.ExecutionContextExecutor

/**
 * Execution contexts for use in JavaScript
 *
 * Enables the use of Futures/Promises
 * @author Tobias Schlatter
 */
object JSExecutionContext {

  /** Execution context that runs immediately.
   *
   *  Beware of stack growth!
   */
  @deprecated("Not asynchronous. Use JSExecutionContext.queue instead.", "0.6.6")
  val runNow: ExecutionContextExecutor = RunNowExecutionContext

  /** Execution context that submits into the JavaScript runtime's task queue.
   *
   *  It uses JavaScript [[scala.scalajs.js.Promise Promises]] if available,
   *  or `setTimeout`s otherwise.
   */
  val queue: ExecutionContextExecutor = QueueExecutionContext()

  object Implicits {
    @deprecated("Not asynchronous. Use JSExecutionContext.Implicits.queue instead.", "0.6.6")
    implicit val runNow: ExecutionContextExecutor = RunNowExecutionContext
    implicit val queue: ExecutionContextExecutor = JSExecutionContext.queue
  }

}
