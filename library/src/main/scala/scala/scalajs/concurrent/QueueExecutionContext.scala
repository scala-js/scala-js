package scala.scalajs.concurrent

import scala.concurrent.ExecutionContext
import scalajs.js

private[concurrent] object QueueExecutionContext extends ExecutionContext {

  def execute(runnable: Runnable) = {
    val lambda: js.Function = () =>
      try { runnable.run() } catch { case t: Throwable => reportFailure(t) }
    js.Dynamic.global.setTimeout(lambda, 0)
  }

  def reportFailure(t: Throwable) =
    Console.err.println("Failure in async execution: " + t)

}
