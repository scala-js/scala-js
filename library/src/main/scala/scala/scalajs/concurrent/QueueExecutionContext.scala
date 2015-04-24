package scala.scalajs.concurrent

import scala.concurrent.ExecutionContextExecutor
import scalajs.js

private[concurrent] object QueueExecutionContext
    extends ExecutionContextExecutor {

  def execute(runnable: Runnable): Unit = {
    js.timers.setTimeout(0) {
      try {
        runnable.run()
      } catch {
        case t: Throwable => reportFailure(t)
      }
    }
  }

  def reportFailure(t: Throwable): Unit =
    t.printStackTrace()

}
