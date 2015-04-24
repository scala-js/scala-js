package scala.scalajs.concurrent

import scala.concurrent.ExecutionContextExecutor

private[concurrent] object RunNowExecutionContext
    extends ExecutionContextExecutor {

  def execute(runnable: Runnable): Unit = {
    try {
      runnable.run()
    } catch {
      case t: Throwable => reportFailure(t)
    }
  }

  def reportFailure(t: Throwable): Unit =
    t.printStackTrace()

}
