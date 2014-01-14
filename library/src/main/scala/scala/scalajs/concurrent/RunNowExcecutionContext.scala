package scala.scalajs.concurrent

import scala.concurrent.ExecutionContext

private[concurrent] object RunNowExecutionContext extends ExecutionContext {

  def execute(runnable: Runnable) =
    try   { runnable.run() }
    catch { case t: Throwable => reportFailure(t) }

  def reportFailure(t: Throwable) =
    Console.err.println("Failure in async execution: " + t)

}
