package scala.scalajs.concurrent

import scala.concurrent.ExecutionContextExecutor

import scala.scalajs.js
import scala.scalajs.js.|

object QueueExecutionContext {
  def timeouts(): ExecutionContextExecutor =
    new TimeoutsExecutionContext

  def promises(): ExecutionContextExecutor =
    new PromisesExecutionContext

  def apply(): ExecutionContextExecutor =
    if (js.isUndefined(js.Dynamic.global.Promise)) timeouts()
    else promises()

  private final class TimeoutsExecutionContext extends ExecutionContextExecutor {
    def execute(runnable: Runnable): Unit = {
      js.Dynamic.global.setTimeout({ () =>
        try {
          runnable.run()
        } catch {
          case t: Throwable => reportFailure(t)
        }
      }, 0)
    }

    def reportFailure(t: Throwable): Unit =
      t.printStackTrace()
  }

  private final class PromisesExecutionContext extends ExecutionContextExecutor {
    private val resolvedUnitPromise = js.Promise.resolve[Unit](())

    def execute(runnable: Runnable): Unit = {
      resolvedUnitPromise.`then` { (_: Unit) =>
        try {
          runnable.run()
        } catch {
          case t: Throwable => reportFailure(t)
        }
        (): Unit | js.Thenable[Unit]
      }
    }

    def reportFailure(t: Throwable): Unit =
      t.printStackTrace()
  }
}
