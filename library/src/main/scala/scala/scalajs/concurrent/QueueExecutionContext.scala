package scala.scalajs.concurrent

import scala.concurrent.ExecutionContextExecutor

import scala.scalajs.js
import scala.scalajs.js.|

private[concurrent] object QueueExecutionContext
    extends ExecutionContextExecutor {

  private val enqueue: js.Function1[js.Function0[Unit], Unit] = {
    if (js.isUndefined(js.Dynamic.global.Promise)) {
      { (f: js.Function0[Unit]) =>
        js.Dynamic.global.setTimeout(f, 0)
        ()
      }
    } else {
      val resolvedUnitPromise = js.Promise.resolve[Unit](())

      { (f: js.Function0[Unit]) =>
        resolvedUnitPromise.`then`(
            f.asInstanceOf[js.Function1[Unit, Unit | js.Thenable[Unit]]])
        ()
      }
    }
  }

  def execute(runnable: Runnable): Unit = {
    enqueue { () =>
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
