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

package scala.concurrent

import scala.scalajs.js
import scala.scalajs.js.|

private[concurrent] object JSGlobalExecutionContext {
  def apply(): ExecutionContextExecutor =
    if (js.typeOf(js.Dynamic.global.Promise) == "undefined") new TimeoutsExecutionContext
    else new PromisesExecutionContext

  private final class TimeoutsExecutionContext extends ExecutionContextExecutor {
    def execute(runnable: Runnable): Unit = {
      js.Dynamic.global.setTimeout({ () =>
        try {
          runnable.run()
        } catch {
          case t: Throwable => reportFailure(t)
        }
      }: js.Function0[Unit], 0)
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
