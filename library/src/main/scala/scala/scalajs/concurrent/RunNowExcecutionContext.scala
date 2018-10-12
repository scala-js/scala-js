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
