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

package org.scalajs.jsenv

import scala.concurrent.duration._

private[jsenv] object Utils {
  final class OptDeadline private (
      val deadline: Deadline /* nullable */) extends AnyVal { // scalastyle:ignore
    def millisLeft: Long =
      if (deadline == null) 0
      else (deadline.timeLeft.toMillis max 1L)

    def isOverdue: Boolean =
      if (deadline == null) false
      else deadline.isOverdue
  }

  object OptDeadline {
    def apply(timeout: Duration): OptDeadline = {
      new OptDeadline(timeout match {
        case timeout: FiniteDuration => timeout.fromNow
        case _                       => null
      })
    }
  }
}
