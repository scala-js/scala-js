/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js JS environments   **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


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
