/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools

import org.scalajs.core.tools.logging._

package object optimizer {

  private[optimizer] def logTime[A](logger: Logger,
      title: String)(body: => A): A = {
    val startTime = System.nanoTime()
    val result = body
    val endTime = System.nanoTime()
    val elapsedTime = endTime - startTime
    logger.time(title, elapsedTime)
    result
  }

}
