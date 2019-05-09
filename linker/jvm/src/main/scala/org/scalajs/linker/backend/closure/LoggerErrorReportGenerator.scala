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

package org.scalajs.linker.backend.closure

import com.google.javascript.jscomp._

import org.scalajs.logging._

private[closure] final class LoggerErrorReportGenerator(logger: Logger)
    extends SortingErrorManager.ErrorReportGenerator {

  def generateReport(manager: SortingErrorManager): Unit = {
    /* We should use `manager.getSortedDiagnostics()` rather than using
     * separately getWarnings() and getErrors(), but it is package-private.
     */
    immutableListForeach(manager.getWarnings) { warning =>
      logger.warn(warning.toString())
    }
    immutableListForeach(manager.getErrors) { error =>
      logger.error(error.toString())
    }

    val errorCount = manager.getErrorCount
    val warningCount = manager.getWarningCount
    val msg = s"Closure: $errorCount error(s), $warningCount warning(s)"

    if (errorCount > 0)
      logger.error(msg)
    else if (warningCount > 0)
      logger.warn(msg)
    else
      logger.info(msg)
  }

  private def immutableListForeach[A](
      list: com.google.common.collect.ImmutableList[A])(f: A => Unit): Unit = {
    list.forEach(new java.util.function.Consumer[A] {
      def accept(x: A): Unit = f(x)
    })
  }

}
