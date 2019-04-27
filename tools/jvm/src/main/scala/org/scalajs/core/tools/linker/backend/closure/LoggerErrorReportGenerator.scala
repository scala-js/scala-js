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

package org.scalajs.core.tools.linker.backend.closure

import com.google.javascript.jscomp._

import org.scalajs.core.tools.logging.Logger

private[closure] final class LoggerErrorReportGenerator(logger: Logger)
    extends SortingErrorManager.ErrorReportGenerator {

  def generateReport(manager: SortingErrorManager): Unit = {
    /* We should use `manager.getSortedDiagnostics()` rather than using
     * separately getWarnings() and getErrors(), but it is package-private.
     */
    for (warning <- manager.getWarnings)
      logger.warn(warning.toString())
    for (error <- manager.getErrors)
      logger.error(error.toString())

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

}
