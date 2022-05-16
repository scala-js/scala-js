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

package org.scalajs.linker.checker

import org.scalajs.logging._

private[checker] final class LoggerErrorReporter(
    logger: Logger) extends ErrorReporter {
  import ErrorReporter.ErrorContext

  private var _errorCount: Int = 0

  def reportError(msg: String)(implicit ctx: ErrorContext): Unit = {
    logger.error(s"$ctx: $msg")
    _errorCount += 1
  }

  def errorCount: Int = _errorCount
}
