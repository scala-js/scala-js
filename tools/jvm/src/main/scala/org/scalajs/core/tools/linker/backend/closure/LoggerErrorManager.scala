package org.scalajs.core.tools.linker.backend.closure

import com.google.javascript.jscomp.{ BasicErrorManager, CheckLevel, JSError }

import org.scalajs.core.tools.logging.Logger

/** A Google Closure Error Manager that forwards to a tools.logging.Logger */
private[closure] class LoggerErrorManager(private val log: Logger)
    extends BasicErrorManager {

  /** Ugly hack to disable FRACTIONAL_BITWISE_OPERAND and UNREACHABLE_CODE warnings.
   *  Since its DiagnosticType (PeepholeFoldConstants.FRACTIONAL_BITWISE_OPERAND) is
   *  package private. Similar issue for UNREACHABLE_CODE.
   */
  override def report(level: CheckLevel, error: JSError): Unit = {
    if (error.getType.key == "JSC_FRACTIONAL_BITWISE_OPERAND" ||
        error.getType.key == "JSC_UNREACHABLE_CODE") {
      super.report(CheckLevel.OFF, error)
    } else {
      super.report(level, error)
    }
  }

  def println(level: CheckLevel, error: JSError): Unit = level match {
    case CheckLevel.WARNING => log.warn (s"Closure: ${error}")
    case CheckLevel.ERROR   => log.error(s"Closure: ${error}")
    case CheckLevel.OFF     =>
  }

  protected def printSummary(): Unit = {
    val msg = s"Closure: ${getErrorCount} error(s), ${getWarningCount} warning(s)"

    if (getErrorCount > 0)
      log.error(msg)
    else if (getWarningCount > 0)
      log.warn(msg)
    else
      log.info(msg)
  }

}
