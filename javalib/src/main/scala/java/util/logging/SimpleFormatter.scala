package java.util.logging

import java.util.Date

class SimpleFormatter extends Formatter {
  // The default format is implementation specific
  private val defaultFormat = "[%4$s] %1s - %3$s - %5$s"

  def format(record: LogRecord): String = {
    // As per spec we check the property or use a default
    val fmt = Option(
        System.getProperty("java.util.logging.SimpleFormatter.format"))
        .getOrElse(defaultFormat)

    String.format(fmt, new Date(record.getMillis),
        Option(record.getSourceClassName).getOrElse(""),
        Option(record.getLoggerName).getOrElse(""),
        record.getLevel,
        formatMessage(record),
        record.getThrown)
  }
}
