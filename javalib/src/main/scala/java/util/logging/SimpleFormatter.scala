package java.util.logging

import java.util.Date

class SimpleFormatter extends Formatter {
  // The default format is implementation specific
  private val defaultFmt = "[%4$s] %1s - %3$s - %5$s"

  def format(record: LogRecord): String = {
    // As per spec we check the property or use a default
    val fmt =
      System.getProperty("java.util.logging.SimpleFormatter.format", defaultFmt)

    String.format(fmt, new Date(record.getMillis),
        Option(record.getSourceClassName).getOrElse(""),
        Option(record.getLoggerName).getOrElse(""),
        record.getLevel,
        formatMessage(record),
        record.getThrown)
  }
}
