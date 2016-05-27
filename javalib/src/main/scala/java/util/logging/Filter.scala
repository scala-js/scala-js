package java.util.logging

trait Filter {
  def isLoggable(record: LogRecord): Boolean
}
