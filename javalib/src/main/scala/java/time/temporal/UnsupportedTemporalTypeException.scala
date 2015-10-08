package java.time.temporal

import java.time.DateTimeException

class UnsupportedTemporalTypeException(message: String, cause: Throwable)
    extends DateTimeException(message, cause) {
  def this(message: String) = this(message, null)
}
