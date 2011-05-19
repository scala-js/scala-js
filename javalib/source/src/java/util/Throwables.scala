package java.util

class NoSuchElementException(message: String, cause: Throwable) extends RuntimeException(message, cause) {
  def this() = this(null, null)
  def this(message: String) = this(message, null)
  def this(cause: Throwable) = this(null, cause)
}
