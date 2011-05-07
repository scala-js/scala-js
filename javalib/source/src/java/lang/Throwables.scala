package java.lang

class Throwable(message: String, cause: Throwable) {
  def this() = this(null, null)
  def this(message: String) = this(message, null)
  def this(cause: Throwable) = this(null, cause)

  def getStackTrace(): scala.Array[StackTraceElement] = null
  def getMessage(): String = message
  def printStackTrace(): Unit = ()
  def getCause: Throwable = cause
  def fillInStackTrace(): Throwable = this
}

class Error(message: String, cause: Throwable) extends Throwable(message, cause) {
  def this() = this(null, null)
  def this(message: String) = this(message, null)
  def this(cause: Throwable) = this(null, cause)
}

class AssertionError private (message: String) extends Throwable(message) {
  def this(o: Object) = this(if (o == null) null else o.toString)
  def this(b: scala.Boolean) = this(b.toString)
  def this(c: scala.Char) = this(c.toString)
  def this(d: scala.Double) = this(d.toString)
  def this(f: scala.Float) = this(f.toString)
  def this(i: scala.Int) = this(i.toString)
  def this(l: scala.Long) = this(l.toString)
}

class Exception(message: String, cause: Throwable) extends Throwable(message, cause) {
  def this() = this(null, null)
  def this(message: String) = this(message, null)
  def this(cause: Throwable) = this(null, cause)
}

class RuntimeException(message: String, cause: Throwable) extends Exception(message, cause) {
  def this() = this(null, null)
  def this(message: String) = this(message, null)
  def this(cause: Throwable) = this(null, cause)
}

class UnsupportedOperationException(message: String, cause: Throwable) extends RuntimeException(message, cause) {
  def this() = this(null, null)
  def this(message: String) = this(message, null)
  def this(cause: Throwable) = this(null, cause)
}

class IndexOutOfBoundsException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class ArrayIndexOutOfBoundsException(s: String) extends IndexOutOfBoundsException(s) {
  def this() = this(null)
}

class StringIndexOutOfBoundsException(s: String) extends IndexOutOfBoundsException(s) {
  def this() = this(null)
}

class NullPointerException(message: String) extends Exception(message) {
  def this() = this(null)
}

class IllegalArgumentException(message: String) extends RuntimeException(message) {
  def this() = this(null)
}

class NumberFormatException(message: String) extends IllegalArgumentException(message) {
  def this() = this(null)
}
