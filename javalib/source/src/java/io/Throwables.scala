package java.io

class IOException(s: String, e: Throwable) extends Exception(s) {
  def this(e: Throwable) = this(null, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}
