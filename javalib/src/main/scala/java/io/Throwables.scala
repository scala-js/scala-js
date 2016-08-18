package java.io

class IOException(s: String, e: Throwable) extends Exception(s, e) {
  def this(e: Throwable) = this(if (e == null) null else e.toString, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class EOFException(s: String) extends IOException(s) {
  def this() = this(null)
}

class UTFDataFormatException(s: String) extends IOException(s) {
  def this() = this(null)
}

class UnsupportedEncodingException(s: String) extends IOException(s) {
  def this() = this(null)
}

abstract class ObjectStreamException protected (s: String) extends IOException(s) {
  protected def this() = this(null)
}

class NotSerializableException(s: String) extends ObjectStreamException(s) {
  def this() = this(null)
}
