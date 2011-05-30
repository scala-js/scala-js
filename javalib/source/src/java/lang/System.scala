package java
package lang

object System {
  var out: java.io.PrintStream = StandardOutPrintStream
  var err: java.io.PrintStream = StandardErrPrintStream
  var in: java.io.InputStream = null

  def getProperty(key: String): String = sys.error("unimplemented")
  def getProperty(key: String, default: String): String = sys.error("unimplemented")
  def currentTimeMillis(): scala.Long = sys.error("unimplemented")
  def exit(status: scala.Int): Unit = sys.error("unimplemented")
  def getenv(): java.util.Map[String,String] = sys.error("unimplemented")
  def getenv(name: String): String = sys.error("unimplemented")
  def getProperties(): java.util.Properties = sys.error("unimplemented")
  def clearProperty(key: String): String = sys.error("unimplemented")
  def setProperty(key: String, value: String): String = sys.error("unimplemented")
  def arraycopy(src: Object, srcPos: scala.Int, dest: Object, destPos: scala.Int, length: scala.Int): Unit = sys.error("unimplemented")
  def identityHashCode(x: Object): scala.Int = sys.error("unimplemented")
  def gc(): Unit = {}
}

private[lang] object StandardOutPrintStream extends io.PrintStream(StandardOut, true) {
  @native override protected[lang] def writeString(s: String): Unit
}

private[lang] object StandardErrPrintStream extends io.PrintStream(StandardErr, true) {
  @native override protected[lang] def writeString(s: String): Unit
}

private[lang] object StandardOut extends io.OutputStream {
  def write(b: Int) = StandardOutPrintStream.writeString(b.toChar.toString)
}

private[lang] object StandardErr extends io.OutputStream {
  def write(b: Int) = StandardErrPrintStream.writeString(b.toChar.toString)
}
