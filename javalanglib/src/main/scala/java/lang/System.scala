package java
package lang

import scala.scalajs.js
import js.Dynamic.global

object System {
  var out: java.io.PrintStream = StandardOutPrintStream
  var err: java.io.PrintStream = StandardErrPrintStream
  var in: java.io.InputStream = null

  def currentTimeMillis(): scala.Long = {
    (new js.Date).getTime().toLong
  }

  private[this] val getHighPrecisionTime: js.Function0[scala.Double] = {
    if (!(!global.performance)) {
      if (!(!global.performance.now)) {
        () => global.performance.now().asInstanceOf[scala.Double]
      } else if (!(!(global.performance.webkitNow))) {
        () => global.performance.webkitNow().asInstanceOf[scala.Double]
      } else {
        () => new js.Date().getTime()
      }
    } else {
      () => new js.Date().getTime()
    }
  }

  def nanoTime(): scala.Long =
    (getHighPrecisionTime() * 1000000).toLong

  def arraycopy(src: Object, srcPos: scala.Int, dest: Object,
      destPos: scala.Int, length: scala.Int): Unit = sys.error("stub")

  // TODO 0.6.0: make identityHashCode itself the primitive
  def identityHashCode(x: Object): scala.Int = identityHashCode0(x)
  protected[this] def identityHashCode0(x: Object): scala.Int = sys.error("stub")

  def getProperties(): java.util.Properties = sys.error("System.getProperties() not implemented")
  def getProperty(key: String): String = sys.error("System.getProperty() not implemented")
  def getProperty(key: String, default: String): String = sys.error("System.getProperty() not implemented")
  def clearProperty(key: String): String = sys.error("System.clearProperty() not implemented")
  def setProperty(key: String, value: String): String = sys.error("System.setProperty() not implemented")

  def getenv(): java.util.Map[String,String] = sys.error("System.getenv() not implemented")
  def getenv(name: String): String = sys.error("System.getenv() not implemented")

  def exit(status: scala.Int) = Runtime.getRuntime().exit(status)
  def gc() = Runtime.getRuntime().gc()
}

private[lang] trait JSConsoleBasedPrintStream extends io.PrintStream {
  /** whether buffer is flushed. Can be true even if buffer != "" because of
   *  line continuations. However, if !flushed => buffer != ""
   */
  private var flushed: scala.Boolean = true
  private var buffer: String = ""

  private val lineContEnd: String = "\u21A9"
  private val lineContStart: String = "\u21AA"

  override def print(s: String): Unit = {
    var rest: String = if (s eq null) "null" else s
    while (!rest.isEmpty) {
      val nlPos = rest.indexOf("\n")
      if (nlPos < 0) {
        buffer += rest
        flushed = false
        rest = ""
      } else {
        doWriteLine(buffer + rest.substring(0, nlPos))
        buffer = ""
        flushed = true
        rest = rest.substring(nlPos+1)
      }
    }
  }

  /**
   * Since we cannot write a partial line in JavaScript, we write a whole
   * line with continuation symbol at the end and schedule a line continuation
   * symbol for the new line if the buffer is flushed.
   */
  override def flush(): Unit = if (!flushed) {
    doWriteLine(buffer + lineContEnd)
    buffer = lineContStart
    flushed = true
  }

  protected def doWriteLine(line: String): Unit
}

private[lang] object StandardOutPrintStream
extends io.PrintStream(StandardOut, true) with JSConsoleBasedPrintStream {

  override protected def doWriteLine(line: String): Unit = {
    if (!(!global.console))
      global.console.log(line)
  }
}

private[lang] object StandardErrPrintStream
extends io.PrintStream(StandardErr, true) with JSConsoleBasedPrintStream {

  override protected def doWriteLine(line: String): Unit = {
    if (!(!global.console)) {
      if (!(!global.console.error))
        global.console.error(line)
      else
        global.console.log(line)
    }
  }
}

private[lang] object StandardOut extends io.OutputStream {
  def write(b: Int) = StandardOutPrintStream.print(b.toChar.toString)
}

private[lang] object StandardErr extends io.OutputStream {
  def write(b: Int) = StandardErrPrintStream.print(b.toChar.toString)
}
