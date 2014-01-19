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

  private[this] val getHighPrecisionTime: js.Function0[js.Number] = {
    if (!(!global.performance)) {
      if (!(!global.performance.now)) {
        () => global.performance.now().asInstanceOf[js.Number]
      } else if (!(!(global.performance.webkitNow))) {
        () => global.performance.webkitNow().asInstanceOf[js.Number]
      } else {
        () => new js.Date().getTime()
      }
    } else {
      () => new js.Date().getTime()
    }
  }

  def nanoTime(): scala.Long =
    (getHighPrecisionTime() * 1000000).toLong

  def arraycopy(src: Object, srcPos: scala.Int,
      dest: Object, destPos: scala.Int, length: scala.Int): Unit = {
    val jsSrc = reflect.Array.getUnderlying[Any](src)
    val jsDest = reflect.Array.getUnderlying[Any](dest)
    var i = 0
    while (i < length) {
      jsDest(destPos+i) = jsSrc(srcPos+i)
      i += 1
    }
  }

  def identityHashCode(x: Object): scala.Int = {
    // TODO
    42
  }

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
  private var buffer: js.String = ""

  override def print(s: String): Unit = {
    var rest: js.String = if (s eq null) "null" else s
    while (!(!rest)) {
      val nlPos = rest.indexOf("\n")
      if (nlPos < 0) {
        buffer += rest
        rest = ""
      } else {
        doWriteLine(buffer + rest.substring(0, nlPos))
        buffer = ""
        rest = rest.substring(nlPos+1)
      }
    }
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
    if (!(!global.console))
      global.console.error(line)
  }
}

private[lang] object StandardOut extends io.OutputStream {
  def write(b: Int) = StandardOutPrintStream.print(b.toChar.toString)
}

private[lang] object StandardErr extends io.OutputStream {
  def write(b: Int) = StandardErrPrintStream.print(b.toChar.toString)
}
