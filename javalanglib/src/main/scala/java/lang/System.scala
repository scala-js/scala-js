package java.lang

import java.io._

import scala.scalajs.js
import js.Dynamic.global

object System {
  var out: PrintStream = new JSConsoleBasedPrintStream(isErr = false)
  var err: PrintStream = new JSConsoleBasedPrintStream(isErr = true)
  var in: InputStream = null

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
      destPos: scala.Int, length: scala.Int): Unit = {

    import scala.{Boolean, Char, Byte, Short, Int, Long, Float, Double}

    @inline def checkIndices(srcLen: Int, destLen: Int): Unit = {
      if (srcPos < 0 || destPos < 0 || length < 0 ||
          srcPos + length > srcLen || destPos + length > destLen)
        throw new ArrayIndexOutOfBoundsException("Array index out of bounds")
    }

    def mismatch(): Nothing =
      throw new ArrayStoreException("Incompatible array types")

    val forward = (src ne dest) || destPos < srcPos || srcPos + length < destPos

    def copyPrim[@specialized T](src: Array[T], dest: Array[T]): Unit = {
      checkIndices(src.length, dest.length)
      if (forward) {
        var i = 0
        while (i < length) {
          dest(i+destPos) = src(i+srcPos)
          i += 1
        }
      } else {
        var i = length-1
        while (i >= 0) {
          dest(i+destPos) = src(i+srcPos)
          i -= 1
        }
      }
    }

    def copyRef(src: Array[AnyRef], dest: Array[AnyRef]): Unit = {
      checkIndices(src.length, dest.length)
      if (forward) {
        var i = 0
        while (i < length) {
          dest(i+destPos) = src(i+srcPos)
          i += 1
        }
      } else {
        var i = length-1
        while (i >= 0) {
          dest(i+destPos) = src(i+srcPos)
          i -= 1
        }
      }
    }

    if (src == null || dest == null) {
      throw new NullPointerException()
    } else (src match {
      case src: Array[AnyRef] =>
        dest match {
          case dest: Array[AnyRef] => copyRef(src, dest)
          case _                   => mismatch()
        }
      case src: Array[Boolean] =>
        dest match {
          case dest: Array[Boolean] => copyPrim(src, dest)
          case _                    => mismatch()
        }
      case src: Array[Char] =>
        dest match {
          case dest: Array[Char] => copyPrim(src, dest)
          case _                 => mismatch()
        }
      case src: Array[Byte] =>
        dest match {
          case dest: Array[Byte] => copyPrim(src, dest)
          case _                 => mismatch()
        }
      case src: Array[Short] =>
        dest match {
          case dest: Array[Short] => copyPrim(src, dest)
          case _                  => mismatch()
        }
      case src: Array[Int] =>
        dest match {
          case dest: Array[Int] => copyPrim(src, dest)
          case _                => mismatch()
        }
      case src: Array[Long] =>
        dest match {
          case dest: Array[Long] => copyPrim(src, dest)
          case _                 => mismatch()
        }
      case src: Array[Float] =>
        dest match {
          case dest: Array[Float] => copyPrim(src, dest)
          case _                  => mismatch()
        }
      case src: Array[Double] =>
        dest match {
          case dest: Array[Double] => copyPrim(src, dest)
          case _                   => mismatch()
        }
      case _ =>
        mismatch()
    })
  }

  def identityHashCode(x: Object): scala.Int = {
    import js.prim
    x match {
      case null => 0
      case _:prim.Boolean | _:prim.Number | _:prim.String | _:prim.Undefined =>
        x.hashCode()
      case _ =>
        if (x.getClass == null) {
          // This is not a Scala.js object
          42
        } else {
          val hash = x.asInstanceOf[js.Dynamic].selectDynamic("$idHashCode$0")
          if (!js.isUndefined(hash)) {
            hash.asInstanceOf[Int]
          } else {
            val newHash = IDHashCode.nextIDHashCode()
            x.asInstanceOf[js.Dynamic].updateDynamic("$idHashCode$0")(newHash)
            newHash
          }
        }
    }
  }

  private object IDHashCode {
    private var lastIDHashCode: Int = 0

    def nextIDHashCode(): Int = {
      val r = lastIDHashCode + 1
      lastIDHashCode = r
      r
    }
  }

  //def getProperties(): java.util.Properties
  //def getProperty(key: String): String
  //def getProperty(key: String, default: String): String
  //def clearProperty(key: String): String
  //def setProperty(key: String, value: String): String

  //def getenv(): java.util.Map[String,String]
  //def getenv(name: String): String

  def exit(status: scala.Int) = Runtime.getRuntime().exit(status)
  def gc() = Runtime.getRuntime().gc()
}

private[lang] final class JSConsoleBasedPrintStream(isErr: Boolean)
    extends PrintStream(new JSConsoleBasedPrintStream.DummyOutputStream) {

  import JSConsoleBasedPrintStream._

  /** Whether the buffer is flushed.
   *  This can be true even if buffer != "" because of line continuations.
   *  However, the converse is never true, i.e., !flushed => buffer != "".
   */
  private var flushed: scala.Boolean = true
  private var buffer: String = ""

  override def write(b: Int): Unit =
    write(Array(b.toByte), 0, 1)

  override def write(buf: Array[scala.Byte], off: Int, len: Int): Unit = {
    /* This does *not* decode buf as a sequence of UTF-8 code units.
     * This is not really useful, and would uselessly pull in the UTF-8 decoder
     * in all applications that use OutputStreams (not just PrintStreams).
     * Instead, we use a trivial ISO-8859-1 decoder in here.
     */
    if (off < 0 || len < 0 || len > buf.length - off)
      throw new IndexOutOfBoundsException

    var i = 0
    while (i < len) {
      print((buf(i + off) & 0xff).toChar)
      i += 1
    }
  }

  override def print(b: scala.Boolean): Unit     = printString(String.valueOf(b))
  override def print(c: scala.Char): Unit        = printString(String.valueOf(c))
  override def print(i: scala.Int): Unit         = printString(String.valueOf(i))
  override def print(l: scala.Long): Unit        = printString(String.valueOf(l))
  override def print(f: scala.Float): Unit       = printString(String.valueOf(f))
  override def print(d: scala.Double): Unit      = printString(String.valueOf(d))
  override def print(s: Array[scala.Char]): Unit = printString(String.valueOf(s))
  override def print(s: String): Unit            = printString(if (s == null) "null" else s)
  override def print(obj: AnyRef): Unit          = printString(String.valueOf(obj))

  override def println(): Unit = printString("\n")

  private def printString(s: String): Unit = {
    var rest: String = s
    while (rest != "") {
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
    doWriteLine(buffer + LineContEnd)
    buffer = LineContStart
    flushed = true
  }

  override def close(): Unit = ()

  private def doWriteLine(line: String): Unit = {
    if (!(!global.console)) {
      if (isErr && !(!global.console.error))
        global.console.error(line)
      else
        global.console.log(line)
    }
  }
}

private[lang] object JSConsoleBasedPrintStream {
  private final val LineContEnd: String = "\u21A9"
  private final val LineContStart: String = "\u21AA"

  class DummyOutputStream extends OutputStream {
    def write(c: Int): Unit =
      throw new AssertionError(
          "Should not get in JSConsoleBasedPrintStream.DummyOutputStream")
  }
}
