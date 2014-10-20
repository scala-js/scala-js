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
