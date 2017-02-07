package java.lang

import java.io._

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.scalajs.LinkingInfo.assumingES6
import scala.scalajs.runtime.{environmentInfo, linkingInfo, SemanticsUtils}

import java.{util => ju}

object System {
  var out: PrintStream = new JSConsoleBasedPrintStream(isErr = false)
  var err: PrintStream = new JSConsoleBasedPrintStream(isErr = true)
  var in: InputStream = null

  def setIn(in: InputStream): Unit =
    this.in = in

  def setOut(out: PrintStream): Unit =
    this.out = out

  def setErr(err: PrintStream): Unit =
    this.err = err

  def currentTimeMillis(): scala.Long = {
    (new js.Date).getTime().toLong
  }

  private[this] val getHighPrecisionTime: js.Function0[scala.Double] = {
    import js.DynamicImplicits.truthValue

    // We've got to use selectDynamic explicitly not to crash Scala 2.10
    if (global.selectDynamic("performance")) {
      if (global.performance.selectDynamic("now")) {
        () => global.performance.now().asInstanceOf[scala.Double]
      } else if (global.performance.selectDynamic("webkitNow")) {
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
      SemanticsUtils.arrayIndexOutOfBoundsCheck({
        srcPos < 0 || destPos < 0 || length < 0 ||
        srcPos > srcLen - length ||
        destPos > destLen - length
      }, {
        new ArrayIndexOutOfBoundsException()
      })
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
    (x: Any) match {
      case null => 0
      case _:scala.Boolean | _:scala.Double | _:String | () =>
        x.hashCode()
      case _ =>
        import IDHashCode._
        if (x.getClass == null) {
          // This is not a Scala.js object: delegate to x.hashCode()
          x.hashCode()
        } else if (assumingES6 || idHashCodeMap != null) {
          // Use the global WeakMap of attributed id hash codes
          val hash = idHashCodeMap.get(x.asInstanceOf[js.Any])
          if (!js.isUndefined(hash)) {
            hash.asInstanceOf[Int]
          } else {
            val newHash = nextIDHashCode()
            idHashCodeMap.set(x.asInstanceOf[js.Any], newHash)
            newHash
          }
        } else {
          val hash = x.asInstanceOf[js.Dynamic].selectDynamic("$idHashCode$0")
          if (!js.isUndefined(hash)) {
            /* Note that this can work even if x is sealed, if
             * identityHashCode() was called for the first time before x was
             * sealed.
             */
            hash.asInstanceOf[Int]
          } else if (!js.Object.isSealed(x.asInstanceOf[js.Object])) {
            /* If x is not sealed, we can (almost) safely create an additional
             * field with a bizarre and relatively long name, even though it is
             * technically undefined behavior.
             */
            val newHash = nextIDHashCode()
            x.asInstanceOf[js.Dynamic].updateDynamic("$idHashCode$0")(newHash)
            newHash
          } else {
            // Otherwise, we unfortunately have to return a constant.
            42
          }
        }
    }
  }

  private object IDHashCode {
    private var lastIDHashCode: Int = 0

    val idHashCodeMap =
      if (assumingES6 || !js.isUndefined(global.WeakMap))
        js.Dynamic.newInstance(global.WeakMap)()
      else
        null

    def nextIDHashCode(): Int = {
      val r = lastIDHashCode + 1
      lastIDHashCode = r
      r
    }
  }

  private object SystemProperties {
    var value = loadSystemProperties()

    private[System] def loadSystemProperties(): ju.Properties = {
      val sysProp = new ju.Properties()
      sysProp.setProperty("java.version", "1.8")
      sysProp.setProperty("java.vm.specification.version", "1.8")
      sysProp.setProperty("java.vm.specification.vendor", "Oracle Corporation")
      sysProp.setProperty("java.vm.specification.name", "Java Virtual Machine Specification")
      sysProp.setProperty("java.vm.name", "Scala.js")
      linkingInfo.linkerVersion.foreach(v => sysProp.setProperty("java.vm.version", v))
      sysProp.setProperty("java.specification.version", "1.8")
      sysProp.setProperty("java.specification.vendor", "Oracle Corporation")
      sysProp.setProperty("java.specification.name", "Java Platform API Specification")
      sysProp.setProperty("file.separator", "/")
      sysProp.setProperty("path.separator", ":")
      sysProp.setProperty("line.separator", "\n")

      for {
        jsEnvProperties <- environmentInfo.javaSystemProperties
        (key, value) <- jsEnvProperties
      } {
        sysProp.setProperty(key, value)
      }
      sysProp
    }
  }

  def getProperties(): ju.Properties =
    SystemProperties.value

  def lineSeparator(): String = "\n"

  def setProperties(properties: ju.Properties): Unit = {
    SystemProperties.value =
      if (properties != null) properties
      else SystemProperties.loadSystemProperties()
  }

  def getProperty(key: String): String =
    SystemProperties.value.getProperty(key)

  def getProperty(key: String, default: String): String =
    SystemProperties.value.getProperty(key, default)

  def clearProperty(key: String): String =
    SystemProperties.value.remove(key).asInstanceOf[String]

  def setProperty(key: String, value: String): String =
    SystemProperties.value.setProperty(key, value).asInstanceOf[String]

  def getenv(): ju.Map[String, String] =
    ju.Collections.emptyMap()

  def getenv(name: String): String = {
    if (name eq null)
      throw new NullPointerException

    null
  }

  def exit(status: scala.Int): Unit = Runtime.getRuntime().exit(status)
  def gc(): Unit = Runtime.getRuntime().gc()
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

  // This is the method invoked by Predef.println(x).
  @inline
  override def println(obj: AnyRef): Unit = printString("" + obj + "\n")

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
    import js.DynamicImplicits.truthValue

    // We've got to use selectDynamic explicitly not to crash Scala 2.10
    if (global.selectDynamic("console")) {
      if (isErr && global.console.selectDynamic("error"))
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
