/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.lang

import java.io._

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.scalajs.LinkingInfo

import java.{util => ju}

object System {
  /* System contains a bag of unrelated features. If we naively implement
   * everything inside System, reaching any of these features can reach
   * unrelated code. For example, using `nanoTime()` would reach
   * `JSConsoleBasedPrintStream` and therefore a bunch of `java.io` classes.
   *
   * Instead, every feature that requires its own fields is extracted in a
   * separate private object, and corresponding methods of System delegate to
   * methods of that private object.
   *
   * All non-intrinsic methods are marked `@inline` so that the module accessor
   * of `System` can always be completely elided.
   */

  // Standard streams (out, err, in) ------------------------------------------

  private object Streams {
    var out: PrintStream = new JSConsoleBasedPrintStream(isErr = false)
    var err: PrintStream = new JSConsoleBasedPrintStream(isErr = true)
    var in: InputStream = null
  }

  @inline
  def out: PrintStream = Streams.out

  @inline
  def err: PrintStream = Streams.err

  @inline
  def in: InputStream = Streams.in

  @inline
  def setIn(in: InputStream): Unit =
    Streams.in = in

  @inline
  def setOut(out: PrintStream): Unit =
    Streams.out = out

  @inline
  def setErr(err: PrintStream): Unit =
    Streams.err = err

  // System time --------------------------------------------------------------

  @inline
  def currentTimeMillis(): scala.Long =
    (new js.Date).getTime().toLong

  private object NanoTime {
    val getHighPrecisionTime: () => scala.Double = {
      import js.DynamicImplicits.truthValue

      if (js.typeOf(global.performance) != "undefined") {
        if (global.performance.now) {
          () => global.performance.now().asInstanceOf[scala.Double]
        } else if (global.performance.webkitNow) {
          () => global.performance.webkitNow().asInstanceOf[scala.Double]
        } else {
          () => new js.Date().getTime()
        }
      } else {
        () => new js.Date().getTime()
      }
    }
  }

  @inline
  def nanoTime(): scala.Long =
    (NanoTime.getHighPrecisionTime() * 1000000).toLong

  // arraycopy ----------------------------------------------------------------

  // Intrinsic
  def arraycopy(src: Object, srcPos: scala.Int, dest: Object,
      destPos: scala.Int, length: scala.Int): Unit = {

    import scala.{Boolean, Char, Byte, Short, Int, Long, Float, Double}

    def mismatch(): Nothing =
      throw new ArrayStoreException("Incompatible array types")

    def impl(srcLen: Int, destLen: Int, f: (Int, Int) => Any): Unit = {
      /* Perform dummy swaps to trigger an ArrayIndexOutOfBoundsException or
       * UBE if the positions / lengths are bad.
       */
      if (srcPos < 0 || destPos < 0)
        f(destPos, srcPos)
      if (length < 0)
        f(length, length)
      if (srcPos > srcLen - length || destPos > destLen - length)
        f(destPos + length, srcPos + length)

      if ((src ne dest) || destPos < srcPos || srcPos + length < destPos) {
        var i = 0
        while (i < length) {
          f(i + destPos, i + srcPos)
          i += 1
        }
      } else {
        var i = length - 1
        while (i >= 0) {
          f(i + destPos, i + srcPos)
          i -= 1
        }
      }
    }

    if (src == null || dest == null) {
      throw new NullPointerException()
    } else (src match {
      case src: Array[AnyRef] =>
        dest match {
          case dest: Array[AnyRef] => impl(src.length, dest.length, (i, j) => dest(i) = src(j))
          case _                   => mismatch()
        }
      case src: Array[Boolean] =>
        dest match {
          case dest: Array[Boolean] => impl(src.length, dest.length, (i, j) => dest(i) = src(j))
          case _                    => mismatch()
        }
      case src: Array[Char] =>
        dest match {
          case dest: Array[Char] => impl(src.length, dest.length, (i, j) => dest(i) = src(j))
          case _                 => mismatch()
        }
      case src: Array[Byte] =>
        dest match {
          case dest: Array[Byte] => impl(src.length, dest.length, (i, j) => dest(i) = src(j))
          case _                 => mismatch()
        }
      case src: Array[Short] =>
        dest match {
          case dest: Array[Short] => impl(src.length, dest.length, (i, j) => dest(i) = src(j))
          case _                  => mismatch()
        }
      case src: Array[Int] =>
        dest match {
          case dest: Array[Int] => impl(src.length, dest.length, (i, j) => dest(i) = src(j))
          case _                => mismatch()
        }
      case src: Array[Long] =>
        dest match {
          case dest: Array[Long] => impl(src.length, dest.length, (i, j) => dest(i) = src(j))
          case _                 => mismatch()
        }
      case src: Array[Float] =>
        dest match {
          case dest: Array[Float] => impl(src.length, dest.length, (i, j) => dest(i) = src(j))
          case _                  => mismatch()
        }
      case src: Array[Double] =>
        dest match {
          case dest: Array[Double] => impl(src.length, dest.length, (i, j) => dest(i) = src(j))
          case _                   => mismatch()
        }
      case _ =>
        mismatch()
    })
  }

  @inline
  def identityHashCode(x: Any): scala.Int =
    scala.scalajs.runtime.identityHashCode(x.asInstanceOf[AnyRef])

  // System properties --------------------------------------------------------

  private object SystemProperties {
    import Utils._

    private[this] var dict: js.Dictionary[String] = loadSystemProperties()
    private[this] var properties: ju.Properties = null

    private def loadSystemProperties(): js.Dictionary[String] = {
      val result = new js.Object().asInstanceOf[js.Dictionary[String]]
      dictSet(result, "java.version", "1.8")
      dictSet(result, "java.vm.specification.version", "1.8")
      dictSet(result, "java.vm.specification.vendor", "Oracle Corporation")
      dictSet(result, "java.vm.specification.name", "Java Virtual Machine Specification")
      dictSet(result, "java.vm.name", "Scala.js")
      dictSet(result, "java.vm.version", LinkingInfo.linkerVersion)
      dictSet(result, "java.specification.version", "1.8")
      dictSet(result, "java.specification.vendor", "Oracle Corporation")
      dictSet(result, "java.specification.name", "Java Platform API Specification")
      dictSet(result, "file.separator", "/")
      dictSet(result, "path.separator", ":")
      dictSet(result, "line.separator", "\n")
      result
    }

    def getProperties(): ju.Properties = {
      if (properties eq null) {
        properties = new ju.Properties
        val keys = js.Object.keys(dict.asInstanceOf[js.Object])
        forArrayElems(keys) { key =>
          properties.setProperty(key, dictRawApply(dict, key))
        }
        dict = null
      }
      properties
    }

    def setProperties(properties: ju.Properties): Unit = {
      if (properties eq null) {
        dict = loadSystemProperties()
        this.properties = null
      } else {
        dict = null
        this.properties = properties
      }
    }

    def getProperty(key: String): String =
      if (dict ne null) dictGetOrElse(dict, key)(null)
      else properties.getProperty(key)

    def getProperty(key: String, default: String): String =
      if (dict ne null) dictGetOrElse(dict, key)(default)
      else properties.getProperty(key, default)

    def clearProperty(key: String): String =
      if (dict ne null) dictGetOrElseAndRemove(dict, key, null)
      else properties.remove(key).asInstanceOf[String]

    def setProperty(key: String, value: String): String = {
      if (dict ne null) {
        val oldValue = getProperty(key)
        dictSet(dict, key, value)
        oldValue
      } else {
        properties.setProperty(key, value).asInstanceOf[String]
      }
    }
  }

  @inline
  def getProperties(): ju.Properties =
    SystemProperties.getProperties()

  @inline
  def lineSeparator(): String = "\n"

  @inline
  def setProperties(properties: ju.Properties): Unit =
    SystemProperties.setProperties(properties)

  @inline
  def getProperty(key: String): String =
    SystemProperties.getProperty(key)

  @inline
  def getProperty(key: String, default: String): String =
    SystemProperties.getProperty(key, default)

  @inline
  def clearProperty(key: String): String =
    SystemProperties.clearProperty(key)

  @inline
  def setProperty(key: String, value: String): String =
    SystemProperties.setProperty(key, value)

  // Environment variables ----------------------------------------------------

  @inline
  def getenv(): ju.Map[String, String] =
    ju.Collections.emptyMap()

  @inline
  def getenv(name: String): String = {
    if (name eq null)
      throw new NullPointerException

    null
  }

  // Runtime ------------------------------------------------------------------

  //def exit(status: scala.Int): Unit

  @inline
  def gc(): Unit = Runtime.getRuntime().gc()
}

private final class JSConsoleBasedPrintStream(isErr: scala.Boolean)
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
  override def println(obj: AnyRef): Unit = printString(s"$obj\n")

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

    if (js.typeOf(global.console) != "undefined") {
      if (isErr && global.console.error)
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
