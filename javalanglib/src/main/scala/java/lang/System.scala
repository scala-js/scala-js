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
import scala.scalajs.runtime.linkingInfo

import java.{util => ju}

import Utils._

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
    import Utils.DynamicImplicits.truthValue

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

  def nanoTime(): scala.Long =
    (getHighPrecisionTime() * 1000000).toLong

  def arraycopy(src: Object, srcPos: scala.Int, dest: Object,
      destPos: scala.Int, length: scala.Int): Unit = {

    import scala.{Boolean, Char, Byte, Short, Int, Long, Float, Double}

    def mismatch(): Nothing =
      throw new ArrayStoreException("Incompatible array types")

    def impl(srcLen: Int, destLen: Int, f: js.Function2[Int, Int, Any]): Unit = {
      SemanticsUtils.arrayIndexOutOfBoundsCheck({ () =>
        srcPos < 0 || destPos < 0 || length < 0 ||
        srcPos > srcLen - length ||
        destPos > destLen - length
      }, { () =>
        new ArrayIndexOutOfBoundsException()
      })

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

  def identityHashCode(x: Object): scala.Int = {
    (x: Any) match {
      case null => 0
      case _:scala.Boolean | _:scala.Double | _:String | () =>
        x.hashCode()
      case _ =>
        import IDHashCode._
        if (x.getClass == null) {
          /* x is not a Scala.js object: we have delegate to x.hashCode().
           * This is very important, as we really need to go through
           * `$objectHashCode()` in `CoreJSLib` instead of using our own
           * `idHashCodeMap`. That's because `$objectHashCode()` uses the
           * intrinsic `$systemIdentityHashCode` for JS objects, regardless of
           * whether the optimizer is enabled or not. If we use our own
           * `idHashCodeMap`, we will get different hash codes when obtained
           * through `System.identityHashCode(x)` than with `x.hashCode()`.
           */
          x.hashCode()
        } else if (linkingInfo.assumingES6 || idHashCodeMap != null) {
          // Use the global WeakMap of attributed id hash codes
          val hash = idHashCodeMap.get(x.asInstanceOf[js.Any])
          if (!Utils.isUndefined(hash)) {
            hash.asInstanceOf[Int]
          } else {
            val newHash = nextIDHashCode()
            idHashCodeMap.set(x.asInstanceOf[js.Any], newHash.asInstanceOf[js.Any])
            newHash
          }
        } else {
          val hash = x.asInstanceOf[js.Dynamic].selectDynamic("$idHashCode$0")
          if (!Utils.isUndefined(hash)) {
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
            x.asInstanceOf[js.Dynamic].updateDynamic("$idHashCode$0")(newHash.asInstanceOf[js.Any])
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
      if (linkingInfo.assumingES6 || js.typeOf(global.WeakMap) != "undefined")
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
    var dict: js.Dictionary[String] = loadSystemProperties()
    var properties: ju.Properties = null

    private[System] def loadSystemProperties(): js.Dictionary[String] = {
      val result = new js.Object().asInstanceOf[js.Dictionary[String]]
      dictSet(result, "java.version", "1.8")
      dictSet(result, "java.vm.specification.version", "1.8")
      dictSet(result, "java.vm.specification.vendor", "Oracle Corporation")
      dictSet(result, "java.vm.specification.name", "Java Virtual Machine Specification")
      dictSet(result, "java.vm.name", "Scala.js")
      dictSet(result, "java.vm.version", linkingInfo.linkerVersion)
      dictSet(result, "java.specification.version", "1.8")
      dictSet(result, "java.specification.vendor", "Oracle Corporation")
      dictSet(result, "java.specification.name", "Java Platform API Specification")
      dictSet(result, "file.separator", "/")
      dictSet(result, "path.separator", ":")
      dictSet(result, "line.separator", "\n")
      result
    }

    private[System] def forceProperties(): ju.Properties = {
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
  }

  def getProperties(): ju.Properties =
    SystemProperties.forceProperties()

  def lineSeparator(): String = "\n"

  def setProperties(properties: ju.Properties): Unit = {
    if (properties eq null) {
      SystemProperties.dict = SystemProperties.loadSystemProperties()
      SystemProperties.properties = null
    } else {
      SystemProperties.dict = null
      SystemProperties.properties = properties
    }
  }

  def getProperty(key: String): String =
    if (SystemProperties.dict ne null) dictGetOrElse(SystemProperties.dict, key, null)
    else SystemProperties.properties.getProperty(key)

  def getProperty(key: String, default: String): String =
    if (SystemProperties.dict ne null) dictGetOrElse(SystemProperties.dict, key, default)
    else SystemProperties.properties.getProperty(key, default)

  def clearProperty(key: String): String =
    if (SystemProperties.dict ne null) dictGetOrElseAndRemove(SystemProperties.dict, key, null)
    else SystemProperties.properties.remove(key).asInstanceOf[String]

  def setProperty(key: String, value: String): String = {
    if (SystemProperties.dict ne null) {
      val oldValue = getProperty(key)
      dictSet(SystemProperties.dict, key, value)
      oldValue
    } else {
      SystemProperties.properties.setProperty(key, value).asInstanceOf[String]
    }
  }

  def getenv(): ju.Map[String, String] =
    ju.Collections.emptyMap()

  def getenv(name: String): String = {
    if (name eq null)
      throw new NullPointerException

    null
  }

  //def exit(status: scala.Int): Unit
  def gc(): Unit = Runtime.getRuntime().gc()
}

private[lang] final class JSConsoleBasedPrintStream(isErr: scala.Boolean)
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
    import Utils.DynamicImplicits.truthValue

    if (js.typeOf(global.console) != "undefined") {
      if (isErr && global.console.error)
        global.console.error(line.asInstanceOf[js.Any])
      else
        global.console.log(line.asInstanceOf[js.Any])
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
