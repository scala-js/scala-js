package scala.scalajs.runtime

import scala.scalajs.js
import scala.scalajs.js.prim.{String => jsString}

import java.util.regex._

/** Implementation for methods on java.lang.String.
 *
 *  Strings are represented at runtime by JavaScript strings, but they have
 *  a lot of methods. The compiler forwards methods on java.lang.String to the
 *  methods in the object, passing `this` as the first argument, that we
 *  consistently call `thiz` in this object.
 */
private[runtime] object RuntimeString {

  @inline
  def charAt(thiz: String, index: Int): Char =
    (thiz: jsString).charCodeAt(index).asInstanceOf[Int].toChar

  def codePointAt(thiz: String, index: Int): Int = {
    val high = thiz.charAt(index)
    if (index+1 < thiz.length) {
      val low = thiz.charAt(index+1)
      if (Character.isSurrogatePair(high, low))
        Character.toCodePoint(high, low)
      else
        high.toInt
    } else {
      high.toInt
    }
  }

  def hashCode(thiz: String): Int = {
    var res = 0
    var mul = 1 // holds pow(31, length-i-1)
    var i = thiz.length-1
    while (i >= 0) {
      res += thiz.charAt(i) * mul
      mul *= 31
      i -= 1
    }
    res
  }

  @inline
  def compareTo(thiz: String, anotherString: String): Int = {
    if (thiz.equals(anotherString)) 0
    else if ((thiz: jsString) < (anotherString: jsString)) -1
    else 1
  }

  def compareToIgnoreCase(thiz: String, str: String): Int =
    thiz.toLowerCase().compareTo(str.toLowerCase())

  @inline
  def equalsIgnoreCase(thiz: String, that: String): Boolean =
    thiz.toLowerCase() == (if (that == null) null else that.toLowerCase())

  @inline
  def concat(thiz: String, s: String): String =
    checkNull(thiz) + s

  @inline
  def contains(thiz: String, s: CharSequence): Boolean =
    thiz.indexOf(s.toString) != -1

  def endsWith(thiz: String, suffix: String): Boolean =
    ((thiz: jsString).substring(thiz.length - suffix.length): String) == suffix

  /** Unimplemented, unused, but referenced */
  def getBytes(thiz: String): Array[Byte] = ???

  /** Unimplemented, unused, but referenced */
  def getBytes(thiz: String, charsetName: String): Array[Byte] = ???

  def getChars(thiz: String, srcBegin: Int, srcEnd: Int,
      dst: Array[Char], dstBegin: Int): Unit = {
    if (srcEnd   > thiz.length || // first test uses thiz
        srcBegin < 0 ||
        srcEnd   < 0 ||
        srcBegin > srcEnd) {
      throw new StringIndexOutOfBoundsException("Index out of Bound")
    }

    val offset = dstBegin - srcBegin
    var i = srcBegin
    while (i < srcEnd) {
      dst(i+offset) = thiz.charAt(i)
      i += 1
    }
  }

  def indexOf(thiz: String, ch: Int): Int =
    thiz.indexOf(fromCodePoint(ch))

  def indexOf(thiz: String, ch: Int, fromIndex: Int): Int =
    thiz.indexOf(fromCodePoint(ch), fromIndex)

  @inline
  def indexOf(thiz: String, str: String): Int =
    (thiz: jsString).indexOf(str).asInstanceOf[Int]

  @inline
  def indexOf(thiz: String, str: String, fromIndex: Int): Int =
    (thiz: jsString).indexOf(str, fromIndex).asInstanceOf[Int]

  /* Just returning this string is a valid implementation for `intern` in
   * JavaScript, since strings are primitive values. Therefore, value equality
   * and reference equality is the same.
   */
  @inline
  def intern(thiz: String): String =
    checkNull(thiz)

  @inline
  def isEmpty(thiz: String): Boolean =
    checkNull(thiz) == ""

  def lastIndexOf(thiz: String, ch: Int): Int =
    thiz.lastIndexOf(fromCodePoint(ch))

  def lastIndexOf(thiz: String, ch: Int, fromIndex: Int): Int =
    thiz.lastIndexOf(fromCodePoint(ch), fromIndex)

  @inline
  def lastIndexOf(thiz: String, str: String): Int =
    (thiz: jsString).lastIndexOf(str).asInstanceOf[Int]

  @inline
  def lastIndexOf(thiz: String, str: String, fromIndex: Int): Int =
    (thiz: jsString).lastIndexOf(str, fromIndex).asInstanceOf[Int]

  @inline
  def length(thiz: String): Int =
    (thiz: jsString).length.asInstanceOf[Int]

  @inline
  def matches(thiz: String, regex: String): Boolean = {
    checkNull(thiz)
    Pattern.matches(regex, thiz)
  }

  @inline
  def replace(thiz: String, oldChar: Char, newChar: Char): String =
    (thiz: String).replace(oldChar.toString, newChar.toString)

  @inline
  def replace(thiz: String, target: CharSequence, replacement: CharSequence): String =
    (thiz: jsString).split(target.toString).join(replacement.toString)

  def replaceAll(thiz: String, regex: String, replacement: String): String = {
    checkNull(thiz)
    Pattern.compile(regex).matcher(thiz).replaceAll(replacement)
  }

  def replaceFirst(thiz: String, regex: String, replacement: String): String = {
    checkNull(thiz)
    Pattern.compile(regex).matcher(thiz).replaceFirst(replacement)
  }

  @inline
  def split(thiz: String, regex: String): Array[String] =
    thiz.split(regex, 0)

  def split(thiz: String, regex: String, limit: Int): Array[String] = {
    checkNull(thiz)
    Pattern.compile(regex).split(thiz, limit)
  }

  @inline
  def startsWith(thiz: String, prefix: String): Boolean =
    thiz.startsWith(prefix, 0)

  @inline
  def startsWith(thiz: String, prefix: String, toffset: Int): Boolean =
    ((thiz: jsString).substring(toffset, prefix.length): String) == prefix

  @inline
  def subSequence(thiz: String, beginIndex: Int, endIndex: Int): CharSequence =
    thiz.substring(beginIndex, endIndex)

  @inline
  def substring(thiz: String, beginIndex: Int): String =
    (thiz: jsString).substring(beginIndex)

  @inline
  def substring(thiz: String, beginIndex: Int, endIndex: Int): String =
    (thiz: jsString).substring(beginIndex, endIndex)

  def toCharArray(thiz: String): Array[Char] = {
    val length = thiz.length
    val result = new Array[Char](length)
    var i = 0
    while (i < length) {
      result(i) = thiz.charAt(i)
      i += 1
    }
    result
  }

  @inline
  def toLowerCase(thiz: String): String =
    (thiz: jsString).toLowerCase()

  @inline
  def toUpperCase(thiz: String): String =
    (thiz: jsString).toUpperCase()

  @inline
  def trim(thiz: String): String =
    (thiz: jsString).trim()

  // Constructors

  def newString(): String = ""

  def newString(value: Array[Char]): String =
    newString(value, 0, value.length)

  def newString(value: Array[Char], offset: Int, count: Int): String = {
    val end = offset + count
    if (offset < 0 || end < offset || end > value.length)
      throw new StringIndexOutOfBoundsException

    val charCodes = new js.Array[Int]
    var i = offset
    while (i != end) {
      charCodes += value(i).toInt
      i += 1
    }
    js.String.fromCharCode(charCodes: _*)
  }

  /** Unimplemented, unused, but referenced */
  def newString(bytes: Array[Byte], charsetName: String): String = ???

  /** Unimplemented, unused, but referenced */
  def newString(bytes: Array[Byte], offset: Int, length: Int,
      charsetName: String): String = ???

  def newString(codePoints: Array[Int], offset: Int, count: Int): String = {
    val end = offset + count
    if (offset < 0 || end < offset || end > codePoints.length)
      throw new StringIndexOutOfBoundsException

    val charCodes = new js.Array[Int]
    var i = offset
    while (i != end) {
      val cp = codePoints(i)
      if (cp < 0 || cp > Character.MAX_CODE_POINT)
        throw new IllegalArgumentException
      if (cp <= Character.MAX_VALUE) {
        charCodes += cp
      } else {
        val offsetCp = cp - 0x10000
        charCodes += (offsetCp >> 10) | 0xd800
        charCodes += (offsetCp & 0x3ff) | 0xdc00
      }
      i += 1
    }
    js.String.fromCharCode(charCodes: _*)
  }

  def newString(original: String): String =
    checkNull(original)

  def newString(buffer: java.lang.StringBuffer): String =
    buffer.toString

  def newString(builder: java.lang.StringBuilder): String =
    builder.toString

  // Static methods (aka methods on the companion object)

  def valueOf(value: Boolean): String = value.toString()
  def valueOf(value: Char): String    = value.toString()
  def valueOf(value: Byte): String    = value.toString()
  def valueOf(value: Short): String   = value.toString()
  def valueOf(value: Int): String     = value.toString()
  def valueOf(value: Long): String    = value.toString()
  def valueOf(value: Float): String   = value.toString()
  def valueOf(value: Double): String  = value.toString()

  def valueOf(value: Object): String =
    if (value eq null) "null" else value.toString()

  def format(format: String, args: Array[AnyRef]): String = {
    val frm = new java.util.Formatter()
    val res = frm.format(format, args: _*).toString()
    frm.close()
    res
  }

  // Helpers

  @inline
  private def checkNull(s: String): s.type =
    if (s == null) throw new NullPointerException()
    else s

  private def fromCodePoint(codePoint: Int): String = {
    if ((codePoint & ~Character.MAX_VALUE) == 0)
      js.String.fromCharCode(codePoint)
    else if (codePoint < 0 || codePoint > Character.MAX_CODE_POINT)
      throw new IllegalArgumentException
    else {
      val offsetCp = codePoint - 0x10000
      js.String.fromCharCode(
          (offsetCp >> 10) | 0xd800, (offsetCp & 0x3ff) | 0xdc00)
    }
  }

}
