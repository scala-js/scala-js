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

package scala.scalajs.runtime

import java.util.Comparator

import scala.scalajs.js
import scala.scalajs.js.annotation._
import js.JSStringOps._

import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.util.regex._

/** Implementation for methods on java.lang.String.
 *
 *  Strings are represented at runtime by JavaScript strings, but they have
 *  a lot of methods. The compiler forwards methods on java.lang.String to the
 *  methods in the object, passing `this` as the first argument, that we
 *  consistently call `thiz` in this object.
 */
private[runtime] object RuntimeString {

  /** Operations on a primitive JS string that are shadowed by Scala methods,
   *  and that we need to implement these very Scala methods.
   */
  @js.native
  private trait SpecialJSStringOps extends js.Any {
    def length: Int = js.native
    def charCodeAt(index: Int): Int = js.native

    def toLowerCase(): String = js.native
    def toUpperCase(): String = js.native
    def trim(): String = js.native
  }

  private def specialJSStringOps(s: String): SpecialJSStringOps =
    s.asInstanceOf[SpecialJSStringOps]

  @inline
  def charAt(thiz: String, index: Int): Char =
    specialJSStringOps(thiz).charCodeAt(index).toChar

  final lazy val CASE_INSENSITIVE_ORDER: Comparator[String] = {
    new Comparator[String] with Serializable {
      def compare(o1: String, o2: String): Int = o1.compareToIgnoreCase(o2)
    }
  }

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

  def codePointBefore(thiz: String, index: Int): Int = {
    val low = thiz.charAt(index - 1)
    if (index > 1) {
      val high = thiz.charAt(index - 2)
      if (Character.isSurrogatePair(high, low))
        Character.toCodePoint(high, low)
      else
        low.toInt
    } else {
      low.toInt
    }
  }

  def codePointCount(thiz: String, beginIndex: Int, endIndex: Int): Int = {
    if (endIndex > thiz.length || beginIndex < 0 || endIndex < beginIndex)
      throw new IndexOutOfBoundsException
    var res = endIndex - beginIndex
    var i = beginIndex
    val end = endIndex - 1
    while (i < end) {
      if (Character.isSurrogatePair(thiz.charAt(i), thiz.charAt(i + 1)))
        res -= 1
      i += 1
    }
    res
  }

  def offsetByCodePoints(thiz: String, index: Int,
      codePointOffset: Int): Int = {
    val len = thiz.length
    if (index < 0 || index > len)
      throw new StringIndexOutOfBoundsException(index)

    if (codePointOffset >= 0) {
      var i = 0
      var result = index
      while (i != codePointOffset) {
        if (result >= len)
          throw new StringIndexOutOfBoundsException
        if ((result < len - 1) &&
            Character.isHighSurrogate(thiz.charAt(result)) &&
            Character.isLowSurrogate(thiz.charAt(result + 1))) {
          result += 2
        } else {
          result += 1
        }
        i += 1
      }
      result
    } else {
      var i = 0
      var result = index
      while (i != codePointOffset) {
        if (result <= 0)
          throw new StringIndexOutOfBoundsException
        if ((result > 1) && Character.isLowSurrogate(thiz.charAt(result - 1)) &&
            Character.isHighSurrogate(thiz.charAt(result - 2))) {
          result -= 2
        } else {
          result -= 1
        }
        i -= 1
      }
      result
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
    else if ((thiz.asInstanceOf[js.Dynamic] <
        anotherString.asInstanceOf[js.Dynamic]).asInstanceOf[Boolean]) -1
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
    thiz.jsSubstring(thiz.length - suffix.length) == suffix

  def getBytes(thiz: String): Array[Byte] =
    thiz.getBytes(Charset.defaultCharset)

  def getBytes(thiz: String, charsetName: String): Array[Byte] =
    thiz.getBytes(Charset.forName(charsetName))

  def getBytes(thiz: String, charset: Charset): Array[Byte] = {
    val buf = charset.encode(thiz)
    val res = new Array[Byte](buf.remaining)
    buf.get(res)
    res
  }

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
    thiz.jsIndexOf(str)

  @inline
  def indexOf(thiz: String, str: String, fromIndex: Int): Int =
    thiz.jsIndexOf(str, fromIndex)

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
    if (fromIndex < 0) -1
    else thiz.lastIndexOf(fromCodePoint(ch), fromIndex)

  @inline
  def lastIndexOf(thiz: String, str: String): Int =
    thiz.jsLastIndexOf(str)

  @inline
  def lastIndexOf(thiz: String, str: String, fromIndex: Int): Int =
    if (fromIndex < 0) -1
    else thiz.jsLastIndexOf(str, fromIndex)

  @inline
  def length(thiz: String): Int =
    specialJSStringOps(thiz).length

  @inline
  def matches(thiz: String, regex: String): Boolean = {
    checkNull(thiz)
    Pattern.matches(regex, thiz)
  }

  /* Both regionMatches ported from
   * https://github.com/gwtproject/gwt/blob/master/user/super/com/google/gwt/emul/java/lang/String.java
   */
  def regionMatches(thiz: String, ignoreCase: Boolean,
      toffset: Int, other: String, ooffset: Int, len: Int): Boolean = {
    checkNull(thiz)
    if (other == null) {
      throw new NullPointerException()
    } else if (toffset < 0 || ooffset < 0 || toffset + len > thiz.length || ooffset + len > other.length) {
      false
    } else if (len <= 0) {
      true
    } else {
      val left = thiz.substring(toffset, toffset + len)
      val right = other.substring(ooffset, ooffset + len)
      if (ignoreCase) left.equalsIgnoreCase(right) else left == right
    }
  }

  @inline
  def regionMatches(thiz: String, toffset: Int,
      other: String, ooffset: Int, len: Int): Boolean = {
    regionMatches(thiz, false, toffset, other, ooffset, len)
  }

  @inline
  def replace(thiz: String, oldChar: Char, newChar: Char): String =
    thiz.replace(oldChar.toString, newChar.toString)

  @inline
  def replace(thiz: String, target: CharSequence, replacement: CharSequence): String =
    thiz.jsSplit(target.toString).join(replacement.toString)

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
  def startsWith(thiz: String, prefix: String, toffset: Int): Boolean = {
    (toffset <= thiz.length && toffset >= 0 &&
        thiz.jsSubstring(toffset, toffset + prefix.length) == prefix)
  }

  @inline
  def subSequence(thiz: String, beginIndex: Int, endIndex: Int): CharSequence =
    thiz.substring(beginIndex, endIndex)

  @inline
  def substring(thiz: String, beginIndex: Int): String =
    thiz.jsSubstring(beginIndex)

  @inline
  def substring(thiz: String, beginIndex: Int, endIndex: Int): String =
    thiz.jsSubstring(beginIndex, endIndex)

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
    specialJSStringOps(thiz).toLowerCase()

  @inline
  def toUpperCase(thiz: String): String =
    specialJSStringOps(thiz).toUpperCase()

  @inline
  def trim(thiz: String): String =
    specialJSStringOps(thiz).trim()

  // Constructors

  def newString(): String = ""

  def newString(value: Array[Char]): String =
    newString(value, 0, value.length)

  def newString(value: Array[Char], offset: Int, count: Int): String = {
    val end = offset + count
    if (offset < 0 || end < offset || end > value.length)
      throw new StringIndexOutOfBoundsException

    var result = ""
    var i = offset
    while (i != end) {
      result += value(i).toString
      i += 1
    }
    result
  }

  def newString(bytes: Array[Byte]): String =
    newString(bytes, Charset.defaultCharset)

  def newString(bytes: Array[Byte], charsetName: String): String =
    newString(bytes, Charset.forName(charsetName))

  def newString(bytes: Array[Byte], charset: Charset): String =
    charset.decode(ByteBuffer.wrap(bytes)).toString()

  def newString(bytes: Array[Byte], offset: Int, length: Int): String =
    newString(bytes, offset, length, Charset.defaultCharset)

  def newString(bytes: Array[Byte], offset: Int, length: Int,
      charsetName: String): String =
    newString(bytes, offset, length, Charset.forName(charsetName))

  def newString(bytes: Array[Byte], offset: Int, length: Int,
      charset: Charset): String =
    charset.decode(ByteBuffer.wrap(bytes, offset, length)).toString()

  def newString(codePoints: Array[Int], offset: Int, count: Int): String = {
    val end = offset + count
    if (offset < 0 || end < offset || end > codePoints.length)
      throw new StringIndexOutOfBoundsException

    var result = ""
    var i = offset
    while (i != end) {
      result += fromCodePoint(codePoints(i))
      i += 1
    }
    result
  }

  def newString(original: String): String =
    checkNull(original)

  def newString(buffer: java.lang.StringBuffer): String =
    buffer.toString

  def newString(builder: java.lang.StringBuilder): String =
    builder.toString

  // Static methods (aka methods on the companion object)

  @deprecated("Not part of the JDK API", "0.6.21")
  def valueOf(b: Byte): String = b.toString()

  @deprecated("Not part of the JDK API", "0.6.21")
  def valueOf(s: Short): String = s.toString()

  def valueOf(b: Boolean): String = b.toString()
  def valueOf(c: Char): String = c.toString()
  def valueOf(i: Int): String = i.toString()
  def valueOf(l: Long): String = l.toString()
  def valueOf(f: Float): String = f.toString()
  def valueOf(d: Double): String = d.toString()

  @inline def valueOf(obj: Object): String =
    "" + obj // if (obj eq null), returns "null"

  def valueOf(data: Array[Char]): String =
    valueOf(data, 0, data.length)

  def valueOf(data: Array[Char], offset: Int, count: Int): String =
    newString(data, offset, count)

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
    if ((codePoint & ~Character.MAX_VALUE) == 0) {
      NativeJSString.fromCharCode(codePoint)
    } else if (codePoint < 0 || codePoint > Character.MAX_CODE_POINT) {
      throw new IllegalArgumentException
    } else {
      val offsetCp = codePoint - 0x10000
      NativeJSString.fromCharCode(
          (offsetCp >> 10) | 0xd800, (offsetCp & 0x3ff) | 0xdc00)
    }
  }

  @js.native
  @JSGlobal("String")
  private object NativeJSString extends js.Object {
    def fromCharCode(charCodes: Int*): String = js.native
  }

}
