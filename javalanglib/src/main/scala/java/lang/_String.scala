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

import java.util.Comparator

import scala.scalajs.js
import scala.scalajs.js.annotation._

import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.util.regex._

import Utils.Implicits.enableJSStringOps

/* This is the implementation of java.lang.String, which is a hijacked class.
 * Its instances are primitive strings. Constructors are not emitted.
 *
 * It should be declared as `class String`, but scalac really does not like
 * being forced to compile java.lang.String, so we call it `_String` instead.
 * The Scala.js compiler back-end applies some magic to rename it into `String`
 * when emitting the IR.
 */
final class _String private () // scalastyle:ignore
    extends AnyRef with java.io.Serializable with Comparable[String]
    with CharSequence {

  import _String._

  @inline
  private def thisString: String =
    this.asInstanceOf[String]

  @inline
  private def thisJSString: SpecialJSStringOps =
    this.asInstanceOf[SpecialJSStringOps]

  @inline
  def charAt(index: Int): Char = {
    this.asInstanceOf[js.Dynamic]
      .charCodeAt(index.asInstanceOf[js.Dynamic])
      .asInstanceOf[Int]
      .toChar
  }

  def codePointAt(index: Int): Int = {
    val high = charAt(index)
    if (index+1 < length()) {
      val low = charAt(index+1)
      if (Character.isSurrogatePair(high, low))
        Character.toCodePoint(high, low)
      else
        high.toInt
    } else {
      high.toInt
    }
  }

  def codePointBefore(index: Int): Int = {
    val low = charAt(index - 1)
    if (index > 1) {
      val high = charAt(index - 2)
      if (Character.isSurrogatePair(high, low))
        Character.toCodePoint(high, low)
      else
        low.toInt
    } else {
      low.toInt
    }
  }

  def codePointCount(beginIndex: Int, endIndex: Int): Int =
    Character.codePointCount(this, beginIndex, endIndex)

  def offsetByCodePoints(index: Int, codePointOffset: Int): Int = {
    val len = length()
    if (index < 0 || index > len)
      throw new StringIndexOutOfBoundsException(index)

    if (codePointOffset >= 0) {
      var i = 0
      var result = index
      while (i != codePointOffset) {
        if (result >= len)
          throw new StringIndexOutOfBoundsException
        if ((result < len - 1) &&
            Character.isHighSurrogate(charAt(result)) &&
            Character.isLowSurrogate(charAt(result + 1))) {
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
        if ((result > 1) && Character.isLowSurrogate(charAt(result - 1)) &&
            Character.isHighSurrogate(charAt(result - 2))) {
          result -= 2
        } else {
          result -= 1
        }
        i -= 1
      }
      result
    }
  }

  override def hashCode(): Int = {
    var res = 0
    var mul = 1 // holds pow(31, length-i-1)
    var i = length() - 1
    while (i >= 0) {
      res += charAt(i) * mul
      mul *= 31
      i -= 1
    }
    res
  }

  @inline
  override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline
  def compareTo(anotherString: String): Int = {
    if (this.equals(anotherString)) 0
    else if ((this.asInstanceOf[js.Dynamic] <
        anotherString.asInstanceOf[js.Dynamic]).asInstanceOf[scala.Boolean]) -1
    else 1
  }

  def compareToIgnoreCase(str: String): Int =
    this.toLowerCase().compareTo(str.toLowerCase())

  @inline
  def equalsIgnoreCase(that: String): scala.Boolean =
    this.toLowerCase() == (if (that == null) null else that.toLowerCase())

  @inline
  def concat(s: String): String =
    thisString + s

  @inline
  def contains(s: CharSequence): scala.Boolean =
    indexOf(s.toString) != -1

  def endsWith(suffix: String): scala.Boolean =
    thisString.jsSubstring(this.length() - suffix.length()) == suffix

  def getBytes(): Array[scala.Byte] =
    getBytes(Charset.defaultCharset)

  def getBytes(charsetName: String): Array[scala.Byte] =
    getBytes(Charset.forName(charsetName))

  def getBytes(charset: Charset): Array[scala.Byte] = {
    val buf = charset.encode(thisString)
    val res = new Array[scala.Byte](buf.remaining)
    buf.get(res)
    res
  }

  def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char],
      dstBegin: Int): Unit = {
    if (srcEnd > length() || srcBegin < 0 || srcEnd < 0 || srcBegin > srcEnd)
      throw new StringIndexOutOfBoundsException("Index out of Bound")

    val offset = dstBegin - srcBegin
    var i = srcBegin
    while (i < srcEnd) {
      dst(i + offset) = charAt(i)
      i += 1
    }
  }

  def indexOf(ch: Int): Int =
    indexOf(fromCodePoint(ch))

  def indexOf(ch: Int, fromIndex: Int): Int =
    indexOf(fromCodePoint(ch), fromIndex)

  @inline
  def indexOf(str: String): Int =
    thisString.jsIndexOf(str)

  @inline
  def indexOf(str: String, fromIndex: Int): Int =
    thisString.jsIndexOf(str, fromIndex)

  /* Just returning this string is a valid implementation for `intern` in
   * JavaScript, since strings are primitive values. Therefore, value equality
   * and reference equality is the same.
   */
  @inline
  def intern(): String = thisString

  @inline
  def isEmpty(): scala.Boolean = (this: AnyRef) eq ("": AnyRef)

  def lastIndexOf(ch: Int): Int =
    lastIndexOf(fromCodePoint(ch))

  def lastIndexOf(ch: Int, fromIndex: Int): Int =
    if (fromIndex < 0) -1
    else lastIndexOf(fromCodePoint(ch), fromIndex)

  @inline
  def lastIndexOf(str: String): Int =
    thisString.jsLastIndexOf(str)

  @inline
  def lastIndexOf(str: String, fromIndex: Int): Int =
    if (fromIndex < 0) -1
    else thisString.jsLastIndexOf(str, fromIndex)

  @inline
  def length(): Int =
    this.asInstanceOf[js.Dynamic].length.asInstanceOf[Int]

  @inline
  def matches(regex: String): scala.Boolean =
    Pattern.matches(regex, thisString)

  /* Both regionMatches ported from
   * https://github.com/gwtproject/gwt/blob/master/user/super/com/google/gwt/emul/java/lang/String.java
   */
  def regionMatches(ignoreCase: scala.Boolean, toffset: Int, other: String,
      ooffset: Int, len: Int): scala.Boolean = {
    if (other == null) {
      throw new NullPointerException()
    } else if (toffset < 0 || ooffset < 0 || toffset + len > this.length() ||
        ooffset + len > other.length()) {
      false
    } else if (len <= 0) {
      true
    } else {
      val left = this.substring(toffset, toffset + len)
      val right = other.substring(ooffset, ooffset + len)
      if (ignoreCase) left.equalsIgnoreCase(right) else left == right
    }
  }

  @inline
  def regionMatches(toffset: Int, other: String, ooffset: Int,
      len: Int): scala.Boolean = {
    regionMatches(false, toffset, other, ooffset, len)
  }

  @inline
  def replace(oldChar: Char, newChar: Char): String =
    replace(oldChar.toString, newChar.toString)

  @inline
  def replace(target: CharSequence, replacement: CharSequence): String =
    thisString.jsSplit(target.toString).join(replacement.toString)

  def replaceAll(regex: String, replacement: String): String =
    Pattern.compile(regex).matcher(thisString).replaceAll(replacement)

  def replaceFirst(regex: String, replacement: String): String =
    Pattern.compile(regex).matcher(thisString).replaceFirst(replacement)

  @inline
  def split(regex: String): Array[String] =
    split(regex, 0)

  def split(regex: String, limit: Int): Array[String] =
    Pattern.compile(regex).split(thisString, limit)

  @inline
  def startsWith(prefix: String): scala.Boolean =
    startsWith(prefix, 0)

  @inline
  def startsWith(prefix: String, toffset: Int): scala.Boolean = {
    (toffset <= length() && toffset >= 0 &&
        thisString.jsSubstring(toffset, toffset + prefix.length()) == prefix)
  }

  @inline
  def subSequence(beginIndex: Int, endIndex: Int): CharSequence =
    substring(beginIndex, endIndex)

  @inline
  def substring(beginIndex: Int): String =
    thisString.jsSubstring(beginIndex)

  @inline
  def substring(beginIndex: Int, endIndex: Int): String = {
    this.asInstanceOf[js.Dynamic]
      .substring(beginIndex.asInstanceOf[js.Dynamic], endIndex.asInstanceOf[js.Dynamic])
      .asInstanceOf[String]
  }

  def toCharArray(): Array[Char] = {
    val len = length()
    val result = new Array[Char](len)
    var i = 0
    while (i < len) {
      result(i) = charAt(i)
      i += 1
    }
    result
  }

  @inline
  def toLowerCase(): String =
    thisJSString.toLowerCase()

  @inline
  def toUpperCase(): String =
    thisJSString.toUpperCase()

  @inline
  def trim(): String =
    thisJSString.trim()

  @inline
  override def toString(): String =
    thisString
}

object _String { // scalastyle:ignore
  /** Operations on a primitive JS string that are shadowed by Scala methods,
   *  and that we need to implement these very Scala methods.
   */
  private trait SpecialJSStringOps extends js.Any {
    def toLowerCase(): String
    def toUpperCase(): String
    def trim(): String
  }

  final lazy val CASE_INSENSITIVE_ORDER: Comparator[String] = {
    new Comparator[String] with Serializable {
      def compare(o1: String, o2: String): Int = o1.compareToIgnoreCase(o2)
    }
  }

  // Constructors

  def `new`(): String = ""

  def `new`(value: Array[Char]): String =
    `new`(value, 0, value.length)

  def `new`(value: Array[Char], offset: Int, count: Int): String = {
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

  def `new`(bytes: Array[scala.Byte]): String =
    `new`(bytes, Charset.defaultCharset)

  def `new`(bytes: Array[scala.Byte], charsetName: String): String =
    `new`(bytes, Charset.forName(charsetName))

  def `new`(bytes: Array[scala.Byte], charset: Charset): String =
    charset.decode(ByteBuffer.wrap(bytes)).toString()

  def `new`(bytes: Array[scala.Byte], offset: Int, length: Int): String =
    `new`(bytes, offset, length, Charset.defaultCharset)

  def `new`(bytes: Array[scala.Byte], offset: Int, length: Int,
      charsetName: String): String =
    `new`(bytes, offset, length, Charset.forName(charsetName))

  def `new`(bytes: Array[scala.Byte], offset: Int, length: Int,
      charset: Charset): String =
    charset.decode(ByteBuffer.wrap(bytes, offset, length)).toString()

  def `new`(codePoints: Array[Int], offset: Int, count: Int): String = {
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

  def `new`(original: String): String = {
    if (original == null)
      throw new NullPointerException
    original
  }

  def `new`(buffer: java.lang.StringBuffer): String =
    buffer.toString

  def `new`(builder: java.lang.StringBuilder): String =
    builder.toString

  // Static methods (aka methods on the companion object)

  def valueOf(b: scala.Boolean): String = b.toString()
  def valueOf(c: scala.Char): String = c.toString()
  def valueOf(i: scala.Int): String = i.toString()
  def valueOf(l: scala.Long): String = l.toString()
  def valueOf(f: scala.Float): String = f.toString()
  def valueOf(d: scala.Double): String = d.toString()

  @inline def valueOf(obj: Object): String =
    "" + obj // if (obj eq null), returns "null"

  def valueOf(data: Array[Char]): String =
    valueOf(data, 0, data.length)

  def valueOf(data: Array[Char], offset: Int, count: Int): String =
    `new`(data, offset, count)

  def format(format: String, args: Array[AnyRef]): String = {
    val frm = new java.util.Formatter()
    val res = frm.format(format, args: _*).toString()
    frm.close()
    res
  }

  // Helpers

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
