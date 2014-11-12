package scala.scalajs.runtime

import scala.scalajs.js
import scala.scalajs.js.prim.{String => jsString}

import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.util.regex._

/**
 * Implementation trait for methods on java.lang.String. The compiler forwards
 * methods on java.lang.String to the implementation proxy of this trait.
 *
 * Attention: You cannot use `this` to use one function of this trait in another
 * one, since the `this` pointer is not properly identified as a
 * java.lang.String and compiler patching is not performed. Use
 * (this: String).foo() instead.
 *
 * Further, you have to take extreme care of not using the methods of
 * RuntimeString inadvertedly instead of the ones on js.String.
 * For example: this.substring(0) will call String.substring, because
 * 0 is of type Int, but should be of type js.Number.
 *
 * Therefore: ALWAYS ascribe the this pointer!
 */
private[runtime] trait RuntimeString { this: jsString =>

  import RuntimeString.fromCodePoint

  def charAt(index: Int): Char =
    (this: jsString).charCodeAt(index).asInstanceOf[Int].toChar

  def codePointAt(index: Int): Int = {
    val thisjs: jsString = this
    val high = thisjs.charCodeAt(index).toChar
    if (index+1 < thisjs.length.asInstanceOf[Int]) {
      val low = thisjs.charCodeAt(index+1).toChar
      if (Character.isSurrogatePair(high, low))
        Character.toCodePoint(high, low)
      else
        high.toInt
    } else {
      high.toInt
    }
  }

  def compareTo(anotherString: String): Int = {
    val thatjs: jsString = anotherString
    val thisjs: jsString = this
    if (thisjs == thatjs) 0
    else if (thisjs < thatjs) -1
    else 1
  }
  def compareToIgnoreCase(str: String): Int = {
    val thatljs = (str: jsString).toLowerCase
    val thisljs = (this: jsString).toLowerCase
    if (thisljs == thatljs) 0
    else if (thisljs < thatljs) -1
    else 1
  }

  def equalsIgnoreCase(that: String) = {
    if (that eq null) false
    else {
      val thatljs = (that: jsString).toLowerCase
      val thisljs = (this: jsString).toLowerCase

      thisljs == thatljs
    }
  }

  def concat(s: String): String = (this: jsString) + s

  def contains(s: CharSequence): Boolean =
    (this: jsString).indexOf(s.toString).toInt != -1

  def endsWith(suffix: String): Boolean = {
    val thisjs: jsString = this
    suffix == thisjs.substring(thisjs.length - suffix.length)
  }

  def getBytes(): Array[Byte] =
    (this: String).getBytes(Charset.defaultCharset)

  def getBytes(charsetName: String): Array[Byte] =
    (this: String).getBytes(Charset.forName(charsetName))

  def getBytes(charset: Charset): Array[Byte] =
    charset.encode(this).array()

  def getChars(srcBegin: Int, srcEnd: Int,
    dst: Array[Char], dstBegin: Int): Unit = {

    val thisjs: jsString = this

    if (srcBegin < 0 ||
        srcEnd   > thisjs.length ||
        srcEnd   < 0 ||
        srcBegin > srcEnd) {
      throw new StringIndexOutOfBoundsException("Index out of Bound")
    }

    val offset = dstBegin - srcBegin
    var i = srcBegin

    while (i < srcEnd) {
      dst(i+offset) = thisjs.charCodeAt(i).toChar
      i += 1
    }

  }

  def indexOf(ch: Int): Int =
    (this: String).indexOf(fromCodePoint(ch))

  def indexOf(ch: Int, fromIndex: Int): Int =
    (this: String).indexOf(fromCodePoint(ch), fromIndex)

  def indexOf(str: String): Int =
    (this: jsString).indexOf(str).toInt
  def indexOf(str: String, fromIndex: Int): Int =
    (this: jsString).indexOf(str, fromIndex).toInt

  /**
   * Just returning this string is a valid implementation for `intern` in
   * JavaScript, since strings are primitive values. Therefore, value equality
   * and reference equality is the same.
   */
  def intern(): String = this

  def isEmpty(): Boolean = (this: jsString).length.toInt == 0

  def lastIndexOf(ch: Int): Int =
    (this: String).lastIndexOf(fromCodePoint(ch))

  def lastIndexOf(ch: Int, fromIndex: Int): Int =
    (this: String).lastIndexOf(fromCodePoint(ch), fromIndex)

  def lastIndexOf(str: String): Int =
    (this: jsString).lastIndexOf(str).toInt
  def lastIndexOf(str: String, fromIndex: Int): Int =
    (this: jsString).lastIndexOf(str, fromIndex).toInt

  def length(): Int = (this: jsString).length.toInt

  def matches(regex: String): Boolean =
    Pattern.matches(regex, this: String)

  def replace(oldChar: Char, newChar: Char): String =
    (this: String).replace(oldChar.toString, newChar.toString)
  def replace(target: CharSequence, replacement: CharSequence): String =
    (this: jsString).split(target.toString).join(replacement.toString)
  def replaceAll(regex: String, replacement: String): String = {
    val pat = Pattern.compile(regex)
    val mat = pat.matcher(this: String)
    mat.replaceAll(replacement)
  }
  def replaceFirst(regex: String, replacement: String): String = {
    val pat = Pattern.compile(regex)
    val mat = pat.matcher(this: String)
    mat.replaceFirst(replacement)
  }

  def split(regex: String): Array[String] =
    (this: String).split(regex, 0)
  def split(regex: String, limit: Int): Array[String] = {
    val pat = Pattern.compile(regex)
    pat.split(this: String, limit)
  }

  def startsWith(prefix: String): Boolean =
    (this: String).startsWith(prefix, 0)
  def startsWith(prefix: String, toffset: Int): Boolean =
    prefix == (this: jsString).substring(toffset, prefix.length)

  def subSequence(beginIndex: Int, endIndex: Int): CharSequence =
    (this: jsString).substring(beginIndex, endIndex)

  def substring(beginIndex: Int): String =
    (this: jsString).substring(beginIndex)
  def substring(beginIndex: Int, endIndex: Int): String =
    (this: jsString).substring(beginIndex, endIndex)

  def toCharArray(): Array[Char] = {
    val length = (this: jsString).length.toInt
    val result = new Array[Char](length)
    var i = 0
    while (i < length) {
      result(i) = (this: String).charAt(i)
      i += 1
    }
    result
  }
  def toLowerCase(): String = (this: jsString).toLowerCase
  def toUpperCase(): String = (this: jsString).toUpperCase

  def trim(): String = (this: jsString).trim()

}

/**
 * Implementations for constructors of java.lang.String. Do not use directly,
 * call new String(...) instead
 */
private[runtime] object RuntimeString {

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
      charCodes.push(value(i).toInt)
      i += 1
    }
    js.String.fromCharCode(charCodes: _*)
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

    val charCodes = new js.Array[Int]
    var i = offset
    while (i != end) {
      val cp = codePoints(i)
      if (cp < 0 || cp > Character.MAX_CODE_POINT)
        throw new IllegalArgumentException
      if (cp <= Character.MAX_VALUE) {
        charCodes.push(cp)
      } else {
        val offsetCp = cp - 0x10000
        charCodes.push((offsetCp >> 10) | 0xd800)
        charCodes.push((offsetCp & 0x3ff) | 0xdc00)
      }
      i += 1
    }
    js.String.fromCharCode(charCodes: _*)
  }

  def newString(original: String): String = original
  def newString(buffer: StringBuffer): String = buffer.toString
  def newString(builder: java.lang.StringBuilder): String = builder.toString

  // Static methods (aka methods on the companion object)

  def valueOf(value: scala.Boolean) = new java.lang.Boolean(value).toString()
  def valueOf(value: scala.Char) = new java.lang.Character(value).toString()
  def valueOf(value: scala.Byte) = new java.lang.Byte(value).toString()
  def valueOf(value: scala.Short) = new java.lang.Short(value).toString()
  def valueOf(value: scala.Int) = new java.lang.Integer(value).toString()
  def valueOf(value: scala.Long) = new java.lang.Long(value).toString()
  def valueOf(value: scala.Float) = new java.lang.Float(value).toString()
  def valueOf(value: scala.Double) = new java.lang.Double(value).toString()
  def valueOf(value: java.lang.Object) =
    if (value eq null) "null" else value.toString()

  def format(format: String, args: Array[AnyRef]): String = {
    val frm = new java.util.Formatter()
    val res = frm.format(format, args: _*).toString()
    frm.close()
    res
  }

  // Helpers

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
