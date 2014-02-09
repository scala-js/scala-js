package scala.scalajs.runtime

import scala.scalajs.js

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
private[runtime] trait RuntimeString { this: js.String =>

  def charAt(index: Int): Char =
    (this: js.String).charCodeAt(index).toChar
  def codePointAt(index: Int): Int =
    (this: js.String).charCodeAt(index).toInt

  def compareTo(anotherString: String): Int = {
    val thatjs: js.String = anotherString
    val thisjs: js.String = this
    if (thisjs == thatjs) 0
    else if (thisjs < thatjs) -1
    else 1
  }
  def compareToIgnoreCase(str: String): Int = {
    val thatljs = (str: js.String).toLowerCase
    val thisljs = (this: js.String).toLowerCase
    if (thisljs == thatljs) 0
    else if (thisljs < thatljs) -1
    else 1
  }

  def equalsIgnoreCase(that: String) = {
    if (that eq null) false
    else {
      val thatljs = (that: js.String).toLowerCase
      val thisljs = (this: js.String).toLowerCase

      thisljs == thatljs
    }
  }

  def concat(s: String): String = (this: js.String) + s

  def contains(s: CharSequence): Boolean =
    (this: js.String).indexOf(s.toString).toInt != -1

  def endsWith(suffix: String): Boolean = {
    val thisjs: js.String = this
    suffix == thisjs.substring(thisjs.length - suffix.length)
  }

  /** Unimplemented, unused, but referenced */
  def getBytes(): Array[Byte] = ???
  /** Unimplemented, unused, but referenced */
  def getBytes(charsetName: String): Array[Byte] = ???

  def getChars(srcBegin: Int, srcEnd: Int,
    dst: Array[Char], dstBegin: Int): Unit = {

    val thisjs: js.String = this

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

  def indexOf(ch: Int): Int = {
    val search: js.String = js.String.fromCharCode(ch)
    (this: js.String).indexOf(search).toInt
  }
  def indexOf(ch: Int, fromIndex: Int): Int = {
    val search = js.String.fromCharCode(ch)
    (this: js.String).indexOf(search, fromIndex).toInt
  }

  def indexOf(str: String): Int =
    (this: js.String).indexOf(str).toInt
  def indexOf(str: String, fromIndex: Int): Int =
    (this: js.String).indexOf(str, fromIndex).toInt

  /**
   * Just returning this string is a valid implementation for `intern` in
   * JavaScript, since strings are primitive values. Therefore, value equality
   * and reference equality is the same.
   */
  def intern(): String = this

  def isEmpty(): Boolean = !(this: js.String).length

  def lastIndexOf(ch: Int): Int = {
    val search = js.String.fromCharCode(ch)
    (this: js.String).lastIndexOf(search).toInt
  }
  def lastIndexOf(ch: Int, fromIndex: Int): Int = {
    val search = js.String.fromCharCode(ch)
    (this: js.String).lastIndexOf(search, fromIndex).toInt
  }
  def lastIndexOf(str: String): Int =
    (this: js.String).lastIndexOf(str).toInt
  def lastIndexOf(str: String, fromIndex: Int): Int =
    (this: js.String).lastIndexOf(str, fromIndex).toInt

  def length(): Int = (this: js.String).length.toInt

  def matches(regex: String): Boolean =
    Pattern.matches(regex, this: String)

  def replace(oldChar: Char, newChar: Char): String =
    (this: String).replace(oldChar.toString, newChar.toString)
  def replace(target: CharSequence, replacement: CharSequence): String =
    (this: js.String).split(target.toString).join(replacement.toString)
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
    prefix == (this: js.String).substring(toffset, prefix.length)

  def subSequence(beginIndex: Int, endIndex: Int): CharSequence =
    (this: js.String).substring(beginIndex, endIndex)

  def substring(beginIndex: Int): String =
    (this: js.String).substring(beginIndex)
  def substring(beginIndex: Int, endIndex: Int): String =
    (this: js.String).substring(beginIndex, endIndex)

  def toCharArray(): Array[Char] = {
    val length = (this: js.String).length.toInt
    val result = new Array[Char](length)
    var i = 0
    while (i < length) {
      result(i) = (this: String).charAt(i)
      i += 1
    }
    result
  }
  def toLowerCase(): String = (this: js.String).toLowerCase
  def toUpperCase(): String = (this: js.String).toUpperCase

  def trim(): String = (this: js.String).trim()

}

/**
 * Implementations for constructors of java.lang.String. Do not use directly,
 * call new String(...) instead
 */
private[runtime] object RuntimeString {

  def newString(): js.String = ""
  def newString(value: Array[Char]): js.String =
    newString(value, 0, value.length)
  def newString(value: Array[Char], offset: Int, count: Int): js.String = {
    var res: js.String = ""
    for (c <- value.view(offset, offset + count))
      res += c.toString
    res
  }
  /** Unimplemented, unused, but referenced */
  def newString(bytes: Array[Byte], charsetName: String): js.String = ???
  /** Unimplemented, unused, but referenced */
  def newString(bytes: Array[Byte], offest: Int, length: Int,
      charsetName: String): js.String = ???
  def newString(codePoints: Array[Int], offset: Int, count: Int): js.String =
    js.String.fromCharCode(
        codePoints.view(offset, offset + count).map(x => x: js.Number) :_*)
  def newString(original: String): js.String = original
  def newString(buffer: StringBuffer): js.String = buffer.toString
  def newString(builder: java.lang.StringBuilder): js.String = builder.toString

}
