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

import scala.scalajs.js
import scala.scalajs.js.annotation._

class StringBuilder(initialCapacity: Int)
    extends AnyRef with CharSequence with Appendable with java.io.Serializable {

  if (initialCapacity < 0)
    throw new NegativeArraySizeException()

  private[this] var content: js.typedarray.Uint16Array = new js.typedarray.Uint16Array(initialCapacity)
  private[this] var length: Int = 0

  private[this] def expand(newCapacity: Int): Unit = {
    // TODO what if newCapacity somehow is less than content.length?
    val newContent = new js.typedarray.Uint16Array(newCapacity)
    newContent.set(content)
    content = newContent
  }

  def this(str: String) = {
    this((if (str eq null) throw new NullPointerException else str.length) * 2)
    this.append(str)
  }

  def this() = {
    this(16)
  }

  def this(seq: CharSequence) = this(seq.toString)

  @inline
  def append(obj: AnyRef): StringBuilder = {
    append(if (obj eq null) "null" else obj.toString)
    this
  }

  @inline
  def append(str: String): StringBuilder = {
    if (str eq null) {
      append("null")
    } else {
      var idx: Int = 0
      while(idx < str.length) {
        append(str.charAt(idx))
        idx += 1
      }
      this
    }
  }

  def append(sb: StringBuffer): StringBuilder = append(sb: AnyRef)

  def append(s: CharSequence): StringBuilder = append(s: AnyRef)

  def append(s: CharSequence, start: Int, end: Int): StringBuilder =
    append((if (s == null) "null" else s).subSequence(start, end))

  def append(str: Array[scala.Char]): StringBuilder =
    append(str, 0, str.length)

  def append(str: Array[scala.Char], offset: Int, len: Int): StringBuilder = {
    var idx = offset
    while(idx < (offset + len)) {
      append(str(idx))
      idx += 1
    }
    this
  }

  def append(b: scala.Boolean): StringBuilder = append(b.toString())

  def append(c: scala.Char): StringBuilder = {
    // TODO should one pull this out into the Array[Char] append to only if-check once?
    if (length >= content.length) {
      expand(length * 2)
    }

    content(length) = c.toInt
    length += 1

    this
  }

  def append(i: scala.Int): StringBuilder = append(i.toString())
  def append(lng: scala.Long): StringBuilder = append(lng.toString())
  def append(f: scala.Float): StringBuilder = append(f.toString())
  def append(d: scala.Double): StringBuilder = append(d.toString())

  def appendCodePoint(codePoint: Int): StringBuilder =
    append(Character.toString(codePoint))

  def delete(start: Int, end: Int): StringBuilder =
    replace(start, end, "")

  def deleteCharAt(index: Int): StringBuilder = {
    /* This is not equivalent to `delete(index, index + 1)` when
     * `index == length`.
     */
    if (index < 0 || index >= length)
      throw new StringIndexOutOfBoundsException(index)

    length -= 1
    var idx = index
    while(idx < length) {
      content(idx) = content(idx + 1)
      idx += 1
    }

    this
  }

  def replace(start: Int, end: Int, str: String): StringBuilder = {
    if (start < 0 || start > length || start > end)
      throw new StringIndexOutOfBoundsException(start)

    val replacementLength = end - start
    val insertLength = str.length

    if(insertLength > replacementLength) {
      // Hard case, we copy characters forward which means we have to work back-to-front
      val displacement = insertLength - replacementLength
      length += displacement
      if(length >= content.length)
        expand(length + displacement)

      var idx = length
      while(idx > (end + displacement)) {
        content(idx) = content(idx - displacement)
        idx -= 1
      }
    } else if (insertLength < replacementLength) {
      // Easy case, we copy characters backward
      val displacement = replacementLength - insertLength
      length -= displacement

      var idx = end
      while(idx < (length + displacement)) {
        content(idx) = content(idx + displacement)
        idx += 1
      }
    }

    // Now emplace the actual string
    var idx = start
    while(idx < end) {
      content(idx) = str.charAt(idx).toInt
      idx += 1
    }

    this
  }

  def insert(index: Int, str: Array[scala.Char], offset: Int,
      len: Int): StringBuilder = {
    insert(index, String.valueOf(str, offset, len))
  }

  @inline def insert(offset: Int, obj: AnyRef): StringBuilder =
    insert(offset, String.valueOf(obj))

  def insert(offset: Int, str: String): StringBuilder = {
    replace(offset, offset, str)
    this
  }

  def insert(offset: Int, str: Array[scala.Char]): StringBuilder =
    insert(offset, String.valueOf(str))

  def insert(dstOffset: Int, s: CharSequence): StringBuilder =
    insert(dstOffset, s: AnyRef)

  def insert(dstOffset: Int, s: CharSequence, start: Int,
      end: Int): StringBuilder = {
    insert(dstOffset, (if (s == null) "null" else s).subSequence(start, end))
  }

  def insert(offset: Int, b: scala.Boolean): StringBuilder =
    insert(offset, b.toString)

  def insert(offset: Int, c: scala.Char): StringBuilder =
    insert(offset, c.toString)

  def insert(offset: Int, i: scala.Int): StringBuilder =
    insert(offset, i.toString)

  def insert(offset: Int, l: scala.Long): StringBuilder =
    insert(offset, l.toString)

  def insert(offset: Int, f: scala.Float): StringBuilder =
    insert(offset, f.toString)

  def insert(offset: Int, d: scala.Double): StringBuilder =
    insert(offset, d.toString)

  def indexOf(str: String): Int =
    indexOf(str, 0)

  def indexOf(str: String, fromIndex: Int): Int = {
    // TODO should fromIndex be checked for OOB access?
    var idx = fromIndex
    var run = 0
    while((idx + run) < length && run < str.length) {
      if (str.charAt(run).toInt == content(idx + run)) {
        run += 1
      } else if (run != 0) {
        idx += run
        run = 0
      } else {
        idx += 1
      }
    }

    if(run == str.length) {
      idx
    } else {
      -1
    }
  }

  def lastIndexOf(str: String): Int =
    lastIndexOf(str, length)

  def lastIndexOf(str: String, fromIndex: Int): Int = {
    // TODO should fromIndex be checked for OOB access?
    var idx = fromIndex
    var run = 0
    while((idx - run) >= 0 && run < str.length) {
      if (str.charAt((str.length - 1) - run).toInt == content(idx - run)) {
        run += 1
      } else if (run != 0) {
        idx -= run
        run = 0
      } else {
        idx -= 1
      }
    }

    if(run == str.length) {
      idx
    } else {
      -1
    }
  }

  def reverse(): StringBuilder = {
    val newContent = new js.typedarray.Uint16Array(length)
    var i = length - 1
    while (i > 0) {
      val c = content(i).toChar
      if (Character.isLowSurrogate(c)) {
        val c2 = content(i - 1).toChar
        if (Character.isHighSurrogate(c2)) {
          newContent((length - 1) - i) = content(i - 1)
          newContent((length - 1) - (i + 1)) = content(i)
          i -= 2
        } else {
          newContent((length - 1) - i) = content(i)
          i -= 1
        }
      } else {
        newContent((length - 1) - i) = content(i)
        i -= 1
      }
    }
    if (i == 0)
      newContent(length - 1) = content(0)
    content = newContent
    this
  }

  override def toString(): String = substring(0)

  def length(): Int = length

  def capacity(): Int = content.length

  def ensureCapacity(minimumCapacity: Int): Unit = expand(minimumCapacity)

  def trimToSize(): Unit = {
    val newContent = new js.typedarray.Uint16Array(length)
    newContent.set(content)
    content = newContent
  }

  def setLength(newLength: Int): Unit = {
    if (newLength < 0)
      throw new StringIndexOutOfBoundsException(newLength)

    while (length < newLength) {
      append('\u0000')
    }
  }

  def charAt(index: Int): Char = content(index).toChar // TODO handle index out of bounds

  def codePointAt(index: Int): Int = {
    // TODO handle index out of bounds
    val c: Char = content(index).toChar
    if (Character.isHighSurrogate(c)) {
      val c2: Char = content(index + 1).toChar
      if (Character.isLowSurrogate(c2)) {
        Character.toCodePoint(c, c2)
      } else {
        content(index)
      }
    } else {
      content(index)
    }
  }

  def codePointBefore(index: Int): Int = {
    // TODO handle index out of bounds
    val c: Char = content(index - 1).toChar
    if (Character.isLowSurrogate(c)) {
      val c2: Char = content(index - 2).toChar
      if (Character.isHighSurrogate(c2)) {
        Character.toCodePoint(c2, c)
      } else {
        content(index)
      }
    } else {
      content(index)
    }
  }

  def codePointCount(beginIndex: Int, endIndex: Int): Int = {
    // TODO handle index out of bounds
    var count = 0
    var idx = beginIndex
    while(idx < endIndex) {
      if (Character.isHighSurrogate(content(idx).toChar)) {
        if (Character.isLowSurrogate(content(idx + 1).toChar)) {
          idx += 2
        } else {
          idx += 1
        }
      } else {
        idx += 1
      }
      count += 1
    }
    count
  }

  def offsetByCodePoints(index: Int, codePointOffset: Int): Int = {
    // TODO handle index out of bounds
    var count = 0
    var idx = index
    while (count < codePointOffset) {
      if (Character.isHighSurrogate(content(idx).toChar)) {
        if (Character.isLowSurrogate(content(idx + 1).toChar)) {
          idx += 2
        } else {
          idx += 1
        }
      } else {
        idx += 1
      }
      count += 1
    }
    idx
  }

  def getChars(srcBegin: Int, srcEnd: Int, dst: Array[scala.Char],
      dstBegin: Int): Unit = {
    // TODO handle index out of bounds
    var srcIdx = srcBegin
    var dstIdx = dstBegin
    while (srcIdx < srcEnd) {
      dst(dstIdx) = content(srcIdx).toChar
      srcIdx += 1
      dstIdx += 1
    }
  }

  def setCharAt(index: Int, ch: scala.Char): Unit = {
    if (index < 0 || index >= length)
      throw new StringIndexOutOfBoundsException(index)

    content(index) = ch.toInt
  }

  def substring(start: Int): String = substring(start, length)

  def subSequence(start: Int, end: Int): CharSequence = substring(start, end)

  def substring(start: Int, end: Int): String = {
    val view = content.subarray(start, end)
    StringBuilder.JSString.fromCharCode.apply(null, view)
  }
}

object StringBuilder {

  @js.native @JSGlobal("String")
  private object JSString extends js.Object {
    @js.native
    object fromCharCode extends js.Any {
      @JSName("apply")
      def apply(thisValue: Any, args: Any*): String = js.native
    }
  }

}
