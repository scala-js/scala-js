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

import java.util.Objects.requireNonNull

class StringBuilder extends AnyRef with CharSequence with Appendable with java.io.Serializable {

  private[this] var content: String = ""

  def this(str: String) = {
    this()
    content = requireNonNull(str)
  }

  def this(initialCapacity: Int) = {
    this()
    new Array[Char](initialCapacity) // NegativeArraySize check
  }

  def this(seq: CharSequence) = this(seq.toString)

  @inline
  def append(obj: AnyRef): StringBuilder = {
    // If (obj eq null), this appends "null", otherwise obj.toString()
    content += obj
    this
  }

  @inline
  def append(str: String): StringBuilder = {
    content += str // if (str eq null), this appends "null"
    this
  }

  def append(sb: StringBuffer): StringBuilder = append(sb: AnyRef)

  def append(s: CharSequence): StringBuilder = append(s: AnyRef)

  def append(s: CharSequence, start: Int, end: Int): StringBuilder = {
    val s2 = if (s == null) "null" else s
    checkStartEnd(start, end, s2.length())
    append(s2.subSequence(start, end).toString())
  }

  def append(str: Array[scala.Char]): StringBuilder =
    append(String.valueOf(str))

  def append(str: Array[scala.Char], offset: Int, len: Int): StringBuilder = {
    /* Since we check offset >= 0, if offset + len can only overflow from the
     * positives into the negatives, in which case offset + len < offset, which
     * is reported as well.
     */
    checkStartEnd(offset, offset + len, str.length)
    append(String.valueOf(str, offset, len))
  }

  def append(b: scala.Boolean): StringBuilder = append(b.toString())
  def append(c: scala.Char): StringBuilder = append(c.toString())
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
     *
     * The two calls to substring imply the required bounds checks.
     */
    val oldContent = content
    content = oldContent.substring(0, index) + oldContent.substring(index + 1)
    this
  }

  def replace(start: Int, end: Int, str: String): StringBuilder = {
    val strNonNull = requireNonNull(str)
    val oldContent = content
    val length = oldContent.length

    // The call to substring implies the bounds checks for 0 <= start <= length
    val firstPart = oldContent.substring(0, start) + strNonNull
    if (end < start)
      oldContent.charAt(-1)

    content =
      if (end >= length) firstPart
      else firstPart + oldContent.substring(end)
    this
  }

  def insert(index: Int, str: Array[scala.Char], offset: Int,
      len: Int): StringBuilder = {
    // Surprisingly, this overload specifies a StringIOOBE.
    insert(index, String.valueOf(str, offset, len))
  }

  @inline def insert(offset: Int, obj: AnyRef): StringBuilder =
    insert(offset, String.valueOf(obj))

  def insert(offset: Int, str: String): StringBuilder = {
    // The first call to substring implies the required bounds checks
    val oldContent = content
    content =
      oldContent.substring(0, offset) + str + oldContent.substring(offset)
    this
  }

  def insert(offset: Int, str: Array[scala.Char]): StringBuilder =
    insert(offset, String.valueOf(str))

  def insert(dstOffset: Int, s: CharSequence): StringBuilder = {
    checkInsertOffset(dstOffset)
    insert(dstOffset, String.valueOf(s))
  }

  def insert(dstOffset: Int, s: CharSequence, start: Int,
      end: Int): StringBuilder = {
    checkInsertOffset(dstOffset)
    val s2 = if (s == null) "null" else s
    checkStartEnd(start, end, s2.length())
    insert(dstOffset, s2.subSequence(start, end).toString())
  }

  def insert(offset: Int, b: scala.Boolean): StringBuilder =
    insert(offset, b.toString)

  def insert(offset: Int, c: scala.Char): StringBuilder = {
    checkInsertOffset(offset)
    insert(offset, c.toString)
  }

  def insert(offset: Int, i: scala.Int): StringBuilder =
    insert(offset, i.toString)

  def insert(offset: Int, l: scala.Long): StringBuilder =
    insert(offset, l.toString)

  def insert(offset: Int, f: scala.Float): StringBuilder =
    insert(offset, f.toString)

  def insert(offset: Int, d: scala.Double): StringBuilder =
    insert(offset, d.toString)

  /** Explicitly checks an insertion offset.
   *
   *  Used by the overloads of insert() that specify IOOBE, instead of UB
   *  StringIOOBE.
   */
  @inline
  private def checkInsertOffset(offset: Int): Unit = {
    if (offset < 0 || offset > content.length)
      throw new IndexOutOfBoundsException(offset)
  }

  /** Explicitly checks start and end indices.
   *
   *  Used by the overloads of append() and insert() that specify IOOBE,
   *  instead of UB StringIOOBE.
   */
  @inline
  private def checkStartEnd(start: Int, end: Int, length: Int): Unit = {
    if (start < 0 || start > end || end > length)
      throw new IndexOutOfBoundsException()
  }

  def indexOf(str: String): Int = content.indexOf(str)

  def indexOf(str: String, fromIndex: Int): Int =
    content.indexOf(str, fromIndex)

  def lastIndexOf(str: String): Int = content.lastIndexOf(str)

  def lastIndexOf(str: String, fromIndex: Int): Int =
    content.lastIndexOf(str, fromIndex)

  def reverse(): StringBuilder = {
    val original = content
    var result = ""
    var i = original.length - 1
    while (i > 0) {
      val c = original.charAt(i)
      if (Character.isLowSurrogate(c)) {
        val c2 = original.charAt(i - 1)
        if (Character.isHighSurrogate(c2)) {
          result = result + c2.toString + c.toString
          i -= 2
        } else {
          result += c.toString
          i -= 1
        }
      } else {
        result += c.toString
        i -= 1
      }
    }
    if (i == 0)
      result += original.charAt(0).toString
    content = result
    this
  }

  override def toString(): String = content

  def length(): Int = content.length()

  def capacity(): Int = length()

  def ensureCapacity(minimumCapacity: Int): Unit = ()

  def trimToSize(): Unit = ()

  def setLength(newLength: Int): Unit = {
    var newContent = content
    var currentLength = newContent.length()

    if (newLength >= currentLength) {
      // Implies newLength >= 0, so bounds are OK
      while (currentLength != newLength) {
        newContent += "\u0000"
        currentLength += 1
      }
      content = newContent
    } else {
      // The call to substring implies the required bounds check
      content = newContent.substring(0, newLength)
    }
  }

  def charAt(index: Int): Char = content.charAt(index)

  def codePointAt(index: Int): Int = content.codePointAt(index)

  def codePointBefore(index: Int): Int = content.codePointBefore(index)

  def codePointCount(beginIndex: Int, endIndex: Int): Int =
    content.codePointCount(beginIndex, endIndex)

  def offsetByCodePoints(index: Int, codePointOffset: Int): Int =
    content.offsetByCodePoints(index, codePointOffset)

  def getChars(srcBegin: Int, srcEnd: Int, dst: Array[scala.Char],
      dstBegin: Int): Unit = {
    content.getChars(srcBegin, srcEnd, dst, dstBegin)
  }

  def setCharAt(index: Int, ch: scala.Char): Unit = {
    // The two calls to substring imply the required bounds checks.
    val oldContent = content
    content =
      oldContent.substring(0, index) + ch + oldContent.substring(index + 1)
  }

  def substring(start: Int): String = content.substring(start)

  def subSequence(start: Int, end: Int): CharSequence = substring(start, end)

  def substring(start: Int, end: Int): String = content.substring(start, end)
}
