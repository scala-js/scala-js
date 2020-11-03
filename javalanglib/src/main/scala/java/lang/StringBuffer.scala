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

/* Given the rare usefulness of StringBuffer in the context of Scala.js, and
 * its general bad reputation, we do not make any effort towards performance.
 * We simply delegate all operations to an underlying j.l.StringBuilder, which
 * shares the specifications of StringBuffer except for the synchronization
 * part, which is irrelevant in Scala.js anyway.
 */
class StringBuffer private (builder: StringBuilder)
    extends AnyRef with CharSequence with Appendable with java.io.Serializable {

  def this() = this(new StringBuilder())
  def this(str: String) = this(new StringBuilder(str))
  def this(capacity: Int) = this(new StringBuilder(capacity))
  def this(seq: CharSequence) = this(seq.toString)

  /** A helper so that we can write stuff like `withThisResult(append(obj))`. */
  @inline
  private def withThisResult(op: StringBuilder): StringBuffer = this

  def length(): Int = builder.length()

  def capacity(): Int = builder.capacity()

  def ensureCapacity(minimumCapacity: Int): Unit =
    builder.ensureCapacity(minimumCapacity)

  def trimToSize(): Unit = builder.trimToSize()

  def setLength(newLength: Int): Unit = builder.setLength(newLength)

  def charAt(index: Int): Char = builder.charAt(index)

  def codePointAt(index: Int): Int = builder.codePointAt(index)

  def codePointBefore(index: Int): Int = builder.codePointBefore(index)

  def codePointCount(beginIndex: Int, endIndex: Int): Int =
    builder.codePointCount(beginIndex, endIndex)

  def offsetByCodePoints(index: Int, codePointOffset: Int): Int =
    builder.offsetByCodePoints(index, codePointOffset)

  def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int): Unit = {
    builder.getChars(srcBegin, srcEnd, dst, dstBegin)
  }

  def setCharAt(index: Int, ch: Char): Unit =
    builder.setCharAt(index, ch)

  def append(obj: AnyRef): StringBuffer =
    withThisResult(builder.append(obj))

  def append(str: String): StringBuffer =
    withThisResult(builder.append(str))

  def append(sb: StringBuffer): StringBuffer =
    withThisResult(builder.append(sb))

  def append(s: CharSequence): StringBuffer =
    withThisResult(builder.append(s))

  def append(s: CharSequence, start: Int, end: Int): StringBuffer =
    withThisResult(builder.append(s, start, end))

  def append(str: Array[Char]): StringBuffer =
    withThisResult(builder.append(str))

  def append(str: Array[Char], offset: Int, len: Int): StringBuffer =
    withThisResult(builder.append(str, offset, len))

  def append(b: scala.Boolean): StringBuffer =
    withThisResult(builder.append(b))

  def append(c: Char): StringBuffer =
    withThisResult(builder.append(c))

  def append(i: Int): StringBuffer =
    withThisResult(builder.append(i))

  def appendCodePoint(codePoint: Int): StringBuffer =
    withThisResult(builder.appendCodePoint(codePoint))

  def append(lng: scala.Long): StringBuffer =
    withThisResult(builder.append(lng))

  def append(f: scala.Float): StringBuffer =
    withThisResult(builder.append(f))

  def append(d: scala.Double): StringBuffer =
    withThisResult(builder.append(d))

  def delete(start: Int, end: Int): StringBuffer =
    withThisResult(builder.delete(start, end))

  def deleteCharAt(index: Int): StringBuffer =
    withThisResult(builder.deleteCharAt(index))

  def replace(start: Int, end: Int, str: String): StringBuffer =
    withThisResult(builder.replace(start, end, str))

  def substring(start: Int): String =
    builder.substring(start)

  def subSequence(start: Int, end: Int): CharSequence =
    builder.subSequence(start, end)

  def substring(start: Int, end: Int): String = builder.substring(start, end)

  def insert(index: Int, str: Array[Char], offset: Int, len: Int): StringBuffer =
    withThisResult(builder.insert(index, str, offset, len))

  def insert(offset: Int, obj: AnyRef): StringBuffer =
    withThisResult(builder.insert(offset, obj))

  def insert(offset: Int, str: String): StringBuffer =
    withThisResult(builder.insert(offset, str))

  def insert(offset: Int, str: Array[Char]): StringBuffer =
    withThisResult(builder.insert(offset, str))

  def insert(dstOffset: Int, s: CharSequence): StringBuffer =
    withThisResult(builder.insert(dstOffset, s))

  def insert(dstOffset: Int, s: CharSequence, start: Int, end: Int): StringBuffer = {
    withThisResult(builder.insert(dstOffset, s, start, end))
  }

  def insert(offset: Int, b: scala.Boolean): StringBuffer =
    withThisResult(builder.insert(offset, b))

  def insert(offset: Int, c: Char): StringBuffer =
    withThisResult(builder.insert(offset, c))

  def insert(offset: Int, i: Int): StringBuffer =
    withThisResult(builder.insert(offset, i))

  def insert(offset: Int, l: scala.Long): StringBuffer =
    withThisResult(builder.insert(offset, l))

  def insert(offset: Int, f: scala.Float): StringBuffer =
    withThisResult(builder.insert(offset, f))

  def insert(offset: Int, d: scala.Double): StringBuffer =
    withThisResult(builder.insert(offset, d))

  def indexOf(str: String): Int =
    builder.indexOf(str)

  def indexOf(str: String, fromIndex: Int): Int =
    builder.indexOf(str, fromIndex)

  def lastIndexOf(str: String): Int =
    builder.lastIndexOf(str)

  def lastIndexOf(str: String, fromIndex: Int): Int =
    builder.lastIndexOf(str, fromIndex)

  def reverse(): StringBuffer =
    withThisResult(builder.reverse())

  override def toString(): String =
    builder.toString()
}
