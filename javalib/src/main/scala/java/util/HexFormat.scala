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

package java.util

import java.io.{IOException, UncheckedIOException}
import java.{lang => jl}

final class HexFormat private (
    private val delimiterString: String,
    private val prefixString: String,
    private val suffixString: String,
    private val uppercase: Boolean
) {

  private val digits: Array[Char] =
    if (uppercase) HexFormat.UpperCaseDigits else HexFormat.LowerCaseDigits

  def withDelimiter(delimiter: String): HexFormat = {
    Objects.requireNonNull(delimiter, "delimiter")
    new HexFormat(delimiter, prefixString, suffixString, uppercase)
  }

  def withPrefix(prefix: String): HexFormat = {
    Objects.requireNonNull(prefix, "prefix")
    new HexFormat(delimiterString, prefix, suffixString, uppercase)
  }

  def withSuffix(suffix: String): HexFormat = {
    Objects.requireNonNull(suffix, "suffix")
    new HexFormat(delimiterString, prefixString, suffix, uppercase)
  }

  def withUpperCase(): HexFormat =
    if (uppercase) this
    else new HexFormat(delimiterString, prefixString, suffixString, true)

  def withLowerCase(): HexFormat =
    if (!uppercase) this
    else new HexFormat(delimiterString, prefixString, suffixString, false)

  def delimiter(): String = delimiterString

  def prefix(): String = prefixString

  def suffix(): String = suffixString

  def isUpperCase(): Boolean = uppercase

  def formatHex(bytes: Array[Byte]): String =
    formatHex(bytes, 0, bytes.length)

  def formatHex(bytes: Array[Byte], fromIndex: Int, toIndex: Int): String = {
    HexFormat.checkFromToIndex(fromIndex, toIndex, bytes.length)

    val length = toIndex - fromIndex
    if (length == 0) ""
    else if (hasNoMarkup) {
      val table = digits
      val sb = new jl.StringBuilder(length << 1)
      var byteIndex = fromIndex
      while (byteIndex < toIndex) {
        val value = bytes(byteIndex) & 0xff
        sb.append(table(value >>> 4))
        sb.append(table(value & 0x0f))
        byteIndex += 1
      }
      sb.toString()
    } else {
      val table = digits
      val sb = new jl.StringBuilder(formattedLength(length))
      var byteIndex = fromIndex
      while (byteIndex < toIndex) {
        if (byteIndex != fromIndex) sb.append(delimiterString)
        sb.append(prefixString)
        val value = bytes(byteIndex) & 0xff
        sb.append(table(value >>> 4))
        sb.append(table(value & 0x0f))
        sb.append(suffixString)
        byteIndex += 1
      }
      sb.toString()
    }
  }

  def formatHex[A <: jl.Appendable](out: A, bytes: Array[Byte]): A =
    formatHex(out, bytes, 0, bytes.length)

  def formatHex[A <: jl.Appendable](
      out: A,
      bytes: Array[Byte],
      fromIndex: Int,
      toIndex: Int
  ): A = {
    Objects.requireNonNull(out, "out")
    HexFormat.checkFromToIndex(fromIndex, toIndex, bytes.length)

    val table = digits
    try {
      if (hasNoMarkup) {
        var byteIndex = fromIndex
        while (byteIndex < toIndex) {
          val value = bytes(byteIndex) & 0xff
          out.append(table(value >>> 4))
          out.append(table(value & 0x0f))
          byteIndex += 1
        }
      } else {
        val prefix = prefixString
        val suffix = suffixString
        val delimiter = delimiterString
        val hasPrefix = !prefix.isEmpty()
        val hasSuffix = !suffix.isEmpty()
        val hasDelimiter = !delimiter.isEmpty()
        var byteIndex = fromIndex
        while (byteIndex < toIndex) {
          if (byteIndex != fromIndex && hasDelimiter)
            out.append(delimiter)
          if (hasPrefix)
            out.append(prefix)
          val value = bytes(byteIndex) & 0xff
          out.append(table(value >>> 4))
          out.append(table(value & 0x0f))
          if (hasSuffix)
            out.append(suffix)
          byteIndex += 1
        }
      }
    } catch {
      case e: IOException => throw new UncheckedIOException(e.getMessage(), e)
    }

    out
  }

  def parseHex(string: jl.CharSequence): Array[Byte] =
    parseHex(string, 0, string.length())

  def parseHex(
      string: jl.CharSequence,
      fromIndex: Int,
      toIndex: Int
  ): Array[Byte] = {
    HexFormat.checkFromToIndex(fromIndex, toIndex, string.length())

    val length = toIndex - fromIndex
    if (hasNoMarkup)
      parseHexWithoutMarkup(string, fromIndex, length)
    else
      parseHexWithMarkup(string, fromIndex, length)
  }

  def parseHex(
      chars: Array[Char],
      fromIndex: Int,
      toIndex: Int
  ): Array[Byte] = {
    HexFormat.checkFromToIndex(fromIndex, toIndex, chars.length)

    val length = toIndex - fromIndex
    if (hasNoMarkup)
      parseHexWithoutMarkup(chars, fromIndex, length)
    else
      parseHexWithMarkup(chars, fromIndex, length)
  }

  def toLowHexDigit(value: Int): Char =
    digits(value & 0x0f)

  def toHighHexDigit(value: Int): Char =
    digits((value >>> 4) & 0x0f)

  def toHexDigits[A <: jl.Appendable](out: A, value: Byte): A = {
    Objects.requireNonNull(out, "out")
    try {
      val intValue = value & 0xff
      out.append(digits(intValue >>> 4))
      out.append(digits(intValue & 0x0f))
    } catch {
      case e: IOException => throw new UncheckedIOException(e.getMessage(), e)
    }
    out
  }

  def toHexDigits(value: Byte): String =
    twoDigits(value & 0xff)

  def toHexDigits(value: Char): String =
    toHexDigits(value.toInt.toLong, 4)

  def toHexDigits(value: Short): String =
    toHexDigits((value.toInt & 0xffff).toLong, 4)

  def toHexDigits(value: Int): String =
    toHexDigits(value.toLong, 8)

  def toHexDigits(value: Long): String =
    toHexDigits(value, 16)

  def toHexDigits(value: Long, digits: Int): String = {
    if (digits < 0 || digits > 16)
      throw new IllegalArgumentException("number of digits: " + digits)

    val sb = new jl.StringBuilder(digits)
    var i = 0
    var shift = (digits - 1) << 2
    while (i < digits) {
      sb.append(this.digits(((value >>> shift) & 0x0fL).toInt))
      i += 1
      shift -= 4
    }
    sb.toString()
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: HexFormat =>
      uppercase == that.uppercase &&
      delimiterString == that.delimiterString &&
      prefixString == that.prefixString &&
      suffixString == that.suffixString
    case _ => false
  }

  override def hashCode(): Int = {
    var h = delimiterString.hashCode()
    h = 31 * h + prefixString.hashCode()
    h = 31 * h + suffixString.hashCode()
    h = 31 * h + (if (uppercase) 1231 else 1237)
    h
  }

  override def toString(): String =
    "uppercase: " + uppercase +
      ", delimiter: \"" + delimiterString +
      "\", prefix: \"" + prefixString +
      "\", suffix: \"" + suffixString + "\""

  private def hasNoMarkup: Boolean =
    delimiterString.isEmpty() && prefixString.isEmpty() && suffixString
      .isEmpty()

  private def formattedLength(byteCount: Int): Int = {
    val valueLength = prefixString.length() + 2 + suffixString.length()
    val total =
      byteCount.toLong * valueLength.toLong +
        (byteCount.toLong - 1L) * delimiterString.length().toLong

    if (total > Integer.MAX_VALUE)
      throw new OutOfMemoryError("Required array size too large")
    total.toInt
  }

  private def twoDigits(value: Int): String = {
    val sb = new jl.StringBuilder(2)
    sb.append(digits(value >>> 4))
    sb.append(digits(value & 0x0f))
    sb.toString()
  }

  private def parseHexWithoutMarkup(
      string: jl.CharSequence,
      fromIndex: Int,
      length: Int
  ): Array[Byte] = {
    if ((length & 1) != 0)
      throw new IllegalArgumentException("string length not even: " + length)

    val bytes = new Array[Byte](length >>> 1)
    val table = HexFormat.DigitValues
    val tableLen = table.length
    var sourceIndex = fromIndex
    var destIndex = 0
    while (destIndex < bytes.length) {
      val high = string.charAt(sourceIndex)
      val low = string.charAt(sourceIndex + 1)
      val hi = if (high < tableLen) table(high).toInt else -1
      val lo = if (low < tableLen) table(low).toInt else -1
      if ((hi | lo) < 0)
        HexFormat.throwBadDigitPair(high, low, hi)
      bytes(destIndex) = ((hi << 4) | lo).toByte
      sourceIndex += 2
      destIndex += 1
    }
    bytes
  }

  private def parseHexWithoutMarkup(
      chars: Array[Char],
      fromIndex: Int,
      length: Int
  ): Array[Byte] = {
    if ((length & 1) != 0)
      throw new IllegalArgumentException("string length not even: " + length)

    val bytes = new Array[Byte](length >>> 1)
    val table = HexFormat.DigitValues
    val tableLen = table.length
    var sourceIndex = fromIndex
    var destIndex = 0
    while (destIndex < bytes.length) {
      val high = chars(sourceIndex)
      val low = chars(sourceIndex + 1)
      val hi = if (high < tableLen) table(high).toInt else -1
      val lo = if (low < tableLen) table(low).toInt else -1
      if ((hi | lo) < 0)
        HexFormat.throwBadDigitPair(high, low, hi)
      bytes(destIndex) = ((hi << 4) | lo).toByte
      sourceIndex += 2
      destIndex += 1
    }
    bytes
  }

  private def parseHexWithMarkup(
      string: jl.CharSequence,
      fromIndex: Int,
      length: Int
  ): Array[Byte] = {
    val byteCount = parsedByteCount(length)
    val bytes = new Array[Byte](byteCount)
    val table = HexFormat.DigitValues
    val tableLen = table.length
    val prefix = prefixString
    val suffix = suffixString
    val delimiter = delimiterString
    val prefixLen = prefix.length()
    val suffixLen = suffix.length()
    val delimiterLen = delimiter.length()
    var sourceIndex = fromIndex
    var destIndex = 0
    while (destIndex < byteCount) {
      if (prefixLen != 0)
        sourceIndex = requireMatch(string, sourceIndex, prefix, prefixLen)
      val high = string.charAt(sourceIndex)
      val low = string.charAt(sourceIndex + 1)
      val hi = if (high < tableLen) table(high).toInt else -1
      val lo = if (low < tableLen) table(low).toInt else -1
      if ((hi | lo) < 0)
        HexFormat.throwBadDigitPair(high, low, hi)
      bytes(destIndex) = ((hi << 4) | lo).toByte
      sourceIndex += 2
      if (suffixLen != 0)
        sourceIndex = requireMatch(string, sourceIndex, suffix, suffixLen)
      if (destIndex != byteCount - 1 && delimiterLen != 0)
        sourceIndex = requireMatch(string, sourceIndex, delimiter, delimiterLen)
      destIndex += 1
    }
    bytes
  }

  private def parseHexWithMarkup(
      chars: Array[Char],
      fromIndex: Int,
      length: Int
  ): Array[Byte] = {
    val byteCount = parsedByteCount(length)
    val bytes = new Array[Byte](byteCount)
    val table = HexFormat.DigitValues
    val tableLen = table.length
    val prefix = prefixString
    val suffix = suffixString
    val delimiter = delimiterString
    val prefixLen = prefix.length()
    val suffixLen = suffix.length()
    val delimiterLen = delimiter.length()
    var sourceIndex = fromIndex
    var destIndex = 0
    while (destIndex < byteCount) {
      if (prefixLen != 0)
        sourceIndex = requireMatch(chars, sourceIndex, prefix, prefixLen)
      val high = chars(sourceIndex)
      val low = chars(sourceIndex + 1)
      val hi = if (high < tableLen) table(high).toInt else -1
      val lo = if (low < tableLen) table(low).toInt else -1
      if ((hi | lo) < 0)
        HexFormat.throwBadDigitPair(high, low, hi)
      bytes(destIndex) = ((hi << 4) | lo).toByte
      sourceIndex += 2
      if (suffixLen != 0)
        sourceIndex = requireMatch(chars, sourceIndex, suffix, suffixLen)
      if (destIndex != byteCount - 1 && delimiterLen != 0)
        sourceIndex = requireMatch(chars, sourceIndex, delimiter, delimiterLen)
      destIndex += 1
    }
    bytes
  }

  private def parsedByteCount(length: Int): Int = {
    if (length == 0)
      0
    else {
      val valueLength = prefixString.length() + 2 + suffixString.length()
      val recordLength = valueLength + delimiterString.length()
      val adjustedLength = length.toLong + delimiterString.length().toLong

      if (adjustedLength % recordLength != 0L)
        throw HexFormat.invalidFormatException()

      val count = adjustedLength / recordLength
      if (count <= 0L || count > Integer.MAX_VALUE)
        throw HexFormat.invalidFormatException()
      count.toInt
    }
  }

  private def requireMatch(
      string: jl.CharSequence,
      index: Int,
      expected: String,
      expectedLen: Int
  ): Int = {
    var i = 0
    while (i < expectedLen) {
      if (string.charAt(index + i) != expected.charAt(i))
        throw HexFormat.invalidFormatException()
      i += 1
    }
    index + expectedLen
  }

  private def requireMatch(
      chars: Array[Char],
      index: Int,
      expected: String,
      expectedLen: Int
  ): Int = {
    var i = 0
    while (i < expectedLen) {
      if (chars(index + i) != expected.charAt(i))
        throw HexFormat.invalidFormatException()
      i += 1
    }
    index + expectedLen
  }
}

object HexFormat {
  private[util] val LowerCaseDigits: Array[Char] =
    "0123456789abcdef".toCharArray()

  private[util] val UpperCaseDigits: Array[Char] =
    "0123456789ABCDEF".toCharArray()

  /* ASCII -> hex digit value, indexed by char code.
   * Returns 0..15 for '0'-'9', 'A'-'F', 'a'-'f' and -1 otherwise.
   * The table stops at 'f' (0x66, the highest valid input); callers must
   * range-check the index before lookup. Built once at class init; the hot
   * path reads a plain `Array[Byte]` indexed by char code, identical to a
   * static literal. (A static `Array[Byte](-1, -1, ...)` literal would pull
   * in `scala.reflect.ClassTag` and is rejected by the javalib IR check.)
   */
  private val DigitValues: Array[Byte] = {
    val table = new Array[Byte]('f' + 1)
    var i = 0
    while (i < table.length) {
      table(i) = -1
      i += 1
    }
    i = 0
    while (i < 10) {        // '0'..'9' -> 0..9
      table('0' + i) = i.toByte
      i += 1
    }
    i = 0
    while (i < 6) {         // 'A'..'F' and 'a'..'f' -> 10..15
      val v = (10 + i).toByte
      table('A' + i) = v
      table('a' + i) = v
      i += 1
    }
    table
  }

  private val Default = new HexFormat("", "", "", false)

  def of(): HexFormat = Default

  def ofDelimiter(delimiter: String): HexFormat = {
    Objects.requireNonNull(delimiter, "delimiter")
    new HexFormat(delimiter, "", "", false)
  }

  def isHexDigit(ch: Int): Boolean =
    ch >= 0 && ch < DigitValues.length && DigitValues(ch) >= 0

  def fromHexDigit(ch: Int): Int = {
    if (ch >= 0 && ch < DigitValues.length) {
      val value = DigitValues(ch).toInt
      if (value >= 0) return value
    }

    val value =
      if (ch >= 0 && ch <= Character.MAX_CODE_POINT)
        new String(Character.toChars(ch))
      else Integer.toString(ch)
    throw new NumberFormatException(
      "not a hexadecimal digit: \"" + value + "\" = " + ch
    )
  }

  def fromHexDigits(string: jl.CharSequence): Int =
    fromHexDigits(string, 0, string.length())

  def fromHexDigits(
      string: jl.CharSequence,
      fromIndex: Int,
      toIndex: Int
  ): Int = {
    checkFromToIndex(fromIndex, toIndex, string.length())
    val length = toIndex - fromIndex
    if (length > 8)
      throw new IllegalArgumentException(
        "string length greater than 8: " + length
      )

    val table = DigitValues
    val tableLen = table.length
    var value = 0
    var i = fromIndex
    while (i < toIndex) {
      val ch = string.charAt(i)
      val v = if (ch < tableLen) table(ch).toInt else -1
      if (v < 0) fromHexDigit(ch.toInt)
      value = (value << 4) | v
      i += 1
    }
    value
  }

  def fromHexDigitsToLong(string: jl.CharSequence): Long =
    fromHexDigitsToLong(string, 0, string.length())

  def fromHexDigitsToLong(
      string: jl.CharSequence,
      fromIndex: Int,
      toIndex: Int
  ): Long = {
    checkFromToIndex(fromIndex, toIndex, string.length())
    val length = toIndex - fromIndex
    if (length > 16)
      throw new IllegalArgumentException(
        "string length greater than 16: " + length
      )

    val table = DigitValues
    val tableLen = table.length
    var value = 0L
    var i = fromIndex
    while (i < toIndex) {
      val ch = string.charAt(i)
      val v = if (ch < tableLen) table(ch).toInt else -1
      if (v < 0) fromHexDigit(ch.toInt)
      value = (value << 4) | v.toLong
      i += 1
    }
    value
  }

  // JDK 9 java.util.Objects.checkFromToIndex equivalent.
  // Inlined here because scala-js's java.util.Objects targets JDK 8.
  private[util] def checkFromToIndex(
      fromIndex: Int,
      toIndex: Int,
      length: Int
  ): Int = {
    if (fromIndex < 0 || fromIndex > toIndex || toIndex > length)
      throw new IndexOutOfBoundsException(
        "Range [" + fromIndex + ", " + toIndex + ") out of bounds for length " + length
      )
    fromIndex
  }

  private[util] def throwBadDigitPair(
      high: Char,
      low: Char,
      hi: Int
  ): Nothing = {
    if (hi < 0) {
      fromHexDigit(high.toInt)
    } else {
      fromHexDigit(low.toInt)
    }
    throw new AssertionError("unreachable")
  }

  private[util] def invalidFormatException(): IllegalArgumentException =
    new IllegalArgumentException(
      "extra or missing delimiters or values consisting of prefix, " +
        "two hexadecimal digits, and suffix"
    )
}
