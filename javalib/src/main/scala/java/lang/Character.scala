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

/* This file contains automatically generated snippets.
 * To regenerate them, run the sbt task `javalibInternal/regenerateUnicodeData`,
 * whose implementation is in `project/UnicodeDataGen.scala`.
 */

import scala.annotation.{tailrec, switch}

import scala.scalajs.js
import scala.scalajs.LinkingInfo
import scala.scalajs.LinkingInfo.ESVersion

import java.lang.constant.Constable
import java.util.{ArrayList, Arrays, HashMap}

/* This is a hijacked class. Its instances are primitive chars.
 *
 * In fact, "primitive" is only true at the IR level. In JS, there is no such
 * thing as a primitive character. Turning IR chars into valid JS is the
 * responsibility of the Emitter.
 *
 * Constructors are not emitted.
 */
class Character private ()
    extends AnyRef with java.io.Serializable with Comparable[Character] with Constable {

  def this(value: scala.Char) = this()

  @inline def charValue(): scala.Char =
    this.asInstanceOf[scala.Char]

  @inline override def hashCode(): Int =
    Character.hashCode(charValue())

  @inline override def equals(that: Any): scala.Boolean = {
    that.isInstanceOf[Character] &&
    (charValue() == that.asInstanceOf[Character].charValue())
  }

  @inline override def toString(): String =
    Character.toString(charValue())

  @inline override def compareTo(that: Character): Int =
    Character.compare(charValue(), that.charValue())
}

object Character {
  /* TYPE should be a `final val`, but that crashes the JVM back-end, so we
   * use a 'def' instead, which is binary compatible.
   */
  def TYPE: Class[_] = scala.Predef.classOf[scala.Char]

  final val MIN_VALUE = '\u0000'
  final val MAX_VALUE = '\uffff'
  final val SIZE = 16
  final val BYTES = 2

  @inline def `new`(value: scala.Char): Character = valueOf(value)

  @inline def valueOf(c: scala.Char): Character = c.asInstanceOf[Character]

  /* These are supposed to be final vals of type Byte, but that's not possible.
   * So we implement them as def's, which are binary compatible with final vals.
   */
  def UNASSIGNED: scala.Byte = UnicodeData.UNASSIGNED.toByte
  def UPPERCASE_LETTER: scala.Byte = UnicodeData.UPPERCASE_LETTER.toByte
  def LOWERCASE_LETTER: scala.Byte = UnicodeData.LOWERCASE_LETTER.toByte
  def TITLECASE_LETTER: scala.Byte = UnicodeData.TITLECASE_LETTER.toByte
  def MODIFIER_LETTER: scala.Byte = UnicodeData.MODIFIER_LETTER.toByte
  def OTHER_LETTER: scala.Byte = UnicodeData.OTHER_LETTER.toByte
  def NON_SPACING_MARK: scala.Byte = UnicodeData.NON_SPACING_MARK.toByte
  def ENCLOSING_MARK: scala.Byte = UnicodeData.ENCLOSING_MARK.toByte
  def COMBINING_SPACING_MARK: scala.Byte = UnicodeData.COMBINING_SPACING_MARK.toByte
  def DECIMAL_DIGIT_NUMBER: scala.Byte = UnicodeData.DECIMAL_DIGIT_NUMBER.toByte
  def LETTER_NUMBER: scala.Byte = UnicodeData.LETTER_NUMBER.toByte
  def OTHER_NUMBER: scala.Byte = UnicodeData.OTHER_NUMBER.toByte
  def SPACE_SEPARATOR: scala.Byte = UnicodeData.SPACE_SEPARATOR.toByte
  def LINE_SEPARATOR: scala.Byte = UnicodeData.LINE_SEPARATOR.toByte
  def PARAGRAPH_SEPARATOR: scala.Byte = UnicodeData.PARAGRAPH_SEPARATOR.toByte
  def CONTROL: scala.Byte = UnicodeData.CONTROL.toByte
  def FORMAT: scala.Byte = UnicodeData.FORMAT.toByte
  def PRIVATE_USE: scala.Byte = UnicodeData.PRIVATE_USE.toByte
  def SURROGATE: scala.Byte = UnicodeData.SURROGATE.toByte
  def DASH_PUNCTUATION: scala.Byte = UnicodeData.DASH_PUNCTUATION.toByte
  def START_PUNCTUATION: scala.Byte = UnicodeData.START_PUNCTUATION.toByte
  def END_PUNCTUATION: scala.Byte = UnicodeData.END_PUNCTUATION.toByte
  def CONNECTOR_PUNCTUATION: scala.Byte = UnicodeData.CONNECTOR_PUNCTUATION.toByte
  def OTHER_PUNCTUATION: scala.Byte = UnicodeData.OTHER_PUNCTUATION.toByte
  def MATH_SYMBOL: scala.Byte = UnicodeData.MATH_SYMBOL.toByte
  def CURRENCY_SYMBOL: scala.Byte = UnicodeData.CURRENCY_SYMBOL.toByte
  def MODIFIER_SYMBOL: scala.Byte = UnicodeData.MODIFIER_SYMBOL.toByte
  def OTHER_SYMBOL: scala.Byte = UnicodeData.OTHER_SYMBOL.toByte
  def INITIAL_QUOTE_PUNCTUATION: scala.Byte = UnicodeData.INITIAL_QUOTE_PUNCTUATION.toByte
  def FINAL_QUOTE_PUNCTUATION: scala.Byte = UnicodeData.FINAL_QUOTE_PUNCTUATION.toByte

  final val MIN_RADIX = 2
  final val MAX_RADIX = 36

  final val MIN_HIGH_SURROGATE = '\uD800'
  final val MAX_HIGH_SURROGATE = '\uDBFF'
  final val MIN_LOW_SURROGATE = '\uDC00'
  final val MAX_LOW_SURROGATE = '\uDFFF'
  final val MIN_SURROGATE = MIN_HIGH_SURROGATE
  final val MAX_SURROGATE = MAX_LOW_SURROGATE

  final val MIN_CODE_POINT = 0
  final val MAX_CODE_POINT = 0x10ffff
  final val MIN_SUPPLEMENTARY_CODE_POINT = 0x10000

  // Hash code and toString ---------------------------------------------------

  @inline def hashCode(value: Char): Int = value.toInt

  @inline def toString(c: Char): String =
    "" + c

  // Wasm intrinsic
  def toString(codePoint: Int): String = {
    if (!isValidCodePoint(codePoint))
      throw new IllegalArgumentException()

    if (LinkingInfo.esVersion >= ESVersion.ES2015) {
      js.Dynamic.global.String.fromCodePoint(codePoint).asInstanceOf[String]
    } else {
      if (codePoint < MIN_SUPPLEMENTARY_CODE_POINT) {
        js.Dynamic.global.String
          .fromCharCode(codePoint)
          .asInstanceOf[String]
      } else {
        js.Dynamic.global.String
          .fromCharCode(highSurrogate(codePoint).toInt, lowSurrogate(codePoint).toInt)
          .asInstanceOf[String]
      }
    }
  }

  // Low-level code point and code unit manipulations -------------------------

  // scalafmt: { align.tokens."+" = [{ code = "=" }, { code = "//" }] }
  private final val HighSurrogateMask       = 0xfc00 // 111111 00  00000000
  private final val HighSurrogateID         = 0xd800 // 110110 00  00000000
  private final val LowSurrogateMask        = 0xfc00 // 111111 00  00000000
  private final val LowSurrogateID          = 0xdc00 // 110111 00  00000000
  private final val SurrogateMask           = 0xf800 // 111110 00  00000000
  private final val SurrogateID             = 0xd800 // 110110 00  00000000
  private final val SurrogateUsefulPartMask = 0x03ff // 000000 11  11111111
  // scalafmt: {}

  private final val SurrogatePairMask = (HighSurrogateMask << 16) | LowSurrogateMask
  private final val SurrogatePairID = (HighSurrogateID << 16) | LowSurrogateID

  private final val HighSurrogateShift = 10
  private final val HighSurrogateAddValue = 0x10000 >> HighSurrogateShift

  @inline def isValidCodePoint(codePoint: Int): scala.Boolean =
    (codePoint >= 0) && (codePoint <= MAX_CODE_POINT)

  @inline def isBmpCodePoint(codePoint: Int): scala.Boolean =
    (codePoint >= 0) && (codePoint < MIN_SUPPLEMENTARY_CODE_POINT)

  @inline def isSupplementaryCodePoint(codePoint: Int): scala.Boolean =
    (codePoint >= MIN_SUPPLEMENTARY_CODE_POINT) && (codePoint <= MAX_CODE_POINT)

  @inline def isHighSurrogate(ch: Char): scala.Boolean =
    (ch & HighSurrogateMask) == HighSurrogateID

  @inline def isLowSurrogate(ch: Char): scala.Boolean =
    (ch & LowSurrogateMask) == LowSurrogateID

  @inline def isSurrogate(ch: Char): scala.Boolean =
    (ch & SurrogateMask) == SurrogateID

  @inline def isSurrogatePair(high: Char, low: Char): scala.Boolean =
    (((high << 16) | low) & SurrogatePairMask) == SurrogatePairID

  @inline def charCount(codePoint: Int): Int =
    if (codePoint >= MIN_SUPPLEMENTARY_CODE_POINT) 2 else 1

  @inline def toCodePoint(high: Char, low: Char): Int = {
    (((high & SurrogateUsefulPartMask) + HighSurrogateAddValue) << HighSurrogateShift) |
    (low & SurrogateUsefulPartMask)
  }

  @inline def highSurrogate(codePoint: Int): Char =
    (HighSurrogateID | ((codePoint >> HighSurrogateShift) - HighSurrogateAddValue)).toChar

  @inline def lowSurrogate(codePoint: Int): Char =
    (LowSurrogateID | (codePoint & SurrogateUsefulPartMask)).toChar

  // Radix info

  /** Tests whether `radix` is invalid, i.e., whether
   *  `MIN_RADIX <= radix <= MAX_RADIX` does *not* hold.
   */
  @inline
  private[java] def isRadixInvalid(radix: Int): scala.Boolean =
    radix < MIN_RADIX || radix > MAX_RADIX

  // Code point manipulation in character sequences ---------------------------

  @noinline
  def codePointAt(seq: CharSequence, index: Int): Int =
    codePointAtImpl(seq, index)

  @noinline
  def codePointAt(a: Array[Char], index: Int): Int =
    codePointAtImpl(CharSequence.ofArray(a), index)

  @noinline
  def codePointAt(a: Array[Char], index: Int, limit: Int): Int = {
    // implicit null check and bounds check
    if (!(limit <= a.length && 0 <= index && index < limit))
      throw new IndexOutOfBoundsException()

    if (index == limit - 1)
      a(index).toInt // the only case where `limit` makes a difference
    else
      codePointAt(a, index)
  }

  @inline
  private[lang] def codePointAtImpl(seq: CharSequence, index: Int): Int = {
    val high = seq.charAt(index) // implicit null check and bounds check
    if (isHighSurrogate(high) && (index + 1 < seq.length())) {
      val low = seq.charAt(index + 1)
      if (isLowSurrogate(low))
        toCodePoint(high, low)
      else
        high.toInt
    } else {
      high.toInt
    }
  }

  @noinline
  def codePointBefore(seq: CharSequence, index: Int): Int =
    codePointBeforeImpl(seq, index)

  @noinline
  def codePointBefore(a: Array[Char], index: Int): Int =
    codePointBeforeImpl(CharSequence.ofArray(a), index)

  @noinline
  def codePointBefore(a: Array[Char], index: Int, start: Int): Int = {
    // implicit null check and bounds check
    if (!(index <= a.length && 0 <= start && start < index))
      throw new IndexOutOfBoundsException()

    if (index == start + 1)
      a(start).toInt // the only case where `start` makes a difference
    else
      codePointBefore(a, index)
  }

  @inline
  private[lang] def codePointBeforeImpl(seq: CharSequence, index: Int): Int = {
    val low = seq.charAt(index - 1) // implicit null check and bounds check
    if (isLowSurrogate(low) && index > 1) {
      val high = seq.charAt(index - 2)
      if (isHighSurrogate(high))
        toCodePoint(high, low)
      else
        low.toInt
    } else {
      low.toInt
    }
  }

  def toChars(codePoint: Int, dst: Array[Char], dstIndex: Int): Int = {
    if (isBmpCodePoint(codePoint)) {
      dst(dstIndex) = codePoint.toChar
      1
    } else if (isValidCodePoint(codePoint)) {
      dst(dstIndex) = highSurrogate(codePoint)
      dst(dstIndex + 1) = lowSurrogate(codePoint)
      2
    } else {
      throw new IllegalArgumentException()
    }
  }

  def toChars(codePoint: Int): Array[Char] = {
    if (isBmpCodePoint(codePoint))
      Array(codePoint.toChar)
    else if (isValidCodePoint(codePoint))
      Array(highSurrogate(codePoint), lowSurrogate(codePoint))
    else
      throw new IllegalArgumentException()
  }

  @noinline
  def codePointCount(seq: CharSequence, beginIndex: Int, endIndex: Int): Int =
    codePointCountImpl(seq, beginIndex, endIndex)

  @noinline
  def codePointCount(a: Array[Char], offset: Int, count: Int): Int =
    codePointCountImpl(CharSequence.ofArray(a), offset, offset + count)

  @inline
  private[lang] def codePointCountImpl(seq: CharSequence, beginIndex: Int, endIndex: Int): Int = {
    // Bounds check (and implicit null check)
    if (endIndex > seq.length() || beginIndex < 0 || endIndex < beginIndex)
      throw new IndexOutOfBoundsException()

    var res = endIndex - beginIndex
    var i = beginIndex
    val end = endIndex - 1
    while (i < end) {
      if (isHighSurrogate(seq.charAt(i)) && isLowSurrogate(seq.charAt(i + 1)))
        res -= 1
      i += 1
    }
    res
  }

  @noinline
  def offsetByCodePoints(seq: CharSequence, index: Int, codePointOffset: Int): Int =
    offsetByCodePointsImpl(seq, index, codePointOffset)

  @noinline
  def offsetByCodePoints(a: Array[Char], start: Int, count: Int, index: Int,
      codePointOffset: Int): Int = {

    val len = a.length // implicit null check

    // Bounds check
    val limit = start + count
    if (start < 0 || count < 0 || limit > len || index < start || index > limit)
      throw new IndexOutOfBoundsException()

    offsetByCodePointsInternal(CharSequence.ofArray(a), start, limit, index, codePointOffset)
  }

  @inline
  private[lang] def offsetByCodePointsImpl(seq: CharSequence, index: Int,
      codePointOffset: Int): Int = {
    val len = seq.length() // implicit null check

    // Bounds check
    if (index < 0 || index > len)
      throw new IndexOutOfBoundsException()

    offsetByCodePointsInternal(seq, start = 0, limit = len, index, codePointOffset)
  }

  @inline
  private[lang] def offsetByCodePointsInternal(seq: CharSequence, start: Int,
      limit: Int, index: Int, codePointOffset: Int): Int = {

    if (codePointOffset >= 0) {
      var i = 0
      var result = index
      while (i != codePointOffset) {
        if (result >= limit)
          throw new IndexOutOfBoundsException()
        if ((result < limit - 1) &&
            isHighSurrogate(seq.charAt(result)) &&
            isLowSurrogate(seq.charAt(result + 1))) {
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
        if (result <= start)
          throw new IndexOutOfBoundsException()
        if ((result > start + 1) &&
            isLowSurrogate(seq.charAt(result - 1)) &&
            isHighSurrogate(seq.charAt(result - 2))) {
          result -= 2
        } else {
          result -= 1
        }
        i -= 1
      }
      result
    }
  }

  // Unicode Character Database-related functions -----------------------------

  @inline def getType(ch: scala.Char): Int =
    getType(ch.toInt)

  @inline def getType(codePoint: Int): Int =
    UnicodeData.getType(codePoint)

  @inline
  def digit(ch: scala.Char, radix: Int): Int =
    digit(ch.toInt, radix)

  @inline // because radix is probably constant at call site
  def digit(codePoint: Int, radix: Int): Int = {
    if (isRadixInvalid(radix))
      -1
    else
      digitWithValidRadix(codePoint, radix)
  }

  private[lang] def digitWithValidRadix(codePoint: Int, radix: Int): Int = {
    val value = if (codePoint < 256) {
      // Fast-path for the ASCII repertoire
      if (codePoint >= '0' && codePoint <= '9')
        codePoint - '0'
      else if (codePoint >= 'A' && codePoint <= 'Z')
        codePoint - ('A' - 10)
      else if (codePoint >= 'a' && codePoint <= 'z')
        codePoint - ('a' - 10)
      else
        -1
    } else {
      if (codePoint >= 0xff21 && codePoint <= 0xff3a) {
        // Fullwidth uppercase Latin letter
        codePoint - (0xff21 - 10)
      } else if (codePoint >= 0xff41 && codePoint <= 0xff5a) {
        // Fullwidth lowercase Latin letter
        codePoint - (0xff41 - 10)
      } else {
        // Maybe it is a digit in a non-ASCII script

        // Find the position of the 0 digit corresponding to this code point
        val p = Arrays.binarySearch(nonASCIIZeroDigitCodePoints, codePoint)
        val zeroCodePointIndex = if (p < 0) -2 - p else p

        /* If the index is below 0, it cannot be a digit. Otherwise, the value
         * is the difference between the given codePoint and the code point of
         * its corresponding 0. We must ensure that it is not bigger than 9.
         */
        if (zeroCodePointIndex < 0) {
          -1
        } else {
          val v = codePoint - nonASCIIZeroDigitCodePoints(zeroCodePointIndex)
          if (v > 9) -1 else v
        }
      }
    }

    if (value < radix) value
    else -1
  }

  private[lang] def isZeroDigit(ch: Char): scala.Boolean =
    if (ch < 256) ch == '0'
    else Arrays.binarySearch(nonASCIIZeroDigitCodePoints, ch.toInt) >= 0

  // ported from https://github.com/gwtproject/gwt/blob/master/user/super/com/google/gwt/emul/java/lang/Character.java
  def forDigit(digit: Int, radix: Int): Char = {
    if (isRadixInvalid(radix) || Integer.unsigned_>=(digit, radix)) {
      0
    } else {
      val overBaseTen = digit - 10
      val result = if (overBaseTen < 0) '0' + digit else 'a' + overBaseTen
      result.toChar
    }
  }

  @inline def isISOControl(ch: scala.Char): scala.Boolean =
    isISOControl(ch.toInt)

  @inline def isISOControl(codePoint: Int): scala.Boolean = {
    // By definition: only two range checks, which is better than using any table
    (0x00 <= codePoint && codePoint <= 0x1f) || (0x7f <= codePoint && codePoint <= 0x9f)
  }

  @deprecated("Replaced by isWhitespace(char)", "")
  @inline def isSpace(c: scala.Char): scala.Boolean = {
    /* By definition: '\t' '\n' '\f' '\r' or ' '
     * We can test those with one range check and one equality test.
     */
    if (c >= '\t' && c <= '\r') c != '\u000b' else c == ' '
  }

  @inline def isWhitespace(c: scala.Char): scala.Boolean =
    isWhitespace(c.toInt)

  @inline def isWhitespace(codePoint: scala.Int): scala.Boolean =
    UnicodeData.isWhitespace(codePoint)

  @inline def isSpaceChar(ch: scala.Char): scala.Boolean =
    isSpaceChar(ch.toInt)

  @inline def isSpaceChar(codePoint: Int): scala.Boolean =
    UnicodeData.isSpaceChar(codePoint)

  @inline def isLowerCase(ch: scala.Char): scala.Boolean =
    isLowerCase(ch.toInt)

  @inline def isLowerCase(codePoint: Int): scala.Boolean =
    UnicodeData.isLowerCase(codePoint)

  @inline def isUpperCase(ch: scala.Char): scala.Boolean =
    isUpperCase(ch.toInt)

  @inline def isUpperCase(codePoint: Int): scala.Boolean =
    UnicodeData.isUpperCase(codePoint)

  @inline def isTitleCase(ch: scala.Char): scala.Boolean =
    isTitleCase(ch.toInt)

  @inline def isTitleCase(codePoint: Int): scala.Boolean =
    UnicodeData.isTitleCase(codePoint)

  @inline def isDigit(ch: scala.Char): scala.Boolean =
    isDigit(ch.toInt)

  @inline def isDigit(codePoint: Int): scala.Boolean =
    UnicodeData.isDigit(codePoint)

  @inline def isDefined(ch: scala.Char): scala.Boolean =
    isDefined(ch.toInt)

  @inline def isDefined(codePoint: scala.Int): scala.Boolean =
    UnicodeData.isDefined(codePoint)

  @inline def isLetter(ch: scala.Char): scala.Boolean =
    isLetter(ch.toInt)

  @inline def isLetter(codePoint: Int): scala.Boolean =
    UnicodeData.isLetter(codePoint)

  @inline def isLetterOrDigit(ch: scala.Char): scala.Boolean =
    isLetterOrDigit(ch.toInt)

  @inline def isLetterOrDigit(codePoint: Int): scala.Boolean =
    UnicodeData.isLetterOrDigit(codePoint)

  @deprecated("Replaced by isJavaIdentifierStart(char)", "")
  @inline def isJavaLetter(ch: scala.Char): scala.Boolean =
    isJavaIdentifierStart(ch)

  @deprecated("Replaced by isJavaIdentifierPart(char)", "")
  @inline def isJavaLetterOrDigit(ch: scala.Char): scala.Boolean =
    isJavaIdentifierPart(ch)

  @inline def isAlphabetic(codePoint: Int): scala.Boolean =
    UnicodeData.isAlphabetic(codePoint)

  @inline def isIdeographic(codePoint: Int): scala.Boolean =
    UnicodeData.isIdeographic(codePoint)

  @inline def isJavaIdentifierStart(ch: scala.Char): scala.Boolean =
    isJavaIdentifierStart(ch.toInt)

  @inline def isJavaIdentifierStart(codePoint: Int): scala.Boolean =
    UnicodeData.isJavaIdentifierStart(codePoint)

  @inline def isJavaIdentifierPart(ch: scala.Char): scala.Boolean =
    isJavaIdentifierPart(ch.toInt)

  @inline def isJavaIdentifierPart(codePoint: Int): scala.Boolean =
    UnicodeData.isJavaIdentifierPart(codePoint)

  @inline def isUnicodeIdentifierStart(ch: scala.Char): scala.Boolean =
    isUnicodeIdentifierStart(ch.toInt)

  @inline def isUnicodeIdentifierStart(codePoint: Int): scala.Boolean =
    UnicodeData.isUnicodeIdentifierStart(codePoint)

  @inline def isUnicodeIdentifierPart(ch: scala.Char): scala.Boolean =
    isUnicodeIdentifierPart(ch.toInt)

  @inline def isUnicodeIdentifierPart(codePoint: Int): scala.Boolean =
    UnicodeData.isUnicodeIdentifierPart(codePoint)

  @inline def isIdentifierIgnorable(ch: scala.Char): scala.Boolean =
    isIdentifierIgnorable(ch.toInt)

  @inline def isIdentifierIgnorable(codePoint: Int): scala.Boolean =
    UnicodeData.isIdentifierIgnorable(codePoint)

  @inline def isMirrored(ch: scala.Char): scala.Boolean =
    isMirrored(ch.toInt)

  @inline def isMirrored(codePoint: Int): scala.Boolean =
    UnicodeData.isMirrored(codePoint)

  // def getDirectionality(c: scala.Char): scala.Byte

  /* Conversions */
  def toUpperCase(ch: Char): Char = toUpperCase(ch.toInt).toChar

  def toUpperCase(codePoint: scala.Int): scala.Int = {
    codePoint match {
      case 0x1fb3 | 0x1fc3 | 0x1ff3 =>
        (codePoint + 0x0009)
      case _ if codePoint >= 0x1f80 && codePoint <= 0x1faf =>
        (codePoint | 0x0008)
      case _ =>
        val upperChars = toString(codePoint).toUpperCase()
        upperChars.length match {
          case 1 =>
            upperChars.charAt(0).toInt
          case 2 =>
            val high = upperChars.charAt(0)
            val low = upperChars.charAt(1)
            if (isSurrogatePair(high, low))
              toCodePoint(high, low)
            else
              codePoint
          case _ =>
            codePoint
        }
    }
  }

  def toLowerCase(ch: scala.Char): scala.Char = toLowerCase(ch.toInt).toChar

  def toLowerCase(codePoint: scala.Int): scala.Int = {
    codePoint match {
      case 0x0130 =>
        0x0069 // İ => i
      case _ =>
        val lowerChars = toString(codePoint).toLowerCase()
        lowerChars.length match {
          case 1 =>
            lowerChars.charAt(0).toInt
          case 2 =>
            val high = lowerChars.charAt(0)
            val low = lowerChars.charAt(1)
            if (isSurrogatePair(high, low))
              toCodePoint(high, low)
            else
              codePoint
          case _ =>
            codePoint
        }
    }
  }

  def toTitleCase(ch: scala.Char): scala.Char = toTitleCase(ch.toInt).toChar

  def toTitleCase(codePoint: scala.Int): scala.Int = {
    (codePoint: @switch) match {
      // BEGIN GENERATED: [titlecase-mappings]
      case 0x01c4 => 0x01c5
      case 0x01c5 => 0x01c5
      case 0x01c6 => 0x01c5
      case 0x01c7 => 0x01c8
      case 0x01c8 => 0x01c8
      case 0x01c9 => 0x01c8
      case 0x01ca => 0x01cb
      case 0x01cb => 0x01cb
      case 0x01cc => 0x01cb
      case 0x01f1 => 0x01f2
      case 0x01f2 => 0x01f2
      case 0x01f3 => 0x01f2
      case 0x10d0 => 0x10d0
      case 0x10d1 => 0x10d1
      case 0x10d2 => 0x10d2
      case 0x10d3 => 0x10d3
      case 0x10d4 => 0x10d4
      case 0x10d5 => 0x10d5
      case 0x10d6 => 0x10d6
      case 0x10d7 => 0x10d7
      case 0x10d8 => 0x10d8
      case 0x10d9 => 0x10d9
      case 0x10da => 0x10da
      case 0x10db => 0x10db
      case 0x10dc => 0x10dc
      case 0x10dd => 0x10dd
      case 0x10de => 0x10de
      case 0x10df => 0x10df
      case 0x10e0 => 0x10e0
      case 0x10e1 => 0x10e1
      case 0x10e2 => 0x10e2
      case 0x10e3 => 0x10e3
      case 0x10e4 => 0x10e4
      case 0x10e5 => 0x10e5
      case 0x10e6 => 0x10e6
      case 0x10e7 => 0x10e7
      case 0x10e8 => 0x10e8
      case 0x10e9 => 0x10e9
      case 0x10ea => 0x10ea
      case 0x10eb => 0x10eb
      case 0x10ec => 0x10ec
      case 0x10ed => 0x10ed
      case 0x10ee => 0x10ee
      case 0x10ef => 0x10ef
      case 0x10f0 => 0x10f0
      case 0x10f1 => 0x10f1
      case 0x10f2 => 0x10f2
      case 0x10f3 => 0x10f3
      case 0x10f4 => 0x10f4
      case 0x10f5 => 0x10f5
      case 0x10f6 => 0x10f6
      case 0x10f7 => 0x10f7
      case 0x10f8 => 0x10f8
      case 0x10f9 => 0x10f9
      case 0x10fa => 0x10fa
      case 0x10fd => 0x10fd
      case 0x10fe => 0x10fe
      case 0x10ff => 0x10ff

      // END GENERATED: [titlecase-mappings]
      case _ => toUpperCase(codePoint)
    }
  }

  // def getNumericValue(c: scala.Char): Int

  // Miscellaneous ------------------------------------------------------------

  @inline def compare(x: scala.Char, y: scala.Char): Int =
    x - y

  @inline def reverseBytes(ch: scala.Char): scala.Char =
    ((ch >> 8) | (ch << 8)).toChar

  // UnicodeBlock

  class Subset protected (name: String) {
    override final def equals(that: Any): scala.Boolean = super.equals(that)
    override final def hashCode(): scala.Int = super.hashCode
    override final def toString(): String = name
  }

  final class UnicodeBlock private (name: String,
      private val start: Int, private val end: Int)
      extends Subset(name)

  object UnicodeBlock {
    // BEGIN GENERATED: [unicode-block-constants]
    private final val BlockCount = 327
    // END GENERATED: [unicode-block-constants]

    private[this] val allBlocks: ArrayList[UnicodeBlock] = new ArrayList[UnicodeBlock](BlockCount)
    private[this] val blocksByNormalizedName = new HashMap[String, UnicodeBlock]()

    private[this] def addNameAliases(properName: String, block: UnicodeBlock): Unit = {
      // Add normalized aliases
      val lower = properName.toLowerCase
      //   lowercase + spaces
      blocksByNormalizedName.put(lower, block)
      //   lowercase + no spaces
      blocksByNormalizedName.put(lower.replace(" ", ""), block)
    }

    private[this] def addUnicodeBlock(properName: String, start: Int, end: Int): UnicodeBlock = {
      val jvmName = properName.toUpperCase()
        .replace(' ', '_')
        .replace('-', '_')

      val block = new UnicodeBlock(jvmName, start, end)
      allBlocks.add(block)
      addNameAliases(properName, block)
      blocksByNormalizedName.put(jvmName.toLowerCase(), block)

      block
    }

    private[this] def addUnicodeBlock(properName: String, historicalName: String,
        start: Int, end: Int): UnicodeBlock = {
      val jvmName = historicalName.toUpperCase()
        .replace(' ', '_')
        .replace('-', '_')

      val block = new UnicodeBlock(jvmName, start, end)
      allBlocks.add(block)
      addNameAliases(properName, block)
      addNameAliases(historicalName, block)
      blocksByNormalizedName.put(jvmName.toLowerCase(), block)

      block
    }

    // Special JVM Constant, don't add to allBlocks
    val SURROGATES_AREA = new UnicodeBlock("SURROGATES_AREA", 0x0, 0x0)
    blocksByNormalizedName.put("surrogates_area", SURROGATES_AREA)

    // scalafmt: { maxColumn = 1000 }

    // BEGIN GENERATED: [unicode-blocks]
    val BASIC_LATIN = addUnicodeBlock("Basic Latin", 0x0000, 0x007f)
    val LATIN_1_SUPPLEMENT = addUnicodeBlock("Latin-1 Supplement", 0x0080, 0x00ff)
    val LATIN_EXTENDED_A = addUnicodeBlock("Latin Extended-A", 0x0100, 0x017f)
    val LATIN_EXTENDED_B = addUnicodeBlock("Latin Extended-B", 0x0180, 0x024f)
    val IPA_EXTENSIONS = addUnicodeBlock("IPA Extensions", 0x0250, 0x02af)
    val SPACING_MODIFIER_LETTERS = addUnicodeBlock("Spacing Modifier Letters", 0x02b0, 0x02ff)
    val COMBINING_DIACRITICAL_MARKS = addUnicodeBlock("Combining Diacritical Marks", 0x0300, 0x036f)
    val GREEK = addUnicodeBlock("Greek and Coptic", "Greek", 0x0370, 0x03ff)
    val CYRILLIC = addUnicodeBlock("Cyrillic", 0x0400, 0x04ff)
    val CYRILLIC_SUPPLEMENTARY = addUnicodeBlock("Cyrillic Supplement", "Cyrillic Supplementary", 0x0500, 0x052f)
    val ARMENIAN = addUnicodeBlock("Armenian", 0x0530, 0x058f)
    val HEBREW = addUnicodeBlock("Hebrew", 0x0590, 0x05ff)
    val ARABIC = addUnicodeBlock("Arabic", 0x0600, 0x06ff)
    val SYRIAC = addUnicodeBlock("Syriac", 0x0700, 0x074f)
    val ARABIC_SUPPLEMENT = addUnicodeBlock("Arabic Supplement", 0x0750, 0x077f)
    val THAANA = addUnicodeBlock("Thaana", 0x0780, 0x07bf)
    val NKO = addUnicodeBlock("NKo", 0x07c0, 0x07ff)
    val SAMARITAN = addUnicodeBlock("Samaritan", 0x0800, 0x083f)
    val MANDAIC = addUnicodeBlock("Mandaic", 0x0840, 0x085f)
    val SYRIAC_SUPPLEMENT = addUnicodeBlock("Syriac Supplement", 0x0860, 0x086f)
    val ARABIC_EXTENDED_B = addUnicodeBlock("Arabic Extended-B", 0x0870, 0x089f)
    val ARABIC_EXTENDED_A = addUnicodeBlock("Arabic Extended-A", 0x08a0, 0x08ff)
    val DEVANAGARI = addUnicodeBlock("Devanagari", 0x0900, 0x097f)
    val BENGALI = addUnicodeBlock("Bengali", 0x0980, 0x09ff)
    val GURMUKHI = addUnicodeBlock("Gurmukhi", 0x0a00, 0x0a7f)
    val GUJARATI = addUnicodeBlock("Gujarati", 0x0a80, 0x0aff)
    val ORIYA = addUnicodeBlock("Oriya", 0x0b00, 0x0b7f)
    val TAMIL = addUnicodeBlock("Tamil", 0x0b80, 0x0bff)
    val TELUGU = addUnicodeBlock("Telugu", 0x0c00, 0x0c7f)
    val KANNADA = addUnicodeBlock("Kannada", 0x0c80, 0x0cff)
    val MALAYALAM = addUnicodeBlock("Malayalam", 0x0d00, 0x0d7f)
    val SINHALA = addUnicodeBlock("Sinhala", 0x0d80, 0x0dff)
    val THAI = addUnicodeBlock("Thai", 0x0e00, 0x0e7f)
    val LAO = addUnicodeBlock("Lao", 0x0e80, 0x0eff)
    val TIBETAN = addUnicodeBlock("Tibetan", 0x0f00, 0x0fff)
    val MYANMAR = addUnicodeBlock("Myanmar", 0x1000, 0x109f)
    val GEORGIAN = addUnicodeBlock("Georgian", 0x10a0, 0x10ff)
    val HANGUL_JAMO = addUnicodeBlock("Hangul Jamo", 0x1100, 0x11ff)
    val ETHIOPIC = addUnicodeBlock("Ethiopic", 0x1200, 0x137f)
    val ETHIOPIC_SUPPLEMENT = addUnicodeBlock("Ethiopic Supplement", 0x1380, 0x139f)
    val CHEROKEE = addUnicodeBlock("Cherokee", 0x13a0, 0x13ff)
    val UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS = addUnicodeBlock("Unified Canadian Aboriginal Syllabics", 0x1400, 0x167f)
    val OGHAM = addUnicodeBlock("Ogham", 0x1680, 0x169f)
    val RUNIC = addUnicodeBlock("Runic", 0x16a0, 0x16ff)
    val TAGALOG = addUnicodeBlock("Tagalog", 0x1700, 0x171f)
    val HANUNOO = addUnicodeBlock("Hanunoo", 0x1720, 0x173f)
    val BUHID = addUnicodeBlock("Buhid", 0x1740, 0x175f)
    val TAGBANWA = addUnicodeBlock("Tagbanwa", 0x1760, 0x177f)
    val KHMER = addUnicodeBlock("Khmer", 0x1780, 0x17ff)
    val MONGOLIAN = addUnicodeBlock("Mongolian", 0x1800, 0x18af)
    val UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS_EXTENDED = addUnicodeBlock("Unified Canadian Aboriginal Syllabics Extended", 0x18b0, 0x18ff)
    val LIMBU = addUnicodeBlock("Limbu", 0x1900, 0x194f)
    val TAI_LE = addUnicodeBlock("Tai Le", 0x1950, 0x197f)
    val NEW_TAI_LUE = addUnicodeBlock("New Tai Lue", 0x1980, 0x19df)
    val KHMER_SYMBOLS = addUnicodeBlock("Khmer Symbols", 0x19e0, 0x19ff)
    val BUGINESE = addUnicodeBlock("Buginese", 0x1a00, 0x1a1f)
    val TAI_THAM = addUnicodeBlock("Tai Tham", 0x1a20, 0x1aaf)
    val COMBINING_DIACRITICAL_MARKS_EXTENDED = addUnicodeBlock("Combining Diacritical Marks Extended", 0x1ab0, 0x1aff)
    val BALINESE = addUnicodeBlock("Balinese", 0x1b00, 0x1b7f)
    val SUNDANESE = addUnicodeBlock("Sundanese", 0x1b80, 0x1bbf)
    val BATAK = addUnicodeBlock("Batak", 0x1bc0, 0x1bff)
    val LEPCHA = addUnicodeBlock("Lepcha", 0x1c00, 0x1c4f)
    val OL_CHIKI = addUnicodeBlock("Ol Chiki", 0x1c50, 0x1c7f)
    val CYRILLIC_EXTENDED_C = addUnicodeBlock("Cyrillic Extended-C", 0x1c80, 0x1c8f)
    val GEORGIAN_EXTENDED = addUnicodeBlock("Georgian Extended", 0x1c90, 0x1cbf)
    val SUNDANESE_SUPPLEMENT = addUnicodeBlock("Sundanese Supplement", 0x1cc0, 0x1ccf)
    val VEDIC_EXTENSIONS = addUnicodeBlock("Vedic Extensions", 0x1cd0, 0x1cff)
    val PHONETIC_EXTENSIONS = addUnicodeBlock("Phonetic Extensions", 0x1d00, 0x1d7f)
    val PHONETIC_EXTENSIONS_SUPPLEMENT = addUnicodeBlock("Phonetic Extensions Supplement", 0x1d80, 0x1dbf)
    val COMBINING_DIACRITICAL_MARKS_SUPPLEMENT = addUnicodeBlock("Combining Diacritical Marks Supplement", 0x1dc0, 0x1dff)
    val LATIN_EXTENDED_ADDITIONAL = addUnicodeBlock("Latin Extended Additional", 0x1e00, 0x1eff)
    val GREEK_EXTENDED = addUnicodeBlock("Greek Extended", 0x1f00, 0x1fff)
    val GENERAL_PUNCTUATION = addUnicodeBlock("General Punctuation", 0x2000, 0x206f)
    val SUPERSCRIPTS_AND_SUBSCRIPTS = addUnicodeBlock("Superscripts and Subscripts", 0x2070, 0x209f)
    val CURRENCY_SYMBOLS = addUnicodeBlock("Currency Symbols", 0x20a0, 0x20cf)
    val COMBINING_MARKS_FOR_SYMBOLS = addUnicodeBlock("Combining Diacritical Marks for Symbols", "Combining Marks For Symbols", 0x20d0, 0x20ff)
    val LETTERLIKE_SYMBOLS = addUnicodeBlock("Letterlike Symbols", 0x2100, 0x214f)
    val NUMBER_FORMS = addUnicodeBlock("Number Forms", 0x2150, 0x218f)
    val ARROWS = addUnicodeBlock("Arrows", 0x2190, 0x21ff)
    val MATHEMATICAL_OPERATORS = addUnicodeBlock("Mathematical Operators", 0x2200, 0x22ff)
    val MISCELLANEOUS_TECHNICAL = addUnicodeBlock("Miscellaneous Technical", 0x2300, 0x23ff)
    val CONTROL_PICTURES = addUnicodeBlock("Control Pictures", 0x2400, 0x243f)
    val OPTICAL_CHARACTER_RECOGNITION = addUnicodeBlock("Optical Character Recognition", 0x2440, 0x245f)
    val ENCLOSED_ALPHANUMERICS = addUnicodeBlock("Enclosed Alphanumerics", 0x2460, 0x24ff)
    val BOX_DRAWING = addUnicodeBlock("Box Drawing", 0x2500, 0x257f)
    val BLOCK_ELEMENTS = addUnicodeBlock("Block Elements", 0x2580, 0x259f)
    val GEOMETRIC_SHAPES = addUnicodeBlock("Geometric Shapes", 0x25a0, 0x25ff)
    val MISCELLANEOUS_SYMBOLS = addUnicodeBlock("Miscellaneous Symbols", 0x2600, 0x26ff)
    val DINGBATS = addUnicodeBlock("Dingbats", 0x2700, 0x27bf)
    val MISCELLANEOUS_MATHEMATICAL_SYMBOLS_A = addUnicodeBlock("Miscellaneous Mathematical Symbols-A", 0x27c0, 0x27ef)
    val SUPPLEMENTAL_ARROWS_A = addUnicodeBlock("Supplemental Arrows-A", 0x27f0, 0x27ff)
    val BRAILLE_PATTERNS = addUnicodeBlock("Braille Patterns", 0x2800, 0x28ff)
    val SUPPLEMENTAL_ARROWS_B = addUnicodeBlock("Supplemental Arrows-B", 0x2900, 0x297f)
    val MISCELLANEOUS_MATHEMATICAL_SYMBOLS_B = addUnicodeBlock("Miscellaneous Mathematical Symbols-B", 0x2980, 0x29ff)
    val SUPPLEMENTAL_MATHEMATICAL_OPERATORS = addUnicodeBlock("Supplemental Mathematical Operators", 0x2a00, 0x2aff)
    val MISCELLANEOUS_SYMBOLS_AND_ARROWS = addUnicodeBlock("Miscellaneous Symbols and Arrows", 0x2b00, 0x2bff)
    val GLAGOLITIC = addUnicodeBlock("Glagolitic", 0x2c00, 0x2c5f)
    val LATIN_EXTENDED_C = addUnicodeBlock("Latin Extended-C", 0x2c60, 0x2c7f)
    val COPTIC = addUnicodeBlock("Coptic", 0x2c80, 0x2cff)
    val GEORGIAN_SUPPLEMENT = addUnicodeBlock("Georgian Supplement", 0x2d00, 0x2d2f)
    val TIFINAGH = addUnicodeBlock("Tifinagh", 0x2d30, 0x2d7f)
    val ETHIOPIC_EXTENDED = addUnicodeBlock("Ethiopic Extended", 0x2d80, 0x2ddf)
    val CYRILLIC_EXTENDED_A = addUnicodeBlock("Cyrillic Extended-A", 0x2de0, 0x2dff)
    val SUPPLEMENTAL_PUNCTUATION = addUnicodeBlock("Supplemental Punctuation", 0x2e00, 0x2e7f)
    val CJK_RADICALS_SUPPLEMENT = addUnicodeBlock("CJK Radicals Supplement", 0x2e80, 0x2eff)
    val KANGXI_RADICALS = addUnicodeBlock("Kangxi Radicals", 0x2f00, 0x2fdf)
    val IDEOGRAPHIC_DESCRIPTION_CHARACTERS = addUnicodeBlock("Ideographic Description Characters", 0x2ff0, 0x2fff)
    val CJK_SYMBOLS_AND_PUNCTUATION = addUnicodeBlock("CJK Symbols and Punctuation", 0x3000, 0x303f)
    val HIRAGANA = addUnicodeBlock("Hiragana", 0x3040, 0x309f)
    val KATAKANA = addUnicodeBlock("Katakana", 0x30a0, 0x30ff)
    val BOPOMOFO = addUnicodeBlock("Bopomofo", 0x3100, 0x312f)
    val HANGUL_COMPATIBILITY_JAMO = addUnicodeBlock("Hangul Compatibility Jamo", 0x3130, 0x318f)
    val KANBUN = addUnicodeBlock("Kanbun", 0x3190, 0x319f)
    val BOPOMOFO_EXTENDED = addUnicodeBlock("Bopomofo Extended", 0x31a0, 0x31bf)
    val CJK_STROKES = addUnicodeBlock("CJK Strokes", 0x31c0, 0x31ef)
    val KATAKANA_PHONETIC_EXTENSIONS = addUnicodeBlock("Katakana Phonetic Extensions", 0x31f0, 0x31ff)
    val ENCLOSED_CJK_LETTERS_AND_MONTHS = addUnicodeBlock("Enclosed CJK Letters and Months", 0x3200, 0x32ff)
    val CJK_COMPATIBILITY = addUnicodeBlock("CJK Compatibility", 0x3300, 0x33ff)
    val CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A = addUnicodeBlock("CJK Unified Ideographs Extension A", 0x3400, 0x4dbf)
    val YIJING_HEXAGRAM_SYMBOLS = addUnicodeBlock("Yijing Hexagram Symbols", 0x4dc0, 0x4dff)
    val CJK_UNIFIED_IDEOGRAPHS = addUnicodeBlock("CJK Unified Ideographs", 0x4e00, 0x9fff)
    val YI_SYLLABLES = addUnicodeBlock("Yi Syllables", 0xa000, 0xa48f)
    val YI_RADICALS = addUnicodeBlock("Yi Radicals", 0xa490, 0xa4cf)
    val LISU = addUnicodeBlock("Lisu", 0xa4d0, 0xa4ff)
    val VAI = addUnicodeBlock("Vai", 0xa500, 0xa63f)
    val CYRILLIC_EXTENDED_B = addUnicodeBlock("Cyrillic Extended-B", 0xa640, 0xa69f)
    val BAMUM = addUnicodeBlock("Bamum", 0xa6a0, 0xa6ff)
    val MODIFIER_TONE_LETTERS = addUnicodeBlock("Modifier Tone Letters", 0xa700, 0xa71f)
    val LATIN_EXTENDED_D = addUnicodeBlock("Latin Extended-D", 0xa720, 0xa7ff)
    val SYLOTI_NAGRI = addUnicodeBlock("Syloti Nagri", 0xa800, 0xa82f)
    val COMMON_INDIC_NUMBER_FORMS = addUnicodeBlock("Common Indic Number Forms", 0xa830, 0xa83f)
    val PHAGS_PA = addUnicodeBlock("Phags-pa", 0xa840, 0xa87f)
    val SAURASHTRA = addUnicodeBlock("Saurashtra", 0xa880, 0xa8df)
    val DEVANAGARI_EXTENDED = addUnicodeBlock("Devanagari Extended", 0xa8e0, 0xa8ff)
    val KAYAH_LI = addUnicodeBlock("Kayah Li", 0xa900, 0xa92f)
    val REJANG = addUnicodeBlock("Rejang", 0xa930, 0xa95f)
    val HANGUL_JAMO_EXTENDED_A = addUnicodeBlock("Hangul Jamo Extended-A", 0xa960, 0xa97f)
    val JAVANESE = addUnicodeBlock("Javanese", 0xa980, 0xa9df)
    val MYANMAR_EXTENDED_B = addUnicodeBlock("Myanmar Extended-B", 0xa9e0, 0xa9ff)
    val CHAM = addUnicodeBlock("Cham", 0xaa00, 0xaa5f)
    val MYANMAR_EXTENDED_A = addUnicodeBlock("Myanmar Extended-A", 0xaa60, 0xaa7f)
    val TAI_VIET = addUnicodeBlock("Tai Viet", 0xaa80, 0xaadf)
    val MEETEI_MAYEK_EXTENSIONS = addUnicodeBlock("Meetei Mayek Extensions", 0xaae0, 0xaaff)
    val ETHIOPIC_EXTENDED_A = addUnicodeBlock("Ethiopic Extended-A", 0xab00, 0xab2f)
    val LATIN_EXTENDED_E = addUnicodeBlock("Latin Extended-E", 0xab30, 0xab6f)
    val CHEROKEE_SUPPLEMENT = addUnicodeBlock("Cherokee Supplement", 0xab70, 0xabbf)
    val MEETEI_MAYEK = addUnicodeBlock("Meetei Mayek", 0xabc0, 0xabff)
    val HANGUL_SYLLABLES = addUnicodeBlock("Hangul Syllables", 0xac00, 0xd7af)
    val HANGUL_JAMO_EXTENDED_B = addUnicodeBlock("Hangul Jamo Extended-B", 0xd7b0, 0xd7ff)
    val HIGH_SURROGATES = addUnicodeBlock("High Surrogates", 0xd800, 0xdb7f)
    val HIGH_PRIVATE_USE_SURROGATES = addUnicodeBlock("High Private Use Surrogates", 0xdb80, 0xdbff)
    val LOW_SURROGATES = addUnicodeBlock("Low Surrogates", 0xdc00, 0xdfff)
    val PRIVATE_USE_AREA = addUnicodeBlock("Private Use Area", 0xe000, 0xf8ff)
    val CJK_COMPATIBILITY_IDEOGRAPHS = addUnicodeBlock("CJK Compatibility Ideographs", 0xf900, 0xfaff)
    val ALPHABETIC_PRESENTATION_FORMS = addUnicodeBlock("Alphabetic Presentation Forms", 0xfb00, 0xfb4f)
    val ARABIC_PRESENTATION_FORMS_A = addUnicodeBlock("Arabic Presentation Forms-A", 0xfb50, 0xfdff)
    val VARIATION_SELECTORS = addUnicodeBlock("Variation Selectors", 0xfe00, 0xfe0f)
    val VERTICAL_FORMS = addUnicodeBlock("Vertical Forms", 0xfe10, 0xfe1f)
    val COMBINING_HALF_MARKS = addUnicodeBlock("Combining Half Marks", 0xfe20, 0xfe2f)
    val CJK_COMPATIBILITY_FORMS = addUnicodeBlock("CJK Compatibility Forms", 0xfe30, 0xfe4f)
    val SMALL_FORM_VARIANTS = addUnicodeBlock("Small Form Variants", 0xfe50, 0xfe6f)
    val ARABIC_PRESENTATION_FORMS_B = addUnicodeBlock("Arabic Presentation Forms-B", 0xfe70, 0xfeff)
    val HALFWIDTH_AND_FULLWIDTH_FORMS = addUnicodeBlock("Halfwidth and Fullwidth Forms", 0xff00, 0xffef)
    val SPECIALS = addUnicodeBlock("Specials", 0xfff0, 0xffff)
    val LINEAR_B_SYLLABARY = addUnicodeBlock("Linear B Syllabary", 0x10000, 0x1007f)
    val LINEAR_B_IDEOGRAMS = addUnicodeBlock("Linear B Ideograms", 0x10080, 0x100ff)
    val AEGEAN_NUMBERS = addUnicodeBlock("Aegean Numbers", 0x10100, 0x1013f)
    val ANCIENT_GREEK_NUMBERS = addUnicodeBlock("Ancient Greek Numbers", 0x10140, 0x1018f)
    val ANCIENT_SYMBOLS = addUnicodeBlock("Ancient Symbols", 0x10190, 0x101cf)
    val PHAISTOS_DISC = addUnicodeBlock("Phaistos Disc", 0x101d0, 0x101ff)
    val LYCIAN = addUnicodeBlock("Lycian", 0x10280, 0x1029f)
    val CARIAN = addUnicodeBlock("Carian", 0x102a0, 0x102df)
    val COPTIC_EPACT_NUMBERS = addUnicodeBlock("Coptic Epact Numbers", 0x102e0, 0x102ff)
    val OLD_ITALIC = addUnicodeBlock("Old Italic", 0x10300, 0x1032f)
    val GOTHIC = addUnicodeBlock("Gothic", 0x10330, 0x1034f)
    val OLD_PERMIC = addUnicodeBlock("Old Permic", 0x10350, 0x1037f)
    val UGARITIC = addUnicodeBlock("Ugaritic", 0x10380, 0x1039f)
    val OLD_PERSIAN = addUnicodeBlock("Old Persian", 0x103a0, 0x103df)
    val DESERET = addUnicodeBlock("Deseret", 0x10400, 0x1044f)
    val SHAVIAN = addUnicodeBlock("Shavian", 0x10450, 0x1047f)
    val OSMANYA = addUnicodeBlock("Osmanya", 0x10480, 0x104af)
    val OSAGE = addUnicodeBlock("Osage", 0x104b0, 0x104ff)
    val ELBASAN = addUnicodeBlock("Elbasan", 0x10500, 0x1052f)
    val CAUCASIAN_ALBANIAN = addUnicodeBlock("Caucasian Albanian", 0x10530, 0x1056f)
    val VITHKUQI = addUnicodeBlock("Vithkuqi", 0x10570, 0x105bf)
    val LINEAR_A = addUnicodeBlock("Linear A", 0x10600, 0x1077f)
    val LATIN_EXTENDED_F = addUnicodeBlock("Latin Extended-F", 0x10780, 0x107bf)
    val CYPRIOT_SYLLABARY = addUnicodeBlock("Cypriot Syllabary", 0x10800, 0x1083f)
    val IMPERIAL_ARAMAIC = addUnicodeBlock("Imperial Aramaic", 0x10840, 0x1085f)
    val PALMYRENE = addUnicodeBlock("Palmyrene", 0x10860, 0x1087f)
    val NABATAEAN = addUnicodeBlock("Nabataean", 0x10880, 0x108af)
    val HATRAN = addUnicodeBlock("Hatran", 0x108e0, 0x108ff)
    val PHOENICIAN = addUnicodeBlock("Phoenician", 0x10900, 0x1091f)
    val LYDIAN = addUnicodeBlock("Lydian", 0x10920, 0x1093f)
    val MEROITIC_HIEROGLYPHS = addUnicodeBlock("Meroitic Hieroglyphs", 0x10980, 0x1099f)
    val MEROITIC_CURSIVE = addUnicodeBlock("Meroitic Cursive", 0x109a0, 0x109ff)
    val KHAROSHTHI = addUnicodeBlock("Kharoshthi", 0x10a00, 0x10a5f)
    val OLD_SOUTH_ARABIAN = addUnicodeBlock("Old South Arabian", 0x10a60, 0x10a7f)
    val OLD_NORTH_ARABIAN = addUnicodeBlock("Old North Arabian", 0x10a80, 0x10a9f)
    val MANICHAEAN = addUnicodeBlock("Manichaean", 0x10ac0, 0x10aff)
    val AVESTAN = addUnicodeBlock("Avestan", 0x10b00, 0x10b3f)
    val INSCRIPTIONAL_PARTHIAN = addUnicodeBlock("Inscriptional Parthian", 0x10b40, 0x10b5f)
    val INSCRIPTIONAL_PAHLAVI = addUnicodeBlock("Inscriptional Pahlavi", 0x10b60, 0x10b7f)
    val PSALTER_PAHLAVI = addUnicodeBlock("Psalter Pahlavi", 0x10b80, 0x10baf)
    val OLD_TURKIC = addUnicodeBlock("Old Turkic", 0x10c00, 0x10c4f)
    val OLD_HUNGARIAN = addUnicodeBlock("Old Hungarian", 0x10c80, 0x10cff)
    val HANIFI_ROHINGYA = addUnicodeBlock("Hanifi Rohingya", 0x10d00, 0x10d3f)
    val RUMI_NUMERAL_SYMBOLS = addUnicodeBlock("Rumi Numeral Symbols", 0x10e60, 0x10e7f)
    val YEZIDI = addUnicodeBlock("Yezidi", 0x10e80, 0x10ebf)
    val ARABIC_EXTENDED_C = addUnicodeBlock("Arabic Extended-C", 0x10ec0, 0x10eff)
    val OLD_SOGDIAN = addUnicodeBlock("Old Sogdian", 0x10f00, 0x10f2f)
    val SOGDIAN = addUnicodeBlock("Sogdian", 0x10f30, 0x10f6f)
    val OLD_UYGHUR = addUnicodeBlock("Old Uyghur", 0x10f70, 0x10faf)
    val CHORASMIAN = addUnicodeBlock("Chorasmian", 0x10fb0, 0x10fdf)
    val ELYMAIC = addUnicodeBlock("Elymaic", 0x10fe0, 0x10fff)
    val BRAHMI = addUnicodeBlock("Brahmi", 0x11000, 0x1107f)
    val KAITHI = addUnicodeBlock("Kaithi", 0x11080, 0x110cf)
    val SORA_SOMPENG = addUnicodeBlock("Sora Sompeng", 0x110d0, 0x110ff)
    val CHAKMA = addUnicodeBlock("Chakma", 0x11100, 0x1114f)
    val MAHAJANI = addUnicodeBlock("Mahajani", 0x11150, 0x1117f)
    val SHARADA = addUnicodeBlock("Sharada", 0x11180, 0x111df)
    val SINHALA_ARCHAIC_NUMBERS = addUnicodeBlock("Sinhala Archaic Numbers", 0x111e0, 0x111ff)
    val KHOJKI = addUnicodeBlock("Khojki", 0x11200, 0x1124f)
    val MULTANI = addUnicodeBlock("Multani", 0x11280, 0x112af)
    val KHUDAWADI = addUnicodeBlock("Khudawadi", 0x112b0, 0x112ff)
    val GRANTHA = addUnicodeBlock("Grantha", 0x11300, 0x1137f)
    val NEWA = addUnicodeBlock("Newa", 0x11400, 0x1147f)
    val TIRHUTA = addUnicodeBlock("Tirhuta", 0x11480, 0x114df)
    val SIDDHAM = addUnicodeBlock("Siddham", 0x11580, 0x115ff)
    val MODI = addUnicodeBlock("Modi", 0x11600, 0x1165f)
    val MONGOLIAN_SUPPLEMENT = addUnicodeBlock("Mongolian Supplement", 0x11660, 0x1167f)
    val TAKRI = addUnicodeBlock("Takri", 0x11680, 0x116cf)
    val AHOM = addUnicodeBlock("Ahom", 0x11700, 0x1174f)
    val DOGRA = addUnicodeBlock("Dogra", 0x11800, 0x1184f)
    val WARANG_CITI = addUnicodeBlock("Warang Citi", 0x118a0, 0x118ff)
    val DIVES_AKURU = addUnicodeBlock("Dives Akuru", 0x11900, 0x1195f)
    val NANDINAGARI = addUnicodeBlock("Nandinagari", 0x119a0, 0x119ff)
    val ZANABAZAR_SQUARE = addUnicodeBlock("Zanabazar Square", 0x11a00, 0x11a4f)
    val SOYOMBO = addUnicodeBlock("Soyombo", 0x11a50, 0x11aaf)
    val UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS_EXTENDED_A = addUnicodeBlock("Unified Canadian Aboriginal Syllabics Extended-A", 0x11ab0, 0x11abf)
    val PAU_CIN_HAU = addUnicodeBlock("Pau Cin Hau", 0x11ac0, 0x11aff)
    val DEVANAGARI_EXTENDED_A = addUnicodeBlock("Devanagari Extended-A", 0x11b00, 0x11b5f)
    val BHAIKSUKI = addUnicodeBlock("Bhaiksuki", 0x11c00, 0x11c6f)
    val MARCHEN = addUnicodeBlock("Marchen", 0x11c70, 0x11cbf)
    val MASARAM_GONDI = addUnicodeBlock("Masaram Gondi", 0x11d00, 0x11d5f)
    val GUNJALA_GONDI = addUnicodeBlock("Gunjala Gondi", 0x11d60, 0x11daf)
    val MAKASAR = addUnicodeBlock("Makasar", 0x11ee0, 0x11eff)
    val KAWI = addUnicodeBlock("Kawi", 0x11f00, 0x11f5f)
    val LISU_SUPPLEMENT = addUnicodeBlock("Lisu Supplement", 0x11fb0, 0x11fbf)
    val TAMIL_SUPPLEMENT = addUnicodeBlock("Tamil Supplement", 0x11fc0, 0x11fff)
    val CUNEIFORM = addUnicodeBlock("Cuneiform", 0x12000, 0x123ff)
    val CUNEIFORM_NUMBERS_AND_PUNCTUATION = addUnicodeBlock("Cuneiform Numbers and Punctuation", 0x12400, 0x1247f)
    val EARLY_DYNASTIC_CUNEIFORM = addUnicodeBlock("Early Dynastic Cuneiform", 0x12480, 0x1254f)
    val CYPRO_MINOAN = addUnicodeBlock("Cypro-Minoan", 0x12f90, 0x12fff)
    val EGYPTIAN_HIEROGLYPHS = addUnicodeBlock("Egyptian Hieroglyphs", 0x13000, 0x1342f)
    val EGYPTIAN_HIEROGLYPH_FORMAT_CONTROLS = addUnicodeBlock("Egyptian Hieroglyph Format Controls", 0x13430, 0x1345f)
    val ANATOLIAN_HIEROGLYPHS = addUnicodeBlock("Anatolian Hieroglyphs", 0x14400, 0x1467f)
    val BAMUM_SUPPLEMENT = addUnicodeBlock("Bamum Supplement", 0x16800, 0x16a3f)
    val MRO = addUnicodeBlock("Mro", 0x16a40, 0x16a6f)
    val TANGSA = addUnicodeBlock("Tangsa", 0x16a70, 0x16acf)
    val BASSA_VAH = addUnicodeBlock("Bassa Vah", 0x16ad0, 0x16aff)
    val PAHAWH_HMONG = addUnicodeBlock("Pahawh Hmong", 0x16b00, 0x16b8f)
    val MEDEFAIDRIN = addUnicodeBlock("Medefaidrin", 0x16e40, 0x16e9f)
    val MIAO = addUnicodeBlock("Miao", 0x16f00, 0x16f9f)
    val IDEOGRAPHIC_SYMBOLS_AND_PUNCTUATION = addUnicodeBlock("Ideographic Symbols and Punctuation", 0x16fe0, 0x16fff)
    val TANGUT = addUnicodeBlock("Tangut", 0x17000, 0x187ff)
    val TANGUT_COMPONENTS = addUnicodeBlock("Tangut Components", 0x18800, 0x18aff)
    val KHITAN_SMALL_SCRIPT = addUnicodeBlock("Khitan Small Script", 0x18b00, 0x18cff)
    val TANGUT_SUPPLEMENT = addUnicodeBlock("Tangut Supplement", 0x18d00, 0x18d7f)
    val KANA_EXTENDED_B = addUnicodeBlock("Kana Extended-B", 0x1aff0, 0x1afff)
    val KANA_SUPPLEMENT = addUnicodeBlock("Kana Supplement", 0x1b000, 0x1b0ff)
    val KANA_EXTENDED_A = addUnicodeBlock("Kana Extended-A", 0x1b100, 0x1b12f)
    val SMALL_KANA_EXTENSION = addUnicodeBlock("Small Kana Extension", 0x1b130, 0x1b16f)
    val NUSHU = addUnicodeBlock("Nushu", 0x1b170, 0x1b2ff)
    val DUPLOYAN = addUnicodeBlock("Duployan", 0x1bc00, 0x1bc9f)
    val SHORTHAND_FORMAT_CONTROLS = addUnicodeBlock("Shorthand Format Controls", 0x1bca0, 0x1bcaf)
    val ZNAMENNY_MUSICAL_NOTATION = addUnicodeBlock("Znamenny Musical Notation", 0x1cf00, 0x1cfcf)
    val BYZANTINE_MUSICAL_SYMBOLS = addUnicodeBlock("Byzantine Musical Symbols", 0x1d000, 0x1d0ff)
    val MUSICAL_SYMBOLS = addUnicodeBlock("Musical Symbols", 0x1d100, 0x1d1ff)
    val ANCIENT_GREEK_MUSICAL_NOTATION = addUnicodeBlock("Ancient Greek Musical Notation", 0x1d200, 0x1d24f)
    val KAKTOVIK_NUMERALS = addUnicodeBlock("Kaktovik Numerals", 0x1d2c0, 0x1d2df)
    val MAYAN_NUMERALS = addUnicodeBlock("Mayan Numerals", 0x1d2e0, 0x1d2ff)
    val TAI_XUAN_JING_SYMBOLS = addUnicodeBlock("Tai Xuan Jing Symbols", 0x1d300, 0x1d35f)
    val COUNTING_ROD_NUMERALS = addUnicodeBlock("Counting Rod Numerals", 0x1d360, 0x1d37f)
    val MATHEMATICAL_ALPHANUMERIC_SYMBOLS = addUnicodeBlock("Mathematical Alphanumeric Symbols", 0x1d400, 0x1d7ff)
    val SUTTON_SIGNWRITING = addUnicodeBlock("Sutton SignWriting", 0x1d800, 0x1daaf)
    val LATIN_EXTENDED_G = addUnicodeBlock("Latin Extended-G", 0x1df00, 0x1dfff)
    val GLAGOLITIC_SUPPLEMENT = addUnicodeBlock("Glagolitic Supplement", 0x1e000, 0x1e02f)
    val CYRILLIC_EXTENDED_D = addUnicodeBlock("Cyrillic Extended-D", 0x1e030, 0x1e08f)
    val NYIAKENG_PUACHUE_HMONG = addUnicodeBlock("Nyiakeng Puachue Hmong", 0x1e100, 0x1e14f)
    val TOTO = addUnicodeBlock("Toto", 0x1e290, 0x1e2bf)
    val WANCHO = addUnicodeBlock("Wancho", 0x1e2c0, 0x1e2ff)
    val NAG_MUNDARI = addUnicodeBlock("Nag Mundari", 0x1e4d0, 0x1e4ff)
    val ETHIOPIC_EXTENDED_B = addUnicodeBlock("Ethiopic Extended-B", 0x1e7e0, 0x1e7ff)
    val MENDE_KIKAKUI = addUnicodeBlock("Mende Kikakui", 0x1e800, 0x1e8df)
    val ADLAM = addUnicodeBlock("Adlam", 0x1e900, 0x1e95f)
    val INDIC_SIYAQ_NUMBERS = addUnicodeBlock("Indic Siyaq Numbers", 0x1ec70, 0x1ecbf)
    val OTTOMAN_SIYAQ_NUMBERS = addUnicodeBlock("Ottoman Siyaq Numbers", 0x1ed00, 0x1ed4f)
    val ARABIC_MATHEMATICAL_ALPHABETIC_SYMBOLS = addUnicodeBlock("Arabic Mathematical Alphabetic Symbols", 0x1ee00, 0x1eeff)
    val MAHJONG_TILES = addUnicodeBlock("Mahjong Tiles", 0x1f000, 0x1f02f)
    val DOMINO_TILES = addUnicodeBlock("Domino Tiles", 0x1f030, 0x1f09f)
    val PLAYING_CARDS = addUnicodeBlock("Playing Cards", 0x1f0a0, 0x1f0ff)
    val ENCLOSED_ALPHANUMERIC_SUPPLEMENT = addUnicodeBlock("Enclosed Alphanumeric Supplement", 0x1f100, 0x1f1ff)
    val ENCLOSED_IDEOGRAPHIC_SUPPLEMENT = addUnicodeBlock("Enclosed Ideographic Supplement", 0x1f200, 0x1f2ff)
    val MISCELLANEOUS_SYMBOLS_AND_PICTOGRAPHS = addUnicodeBlock("Miscellaneous Symbols and Pictographs", 0x1f300, 0x1f5ff)
    val EMOTICONS = addUnicodeBlock("Emoticons", 0x1f600, 0x1f64f)
    val ORNAMENTAL_DINGBATS = addUnicodeBlock("Ornamental Dingbats", 0x1f650, 0x1f67f)
    val TRANSPORT_AND_MAP_SYMBOLS = addUnicodeBlock("Transport and Map Symbols", 0x1f680, 0x1f6ff)
    val ALCHEMICAL_SYMBOLS = addUnicodeBlock("Alchemical Symbols", 0x1f700, 0x1f77f)
    val GEOMETRIC_SHAPES_EXTENDED = addUnicodeBlock("Geometric Shapes Extended", 0x1f780, 0x1f7ff)
    val SUPPLEMENTAL_ARROWS_C = addUnicodeBlock("Supplemental Arrows-C", 0x1f800, 0x1f8ff)
    val SUPPLEMENTAL_SYMBOLS_AND_PICTOGRAPHS = addUnicodeBlock("Supplemental Symbols and Pictographs", 0x1f900, 0x1f9ff)
    val CHESS_SYMBOLS = addUnicodeBlock("Chess Symbols", 0x1fa00, 0x1fa6f)
    val SYMBOLS_AND_PICTOGRAPHS_EXTENDED_A = addUnicodeBlock("Symbols and Pictographs Extended-A", 0x1fa70, 0x1faff)
    val SYMBOLS_FOR_LEGACY_COMPUTING = addUnicodeBlock("Symbols for Legacy Computing", 0x1fb00, 0x1fbff)
    val CJK_UNIFIED_IDEOGRAPHS_EXTENSION_B = addUnicodeBlock("CJK Unified Ideographs Extension B", 0x20000, 0x2a6df)
    val CJK_UNIFIED_IDEOGRAPHS_EXTENSION_C = addUnicodeBlock("CJK Unified Ideographs Extension C", 0x2a700, 0x2b73f)
    val CJK_UNIFIED_IDEOGRAPHS_EXTENSION_D = addUnicodeBlock("CJK Unified Ideographs Extension D", 0x2b740, 0x2b81f)
    val CJK_UNIFIED_IDEOGRAPHS_EXTENSION_E = addUnicodeBlock("CJK Unified Ideographs Extension E", 0x2b820, 0x2ceaf)
    val CJK_UNIFIED_IDEOGRAPHS_EXTENSION_F = addUnicodeBlock("CJK Unified Ideographs Extension F", 0x2ceb0, 0x2ebef)
    val CJK_COMPATIBILITY_IDEOGRAPHS_SUPPLEMENT = addUnicodeBlock("CJK Compatibility Ideographs Supplement", 0x2f800, 0x2fa1f)
    val CJK_UNIFIED_IDEOGRAPHS_EXTENSION_G = addUnicodeBlock("CJK Unified Ideographs Extension G", 0x30000, 0x3134f)
    val CJK_UNIFIED_IDEOGRAPHS_EXTENSION_H = addUnicodeBlock("CJK Unified Ideographs Extension H", 0x31350, 0x323af)
    val TAGS = addUnicodeBlock("Tags", 0xe0000, 0xe007f)
    val VARIATION_SELECTORS_SUPPLEMENT = addUnicodeBlock("Variation Selectors Supplement", 0xe0100, 0xe01ef)
    val SUPPLEMENTARY_PRIVATE_USE_AREA_A = addUnicodeBlock("Supplementary Private Use Area-A", 0xf0000, 0xfffff)
    val SUPPLEMENTARY_PRIVATE_USE_AREA_B = addUnicodeBlock("Supplementary Private Use Area-B", 0x100000, 0x10ffff)
    // END GENERATED: [unicode-blocks]

    // scalafmt: {}

    def forName(blockName: String): UnicodeBlock = {
      val key: String = blockName.toLowerCase()
      val block = blocksByNormalizedName.get(key)
      if (block == null)
        throw new IllegalArgumentException()
      block
    }

    def of(c: scala.Char): UnicodeBlock = of(c.toInt)

    def of(codePoint: scala.Int): UnicodeBlock = {
      if (!Character.isValidCodePoint(codePoint))
        throw new IllegalArgumentException()

      binarySearch(codePoint, 0, allBlocks.size())
    }

    @tailrec
    private[this] def binarySearch(codePoint: scala.Int, lo: scala.Int,
        hi: scala.Int): UnicodeBlock = {
      if (lo < hi) {
        val mid = lo + (hi - lo) / 2
        val block = allBlocks.get(mid)

        if (codePoint >= block.start && codePoint <= block.end) block
        else if (codePoint > block.end) binarySearch(codePoint, mid + 1, hi)
        else binarySearch(codePoint, lo, mid)
      } else {
        null
      }
    }
  }

  private[lang] final val CombiningClassIsNone = 0
  private[lang] final val CombiningClassIsAbove = 1
  private[lang] final val CombiningClassIsOther = 2

  /* Indices representing the start of ranges of codePoint that have the same
   * `combiningClassNoneOrAboveOrOther` result. The results cycle modulo 3 at
   * every range:
   *
   * - 0 for the range [0, array(0))
   * - 1 for the range [array(0), array(1))
   * - 2 for the range [array(1), array(2))
   * - 0 for the range [array(2), array(3))
   * - etc.
   *
   * In general, for a range ending at `array(i)` (excluded), the result is
   * `i % 3`.
   *
   * A range can be empty, i.e., it can happen that `array(i) == array(i + 1)`
   * (but then it is different from `array(i - 1)` and `array(i + 2)`).
   */
  private[this] lazy val combiningClassNoneOrAboveOrOtherIndices: Array[Int] = Array(
      // BEGIN GENERATED: [combining-classes]
      768, 789, 829, 829, 837, 838, 838, 839, 842, 842, 845, 847, 848, 851,
      855, 855, 856, 859, 859, 860, 867, 867, 880, 880, 1155, 1160, 1160, 1425,
      1425, 1426, 1426, 1430, 1431, 1431, 1434, 1436, 1436, 1442, 1448, 1448,
      1450, 1451, 1451, 1453, 1455, 1455, 1456, 1470, 1471, 1471, 1472, 1473,
      1473, 1475, 1476, 1477, 1478, 1479, 1479, 1480, 1552, 1560, 1563, 1611,
      1611, 1619, 1619, 1621, 1623, 1623, 1628, 1629, 1629, 1631, 1632, 1648,
      1648, 1649, 1750, 1757, 1757, 1759, 1763, 1764, 1764, 1765, 1765, 1767,
      1769, 1769, 1770, 1770, 1771, 1771, 1773, 1774, 1809, 1809, 1810, 1840,
      1841, 1842, 1842, 1844, 1845, 1845, 1847, 1850, 1850, 1851, 1853, 1853,
      1854, 1855, 1855, 1858, 1859, 1859, 1860, 1861, 1861, 1862, 1863, 1863,
      1864, 1865, 1865, 1867, 1867, 2027, 2034, 2035, 2035, 2036, 2036, 2045,
      2045, 2046, 2070, 2074, 2074, 2075, 2084, 2084, 2085, 2088, 2088, 2089,
      2094, 2094, 2137, 2137, 2140, 2200, 2201, 2204, 2204, 2208, 2208, 2250,
      2255, 2260, 2260, 2274, 2274, 2275, 2275, 2276, 2276, 2278, 2279, 2279,
      2281, 2282, 2282, 2285, 2291, 2291, 2294, 2295, 2295, 2297, 2299, 2299,
      2304, 2304, 2364, 2364, 2365, 2381, 2381, 2382, 2385, 2386, 2387, 2387,
      2389, 2389, 2492, 2492, 2493, 2509, 2509, 2510, 2558, 2559, 2559, 2620,
      2620, 2621, 2637, 2637, 2638, 2748, 2748, 2749, 2765, 2765, 2766, 2876,
      2876, 2877, 2893, 2893, 2894, 3021, 3021, 3022, 3132, 3132, 3133, 3149,
      3149, 3150, 3157, 3157, 3159, 3260, 3260, 3261, 3277, 3277, 3278, 3387,
      3387, 3389, 3405, 3405, 3406, 3530, 3530, 3531, 3640, 3640, 3643, 3656,
      3656, 3660, 3768, 3768, 3771, 3784, 3784, 3788, 3864, 3864, 3866, 3893,
      3893, 3894, 3895, 3895, 3896, 3897, 3897, 3898, 3953, 3953, 3955, 3956,
      3956, 3957, 3962, 3962, 3966, 3968, 3968, 3969, 3970, 3972, 3973, 3974,
      3976, 3976, 4038, 4038, 4039, 4151, 4151, 4152, 4153, 4153, 4155, 4237,
      4237, 4238, 4957, 4960, 4960, 5908, 5908, 5910, 5940, 5940, 5941, 6098,
      6098, 6099, 6109, 6110, 6110, 6313, 6313, 6314, 6457, 6457, 6458, 6458,
      6459, 6460, 6679, 6680, 6681, 6752, 6752, 6753, 6773, 6781, 6781, 6783,
      6783, 6784, 6832, 6837, 6843, 6843, 6845, 6846, 6847, 6847, 6849, 6849,
      6851, 6853, 6853, 6858, 6859, 6859, 6863, 6863, 6964, 6964, 6965, 6980,
      6980, 6981, 7019, 7020, 7021, 7021, 7028, 7028, 7082, 7082, 7084, 7142,
      7142, 7143, 7154, 7154, 7156, 7223, 7223, 7224, 7376, 7379, 7379, 7380,
      7380, 7386, 7386, 7388, 7392, 7392, 7393, 7393, 7394, 7394, 7401, 7405,
      7405, 7406, 7412, 7413, 7413, 7416, 7418, 7418, 7616, 7618, 7619, 7619,
      7626, 7627, 7627, 7629, 7633, 7633, 7670, 7675, 7675, 7676, 7678, 7678,
      7679, 7680, 8400, 8402, 8404, 8404, 8408, 8411, 8411, 8413, 8413, 8417,
      8418, 8418, 8421, 8421, 8423, 8423, 8424, 8425, 8425, 8426, 8432, 8432,
      8433, 8433, 11503, 11506, 11506, 11647, 11647, 11648, 11744, 11776,
      11776, 12330, 12330, 12336, 12441, 12441, 12443, 42607, 42608, 42608,
      42612, 42622, 42622, 42654, 42656, 42656, 42736, 42738, 42738, 43014,
      43014, 43015, 43052, 43052, 43053, 43204, 43204, 43205, 43232, 43250,
      43250, 43307, 43307, 43310, 43347, 43347, 43348, 43443, 43443, 43444,
      43456, 43456, 43457, 43696, 43697, 43697, 43698, 43700, 43701, 43703,
      43705, 43705, 43710, 43712, 43712, 43713, 43714, 43714, 43766, 43766,
      43767, 44013, 44013, 44014, 64286, 64286, 64287, 65056, 65063, 65070,
      65070, 65072, 65072, 66045, 66045, 66046, 66272, 66272, 66273, 66422,
      66427, 66427, 68109, 68109, 68110, 68111, 68112, 68112, 68152, 68153,
      68155, 68159, 68159, 68160, 68325, 68326, 68327, 68900, 68904, 68904,
      69291, 69293, 69293, 69373, 69373, 69376, 69446, 69446, 69448, 69448,
      69451, 69452, 69452, 69453, 69457, 69506, 69507, 69508, 69508, 69509,
      69510, 69702, 69702, 69703, 69744, 69744, 69745, 69759, 69759, 69760,
      69817, 69817, 69819, 69888, 69891, 69891, 69939, 69939, 69941, 70003,
      70003, 70004, 70080, 70080, 70081, 70090, 70090, 70091, 70197, 70197,
      70199, 70377, 70377, 70379, 70459, 70459, 70461, 70477, 70477, 70478,
      70502, 70509, 70509, 70512, 70517, 70517, 70722, 70722, 70723, 70726,
      70726, 70727, 70750, 70751, 70751, 70850, 70850, 70852, 71103, 71103,
      71105, 71231, 71231, 71232, 71350, 71350, 71352, 71467, 71467, 71468,
      71737, 71737, 71739, 71997, 71997, 71999, 72003, 72003, 72004, 72160,
      72160, 72161, 72244, 72244, 72245, 72263, 72263, 72264, 72345, 72345,
      72346, 72767, 72767, 72768, 73026, 73026, 73027, 73028, 73028, 73030,
      73111, 73111, 73112, 73537, 73537, 73539, 92912, 92912, 92917, 92976,
      92983, 92983, 94192, 94192, 94194, 113822, 113822, 113823, 119141,
      119141, 119146, 119149, 119149, 119155, 119163, 119163, 119171, 119173,
      119178, 119180, 119210, 119214, 119214, 119362, 119365, 119365, 122880,
      122887, 122887, 122888, 122905, 122905, 122907, 122914, 122914, 122915,
      122917, 122917, 122918, 122923, 122923, 123023, 123024, 123024, 123184,
      123191, 123191, 123566, 123567, 123567, 123628, 123632, 123632, 124140,
      124140, 124143, 124143, 124144, 124144, 125136, 125136, 125143, 125252,
      125258, 125259
      // END GENERATED: [combining-classes]
  )

  /** Tests whether the given code point's combining class is 0 (None), 230
   *  (Above) or something else (Other).
   *
   *  This is a special-purpose method for use by `String.toLowerCase` and
   *  `String.toUpperCase`.
   */
  private[lang] def combiningClassNoneOrAboveOrOther(cp: Int): Int = {
    val indexOfRange = findIndexOfRange(
        combiningClassNoneOrAboveOrOtherIndices, cp, hasEmptyRanges = true)
    indexOfRange % 3
  }

  private[this] def findIndexOfRange(startOfRangesArray: Array[Int],
      value: Int, hasEmptyRanges: scala.Boolean): Int = {
    val i = Arrays.binarySearch(startOfRangesArray, value)
    if (i >= 0) {
      /* `value` is at the start of a range. Its range index is therefore
       * `i + 1`, since there is an implicit range starting at 0 in the
       * beginning.
       *
       * If the array has empty ranges, we may need to advance further than
       * `i + 1` until the first index `j > i` where
       * `startOfRangesArray(j) != value`.
       */
      if (hasEmptyRanges) {
        var j = i + 1
        while (j < startOfRangesArray.length && startOfRangesArray(j) == value)
          j += 1
        j
      } else {
        i + 1
      }
    } else {
      /* i is `-p - 1` where `p` is the insertion point. In that case the index
       * of the range is precisely `p`.
       */
      -i - 1
    }
  }

  /** All the non-ASCII code points that map to the digit 0.
   *
   *  Each of them is directly followed by 9 other code points mapping to the
   *  digits 1 to 9, in order. Conversely, there are no other non-ASCII code
   *  point mapping to digits from 0 to 9.
   *
   *  These assumptions are checked when generating the table.
   */
  private[this] val nonASCIIZeroDigitCodePoints: Array[Int] = {
    Array(
        // BEGIN GENERATED: [non-ascii-zero-digits]
        0x0660, 0x06f0, 0x07c0, 0x0966, 0x09e6, 0x0a66, 0x0ae6, 0x0b66, 0x0be6,
        0x0c66, 0x0ce6, 0x0d66, 0x0de6, 0x0e50, 0x0ed0, 0x0f20, 0x1040, 0x1090,
        0x17e0, 0x1810, 0x1946, 0x19d0, 0x1a80, 0x1a90, 0x1b50, 0x1bb0, 0x1c40,
        0x1c50, 0xa620, 0xa8d0, 0xa900, 0xa9d0, 0xa9f0, 0xaa50, 0xabf0, 0xff10,
        0x104a0, 0x10d30, 0x11066, 0x110f0, 0x11136, 0x111d0, 0x112f0, 0x11450,
        0x114d0, 0x11650, 0x116c0, 0x11730, 0x118e0, 0x11950, 0x11c50, 0x11d50,
        0x11da0, 0x11f50, 0x16a60, 0x16ac0, 0x16b50, 0x1d7ce, 0x1d7d8, 0x1d7e2,
        0x1d7ec, 0x1d7f6, 0x1e140, 0x1e2f0, 0x1e4f0, 0x1e950, 0x1fbf0
        // END GENERATED: [non-ascii-zero-digits]
    )
  }
}
