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
    extends AnyRef with java.io.Serializable with Comparable[Character]
    with Constable {

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
  def UNASSIGNED: scala.Byte = 0
  def UPPERCASE_LETTER: scala.Byte = 1
  def LOWERCASE_LETTER: scala.Byte = 2
  def TITLECASE_LETTER: scala.Byte = 3
  def MODIFIER_LETTER: scala.Byte = 4
  def OTHER_LETTER: scala.Byte = 5
  def NON_SPACING_MARK: scala.Byte = 6
  def ENCLOSING_MARK: scala.Byte = 7
  def COMBINING_SPACING_MARK: scala.Byte = 8
  def DECIMAL_DIGIT_NUMBER: scala.Byte = 9
  def LETTER_NUMBER: scala.Byte = 10
  def OTHER_NUMBER: scala.Byte = 11
  def SPACE_SEPARATOR: scala.Byte = 12
  def LINE_SEPARATOR: scala.Byte = 13
  def PARAGRAPH_SEPARATOR: scala.Byte = 14
  def CONTROL: scala.Byte = 15
  def FORMAT: scala.Byte = 16
  def PRIVATE_USE: scala.Byte = 18
  def SURROGATE: scala.Byte = 19
  def DASH_PUNCTUATION: scala.Byte = 20
  def START_PUNCTUATION: scala.Byte = 21
  def END_PUNCTUATION: scala.Byte = 22
  def CONNECTOR_PUNCTUATION: scala.Byte = 23
  def OTHER_PUNCTUATION: scala.Byte = 24
  def MATH_SYMBOL: scala.Byte = 25
  def CURRENCY_SYMBOL: scala.Byte = 26
  def MODIFIER_SYMBOL: scala.Byte = 27
  def OTHER_SYMBOL: scala.Byte = 28
  def INITIAL_QUOTE_PUNCTUATION: scala.Byte = 29
  def FINAL_QUOTE_PUNCTUATION: scala.Byte = 30

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

  private final val HighSurrogateMask       = 0xfc00 // 111111 00  00000000
  private final val HighSurrogateID         = 0xd800 // 110110 00  00000000
  private final val LowSurrogateMask        = 0xfc00 // 111111 00  00000000
  private final val LowSurrogateID          = 0xdc00 // 110111 00  00000000
  private final val SurrogateMask           = 0xf800 // 111110 00  00000000
  private final val SurrogateID             = 0xd800 // 110110 00  00000000
  private final val SurrogateUsefulPartMask = 0x03ff // 000000 11  11111111

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
  private[lang] def offsetByCodePointsImpl(seq: CharSequence, index: Int, codePointOffset: Int): Int = {
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

  def getType(ch: scala.Char): Int = getType(ch.toInt)

  def getType(codePoint: Int): Int = {
    if (codePoint < 0) UNASSIGNED.toInt
    else if (codePoint < 256) getTypeLT256(codePoint)
    else getTypeGE256(codePoint)
  }

  @inline
  private[this] def getTypeLT256(codePoint: Int): Int =
    charTypesFirst256(codePoint)

  private[this] def getTypeGE256(codePoint: Int): Int = {
    charTypes(findIndexOfRange(
        charTypeIndices, codePoint, hasEmptyRanges = false))
  }

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

  def isISOControl(c: scala.Char): scala.Boolean = isISOControl(c.toInt)

  def isISOControl(codePoint: Int): scala.Boolean = {
    (0x00 <= codePoint && codePoint <= 0x1F) || (0x7F <= codePoint && codePoint <= 0x9F)
  }

  @deprecated("Replaced by isWhitespace(char)", "")
  def isSpace(c: scala.Char): scala.Boolean =
    c == '\t' || c == '\n' || c == '\f' || c == '\r' || c == ' '

  def isWhitespace(c: scala.Char): scala.Boolean =
    isWhitespace(c.toInt)

  def isWhitespace(codePoint: scala.Int): scala.Boolean = {
    def isSeparator(tpe: Int): scala.Boolean =
      tpe == SPACE_SEPARATOR || tpe == LINE_SEPARATOR || tpe == PARAGRAPH_SEPARATOR
    if (codePoint < 256) {
      codePoint == '\t' || codePoint == '\n' || codePoint == '\u000B' ||
      codePoint == '\f' || codePoint == '\r' ||
      ('\u001C' <= codePoint && codePoint <= '\u001F') ||
      (codePoint != '\u00A0' && isSeparator(getTypeLT256(codePoint)))
    } else {
      (codePoint != '\u2007' && codePoint != '\u202F') &&
      isSeparator(getTypeGE256(codePoint))
    }
  }

  def isSpaceChar(ch: scala.Char): scala.Boolean =
    isSpaceChar(ch.toInt)

  def isSpaceChar(codePoint: Int): scala.Boolean =
    isSpaceCharImpl(getType(codePoint))

  @inline private[this] def isSpaceCharImpl(tpe: Int): scala.Boolean =
    tpe == SPACE_SEPARATOR || tpe == LINE_SEPARATOR || tpe == PARAGRAPH_SEPARATOR

  def isLowerCase(c: scala.Char): scala.Boolean =
    isLowerCase(c.toInt)

  def isLowerCase(c: Int): scala.Boolean = {
    if (c < 256)
      c == '\u00AA' || c == '\u00BA' || getTypeLT256(c) == LOWERCASE_LETTER
    else
      isLowerCaseGE256(c)
  }

  private[this] def isLowerCaseGE256(c: Int): scala.Boolean = {
    ('\u02B0' <= c && c <= '\u02B8') || ('\u02C0' <= c && c <= '\u02C1') ||
    ('\u02E0' <= c && c <= '\u02E4') || c == '\u0345' || c == '\u037A' ||
    ('\u1D2C' <= c && c <= '\u1D6A') || c == '\u1D78' ||
    ('\u1D9B' <= c && c <= '\u1DBF') || c == '\u2071' || c == '\u207F' ||
    ('\u2090' <= c && c <= '\u209C') || ('\u2170' <= c && c <= '\u217F') ||
    ('\u24D0' <= c && c <= '\u24E9') || ('\u2C7C' <= c && c <= '\u2C7D') ||
    c == '\uA770' || ('\uA7F8' <= c && c <= '\uA7F9') ||
    getTypeGE256(c) == LOWERCASE_LETTER
  }

  def isUpperCase(c: scala.Char): scala.Boolean =
    isUpperCase(c.toInt)

  def isUpperCase(c: Int): scala.Boolean = {
    ('\u2160' <= c && c <= '\u216F') || ('\u24B6' <= c && c <= '\u24CF') ||
    getType(c) == UPPERCASE_LETTER
  }

  def isTitleCase(c: scala.Char): scala.Boolean =
    isTitleCase(c.toInt)

  def isTitleCase(cp: Int): scala.Boolean =
    if (cp < 256) false
    else isTitleCaseImpl(getTypeGE256(cp))

  @inline private[this] def isTitleCaseImpl(tpe: Int): scala.Boolean =
    tpe == TITLECASE_LETTER

  def isDigit(c: scala.Char): scala.Boolean =
    isDigit(c.toInt)

  def isDigit(cp: Int): scala.Boolean =
    if (cp < 256) '0' <= cp && cp <= '9'
    else isDigitImpl(getTypeGE256(cp))

  @inline private[this] def isDigitImpl(tpe: Int): scala.Boolean =
    tpe == DECIMAL_DIGIT_NUMBER

  def isDefined(c: scala.Char): scala.Boolean =
    isDefined(c.toInt)

  def isDefined(c: scala.Int): scala.Boolean = {
    if (c < 0) false
    else if (c < 888) true
    else getTypeGE256(c) != UNASSIGNED
  }

  def isLetter(c: scala.Char): scala.Boolean = isLetter(c.toInt)

  def isLetter(cp: Int): scala.Boolean = isLetterImpl(getType(cp))

  @inline private[this] def isLetterImpl(tpe: Int): scala.Boolean = {
    tpe == UPPERCASE_LETTER || tpe == LOWERCASE_LETTER ||
    tpe == TITLECASE_LETTER || tpe == MODIFIER_LETTER || tpe == OTHER_LETTER
  }

  def isLetterOrDigit(c: scala.Char): scala.Boolean =
    isLetterOrDigit(c.toInt)

  def isLetterOrDigit(cp: Int): scala.Boolean =
    isLetterOrDigitImpl(getType(cp))

  @inline private[this] def isLetterOrDigitImpl(tpe: Int): scala.Boolean =
    isDigitImpl(tpe) || isLetterImpl(tpe)

  def isJavaLetter(ch: scala.Char): scala.Boolean = isJavaLetterImpl(getType(ch))

  @inline private[this] def isJavaLetterImpl(tpe: Int): scala.Boolean = {
    isLetterImpl(tpe) || tpe == LETTER_NUMBER || tpe == CURRENCY_SYMBOL ||
    tpe == CONNECTOR_PUNCTUATION
  }

  def isJavaLetterOrDigit(ch: scala.Char): scala.Boolean =
    isJavaLetterOrDigitImpl(ch, getType(ch))

  @inline private[this] def isJavaLetterOrDigitImpl(codePoint: Int,
      tpe: Int): scala.Boolean = {
    isJavaLetterImpl(tpe) || tpe == COMBINING_SPACING_MARK ||
    tpe == NON_SPACING_MARK || isIdentifierIgnorableImpl(codePoint, tpe)
  }

  def isAlphabetic(codePoint: Int): scala.Boolean = {
    val tpe = getType(codePoint)
    tpe == UPPERCASE_LETTER || tpe == LOWERCASE_LETTER ||
    tpe == TITLECASE_LETTER || tpe == MODIFIER_LETTER ||
    tpe == OTHER_LETTER || tpe == LETTER_NUMBER
  }

  def isIdeographic(c: Int): scala.Boolean = {
    (12294 <= c && c <= 12295) || (12321 <= c && c <= 12329) ||
    (12344 <= c && c <= 12346) || (13312 <= c && c <= 19893) ||
    (19968 <= c && c <= 40908) || (63744 <= c && c <= 64109) ||
    (64112 <= c && c <= 64217) || (131072 <= c && c <= 173782) ||
    (173824 <= c && c <= 177972) || (177984 <= c && c <= 178205) ||
    (194560 <= c && c <= 195101)
  }

  def isJavaIdentifierStart(ch: scala.Char): scala.Boolean =
    isJavaIdentifierStart(ch.toInt)

  def isJavaIdentifierStart(codePoint: Int): scala.Boolean =
    isJavaIdentifierStartImpl(getType(codePoint))

  @inline
  private[this] def isJavaIdentifierStartImpl(tpe: Int): scala.Boolean = {
    isLetterImpl(tpe) || tpe == LETTER_NUMBER || tpe == CURRENCY_SYMBOL ||
    tpe == CONNECTOR_PUNCTUATION
  }

  def isJavaIdentifierPart(ch: scala.Char): scala.Boolean =
    isJavaIdentifierPart(ch.toInt)

  def isJavaIdentifierPart(codePoint: Int): scala.Boolean =
    isJavaIdentifierPartImpl(codePoint, getType(codePoint))

  @inline private[this] def isJavaIdentifierPartImpl(codePoint: Int,
      tpe: Int): scala.Boolean = {
    isLetterImpl(tpe) || tpe == CURRENCY_SYMBOL ||
    tpe == CONNECTOR_PUNCTUATION || tpe == DECIMAL_DIGIT_NUMBER ||
    tpe == LETTER_NUMBER || tpe == COMBINING_SPACING_MARK ||
    tpe == NON_SPACING_MARK || isIdentifierIgnorableImpl(codePoint, tpe)
  }

  def isUnicodeIdentifierStart(ch: scala.Char): scala.Boolean =
    isUnicodeIdentifierStart(ch.toInt)

  def isUnicodeIdentifierStart(codePoint: Int): scala.Boolean =
    isUnicodeIdentifierStartImpl(getType(codePoint))

  @inline
  private[this] def isUnicodeIdentifierStartImpl(tpe: Int): scala.Boolean =
    isLetterImpl(tpe) || tpe == LETTER_NUMBER

  def isUnicodeIdentifierPart(ch: scala.Char): scala.Boolean =
    isUnicodeIdentifierPart(ch.toInt)

  def isUnicodeIdentifierPart(codePoint: Int): scala.Boolean =
    isUnicodeIdentifierPartImpl(codePoint, getType(codePoint))

  def isUnicodeIdentifierPartImpl(codePoint: Int,
      tpe: Int): scala.Boolean = {
    tpe == CONNECTOR_PUNCTUATION || tpe == DECIMAL_DIGIT_NUMBER ||
    tpe == COMBINING_SPACING_MARK || tpe == NON_SPACING_MARK ||
    isUnicodeIdentifierStartImpl(tpe) ||
    isIdentifierIgnorableImpl(codePoint, tpe)
  }

  def isIdentifierIgnorable(c: scala.Char): scala.Boolean =
    isIdentifierIgnorable(c.toInt)

  def isIdentifierIgnorable(codePoint: Int): scala.Boolean =
    isIdentifierIgnorableImpl(codePoint, getType(codePoint))

  @inline private[this] def isIdentifierIgnorableImpl(codePoint: Int,
      tpe: Int): scala.Boolean = {
    ('\u0000' <= codePoint && codePoint <= '\u0008') ||
    ('\u000E' <= codePoint && codePoint <= '\u001B') ||
    ('\u007F' <= codePoint && codePoint <= '\u009F') ||
    tpe == FORMAT
  }

  def isMirrored(c: scala.Char): scala.Boolean =
    isMirrored(c.toInt)

  def isMirrored(codePoint: Int): scala.Boolean = {
    val indexOfRange = findIndexOfRange(
        isMirroredIndices, codePoint, hasEmptyRanges = false)
    (indexOfRange & 1) != 0
  }

  //def getDirectionality(c: scala.Char): scala.Byte

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

  //def getNumericValue(c: scala.Char): Int

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
      private val start: Int, private val end: Int) extends Subset(name)

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

    private[this] def addUnicodeBlock(properName: String, start: Int, end: Int): UnicodeBlock =  {
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
        start: Int, end: Int): UnicodeBlock =  {
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

    // scalastyle:off line.size.limit

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

    // scalastyle:on line.size.limit

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
    private[this] def binarySearch(codePoint: scala.Int, lo: scala.Int, hi: scala.Int): UnicodeBlock = {
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

  // Types of characters from 0 to 255
  private[this] lazy val charTypesFirst256: Array[Int] = Array(
    // BEGIN GENERATED: [char-types-first-256]
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 12, 24, 24, 24, 26, 24,
    24, 24, 21, 22, 24, 25, 24, 20, 24, 24, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 24,
    24, 25, 25, 25, 24, 24, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 21, 24, 22, 27, 23, 27, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 21, 25, 22, 25, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 12, 24, 26, 26, 26, 26,
    28, 24, 27, 28, 5, 29, 25, 16, 28, 27, 28, 25, 11, 11, 27, 2, 24, 24, 27,
    11, 5, 30, 11, 11, 11, 24, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 25, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 25, 2, 2, 2, 2, 2, 2, 2, 2
    // END GENERATED: [char-types-first-256]
  )

  /* Character type data by ranges of types
   * charTypeIndices: contains the index where the range ends
   * charType: contains the type of the character in the range ends
   * note that charTypeIndices.length + 1 = charType.length and that the
   * range 0 to 255 is not included because it is contained in charTypesFirst256
   */

  private[this] lazy val charTypeIndices: Array[Int] = {
    val deltas = Array(
        // BEGIN GENERATED: [char-types-indices]
        257, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2,
        1, 1, 1, 1, 3, 2, 1, 1, 1, 2, 1, 3, 2, 4, 1, 2, 1, 3, 3, 2, 1, 2, 1, 1,
        1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 3, 1, 1, 1, 2, 2, 1, 1, 3, 4, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 3, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 7, 2, 1, 2, 2, 1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 1,
        69, 1, 27, 18, 4, 12, 14, 5, 7, 1, 1, 1, 17, 112, 1, 1, 1, 1, 1, 1, 1,
        1, 2, 1, 3, 1, 1, 4, 2, 1, 1, 3, 1, 1, 1, 2, 1, 17, 1, 9, 35, 1, 2, 3,
        3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        5, 1, 1, 1, 1, 1, 2, 2, 51, 48, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 2,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 38, 2,
        1, 6, 41, 1, 1, 2, 2, 1, 1, 45, 1, 1, 1, 2, 1, 2, 1, 1, 8, 27, 4, 4, 2,
        11, 6, 3, 2, 1, 2, 2, 11, 1, 1, 3, 32, 1, 10, 21, 10, 4, 2, 1, 99, 1,
        1, 7, 1, 1, 6, 2, 2, 1, 4, 2, 10, 3, 2, 1, 14, 1, 1, 1, 1, 30, 27, 2,
        89, 11, 1, 14, 10, 33, 9, 2, 1, 3, 1, 2, 1, 2, 22, 4, 1, 9, 1, 3, 1, 5,
        2, 15, 1, 25, 3, 2, 1, 1, 11, 5, 24, 1, 6, 1, 2, 6, 8, 41, 1, 24, 1,
        32, 1, 54, 1, 1, 1, 1, 3, 8, 4, 1, 2, 1, 7, 10, 2, 2, 10, 1, 1, 15, 1,
        2, 1, 8, 2, 2, 2, 22, 1, 7, 1, 1, 3, 4, 2, 1, 1, 3, 4, 2, 2, 2, 2, 1,
        1, 8, 1, 4, 2, 1, 3, 2, 2, 10, 2, 2, 6, 1, 1, 1, 1, 1, 2, 2, 1, 1, 6,
        4, 2, 2, 22, 1, 7, 1, 2, 1, 2, 1, 2, 2, 1, 1, 3, 2, 4, 2, 2, 3, 3, 1,
        7, 4, 1, 1, 7, 10, 2, 3, 1, 1, 10, 2, 1, 1, 9, 1, 3, 1, 22, 1, 7, 1, 2,
        1, 5, 2, 1, 1, 3, 5, 1, 2, 1, 1, 2, 1, 2, 1, 15, 2, 2, 2, 10, 1, 1, 7,
        1, 6, 1, 1, 2, 1, 8, 2, 2, 2, 22, 1, 7, 1, 2, 1, 5, 2, 1, 1, 1, 1, 1,
        4, 2, 2, 2, 2, 1, 7, 2, 1, 4, 2, 1, 3, 2, 2, 10, 1, 1, 6, 10, 1, 1, 1,
        6, 3, 3, 1, 4, 3, 2, 1, 1, 1, 2, 3, 2, 3, 3, 3, 12, 4, 2, 1, 2, 3, 3,
        1, 3, 1, 2, 1, 6, 1, 14, 10, 3, 6, 1, 1, 5, 1, 3, 1, 8, 1, 3, 1, 23, 1,
        16, 2, 1, 1, 3, 4, 1, 3, 1, 4, 7, 2, 1, 3, 2, 1, 2, 2, 2, 2, 10, 7, 1,
        7, 1, 1, 1, 2, 1, 8, 1, 3, 1, 23, 1, 10, 1, 5, 2, 1, 1, 1, 1, 5, 1, 1,
        2, 1, 2, 2, 7, 2, 6, 2, 1, 2, 2, 2, 10, 1, 2, 1, 12, 2, 2, 9, 1, 3, 1,
        41, 2, 1, 3, 4, 1, 3, 1, 3, 1, 1, 1, 4, 3, 1, 7, 3, 2, 2, 10, 9, 1, 6,
        1, 1, 2, 1, 18, 3, 24, 1, 9, 1, 1, 2, 7, 3, 1, 4, 3, 3, 1, 1, 1, 8, 6,
        10, 2, 2, 1, 12, 48, 1, 2, 7, 4, 1, 6, 1, 8, 1, 10, 2, 37, 2, 1, 1, 1,
        5, 1, 24, 1, 1, 1, 10, 1, 2, 9, 1, 2, 5, 1, 1, 1, 7, 1, 10, 2, 4, 32,
        1, 3, 15, 1, 1, 3, 2, 6, 10, 10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 8, 1,
        36, 4, 14, 1, 5, 1, 2, 5, 11, 1, 36, 1, 8, 1, 6, 1, 2, 5, 4, 2, 37, 43,
        2, 4, 1, 6, 1, 2, 2, 2, 1, 10, 6, 6, 2, 2, 4, 3, 1, 3, 2, 7, 3, 4, 13,
        1, 2, 2, 6, 1, 1, 1, 10, 3, 1, 2, 38, 1, 1, 5, 1, 2, 43, 1, 1, 3, 329,
        1, 4, 2, 7, 1, 1, 1, 4, 2, 41, 1, 4, 2, 33, 1, 4, 2, 7, 1, 1, 1, 4, 2,
        15, 1, 57, 1, 4, 2, 67, 2, 3, 9, 20, 3, 16, 10, 6, 86, 2, 6, 2, 1, 620,
        1, 1, 17, 1, 26, 1, 1, 3, 75, 3, 3, 8, 7, 18, 3, 1, 9, 19, 2, 1, 2, 9,
        18, 2, 12, 13, 1, 3, 1, 2, 12, 52, 2, 1, 7, 8, 1, 2, 11, 3, 1, 3, 1, 1,
        1, 2, 10, 6, 10, 6, 6, 1, 4, 3, 1, 1, 10, 6, 35, 1, 53, 7, 5, 2, 34, 1,
        1, 5, 70, 10, 31, 1, 3, 4, 2, 3, 4, 2, 1, 6, 3, 4, 1, 3, 2, 10, 30, 2,
        5, 11, 44, 4, 26, 6, 10, 1, 3, 34, 23, 2, 2, 1, 2, 2, 53, 1, 1, 1, 7,
        1, 1, 1, 1, 2, 8, 6, 10, 2, 1, 10, 6, 10, 6, 7, 1, 6, 2, 14, 1, 16, 49,
        4, 1, 47, 1, 1, 5, 1, 1, 5, 1, 2, 8, 3, 10, 7, 10, 9, 9, 2, 1, 2, 1,
        30, 1, 4, 2, 2, 1, 3, 2, 10, 44, 1, 1, 2, 3, 1, 1, 3, 2, 8, 4, 36, 8,
        8, 2, 2, 3, 5, 10, 3, 3, 10, 30, 6, 2, 9, 7, 43, 2, 3, 8, 8, 3, 1, 13,
        1, 7, 4, 1, 6, 1, 2, 1, 2, 1, 5, 44, 63, 13, 1, 34, 37, 64, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 9, 8, 6, 2, 6, 2, 8, 8, 8, 8, 6, 2, 6, 2, 8, 1, 1, 1, 1, 1, 1,
        1, 1, 8, 8, 14, 2, 8, 8, 8, 8, 8, 8, 5, 1, 2, 4, 1, 1, 1, 3, 3, 1, 2,
        4, 1, 3, 4, 2, 2, 4, 1, 3, 8, 5, 3, 2, 3, 1, 2, 4, 1, 2, 1, 11, 5, 6,
        2, 1, 1, 1, 2, 1, 1, 1, 8, 1, 1, 5, 1, 9, 1, 1, 4, 2, 3, 1, 1, 1, 11,
        1, 1, 1, 10, 1, 5, 1, 10, 1, 1, 2, 6, 3, 1, 1, 1, 10, 3, 1, 1, 1, 13,
        3, 33, 15, 13, 4, 1, 3, 12, 15, 2, 1, 4, 1, 2, 1, 3, 2, 3, 1, 1, 1, 2,
        1, 5, 6, 1, 1, 1, 1, 1, 1, 4, 1, 1, 4, 1, 4, 1, 2, 2, 2, 5, 1, 4, 1, 1,
        2, 1, 1, 16, 35, 1, 1, 4, 1, 2, 4, 5, 5, 2, 4, 1, 2, 1, 2, 1, 7, 1, 31,
        2, 2, 1, 1, 1, 31, 268, 8, 1, 1, 1, 1, 20, 2, 7, 1, 1, 81, 1, 30, 25,
        40, 6, 69, 25, 11, 21, 60, 78, 22, 183, 1, 9, 1, 54, 8, 111, 1, 248, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 30, 44, 5, 1, 1, 31, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 16, 256, 131, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 63, 1, 1, 1, 1, 32, 1, 1, 258, 48, 21, 2, 6,
        39, 2, 32, 1, 105, 48, 48, 1, 1, 3, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 2, 1,
        6, 2, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 2, 6, 1, 1, 1, 1, 3, 1, 1, 5, 4, 1, 2, 38, 1, 1, 5, 1,
        2, 56, 7, 1, 1, 14, 1, 23, 9, 7, 1, 7, 1, 7, 1, 7, 1, 7, 1, 7, 1, 7, 1,
        7, 1, 32, 2, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 9, 1, 2, 1, 1, 1, 1, 2, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 1, 10, 2, 4, 1, 1, 1, 13, 2, 3, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 34, 26, 1, 89, 12, 214, 26, 12, 4, 1, 3, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 9,
        4, 2, 1, 5, 2, 3, 1, 1, 1, 2, 1, 86, 2, 2, 2, 2, 1, 1, 90, 1, 3, 1, 5,
        43, 1, 94, 1, 2, 4, 10, 32, 36, 12, 16, 31, 1, 10, 30, 8, 1, 15, 32,
        10, 39, 15, 320, 6592, 64, 21013, 1, 1143, 3, 55, 9, 40, 6, 2, 268, 1,
        3, 16, 10, 2, 20, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 3, 1, 10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 70, 10, 2, 6, 8,
        23, 9, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 8, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 5, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 4, 1, 1, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 24, 3, 1, 1, 1, 2, 1,
        7, 1, 3, 1, 4, 1, 23, 2, 2, 1, 4, 1, 3, 6, 2, 1, 1, 6, 52, 4, 8, 2, 50,
        16, 2, 8, 2, 10, 6, 18, 6, 3, 1, 1, 2, 1, 10, 28, 8, 2, 23, 11, 2, 11,
        1, 29, 3, 3, 1, 47, 1, 2, 4, 2, 2, 3, 13, 1, 1, 10, 4, 2, 5, 1, 1, 9,
        10, 5, 1, 41, 6, 2, 2, 2, 2, 9, 3, 1, 8, 1, 1, 2, 10, 2, 4, 16, 1, 6,
        3, 1, 1, 1, 1, 50, 1, 1, 3, 2, 2, 5, 2, 1, 1, 1, 24, 2, 1, 2, 11, 1, 2,
        2, 2, 1, 2, 1, 1, 10, 6, 2, 6, 2, 6, 9, 7, 1, 7, 1, 43, 1, 4, 9, 1, 2,
        4, 80, 35, 2, 1, 2, 1, 2, 1, 1, 1, 2, 10, 6, 11172, 12, 23, 4, 49, 4,
        2048, 6400, 366, 2, 106, 38, 7, 12, 5, 5, 1, 1, 10, 1, 13, 1, 5, 1, 1,
        1, 2, 1, 2, 1, 108, 17, 16, 363, 1, 1, 16, 64, 2, 54, 7, 1, 32, 12, 1,
        3, 16, 7, 1, 1, 1, 6, 16, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 2, 1, 1, 4, 3, 3, 1, 4, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 3, 1,
        1, 1, 2, 4, 5, 1, 135, 2, 1, 1, 3, 1, 3, 1, 1, 1, 1, 1, 1, 2, 10, 2, 3,
        2, 26, 1, 1, 1, 1, 1, 1, 26, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 10, 1, 45,
        2, 31, 3, 6, 2, 6, 2, 6, 2, 3, 3, 2, 1, 1, 1, 2, 1, 1, 4, 2, 10, 3, 2,
        2, 12, 1, 26, 1, 19, 1, 2, 1, 15, 2, 14, 34, 123, 5, 3, 4, 45, 3, 9,
        53, 4, 17, 2, 3, 1, 13, 3, 1, 47, 45, 1, 130, 29, 3, 49, 15, 1, 27, 4,
        32, 4, 9, 20, 1, 8, 1, 5, 38, 5, 5, 30, 1, 1, 36, 4, 8, 1, 5, 42, 40,
        40, 78, 2, 10, 6, 36, 4, 36, 4, 40, 8, 52, 11, 1, 11, 1, 15, 1, 7, 1,
        2, 1, 11, 1, 15, 1, 7, 1, 2, 67, 311, 9, 22, 10, 8, 24, 6, 1, 42, 1, 9,
        69, 6, 2, 1, 1, 44, 1, 2, 3, 1, 2, 23, 1, 1, 8, 23, 2, 7, 31, 8, 9, 48,
        19, 1, 2, 5, 5, 22, 6, 3, 1, 26, 5, 1, 64, 56, 4, 2, 2, 16, 2, 46, 1,
        3, 1, 2, 5, 4, 4, 1, 3, 1, 29, 2, 3, 4, 1, 9, 7, 9, 7, 29, 2, 1, 29, 3,
        32, 8, 1, 28, 2, 4, 5, 7, 9, 54, 3, 7, 22, 2, 8, 19, 5, 8, 18, 7, 4,
        12, 7, 80, 73, 55, 51, 13, 51, 7, 6, 36, 4, 8, 10, 294, 31, 1, 42, 1,
        2, 1, 2, 2, 75, 3, 29, 10, 1, 8, 22, 11, 4, 5, 22, 18, 4, 4, 38, 21, 7,
        20, 23, 9, 1, 1, 1, 53, 15, 7, 4, 20, 10, 1, 2, 2, 1, 9, 3, 1, 45, 3,
        4, 2, 2, 2, 1, 4, 1, 10, 1, 2, 25, 7, 10, 6, 3, 36, 5, 1, 8, 1, 10, 4,
        1, 2, 1, 8, 35, 1, 2, 1, 9, 2, 1, 48, 3, 9, 2, 4, 4, 4, 1, 1, 1, 10, 1,
        1, 1, 3, 1, 20, 11, 18, 1, 25, 3, 3, 2, 1, 1, 2, 6, 1, 2, 1, 62, 7, 1,
        1, 1, 4, 1, 15, 1, 10, 1, 6, 47, 1, 3, 8, 5, 10, 6, 2, 2, 1, 8, 2, 2,
        2, 22, 1, 7, 1, 2, 1, 5, 1, 2, 1, 2, 1, 4, 2, 2, 2, 3, 2, 1, 6, 1, 5,
        5, 2, 2, 7, 3, 5, 139, 53, 3, 8, 2, 3, 1, 1, 4, 5, 10, 2, 1, 1, 1, 3,
        30, 48, 3, 6, 1, 1, 4, 2, 1, 2, 2, 1, 1, 8, 10, 166, 47, 3, 4, 2, 4, 2,
        1, 2, 23, 4, 2, 34, 48, 3, 8, 2, 1, 1, 2, 3, 1, 11, 10, 6, 13, 19, 43,
        1, 1, 1, 2, 6, 1, 1, 1, 1, 6, 10, 54, 27, 2, 3, 2, 4, 1, 5, 4, 10, 2,
        3, 1, 7, 185, 44, 3, 9, 1, 2, 1, 100, 32, 32, 10, 9, 12, 8, 2, 1, 2, 8,
        1, 2, 1, 24, 6, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 3, 9, 10, 70, 8, 2,
        39, 3, 4, 2, 2, 4, 1, 1, 1, 1, 1, 27, 1, 10, 40, 6, 1, 1, 4, 8, 1, 8,
        1, 6, 2, 3, 46, 13, 1, 2, 3, 1, 5, 13, 73, 7, 10, 246, 9, 1, 37, 1, 7,
        1, 6, 1, 1, 1, 5, 10, 10, 19, 3, 2, 30, 2, 22, 1, 1, 7, 1, 2, 1, 2, 73,
        7, 1, 2, 1, 38, 6, 3, 1, 1, 2, 1, 7, 1, 1, 8, 10, 6, 6, 1, 2, 1, 32, 5,
        1, 2, 1, 2, 1, 1, 1, 1, 7, 10, 310, 19, 2, 2, 2, 7, 2, 1, 1, 13, 1, 34,
        2, 5, 3, 2, 1, 1, 1, 13, 10, 86, 1, 15, 21, 8, 4, 17, 13, 1, 922, 102,
        111, 1, 5, 11, 196, 2636, 97, 2, 13, 1072, 16, 1, 6, 15, 4010, 583,
        8633, 569, 7, 31, 1, 10, 4, 2, 79, 1, 10, 6, 30, 2, 5, 1, 10, 48, 7, 5,
        4, 4, 1, 1, 10, 10, 1, 7, 1, 21, 5, 19, 688, 32, 32, 23, 4, 101, 75, 4,
        1, 1, 55, 7, 4, 13, 64, 2, 1, 1, 1, 11, 2, 14, 6136, 8, 1238, 42, 9,
        8935, 4, 1, 7, 1, 2, 1, 291, 15, 1, 29, 3, 2, 1, 14, 4, 8, 396, 2308,
        107, 5, 13, 3, 9, 7, 10, 2, 1, 2, 1, 4, 4700, 46, 2, 23, 9, 116, 60,
        246, 10, 39, 2, 60, 2, 3, 3, 6, 8, 8, 2, 7, 30, 4, 61, 21, 66, 3, 1,
        122, 20, 12, 20, 12, 87, 9, 25, 135, 26, 26, 26, 7, 1, 18, 26, 26, 1,
        1, 2, 2, 1, 2, 2, 2, 4, 1, 8, 4, 1, 1, 1, 7, 1, 11, 26, 26, 2, 1, 4, 2,
        8, 1, 7, 1, 26, 2, 1, 4, 1, 5, 1, 1, 3, 7, 1, 26, 26, 26, 26, 26, 26,
        26, 26, 26, 26, 26, 26, 28, 2, 25, 1, 25, 1, 6, 25, 1, 25, 1, 6, 25, 1,
        25, 1, 6, 25, 1, 25, 1, 6, 25, 1, 25, 1, 6, 1, 1, 2, 50, 512, 55, 4,
        50, 8, 1, 14, 1, 2, 5, 15, 5, 1, 15, 1104, 10, 1, 20, 6, 6, 213, 7, 1,
        17, 2, 7, 1, 2, 1, 5, 5, 62, 33, 1, 112, 45, 3, 7, 7, 2, 10, 4, 1, 1,
        320, 30, 1, 17, 44, 4, 10, 5, 1, 464, 27, 1, 4, 10, 742, 7, 1, 4, 1, 2,
        1, 15, 1, 197, 2, 9, 7, 41, 34, 34, 7, 1, 4, 10, 4, 2, 785, 59, 1, 3,
        1, 4, 76, 45, 1, 15, 194, 4, 1, 27, 1, 2, 1, 1, 2, 1, 1, 10, 1, 4, 1,
        1, 1, 1, 6, 1, 4, 1, 1, 1, 1, 1, 1, 3, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 2, 1, 1, 2, 4, 1, 7, 1, 4, 1, 4, 1, 1, 1, 10, 1, 17, 5, 3,
        1, 5, 1, 17, 52, 2, 270, 44, 4, 100, 12, 15, 2, 15, 1, 15, 1, 37, 10,
        13, 161, 56, 29, 13, 44, 4, 9, 7, 2, 14, 6, 154, 251, 5, 728, 4, 17, 3,
        13, 3, 119, 4, 95, 6, 12, 4, 1, 15, 12, 4, 56, 8, 10, 6, 40, 8, 30, 2,
        2, 78, 340, 12, 14, 2, 13, 3, 9, 7, 46, 1, 7, 8, 14, 4, 9, 7, 9, 7,
        147, 1, 55, 37, 10, 1030, 42720, 32, 4154, 6, 222, 2, 5762, 14, 7473,
        3103, 542, 1506, 4939, 5, 4192, 711761, 1, 30, 96, 128, 240, 65040,
        65534, 2, 65534
        // END GENERATED: [char-types-indices]
    )
    uncompressDeltas(deltas)
  }

  private[this] lazy val charTypes: Array[Int] = Array(
      // BEGIN GENERATED: [char-types]
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 5, 1, 2, 5, 1, 3, 2, 1,
      3, 2, 1, 3, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 3, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
      5, 2, 4, 27, 4, 27, 4, 27, 4, 27, 4, 27, 6, 1, 2, 1, 2, 4, 27, 1, 2, 0,
      4, 2, 24, 1, 0, 27, 1, 24, 1, 0, 1, 0, 1, 2, 1, 0, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 25, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 28, 6, 7, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0, 1, 0, 4, 24, 2, 24,
      20, 0, 28, 26, 0, 6, 20, 6, 24, 6, 24, 6, 24, 6, 0, 5, 0, 5, 24, 0, 16,
      25, 24, 26, 24, 28, 6, 24, 16, 24, 5, 4, 5, 6, 9, 24, 5, 6, 5, 24, 5, 6,
      16, 28, 6, 4, 6, 28, 6, 5, 9, 5, 28, 5, 24, 0, 16, 5, 6, 5, 6, 0, 5, 6,
      5, 0, 9, 5, 6, 4, 28, 24, 4, 0, 6, 26, 5, 6, 4, 6, 4, 6, 4, 6, 0, 24, 0,
      5, 6, 0, 24, 0, 5, 0, 5, 27, 5, 0, 16, 0, 6, 5, 4, 6, 16, 6, 8, 5, 6, 8,
      6, 5, 8, 6, 8, 6, 8, 5, 6, 5, 6, 24, 9, 24, 4, 5, 6, 8, 0, 5, 0, 5, 0, 5,
      0, 5, 0, 5, 0, 5, 0, 6, 5, 8, 6, 0, 8, 0, 8, 6, 5, 0, 8, 0, 5, 0, 5, 6,
      0, 9, 5, 26, 11, 28, 26, 5, 24, 6, 0, 6, 8, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5,
      0, 5, 0, 5, 0, 6, 0, 8, 6, 0, 6, 0, 6, 0, 6, 0, 5, 0, 5, 0, 9, 6, 5, 6,
      24, 0, 6, 8, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 6, 5, 8, 6, 0, 6, 8,
      0, 8, 6, 0, 5, 0, 5, 6, 0, 9, 24, 26, 0, 5, 6, 0, 6, 8, 0, 5, 0, 5, 0, 5,
      0, 5, 0, 5, 0, 5, 0, 6, 5, 8, 6, 8, 6, 0, 8, 0, 8, 6, 0, 6, 8, 0, 5, 0,
      5, 6, 0, 9, 28, 5, 11, 0, 6, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5,
      0, 5, 0, 5, 0, 8, 6, 8, 0, 8, 0, 8, 6, 0, 5, 0, 8, 0, 9, 11, 28, 26, 28,
      0, 6, 8, 6, 5, 0, 5, 0, 5, 0, 5, 0, 6, 5, 6, 8, 0, 6, 0, 6, 0, 6, 0, 5,
      0, 5, 0, 5, 6, 0, 9, 0, 24, 11, 28, 5, 6, 8, 24, 5, 0, 5, 0, 5, 0, 5, 0,
      5, 0, 6, 5, 8, 6, 8, 0, 6, 8, 0, 8, 6, 0, 8, 0, 5, 0, 5, 6, 0, 9, 0, 5,
      8, 0, 6, 8, 5, 0, 5, 0, 5, 6, 5, 8, 6, 0, 8, 0, 8, 6, 5, 28, 0, 5, 8, 11,
      5, 6, 0, 9, 11, 28, 5, 0, 6, 8, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 6, 0, 8,
      6, 0, 6, 0, 8, 0, 9, 0, 8, 24, 0, 5, 6, 5, 6, 0, 26, 5, 4, 6, 24, 9, 24,
      0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 6, 5, 6, 5, 0, 5, 0, 4, 0, 6, 0, 9,
      0, 5, 0, 5, 28, 24, 28, 24, 28, 6, 28, 9, 11, 28, 6, 28, 6, 28, 6, 21,
      22, 21, 22, 8, 5, 0, 5, 0, 6, 8, 6, 24, 6, 5, 6, 0, 6, 0, 28, 6, 28, 0,
      28, 24, 28, 24, 0, 5, 8, 6, 8, 6, 8, 6, 8, 6, 5, 9, 24, 5, 8, 6, 5, 6, 5,
      8, 5, 8, 5, 6, 5, 6, 8, 6, 8, 6, 5, 8, 9, 8, 6, 28, 1, 0, 1, 0, 1, 0, 2,
      24, 4, 2, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5,
      0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 6, 24, 11, 0, 5, 28, 0, 1, 0, 2, 0, 20,
      5, 28, 24, 5, 12, 5, 21, 22, 0, 5, 24, 10, 5, 0, 5, 6, 8, 0, 5, 6, 8, 24,
      0, 5, 6, 0, 5, 0, 5, 0, 6, 0, 5, 6, 8, 6, 8, 6, 8, 6, 24, 4, 24, 26, 5,
      6, 0, 9, 0, 11, 0, 24, 20, 24, 6, 16, 6, 9, 0, 5, 4, 5, 0, 5, 6, 5, 6, 5,
      0, 5, 0, 5, 0, 6, 8, 6, 8, 0, 8, 6, 8, 6, 0, 28, 0, 24, 9, 5, 0, 5, 0, 5,
      0, 5, 0, 9, 11, 0, 28, 5, 6, 8, 6, 0, 24, 5, 8, 6, 8, 6, 0, 6, 8, 6, 8,
      6, 8, 6, 0, 6, 9, 0, 9, 0, 24, 4, 24, 0, 6, 7, 6, 0, 6, 8, 5, 6, 8, 6, 8,
      6, 8, 6, 8, 5, 0, 9, 24, 28, 6, 28, 24, 0, 6, 8, 5, 8, 6, 8, 6, 8, 6, 5,
      9, 5, 6, 8, 6, 8, 6, 8, 6, 8, 0, 24, 5, 8, 6, 8, 6, 0, 24, 9, 0, 5, 9, 5,
      4, 24, 2, 0, 1, 0, 1, 24, 0, 6, 24, 6, 8, 6, 5, 6, 5, 6, 5, 8, 6, 5, 0,
      2, 4, 2, 4, 2, 4, 6, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0, 1, 0, 2, 1, 2, 1,
      2, 0, 1, 0, 2, 0, 1, 0, 1, 0, 1, 0, 1, 2, 1, 2, 0, 2, 3, 2, 3, 2, 3, 2,
      0, 2, 1, 3, 27, 2, 27, 2, 0, 2, 1, 3, 27, 2, 0, 2, 1, 0, 27, 2, 1, 27, 0,
      2, 0, 2, 1, 3, 27, 0, 12, 16, 20, 24, 29, 30, 21, 29, 30, 21, 29, 24, 13,
      14, 16, 12, 24, 29, 30, 24, 23, 24, 25, 21, 22, 24, 25, 24, 23, 24, 12,
      16, 0, 16, 11, 4, 0, 11, 25, 21, 22, 4, 11, 25, 21, 22, 0, 4, 0, 26, 0,
      6, 7, 6, 7, 6, 0, 28, 1, 28, 1, 28, 2, 1, 2, 1, 2, 28, 1, 28, 25, 1, 28,
      1, 28, 1, 28, 1, 28, 1, 28, 2, 1, 2, 5, 2, 28, 2, 1, 25, 1, 2, 28, 25,
      28, 2, 28, 11, 10, 1, 2, 10, 11, 28, 0, 25, 28, 25, 28, 25, 28, 25, 28,
      25, 28, 25, 28, 25, 28, 25, 28, 25, 28, 25, 28, 21, 22, 21, 22, 28, 25,
      28, 21, 22, 28, 25, 28, 25, 28, 25, 28, 0, 28, 0, 11, 28, 11, 28, 25, 28,
      25, 28, 25, 28, 25, 28, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22,
      21, 22, 11, 28, 25, 21, 22, 25, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22,
      25, 28, 25, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22, 21,
      22, 21, 22, 21, 22, 21, 22, 25, 21, 22, 21, 22, 25, 21, 22, 25, 28, 25,
      28, 25, 28, 0, 28, 0, 28, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 4, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 28, 1, 2, 1, 2, 6, 1, 2, 0, 24, 11, 24, 2, 0, 2, 0,
      2, 0, 5, 0, 4, 24, 0, 6, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0,
      5, 0, 6, 24, 29, 30, 29, 30, 24, 29, 30, 24, 29, 30, 24, 20, 24, 20, 24,
      29, 30, 24, 29, 30, 21, 22, 21, 22, 21, 22, 21, 22, 24, 4, 24, 20, 24,
      20, 24, 21, 24, 28, 24, 21, 22, 21, 22, 21, 22, 21, 22, 20, 0, 28, 0, 28,
      0, 28, 0, 28, 0, 12, 24, 28, 4, 5, 10, 21, 22, 21, 22, 21, 22, 21, 22,
      21, 22, 28, 21, 22, 21, 22, 21, 22, 21, 22, 20, 21, 22, 28, 10, 6, 8, 20,
      4, 28, 10, 4, 5, 24, 28, 0, 5, 0, 6, 27, 4, 5, 20, 5, 24, 4, 5, 0, 5, 0,
      5, 0, 28, 11, 28, 5, 28, 0, 5, 28, 0, 11, 28, 11, 28, 11, 28, 11, 28, 11,
      28, 5, 28, 5, 4, 5, 0, 28, 0, 5, 4, 24, 5, 4, 24, 5, 9, 5, 0, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 5, 6, 7, 24, 6, 24,
      4, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 4, 6, 5, 10, 6, 24, 0, 27, 4, 27, 1, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 4, 2, 1, 2,
      1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 4, 27, 1, 2, 1, 2, 5, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0, 1, 2, 0, 2, 0, 2,
      1, 2, 1, 2, 0, 4, 1, 2, 5, 4, 2, 5, 6, 5, 6, 5, 6, 5, 8, 6, 8, 28, 6, 0,
      11, 28, 26, 28, 0, 5, 24, 0, 8, 5, 8, 6, 0, 24, 9, 0, 6, 5, 24, 5, 24, 5,
      6, 9, 5, 6, 24, 5, 6, 8, 0, 24, 5, 0, 6, 8, 5, 6, 8, 6, 8, 6, 8, 24, 0,
      4, 9, 0, 24, 5, 6, 4, 5, 9, 5, 0, 5, 6, 8, 6, 8, 6, 0, 5, 6, 5, 6, 8, 0,
      9, 0, 24, 5, 4, 5, 28, 5, 8, 6, 8, 5, 6, 5, 6, 5, 6, 5, 6, 5, 6, 5, 0, 5,
      4, 24, 5, 8, 6, 8, 24, 5, 4, 8, 6, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 2,
      27, 4, 2, 4, 27, 0, 2, 5, 8, 6, 8, 6, 8, 24, 8, 6, 0, 9, 0, 5, 0, 5, 0,
      5, 0, 19, 18, 5, 0, 5, 0, 2, 0, 2, 0, 5, 6, 5, 25, 5, 0, 5, 0, 5, 0, 5,
      0, 5, 0, 5, 27, 0, 5, 22, 21, 28, 5, 0, 5, 0, 28, 0, 5, 26, 28, 6, 24,
      21, 22, 24, 0, 6, 24, 20, 23, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22, 21,
      22, 21, 22, 21, 22, 24, 21, 22, 24, 23, 24, 0, 24, 20, 21, 22, 21, 22,
      21, 22, 24, 25, 20, 25, 0, 24, 26, 24, 0, 5, 0, 5, 0, 16, 0, 24, 26, 24,
      21, 22, 24, 25, 24, 20, 24, 9, 24, 25, 24, 1, 21, 24, 22, 27, 23, 27, 2,
      21, 25, 22, 25, 21, 22, 24, 21, 22, 24, 5, 4, 5, 4, 5, 0, 5, 0, 5, 0, 5,
      0, 5, 0, 26, 25, 27, 28, 26, 0, 28, 25, 28, 0, 16, 28, 0, 5, 0, 5, 0, 5,
      0, 5, 0, 5, 0, 5, 0, 5, 0, 24, 0, 11, 0, 28, 10, 11, 28, 11, 28, 0, 28,
      0, 28, 0, 28, 6, 0, 5, 0, 5, 0, 6, 11, 0, 5, 11, 0, 5, 10, 5, 10, 0, 5,
      6, 0, 5, 0, 24, 5, 0, 5, 24, 10, 0, 1, 2, 5, 0, 9, 0, 1, 0, 2, 0, 5, 0,
      5, 0, 24, 1, 0, 1, 0, 1, 0, 1, 0, 2, 0, 2, 0, 2, 0, 2, 0, 5, 0, 5, 0, 5,
      0, 4, 0, 4, 0, 4, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 24, 11, 5, 28,
      11, 5, 0, 11, 0, 5, 0, 5, 0, 11, 5, 11, 0, 24, 5, 0, 24, 0, 5, 0, 11, 5,
      11, 0, 11, 5, 6, 0, 6, 0, 6, 5, 0, 5, 0, 5, 0, 6, 0, 6, 11, 0, 24, 0, 5,
      11, 24, 5, 11, 0, 5, 28, 5, 6, 0, 11, 24, 0, 5, 0, 24, 5, 0, 11, 5, 0,
      11, 5, 0, 24, 0, 11, 0, 5, 0, 1, 0, 2, 0, 11, 5, 6, 0, 9, 0, 11, 0, 5, 0,
      6, 20, 0, 5, 0, 6, 5, 11, 5, 0, 5, 6, 11, 24, 0, 5, 6, 24, 0, 5, 11, 0,
      5, 0, 8, 6, 8, 5, 6, 24, 0, 11, 9, 6, 5, 6, 5, 0, 6, 8, 5, 8, 6, 8, 6,
      24, 16, 24, 6, 0, 16, 0, 5, 0, 9, 0, 6, 5, 6, 8, 6, 0, 9, 24, 5, 8, 5, 0,
      5, 6, 24, 5, 0, 6, 8, 5, 8, 6, 8, 5, 24, 6, 24, 8, 6, 9, 5, 24, 5, 24, 0,
      11, 0, 5, 0, 5, 8, 6, 8, 6, 8, 6, 24, 6, 5, 6, 0, 5, 0, 5, 0, 5, 0, 5, 0,
      5, 24, 0, 5, 6, 8, 6, 0, 9, 0, 6, 8, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5,
      0, 6, 5, 8, 6, 8, 0, 8, 0, 8, 0, 5, 0, 8, 0, 5, 8, 0, 6, 0, 6, 0, 5, 8,
      6, 8, 6, 8, 6, 5, 24, 9, 24, 0, 24, 6, 5, 0, 5, 8, 6, 8, 6, 8, 6, 8, 6,
      5, 24, 5, 0, 9, 0, 5, 8, 6, 0, 8, 6, 8, 6, 24, 5, 6, 0, 5, 8, 6, 8, 6, 8,
      6, 24, 5, 0, 9, 0, 24, 0, 5, 6, 8, 6, 8, 6, 8, 6, 5, 24, 0, 9, 0, 5, 0,
      6, 8, 6, 8, 6, 0, 9, 11, 24, 28, 5, 0, 5, 8, 6, 8, 6, 24, 0, 1, 2, 9, 11,
      0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 8, 0, 8, 0, 6, 8, 6, 5, 8, 5, 8, 6, 24, 0,
      9, 0, 5, 0, 5, 8, 6, 0, 6, 8, 6, 5, 24, 5, 8, 0, 5, 6, 5, 6, 8, 5, 6, 24,
      6, 0, 5, 6, 8, 6, 5, 6, 8, 6, 24, 5, 24, 0, 5, 0, 24, 0, 5, 0, 5, 8, 6,
      0, 6, 8, 6, 5, 24, 0, 9, 11, 0, 24, 5, 0, 6, 0, 8, 6, 8, 6, 8, 6, 0, 5,
      0, 5, 0, 5, 6, 0, 6, 0, 6, 0, 6, 5, 6, 0, 9, 0, 5, 0, 5, 0, 5, 8, 0, 6,
      0, 8, 6, 8, 6, 5, 0, 9, 0, 5, 6, 8, 24, 0, 6, 5, 8, 5, 0, 5, 8, 6, 0, 8,
      6, 8, 6, 24, 9, 0, 5, 0, 11, 28, 26, 28, 0, 24, 5, 0, 10, 0, 24, 0, 5, 0,
      5, 24, 0, 5, 16, 6, 5, 6, 0, 5, 0, 5, 0, 5, 0, 9, 0, 24, 5, 0, 9, 0, 5,
      0, 6, 24, 0, 5, 6, 24, 28, 4, 24, 28, 0, 9, 0, 11, 0, 5, 0, 5, 0, 1, 2,
      11, 24, 0, 5, 0, 6, 5, 8, 0, 6, 4, 0, 4, 24, 4, 6, 0, 8, 0, 5, 0, 5, 0,
      5, 0, 4, 0, 4, 0, 4, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0,
      5, 0, 5, 0, 28, 6, 24, 16, 0, 6, 0, 6, 0, 28, 0, 28, 0, 28, 0, 28, 8, 6,
      28, 8, 16, 6, 28, 6, 28, 6, 28, 0, 28, 6, 28, 0, 11, 0, 11, 0, 28, 0, 11,
      0, 1, 2, 1, 2, 0, 2, 1, 2, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 2, 0, 2, 0,
      2, 0, 2, 1, 2, 1, 0, 1, 0, 1, 0, 1, 0, 2, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
      2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0, 1, 25, 2, 25, 2, 1, 25, 2, 25,
      2, 1, 25, 2, 25, 2, 1, 25, 2, 25, 2, 1, 25, 2, 25, 2, 1, 2, 0, 9, 28, 6,
      28, 6, 28, 6, 28, 6, 28, 24, 0, 6, 0, 6, 0, 2, 5, 2, 0, 2, 0, 6, 0, 6, 0,
      6, 0, 6, 0, 6, 0, 4, 0, 6, 0, 5, 0, 6, 4, 0, 9, 0, 5, 28, 0, 5, 6, 0, 5,
      6, 9, 0, 26, 0, 5, 4, 6, 9, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 11, 6, 0, 1,
      2, 6, 4, 0, 9, 0, 24, 0, 11, 28, 11, 26, 11, 0, 11, 28, 11, 0, 5, 0, 5,
      0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5,
      0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5,
      0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 25, 0, 28, 0, 28, 0, 28, 0,
      28, 0, 28, 0, 28, 0, 11, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28,
      27, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0,
      28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28,
      0, 28, 0, 28, 0, 28, 0, 9, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0,
      5, 0, 16, 0, 16, 0, 6, 0, 18, 0, 18, 0
      // END GENERATED: [char-types]
  )

  /* Indices representing the start of ranges of codePoint that have the same
   * `isMirrored` result. It is true for the first range
   * (i.e. isMirrored(40)==true, isMirrored(41)==true, isMirrored(42)==false)
   */
  private[this] lazy val isMirroredIndices: Array[Int] = {
    val deltas = Array(
        // BEGIN GENERATED: [mirrored-indices]
        40, 2, 18, 1, 1, 1, 28, 1, 1, 1, 29, 1, 1, 1, 45, 1, 15, 1, 3710, 4,
        1885, 2, 2460, 2, 10, 2, 54, 2, 14, 2, 177, 1, 192, 4, 3, 6, 3, 1, 3,
        2, 3, 4, 1, 4, 1, 1, 1, 1, 4, 9, 5, 1, 1, 18, 5, 4, 9, 2, 1, 1, 1, 8,
        2, 31, 2, 4, 5, 1, 9, 2, 2, 19, 5, 2, 9, 5, 2, 2, 4, 24, 2, 16, 8, 4,
        20, 2, 7, 2, 1085, 14, 74, 1, 2, 4, 1, 2, 1, 3, 5, 4, 5, 3, 3, 14, 403,
        22, 2, 6, 1, 14, 8, 1, 7, 6, 3, 1, 4, 5, 1, 2, 2, 5, 4, 1, 1, 3, 2, 2,
        10, 6, 2, 2, 12, 19, 1, 4, 2, 1, 1, 1, 2, 1, 1, 4, 5, 2, 6, 3, 24, 2,
        11, 2, 4, 4, 1, 2, 2, 2, 4, 43, 2, 8, 1, 40, 5, 1, 1, 1, 3, 5, 5, 3, 4,
        1, 3, 5, 1, 1, 256, 1, 515, 4, 3, 2, 1, 2, 14, 2, 2, 10, 43, 8, 427,
        10, 2, 8, 52797, 6, 5, 2, 162, 2, 18, 1, 1, 1, 28, 1, 1, 1, 29, 1, 1,
        1, 1, 2, 1, 2, 55159, 1, 57, 1, 57, 1, 57, 1, 57, 1
        // END GENERATED: [mirrored-indices]
    )
    uncompressDeltas(deltas)
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
  private[this] lazy val combiningClassNoneOrAboveOrOtherIndices: Array[Int] = {
    val deltas = Array(
        // BEGIN GENERATED: [combining-classes]
        768, 21, 40, 0, 8, 1, 0, 1, 3, 0, 3, 2, 1, 3, 4, 0, 1, 3, 0, 1, 7, 0,
        13, 0, 275, 5, 0, 265, 0, 1, 0, 4, 1, 0, 3, 2, 0, 6, 6, 0, 2, 1, 0, 2,
        2, 0, 1, 14, 1, 0, 1, 1, 0, 2, 1, 1, 1, 1, 0, 1, 72, 8, 3, 48, 0, 8, 0,
        2, 2, 0, 5, 1, 0, 2, 1, 16, 0, 1, 101, 7, 0, 2, 4, 1, 0, 1, 0, 2, 2, 0,
        1, 0, 1, 0, 2, 1, 35, 0, 1, 30, 1, 1, 0, 2, 1, 0, 2, 3, 0, 1, 2, 0, 1,
        1, 0, 3, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 2, 0, 160, 7, 1, 0, 1, 0, 9,
        0, 1, 24, 4, 0, 1, 9, 0, 1, 3, 0, 1, 5, 0, 43, 0, 3, 60, 1, 3, 0, 4, 0,
        42, 5, 5, 0, 14, 0, 1, 0, 1, 0, 2, 1, 0, 2, 1, 0, 3, 6, 0, 3, 1, 0, 2,
        2, 0, 5, 0, 60, 0, 1, 16, 0, 1, 3, 1, 1, 0, 2, 0, 103, 0, 1, 16, 0, 1,
        48, 1, 0, 61, 0, 1, 16, 0, 1, 110, 0, 1, 16, 0, 1, 110, 0, 1, 16, 0, 1,
        127, 0, 1, 110, 0, 1, 16, 0, 1, 7, 0, 2, 101, 0, 1, 16, 0, 1, 109, 0,
        2, 16, 0, 1, 124, 0, 1, 109, 0, 3, 13, 0, 4, 108, 0, 3, 13, 0, 4, 76,
        0, 2, 27, 0, 1, 1, 0, 1, 1, 0, 1, 55, 0, 2, 1, 0, 1, 5, 0, 4, 2, 0, 1,
        1, 2, 1, 1, 2, 0, 62, 0, 1, 112, 0, 1, 1, 0, 2, 82, 0, 1, 719, 3, 0,
        948, 0, 2, 30, 0, 1, 157, 0, 1, 10, 1, 0, 203, 0, 1, 143, 0, 1, 0, 1,
        1, 219, 1, 1, 71, 0, 1, 20, 8, 0, 2, 0, 1, 48, 5, 6, 0, 2, 1, 1, 0, 2,
        0, 2, 2, 0, 5, 1, 0, 4, 0, 101, 0, 1, 15, 0, 1, 38, 1, 1, 0, 7, 0, 54,
        0, 2, 58, 0, 1, 11, 0, 2, 67, 0, 1, 152, 3, 0, 1, 0, 6, 0, 2, 4, 0, 1,
        0, 1, 0, 7, 4, 0, 1, 6, 1, 0, 3, 2, 0, 198, 2, 1, 0, 7, 1, 0, 2, 4, 0,
        37, 5, 0, 1, 2, 0, 1, 1, 720, 2, 2, 0, 4, 3, 0, 2, 0, 4, 1, 0, 3, 0, 2,
        0, 1, 1, 0, 1, 6, 0, 1, 0, 3070, 3, 0, 141, 0, 1, 96, 32, 0, 554, 0, 6,
        105, 0, 2, 30164, 1, 0, 4, 10, 0, 32, 2, 0, 80, 2, 0, 276, 0, 1, 37, 0,
        1, 151, 0, 1, 27, 18, 0, 57, 0, 3, 37, 0, 1, 95, 0, 1, 12, 0, 1, 239,
        1, 0, 1, 2, 1, 2, 2, 0, 5, 2, 0, 1, 1, 0, 52, 0, 1, 246, 0, 1, 20272,
        0, 1, 769, 7, 7, 0, 2, 0, 973, 0, 1, 226, 0, 1, 149, 5, 0, 1682, 0, 1,
        1, 1, 0, 40, 1, 2, 4, 0, 1, 165, 1, 1, 573, 4, 0, 387, 2, 0, 80, 0, 3,
        70, 0, 2, 0, 3, 1, 0, 1, 4, 49, 1, 1, 0, 1, 1, 192, 0, 1, 41, 0, 1, 14,
        0, 1, 57, 0, 2, 69, 3, 0, 48, 0, 2, 62, 0, 1, 76, 0, 1, 9, 0, 1, 106,
        0, 2, 178, 0, 2, 80, 0, 2, 16, 0, 1, 24, 7, 0, 3, 5, 0, 205, 0, 1, 3,
        0, 1, 23, 1, 0, 99, 0, 2, 251, 0, 2, 126, 0, 1, 118, 0, 2, 115, 0, 1,
        269, 0, 2, 258, 0, 2, 4, 0, 1, 156, 0, 1, 83, 0, 1, 18, 0, 1, 81, 0, 1,
        421, 0, 1, 258, 0, 1, 1, 0, 2, 81, 0, 1, 425, 0, 2, 19373, 0, 5, 59, 7,
        0, 1209, 0, 2, 19628, 0, 1, 5318, 0, 5, 3, 0, 6, 8, 0, 8, 2, 5, 2, 30,
        4, 0, 148, 3, 0, 3515, 7, 0, 1, 17, 0, 2, 7, 0, 1, 2, 0, 1, 5, 0, 100,
        1, 0, 160, 7, 0, 375, 1, 0, 61, 4, 0, 508, 0, 3, 0, 1, 0, 992, 0, 7,
        109, 6, 1
        // END GENERATED: [combining-classes]
    )
    uncompressDeltas(deltas)
  }

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

  private[this] def uncompressDeltas(deltas: Array[Int]): Array[Int] = {
    var acc = deltas(0)
    var i = 1
    val len = deltas.length
    while (i != len) {
      acc += deltas(i)
      deltas(i) = acc
      i += 1
    }
    deltas
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
  private[this] lazy val nonASCIIZeroDigitCodePoints: Array[Int] = {
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
