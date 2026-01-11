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

  def isISOControl(codePoint: Int): scala.Boolean =
    (0x00 <= codePoint && codePoint <= 0x1f) || (0x7f <= codePoint && codePoint <= 0x9f)

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

  // Types of characters from 0 to 255
  private[this] val charTypesFirst256: Array[Int] = Array(
      // BEGIN GENERATED: [char-types-first-256]
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 12, 24, 24, 24,
      26, 24, 24, 24, 21, 22, 24, 25, 24, 20, 24, 24, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 24, 24, 25, 25, 25, 24, 24, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 21, 24, 22, 27, 23, 27, 2, 2, 2,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 21,
      25, 22, 25, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      12, 24, 26, 26, 26, 26, 28, 24, 27, 28, 5, 29, 25, 16, 28, 27, 28, 25,
      11, 11, 27, 2, 24, 24, 27, 11, 5, 30, 11, 11, 11, 24, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 25, 1, 1, 1, 1, 1, 1,
      1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      2, 25, 2, 2, 2, 2, 2, 2, 2, 2
      // END GENERATED: [char-types-first-256]
  )

  /* Character type data by ranges of types
   * charTypeIndices: contains the index where the range ends
   * charType: contains the type of the character in the range ends
   * note that charTypeIndices.length + 1 = charType.length and that the
   * range 0 to 255 is not included because it is contained in charTypesFirst256
   */

  private[this] lazy val charTypeIndices: Array[Int] = Array(
      // BEGIN GENERATED: [char-types-indices]
      257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 270,
      271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284,
      285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298,
      299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 313,
      314, 315, 316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 327,
      328, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342,
      343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356,
      357, 358, 359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 370,
      371, 372, 373, 374, 375, 376, 378, 379, 380, 381, 382, 385, 387, 388,
      389, 390, 392, 393, 396, 398, 402, 403, 405, 406, 409, 412, 414, 415,
      417, 418, 419, 420, 421, 422, 424, 425, 426, 428, 429, 430, 432, 433,
      436, 437, 438, 439, 441, 443, 444, 445, 448, 452, 453, 454, 455, 456,
      457, 458, 459, 460, 461, 462, 463, 464, 465, 466, 467, 468, 469, 470,
      471, 472, 473, 474, 475, 476, 478, 479, 480, 481, 482, 483, 484, 485,
      486, 487, 488, 489, 490, 491, 492, 493, 494, 495, 497, 498, 499, 500,
      501, 502, 505, 506, 507, 508, 509, 510, 511, 512, 513, 514, 515, 516,
      517, 518, 519, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529, 530,
      531, 532, 533, 534, 535, 536, 537, 538, 539, 540, 541, 542, 543, 544,
      545, 546, 547, 548, 549, 550, 551, 552, 553, 554, 555, 556, 557, 558,
      559, 560, 561, 562, 563, 570, 572, 573, 575, 577, 578, 579, 583, 584,
      585, 586, 587, 588, 589, 590, 591, 660, 661, 688, 706, 710, 722, 736,
      741, 748, 749, 750, 751, 768, 880, 881, 882, 883, 884, 885, 886, 887,
      888, 890, 891, 894, 895, 896, 900, 902, 903, 904, 907, 908, 909, 910,
      912, 913, 930, 931, 940, 975, 976, 978, 981, 984, 985, 986, 987, 988,
      989, 990, 991, 992, 993, 994, 995, 996, 997, 998, 999, 1000, 1001, 1002,
      1003, 1004, 1005, 1006, 1007, 1012, 1013, 1014, 1015, 1016, 1017, 1019,
      1021, 1072, 1120, 1121, 1122, 1123, 1124, 1125, 1126, 1127, 1128, 1129,
      1130, 1131, 1132, 1133, 1134, 1135, 1136, 1137, 1138, 1139, 1140, 1141,
      1142, 1143, 1144, 1145, 1146, 1147, 1148, 1149, 1150, 1151, 1152, 1153,
      1154, 1155, 1160, 1162, 1163, 1164, 1165, 1166, 1167, 1168, 1169, 1170,
      1171, 1172, 1173, 1174, 1175, 1176, 1177, 1178, 1179, 1180, 1181, 1182,
      1183, 1184, 1185, 1186, 1187, 1188, 1189, 1190, 1191, 1192, 1193, 1194,
      1195, 1196, 1197, 1198, 1199, 1200, 1201, 1202, 1203, 1204, 1205, 1206,
      1207, 1208, 1209, 1210, 1211, 1212, 1213, 1214, 1215, 1216, 1218, 1219,
      1220, 1221, 1222, 1223, 1224, 1225, 1226, 1227, 1228, 1229, 1230, 1232,
      1233, 1234, 1235, 1236, 1237, 1238, 1239, 1240, 1241, 1242, 1243, 1244,
      1245, 1246, 1247, 1248, 1249, 1250, 1251, 1252, 1253, 1254, 1255, 1256,
      1257, 1258, 1259, 1260, 1261, 1262, 1263, 1264, 1265, 1266, 1267, 1268,
      1269, 1270, 1271, 1272, 1273, 1274, 1275, 1276, 1277, 1278, 1279, 1280,
      1281, 1282, 1283, 1284, 1285, 1286, 1287, 1288, 1289, 1290, 1291, 1292,
      1293, 1294, 1295, 1296, 1297, 1298, 1299, 1300, 1301, 1302, 1303, 1304,
      1305, 1306, 1307, 1308, 1309, 1310, 1311, 1312, 1313, 1314, 1315, 1316,
      1317, 1318, 1319, 1320, 1321, 1322, 1323, 1324, 1325, 1326, 1327, 1328,
      1329, 1367, 1369, 1370, 1376, 1417, 1418, 1419, 1421, 1423, 1424, 1425,
      1470, 1471, 1472, 1473, 1475, 1476, 1478, 1479, 1480, 1488, 1515, 1519,
      1523, 1525, 1536, 1542, 1545, 1547, 1548, 1550, 1552, 1563, 1564, 1565,
      1568, 1600, 1601, 1611, 1632, 1642, 1646, 1648, 1649, 1748, 1749, 1750,
      1757, 1758, 1759, 1765, 1767, 1769, 1770, 1774, 1776, 1786, 1789, 1791,
      1792, 1806, 1807, 1808, 1809, 1810, 1840, 1867, 1869, 1958, 1969, 1970,
      1984, 1994, 2027, 2036, 2038, 2039, 2042, 2043, 2045, 2046, 2048, 2070,
      2074, 2075, 2084, 2085, 2088, 2089, 2094, 2096, 2111, 2112, 2137, 2140,
      2142, 2143, 2144, 2155, 2160, 2184, 2185, 2191, 2192, 2194, 2200, 2208,
      2249, 2250, 2274, 2275, 2307, 2308, 2362, 2363, 2364, 2365, 2366, 2369,
      2377, 2381, 2382, 2384, 2385, 2392, 2402, 2404, 2406, 2416, 2417, 2418,
      2433, 2434, 2436, 2437, 2445, 2447, 2449, 2451, 2473, 2474, 2481, 2482,
      2483, 2486, 2490, 2492, 2493, 2494, 2497, 2501, 2503, 2505, 2507, 2509,
      2510, 2511, 2519, 2520, 2524, 2526, 2527, 2530, 2532, 2534, 2544, 2546,
      2548, 2554, 2555, 2556, 2557, 2558, 2559, 2561, 2563, 2564, 2565, 2571,
      2575, 2577, 2579, 2601, 2602, 2609, 2610, 2612, 2613, 2615, 2616, 2618,
      2620, 2621, 2622, 2625, 2627, 2631, 2633, 2635, 2638, 2641, 2642, 2649,
      2653, 2654, 2655, 2662, 2672, 2674, 2677, 2678, 2679, 2689, 2691, 2692,
      2693, 2702, 2703, 2706, 2707, 2729, 2730, 2737, 2738, 2740, 2741, 2746,
      2748, 2749, 2750, 2753, 2758, 2759, 2761, 2762, 2763, 2765, 2766, 2768,
      2769, 2784, 2786, 2788, 2790, 2800, 2801, 2802, 2809, 2810, 2816, 2817,
      2818, 2820, 2821, 2829, 2831, 2833, 2835, 2857, 2858, 2865, 2866, 2868,
      2869, 2874, 2876, 2877, 2878, 2879, 2880, 2881, 2885, 2887, 2889, 2891,
      2893, 2894, 2901, 2903, 2904, 2908, 2910, 2911, 2914, 2916, 2918, 2928,
      2929, 2930, 2936, 2946, 2947, 2948, 2949, 2955, 2958, 2961, 2962, 2966,
      2969, 2971, 2972, 2973, 2974, 2976, 2979, 2981, 2984, 2987, 2990, 3002,
      3006, 3008, 3009, 3011, 3014, 3017, 3018, 3021, 3022, 3024, 3025, 3031,
      3032, 3046, 3056, 3059, 3065, 3066, 3067, 3072, 3073, 3076, 3077, 3085,
      3086, 3089, 3090, 3113, 3114, 3130, 3132, 3133, 3134, 3137, 3141, 3142,
      3145, 3146, 3150, 3157, 3159, 3160, 3163, 3165, 3166, 3168, 3170, 3172,
      3174, 3184, 3191, 3192, 3199, 3200, 3201, 3202, 3204, 3205, 3213, 3214,
      3217, 3218, 3241, 3242, 3252, 3253, 3258, 3260, 3261, 3262, 3263, 3264,
      3269, 3270, 3271, 3273, 3274, 3276, 3278, 3285, 3287, 3293, 3295, 3296,
      3298, 3300, 3302, 3312, 3313, 3315, 3316, 3328, 3330, 3332, 3341, 3342,
      3345, 3346, 3387, 3389, 3390, 3393, 3397, 3398, 3401, 3402, 3405, 3406,
      3407, 3408, 3412, 3415, 3416, 3423, 3426, 3428, 3430, 3440, 3449, 3450,
      3456, 3457, 3458, 3460, 3461, 3479, 3482, 3506, 3507, 3516, 3517, 3518,
      3520, 3527, 3530, 3531, 3535, 3538, 3541, 3542, 3543, 3544, 3552, 3558,
      3568, 3570, 3572, 3573, 3585, 3633, 3634, 3636, 3643, 3647, 3648, 3654,
      3655, 3663, 3664, 3674, 3676, 3713, 3715, 3716, 3717, 3718, 3723, 3724,
      3748, 3749, 3750, 3751, 3761, 3762, 3764, 3773, 3774, 3776, 3781, 3782,
      3783, 3784, 3791, 3792, 3802, 3804, 3808, 3840, 3841, 3844, 3859, 3860,
      3861, 3864, 3866, 3872, 3882, 3892, 3893, 3894, 3895, 3896, 3897, 3898,
      3899, 3900, 3901, 3902, 3904, 3912, 3913, 3949, 3953, 3967, 3968, 3973,
      3974, 3976, 3981, 3992, 3993, 4029, 4030, 4038, 4039, 4045, 4046, 4048,
      4053, 4057, 4059, 4096, 4139, 4141, 4145, 4146, 4152, 4153, 4155, 4157,
      4159, 4160, 4170, 4176, 4182, 4184, 4186, 4190, 4193, 4194, 4197, 4199,
      4206, 4209, 4213, 4226, 4227, 4229, 4231, 4237, 4238, 4239, 4240, 4250,
      4253, 4254, 4256, 4294, 4295, 4296, 4301, 4302, 4304, 4347, 4348, 4349,
      4352, 4681, 4682, 4686, 4688, 4695, 4696, 4697, 4698, 4702, 4704, 4745,
      4746, 4750, 4752, 4785, 4786, 4790, 4792, 4799, 4800, 4801, 4802, 4806,
      4808, 4823, 4824, 4881, 4882, 4886, 4888, 4955, 4957, 4960, 4969, 4989,
      4992, 5008, 5018, 5024, 5110, 5112, 5118, 5120, 5121, 5741, 5742, 5743,
      5760, 5761, 5787, 5788, 5789, 5792, 5867, 5870, 5873, 5881, 5888, 5906,
      5909, 5910, 5919, 5938, 5940, 5941, 5943, 5952, 5970, 5972, 5984, 5997,
      5998, 6001, 6002, 6004, 6016, 6068, 6070, 6071, 6078, 6086, 6087, 6089,
      6100, 6103, 6104, 6107, 6108, 6109, 6110, 6112, 6122, 6128, 6138, 6144,
      6150, 6151, 6155, 6158, 6159, 6160, 6170, 6176, 6211, 6212, 6265, 6272,
      6277, 6279, 6313, 6314, 6315, 6320, 6390, 6400, 6431, 6432, 6435, 6439,
      6441, 6444, 6448, 6450, 6451, 6457, 6460, 6464, 6465, 6468, 6470, 6480,
      6510, 6512, 6517, 6528, 6572, 6576, 6602, 6608, 6618, 6619, 6622, 6656,
      6679, 6681, 6683, 6684, 6686, 6688, 6741, 6742, 6743, 6744, 6751, 6752,
      6753, 6754, 6755, 6757, 6765, 6771, 6781, 6783, 6784, 6794, 6800, 6810,
      6816, 6823, 6824, 6830, 6832, 6846, 6847, 6863, 6912, 6916, 6917, 6964,
      6965, 6966, 6971, 6972, 6973, 6978, 6979, 6981, 6989, 6992, 7002, 7009,
      7019, 7028, 7037, 7039, 7040, 7042, 7043, 7073, 7074, 7078, 7080, 7082,
      7083, 7086, 7088, 7098, 7142, 7143, 7144, 7146, 7149, 7150, 7151, 7154,
      7156, 7164, 7168, 7204, 7212, 7220, 7222, 7224, 7227, 7232, 7242, 7245,
      7248, 7258, 7288, 7294, 7296, 7305, 7312, 7355, 7357, 7360, 7368, 7376,
      7379, 7380, 7393, 7394, 7401, 7405, 7406, 7412, 7413, 7415, 7416, 7418,
      7419, 7424, 7468, 7531, 7544, 7545, 7579, 7616, 7680, 7681, 7682, 7683,
      7684, 7685, 7686, 7687, 7688, 7689, 7690, 7691, 7692, 7693, 7694, 7695,
      7696, 7697, 7698, 7699, 7700, 7701, 7702, 7703, 7704, 7705, 7706, 7707,
      7708, 7709, 7710, 7711, 7712, 7713, 7714, 7715, 7716, 7717, 7718, 7719,
      7720, 7721, 7722, 7723, 7724, 7725, 7726, 7727, 7728, 7729, 7730, 7731,
      7732, 7733, 7734, 7735, 7736, 7737, 7738, 7739, 7740, 7741, 7742, 7743,
      7744, 7745, 7746, 7747, 7748, 7749, 7750, 7751, 7752, 7753, 7754, 7755,
      7756, 7757, 7758, 7759, 7760, 7761, 7762, 7763, 7764, 7765, 7766, 7767,
      7768, 7769, 7770, 7771, 7772, 7773, 7774, 7775, 7776, 7777, 7778, 7779,
      7780, 7781, 7782, 7783, 7784, 7785, 7786, 7787, 7788, 7789, 7790, 7791,
      7792, 7793, 7794, 7795, 7796, 7797, 7798, 7799, 7800, 7801, 7802, 7803,
      7804, 7805, 7806, 7807, 7808, 7809, 7810, 7811, 7812, 7813, 7814, 7815,
      7816, 7817, 7818, 7819, 7820, 7821, 7822, 7823, 7824, 7825, 7826, 7827,
      7828, 7829, 7838, 7839, 7840, 7841, 7842, 7843, 7844, 7845, 7846, 7847,
      7848, 7849, 7850, 7851, 7852, 7853, 7854, 7855, 7856, 7857, 7858, 7859,
      7860, 7861, 7862, 7863, 7864, 7865, 7866, 7867, 7868, 7869, 7870, 7871,
      7872, 7873, 7874, 7875, 7876, 7877, 7878, 7879, 7880, 7881, 7882, 7883,
      7884, 7885, 7886, 7887, 7888, 7889, 7890, 7891, 7892, 7893, 7894, 7895,
      7896, 7897, 7898, 7899, 7900, 7901, 7902, 7903, 7904, 7905, 7906, 7907,
      7908, 7909, 7910, 7911, 7912, 7913, 7914, 7915, 7916, 7917, 7918, 7919,
      7920, 7921, 7922, 7923, 7924, 7925, 7926, 7927, 7928, 7929, 7930, 7931,
      7932, 7933, 7934, 7935, 7944, 7952, 7958, 7960, 7966, 7968, 7976, 7984,
      7992, 8000, 8006, 8008, 8014, 8016, 8024, 8025, 8026, 8027, 8028, 8029,
      8030, 8031, 8032, 8040, 8048, 8062, 8064, 8072, 8080, 8088, 8096, 8104,
      8112, 8117, 8118, 8120, 8124, 8125, 8126, 8127, 8130, 8133, 8134, 8136,
      8140, 8141, 8144, 8148, 8150, 8152, 8156, 8157, 8160, 8168, 8173, 8176,
      8178, 8181, 8182, 8184, 8188, 8189, 8191, 8192, 8203, 8208, 8214, 8216,
      8217, 8218, 8219, 8221, 8222, 8223, 8224, 8232, 8233, 8234, 8239, 8240,
      8249, 8250, 8251, 8255, 8257, 8260, 8261, 8262, 8263, 8274, 8275, 8276,
      8277, 8287, 8288, 8293, 8294, 8304, 8305, 8306, 8308, 8314, 8317, 8318,
      8319, 8320, 8330, 8333, 8334, 8335, 8336, 8349, 8352, 8385, 8400, 8413,
      8417, 8418, 8421, 8433, 8448, 8450, 8451, 8455, 8456, 8458, 8459, 8462,
      8464, 8467, 8468, 8469, 8470, 8472, 8473, 8478, 8484, 8485, 8486, 8487,
      8488, 8489, 8490, 8494, 8495, 8496, 8500, 8501, 8505, 8506, 8508, 8510,
      8512, 8517, 8518, 8522, 8523, 8524, 8526, 8527, 8528, 8544, 8579, 8580,
      8581, 8585, 8586, 8588, 8592, 8597, 8602, 8604, 8608, 8609, 8611, 8612,
      8614, 8615, 8622, 8623, 8654, 8656, 8658, 8659, 8660, 8661, 8692, 8960,
      8968, 8969, 8970, 8971, 8972, 8992, 8994, 9001, 9002, 9003, 9084, 9085,
      9115, 9140, 9180, 9186, 9255, 9280, 9291, 9312, 9372, 9450, 9472, 9655,
      9656, 9665, 9666, 9720, 9728, 9839, 9840, 10088, 10089, 10090, 10091,
      10092, 10093, 10094, 10095, 10096, 10097, 10098, 10099, 10100, 10101,
      10102, 10132, 10176, 10181, 10182, 10183, 10214, 10215, 10216, 10217,
      10218, 10219, 10220, 10221, 10222, 10223, 10224, 10240, 10496, 10627,
      10628, 10629, 10630, 10631, 10632, 10633, 10634, 10635, 10636, 10637,
      10638, 10639, 10640, 10641, 10642, 10643, 10644, 10645, 10646, 10647,
      10648, 10649, 10712, 10713, 10714, 10715, 10716, 10748, 10749, 10750,
      11008, 11056, 11077, 11079, 11085, 11124, 11126, 11158, 11159, 11264,
      11312, 11360, 11361, 11362, 11365, 11367, 11368, 11369, 11370, 11371,
      11372, 11373, 11377, 11378, 11379, 11381, 11382, 11388, 11390, 11393,
      11394, 11395, 11396, 11397, 11398, 11399, 11400, 11401, 11402, 11403,
      11404, 11405, 11406, 11407, 11408, 11409, 11410, 11411, 11412, 11413,
      11414, 11415, 11416, 11417, 11418, 11419, 11420, 11421, 11422, 11423,
      11424, 11425, 11426, 11427, 11428, 11429, 11430, 11431, 11432, 11433,
      11434, 11435, 11436, 11437, 11438, 11439, 11440, 11441, 11442, 11443,
      11444, 11445, 11446, 11447, 11448, 11449, 11450, 11451, 11452, 11453,
      11454, 11455, 11456, 11457, 11458, 11459, 11460, 11461, 11462, 11463,
      11464, 11465, 11466, 11467, 11468, 11469, 11470, 11471, 11472, 11473,
      11474, 11475, 11476, 11477, 11478, 11479, 11480, 11481, 11482, 11483,
      11484, 11485, 11486, 11487, 11488, 11489, 11490, 11491, 11493, 11499,
      11500, 11501, 11502, 11503, 11506, 11507, 11508, 11513, 11517, 11518,
      11520, 11558, 11559, 11560, 11565, 11566, 11568, 11624, 11631, 11632,
      11633, 11647, 11648, 11671, 11680, 11687, 11688, 11695, 11696, 11703,
      11704, 11711, 11712, 11719, 11720, 11727, 11728, 11735, 11736, 11743,
      11744, 11776, 11778, 11779, 11780, 11781, 11782, 11785, 11786, 11787,
      11788, 11789, 11790, 11799, 11800, 11802, 11803, 11804, 11805, 11806,
      11808, 11809, 11810, 11811, 11812, 11813, 11814, 11815, 11816, 11817,
      11818, 11823, 11824, 11834, 11836, 11840, 11841, 11842, 11843, 11856,
      11858, 11861, 11862, 11863, 11864, 11865, 11866, 11867, 11868, 11869,
      11870, 11904, 11930, 11931, 12020, 12032, 12246, 12272, 12284, 12288,
      12289, 12292, 12293, 12294, 12295, 12296, 12297, 12298, 12299, 12300,
      12301, 12302, 12303, 12304, 12305, 12306, 12308, 12309, 12310, 12311,
      12312, 12313, 12314, 12315, 12316, 12317, 12318, 12320, 12321, 12330,
      12334, 12336, 12337, 12342, 12344, 12347, 12348, 12349, 12350, 12352,
      12353, 12439, 12441, 12443, 12445, 12447, 12448, 12449, 12539, 12540,
      12543, 12544, 12549, 12592, 12593, 12687, 12688, 12690, 12694, 12704,
      12736, 12772, 12784, 12800, 12831, 12832, 12842, 12872, 12880, 12881,
      12896, 12928, 12938, 12977, 12992, 13312, 19904, 19968, 40981, 40982,
      42125, 42128, 42183, 42192, 42232, 42238, 42240, 42508, 42509, 42512,
      42528, 42538, 42540, 42560, 42561, 42562, 42563, 42564, 42565, 42566,
      42567, 42568, 42569, 42570, 42571, 42572, 42573, 42574, 42575, 42576,
      42577, 42578, 42579, 42580, 42581, 42582, 42583, 42584, 42585, 42586,
      42587, 42588, 42589, 42590, 42591, 42592, 42593, 42594, 42595, 42596,
      42597, 42598, 42599, 42600, 42601, 42602, 42603, 42604, 42605, 42606,
      42607, 42608, 42611, 42612, 42622, 42623, 42624, 42625, 42626, 42627,
      42628, 42629, 42630, 42631, 42632, 42633, 42634, 42635, 42636, 42637,
      42638, 42639, 42640, 42641, 42642, 42643, 42644, 42645, 42646, 42647,
      42648, 42649, 42650, 42651, 42652, 42654, 42656, 42726, 42736, 42738,
      42744, 42752, 42775, 42784, 42786, 42787, 42788, 42789, 42790, 42791,
      42792, 42793, 42794, 42795, 42796, 42797, 42798, 42799, 42802, 42803,
      42804, 42805, 42806, 42807, 42808, 42809, 42810, 42811, 42812, 42813,
      42814, 42815, 42816, 42817, 42818, 42819, 42820, 42821, 42822, 42823,
      42824, 42825, 42826, 42827, 42828, 42829, 42830, 42831, 42832, 42833,
      42834, 42835, 42836, 42837, 42838, 42839, 42840, 42841, 42842, 42843,
      42844, 42845, 42846, 42847, 42848, 42849, 42850, 42851, 42852, 42853,
      42854, 42855, 42856, 42857, 42858, 42859, 42860, 42861, 42862, 42863,
      42864, 42865, 42873, 42874, 42875, 42876, 42877, 42879, 42880, 42881,
      42882, 42883, 42884, 42885, 42886, 42887, 42888, 42889, 42891, 42892,
      42893, 42894, 42895, 42896, 42897, 42898, 42899, 42902, 42903, 42904,
      42905, 42906, 42907, 42908, 42909, 42910, 42911, 42912, 42913, 42914,
      42915, 42916, 42917, 42918, 42919, 42920, 42921, 42922, 42927, 42928,
      42933, 42934, 42935, 42936, 42937, 42938, 42939, 42940, 42941, 42942,
      42943, 42944, 42945, 42946, 42947, 42948, 42952, 42953, 42954, 42955,
      42960, 42961, 42962, 42963, 42964, 42965, 42966, 42967, 42968, 42969,
      42970, 42994, 42997, 42998, 42999, 43000, 43002, 43003, 43010, 43011,
      43014, 43015, 43019, 43020, 43043, 43045, 43047, 43048, 43052, 43053,
      43056, 43062, 43064, 43065, 43066, 43072, 43124, 43128, 43136, 43138,
      43188, 43204, 43206, 43214, 43216, 43226, 43232, 43250, 43256, 43259,
      43260, 43261, 43263, 43264, 43274, 43302, 43310, 43312, 43335, 43346,
      43348, 43359, 43360, 43389, 43392, 43395, 43396, 43443, 43444, 43446,
      43450, 43452, 43454, 43457, 43470, 43471, 43472, 43482, 43486, 43488,
      43493, 43494, 43495, 43504, 43514, 43519, 43520, 43561, 43567, 43569,
      43571, 43573, 43575, 43584, 43587, 43588, 43596, 43597, 43598, 43600,
      43610, 43612, 43616, 43632, 43633, 43639, 43642, 43643, 43644, 43645,
      43646, 43696, 43697, 43698, 43701, 43703, 43705, 43710, 43712, 43713,
      43714, 43715, 43739, 43741, 43742, 43744, 43755, 43756, 43758, 43760,
      43762, 43763, 43765, 43766, 43767, 43777, 43783, 43785, 43791, 43793,
      43799, 43808, 43815, 43816, 43823, 43824, 43867, 43868, 43872, 43881,
      43882, 43884, 43888, 43968, 44003, 44005, 44006, 44008, 44009, 44011,
      44012, 44013, 44014, 44016, 44026, 44032, 55204, 55216, 55239, 55243,
      55292, 55296, 57344, 63744, 64110, 64112, 64218, 64256, 64263, 64275,
      64280, 64285, 64286, 64287, 64297, 64298, 64311, 64312, 64317, 64318,
      64319, 64320, 64322, 64323, 64325, 64326, 64434, 64451, 64467, 64830,
      64831, 64832, 64848, 64912, 64914, 64968, 64975, 64976, 65008, 65020,
      65021, 65024, 65040, 65047, 65048, 65049, 65050, 65056, 65072, 65073,
      65075, 65077, 65078, 65079, 65080, 65081, 65082, 65083, 65084, 65085,
      65086, 65087, 65088, 65089, 65090, 65091, 65092, 65093, 65095, 65096,
      65097, 65101, 65104, 65107, 65108, 65112, 65113, 65114, 65115, 65116,
      65117, 65118, 65119, 65122, 65123, 65124, 65127, 65128, 65129, 65130,
      65132, 65136, 65141, 65142, 65277, 65279, 65280, 65281, 65284, 65285,
      65288, 65289, 65290, 65291, 65292, 65293, 65294, 65296, 65306, 65308,
      65311, 65313, 65339, 65340, 65341, 65342, 65343, 65344, 65345, 65371,
      65372, 65373, 65374, 65375, 65376, 65377, 65378, 65379, 65380, 65382,
      65392, 65393, 65438, 65440, 65471, 65474, 65480, 65482, 65488, 65490,
      65496, 65498, 65501, 65504, 65506, 65507, 65508, 65509, 65511, 65512,
      65513, 65517, 65519, 65529, 65532, 65534, 65536, 65548, 65549, 65575,
      65576, 65595, 65596, 65598, 65599, 65614, 65616, 65630, 65664, 65787,
      65792, 65795, 65799, 65844, 65847, 65856, 65909, 65913, 65930, 65932,
      65935, 65936, 65949, 65952, 65953, 66000, 66045, 66046, 66176, 66205,
      66208, 66257, 66272, 66273, 66300, 66304, 66336, 66340, 66349, 66369,
      66370, 66378, 66379, 66384, 66422, 66427, 66432, 66462, 66463, 66464,
      66500, 66504, 66512, 66513, 66518, 66560, 66600, 66640, 66718, 66720,
      66730, 66736, 66772, 66776, 66812, 66816, 66856, 66864, 66916, 66927,
      66928, 66939, 66940, 66955, 66956, 66963, 66964, 66966, 66967, 66978,
      66979, 66994, 66995, 67002, 67003, 67005, 67072, 67383, 67392, 67414,
      67424, 67432, 67456, 67462, 67463, 67505, 67506, 67515, 67584, 67590,
      67592, 67593, 67594, 67638, 67639, 67641, 67644, 67645, 67647, 67670,
      67671, 67672, 67680, 67703, 67705, 67712, 67743, 67751, 67760, 67808,
      67827, 67828, 67830, 67835, 67840, 67862, 67868, 67871, 67872, 67898,
      67903, 67904, 67968, 68024, 68028, 68030, 68032, 68048, 68050, 68096,
      68097, 68100, 68101, 68103, 68108, 68112, 68116, 68117, 68120, 68121,
      68150, 68152, 68155, 68159, 68160, 68169, 68176, 68185, 68192, 68221,
      68223, 68224, 68253, 68256, 68288, 68296, 68297, 68325, 68327, 68331,
      68336, 68343, 68352, 68406, 68409, 68416, 68438, 68440, 68448, 68467,
      68472, 68480, 68498, 68505, 68509, 68521, 68528, 68608, 68681, 68736,
      68787, 68800, 68851, 68858, 68864, 68900, 68904, 68912, 68922, 69216,
      69247, 69248, 69290, 69291, 69293, 69294, 69296, 69298, 69373, 69376,
      69405, 69415, 69416, 69424, 69446, 69457, 69461, 69466, 69488, 69506,
      69510, 69514, 69552, 69573, 69580, 69600, 69623, 69632, 69633, 69634,
      69635, 69688, 69703, 69710, 69714, 69734, 69744, 69745, 69747, 69749,
      69750, 69759, 69762, 69763, 69808, 69811, 69815, 69817, 69819, 69821,
      69822, 69826, 69827, 69837, 69838, 69840, 69865, 69872, 69882, 69888,
      69891, 69927, 69932, 69933, 69941, 69942, 69952, 69956, 69957, 69959,
      69960, 69968, 70003, 70004, 70006, 70007, 70016, 70018, 70019, 70067,
      70070, 70079, 70081, 70085, 70089, 70093, 70094, 70095, 70096, 70106,
      70107, 70108, 70109, 70112, 70113, 70133, 70144, 70162, 70163, 70188,
      70191, 70194, 70196, 70197, 70198, 70200, 70206, 70207, 70209, 70210,
      70272, 70279, 70280, 70281, 70282, 70286, 70287, 70302, 70303, 70313,
      70314, 70320, 70367, 70368, 70371, 70379, 70384, 70394, 70400, 70402,
      70404, 70405, 70413, 70415, 70417, 70419, 70441, 70442, 70449, 70450,
      70452, 70453, 70458, 70459, 70461, 70462, 70464, 70465, 70469, 70471,
      70473, 70475, 70478, 70480, 70481, 70487, 70488, 70493, 70498, 70500,
      70502, 70509, 70512, 70517, 70656, 70709, 70712, 70720, 70722, 70725,
      70726, 70727, 70731, 70736, 70746, 70748, 70749, 70750, 70751, 70754,
      70784, 70832, 70835, 70841, 70842, 70843, 70847, 70849, 70850, 70852,
      70854, 70855, 70856, 70864, 70874, 71040, 71087, 71090, 71094, 71096,
      71100, 71102, 71103, 71105, 71128, 71132, 71134, 71168, 71216, 71219,
      71227, 71229, 71230, 71231, 71233, 71236, 71237, 71248, 71258, 71264,
      71277, 71296, 71339, 71340, 71341, 71342, 71344, 71350, 71351, 71352,
      71353, 71354, 71360, 71370, 71424, 71451, 71453, 71456, 71458, 71462,
      71463, 71468, 71472, 71482, 71484, 71487, 71488, 71495, 71680, 71724,
      71727, 71736, 71737, 71739, 71740, 71840, 71872, 71904, 71914, 71923,
      71935, 71943, 71945, 71946, 71948, 71956, 71957, 71959, 71960, 71984,
      71990, 71991, 71993, 71995, 71997, 71998, 71999, 72000, 72001, 72002,
      72003, 72004, 72007, 72016, 72026, 72096, 72104, 72106, 72145, 72148,
      72152, 72154, 72156, 72160, 72161, 72162, 72163, 72164, 72165, 72192,
      72193, 72203, 72243, 72249, 72250, 72251, 72255, 72263, 72264, 72272,
      72273, 72279, 72281, 72284, 72330, 72343, 72344, 72346, 72349, 72350,
      72355, 72368, 72441, 72448, 72458, 72704, 72713, 72714, 72751, 72752,
      72759, 72760, 72766, 72767, 72768, 72769, 72774, 72784, 72794, 72813,
      72816, 72818, 72848, 72850, 72872, 72873, 72874, 72881, 72882, 72884,
      72885, 72887, 72960, 72967, 72968, 72970, 72971, 73009, 73015, 73018,
      73019, 73020, 73022, 73023, 73030, 73031, 73032, 73040, 73050, 73056,
      73062, 73063, 73065, 73066, 73098, 73103, 73104, 73106, 73107, 73109,
      73110, 73111, 73112, 73113, 73120, 73130, 73440, 73459, 73461, 73463,
      73465, 73472, 73474, 73475, 73476, 73489, 73490, 73524, 73526, 73531,
      73534, 73536, 73537, 73538, 73539, 73552, 73562, 73648, 73649, 73664,
      73685, 73693, 73697, 73714, 73727, 73728, 74650, 74752, 74863, 74864,
      74869, 74880, 75076, 77712, 77809, 77811, 77824, 78896, 78912, 78913,
      78919, 78934, 82944, 83527, 92160, 92729, 92736, 92767, 92768, 92778,
      92782, 92784, 92863, 92864, 92874, 92880, 92910, 92912, 92917, 92918,
      92928, 92976, 92983, 92988, 92992, 92996, 92997, 92998, 93008, 93018,
      93019, 93026, 93027, 93048, 93053, 93072, 93760, 93792, 93824, 93847,
      93851, 93952, 94027, 94031, 94032, 94033, 94088, 94095, 94099, 94112,
      94176, 94178, 94179, 94180, 94181, 94192, 94194, 94208, 100344, 100352,
      101590, 101632, 101641, 110576, 110580, 110581, 110588, 110589, 110591,
      110592, 110883, 110898, 110899, 110928, 110931, 110933, 110934, 110948,
      110952, 110960, 111356, 113664, 113771, 113776, 113789, 113792, 113801,
      113808, 113818, 113820, 113821, 113823, 113824, 113828, 118528, 118574,
      118576, 118599, 118608, 118724, 118784, 119030, 119040, 119079, 119081,
      119141, 119143, 119146, 119149, 119155, 119163, 119171, 119173, 119180,
      119210, 119214, 119275, 119296, 119362, 119365, 119366, 119488, 119508,
      119520, 119540, 119552, 119639, 119648, 119673, 119808, 119834, 119860,
      119886, 119893, 119894, 119912, 119938, 119964, 119965, 119966, 119968,
      119970, 119971, 119973, 119975, 119977, 119981, 119982, 119990, 119994,
      119995, 119996, 119997, 120004, 120005, 120016, 120042, 120068, 120070,
      120071, 120075, 120077, 120085, 120086, 120093, 120094, 120120, 120122,
      120123, 120127, 120128, 120133, 120134, 120135, 120138, 120145, 120146,
      120172, 120198, 120224, 120250, 120276, 120302, 120328, 120354, 120380,
      120406, 120432, 120458, 120486, 120488, 120513, 120514, 120539, 120540,
      120546, 120571, 120572, 120597, 120598, 120604, 120629, 120630, 120655,
      120656, 120662, 120687, 120688, 120713, 120714, 120720, 120745, 120746,
      120771, 120772, 120778, 120779, 120780, 120782, 120832, 121344, 121399,
      121403, 121453, 121461, 121462, 121476, 121477, 121479, 121484, 121499,
      121504, 121505, 121520, 122624, 122634, 122635, 122655, 122661, 122667,
      122880, 122887, 122888, 122905, 122907, 122914, 122915, 122917, 122918,
      122923, 122928, 122990, 123023, 123024, 123136, 123181, 123184, 123191,
      123198, 123200, 123210, 123214, 123215, 123216, 123536, 123566, 123567,
      123584, 123628, 123632, 123642, 123647, 123648, 124112, 124139, 124140,
      124144, 124154, 124896, 124903, 124904, 124908, 124909, 124911, 124912,
      124927, 124928, 125125, 125127, 125136, 125143, 125184, 125218, 125252,
      125259, 125260, 125264, 125274, 125278, 125280, 126065, 126124, 126125,
      126128, 126129, 126133, 126209, 126254, 126255, 126270, 126464, 126468,
      126469, 126496, 126497, 126499, 126500, 126501, 126503, 126504, 126505,
      126515, 126516, 126520, 126521, 126522, 126523, 126524, 126530, 126531,
      126535, 126536, 126537, 126538, 126539, 126540, 126541, 126544, 126545,
      126547, 126548, 126549, 126551, 126552, 126553, 126554, 126555, 126556,
      126557, 126558, 126559, 126560, 126561, 126563, 126564, 126565, 126567,
      126571, 126572, 126579, 126580, 126584, 126585, 126589, 126590, 126591,
      126592, 126602, 126603, 126620, 126625, 126628, 126629, 126634, 126635,
      126652, 126704, 126706, 126976, 127020, 127024, 127124, 127136, 127151,
      127153, 127168, 127169, 127184, 127185, 127222, 127232, 127245, 127406,
      127462, 127491, 127504, 127548, 127552, 127561, 127568, 127570, 127584,
      127590, 127744, 127995, 128000, 128728, 128732, 128749, 128752, 128765,
      128768, 128887, 128891, 128986, 128992, 129004, 129008, 129009, 129024,
      129036, 129040, 129096, 129104, 129114, 129120, 129160, 129168, 129198,
      129200, 129202, 129280, 129620, 129632, 129646, 129648, 129661, 129664,
      129673, 129680, 129726, 129727, 129734, 129742, 129756, 129760, 129769,
      129776, 129785, 129792, 129939, 129940, 129995, 130032, 130042, 131072,
      173792, 173824, 177978, 177984, 178206, 178208, 183970, 183984, 191457,
      194560, 195102, 196608, 201547, 201552, 205744, 917505, 917506, 917536,
      917632, 917760, 918000, 983040, 1048574, 1048576, 1114110
      // END GENERATED: [char-types-indices]
  )

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
  private[this] lazy val isMirroredIndices: Array[Int] = Array(
      // BEGIN GENERATED: [mirrored-indices]
      40, 42, 60, 61, 62, 63, 91, 92, 93, 94, 123, 124, 125, 126, 171, 172,
      187, 188, 3898, 3902, 5787, 5789, 8249, 8251, 8261, 8263, 8317, 8319,
      8333, 8335, 8512, 8513, 8705, 8709, 8712, 8718, 8721, 8722, 8725, 8727,
      8730, 8734, 8735, 8739, 8740, 8741, 8742, 8743, 8747, 8756, 8761, 8762,
      8763, 8781, 8786, 8790, 8799, 8801, 8802, 8803, 8804, 8812, 8814, 8845,
      8847, 8851, 8856, 8857, 8866, 8868, 8870, 8889, 8894, 8896, 8905, 8910,
      8912, 8914, 8918, 8942, 8944, 8960, 8968, 8972, 8992, 8994, 9001, 9003,
      10088, 10102, 10176, 10177, 10179, 10183, 10184, 10186, 10187, 10190,
      10195, 10199, 10204, 10207, 10210, 10224, 10627, 10649, 10651, 10657,
      10658, 10672, 10680, 10681, 10688, 10694, 10697, 10698, 10702, 10707,
      10708, 10710, 10712, 10717, 10721, 10722, 10723, 10726, 10728, 10730,
      10740, 10746, 10748, 10750, 10762, 10781, 10782, 10786, 10788, 10789,
      10790, 10791, 10793, 10794, 10795, 10799, 10804, 10806, 10812, 10815,
      10839, 10841, 10852, 10854, 10858, 10862, 10863, 10865, 10867, 10869,
      10873, 10916, 10918, 10926, 10927, 10967, 10972, 10973, 10974, 10975,
      10978, 10983, 10988, 10991, 10995, 10996, 10999, 11004, 11005, 11006,
      11262, 11263, 11778, 11782, 11785, 11787, 11788, 11790, 11804, 11806,
      11808, 11818, 11861, 11869, 12296, 12306, 12308, 12316, 65113, 65119,
      65124, 65126, 65288, 65290, 65308, 65309, 65310, 65311, 65339, 65340,
      65341, 65342, 65371, 65372, 65373, 65374, 65375, 65377, 65378, 65380,
      120539, 120540, 120597, 120598, 120655, 120656, 120713, 120714, 120771,
      120772
      // END GENERATED: [mirrored-indices]
  )

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
