package java.lang

import scala.scalajs.js

import java.util.Arrays

@inline
class Character(private val value: scala.Char)
    extends AnyRef with java.io.Serializable with Comparable[Character] {

  def charValue(): scala.Char = value

  override def equals(that: Any): scala.Boolean =
    that.isInstanceOf[Character] && (value == that.asInstanceOf[Character].charValue)

  override def compareTo(that: Character): Int =
    Character.compare(charValue, that.charValue)

  override def toString(): String =
    Character.toString(value)

  override def hashCode(): Int = value.##

  /*
   * Methods on scala.Char
   * The following methods are only here to properly support reflective calls
   * on boxed primitive values. YOU WILL NOT BE ABLE TO USE THESE METHODS, since
   * we use the true javalib to lookup symbols, this file contains only
   * implementations.
   */
  protected def toByte: scala.Byte     = value.toByte
  protected def toShort: scala.Short   = value.toShort
  protected def toChar: scala.Char     = value.toChar
  protected def toInt: scala.Int       = value
  protected def toLong: scala.Long     = value.toLong
  protected def toFloat: scala.Float   = value.toFloat
  protected def toDouble: scala.Double = value.toDouble

  // scalastyle:off disallow.space.before.token
  protected def unary_~ : scala.Int = ~value
  protected def unary_+ : scala.Int = value
  protected def unary_- : scala.Int = -value
  // scalastyle:on disallow.space.before.token

  protected def +(x: String): String = value + x

  protected def <<(x: scala.Int): scala.Int = value << x
  protected def <<(x: scala.Long): scala.Int = value << x
  protected def >>>(x: scala.Int): scala.Int = value >>> x
  protected def >>>(x: scala.Long): scala.Int = value >>> x
  protected def >>(x: scala.Int): scala.Int = value >> x
  protected def >>(x: scala.Long): scala.Int = value >> x

  protected def ==(x: scala.Byte): scala.Boolean = value == x
  protected def ==(x: scala.Short): scala.Boolean = value == x
  protected def ==(x: scala.Char): scala.Boolean = value == x
  protected def ==(x: scala.Int): scala.Boolean = value == x
  protected def ==(x: scala.Long): scala.Boolean = value == x
  protected def ==(x: scala.Float): scala.Boolean = value == x
  protected def ==(x: scala.Double): scala.Boolean = value == x

  protected def !=(x: scala.Byte): scala.Boolean = value != x
  protected def !=(x: scala.Short): scala.Boolean = value != x
  protected def !=(x: scala.Char): scala.Boolean = value != x
  protected def !=(x: scala.Int): scala.Boolean = value != x
  protected def !=(x: scala.Long): scala.Boolean = value != x
  protected def !=(x: scala.Float): scala.Boolean = value != x
  protected def !=(x: scala.Double): scala.Boolean = value != x

  protected def <(x: scala.Byte): scala.Boolean = value < x
  protected def <(x: scala.Short): scala.Boolean = value < x
  protected def <(x: scala.Char): scala.Boolean = value < x
  protected def <(x: scala.Int): scala.Boolean = value < x
  protected def <(x: scala.Long): scala.Boolean = value < x
  protected def <(x: scala.Float): scala.Boolean = value < x
  protected def <(x: scala.Double): scala.Boolean = value < x

  protected def <=(x: scala.Byte): scala.Boolean = value <= x
  protected def <=(x: scala.Short): scala.Boolean = value <= x
  protected def <=(x: scala.Char): scala.Boolean = value <= x
  protected def <=(x: scala.Int): scala.Boolean = value <= x
  protected def <=(x: scala.Long): scala.Boolean = value <= x
  protected def <=(x: scala.Float): scala.Boolean = value <= x
  protected def <=(x: scala.Double): scala.Boolean = value <= x

  protected def >(x: scala.Byte): scala.Boolean = value > x
  protected def >(x: scala.Short): scala.Boolean = value > x
  protected def >(x: scala.Char): scala.Boolean = value > x
  protected def >(x: scala.Int): scala.Boolean = value > x
  protected def >(x: scala.Long): scala.Boolean = value > x
  protected def >(x: scala.Float): scala.Boolean = value > x
  protected def >(x: scala.Double): scala.Boolean = value > x

  protected def >=(x: scala.Byte): scala.Boolean = value >= x
  protected def >=(x: scala.Short): scala.Boolean = value >= x
  protected def >=(x: scala.Char): scala.Boolean = value >= x
  protected def >=(x: scala.Int): scala.Boolean = value >= x
  protected def >=(x: scala.Long): scala.Boolean = value >= x
  protected def >=(x: scala.Float): scala.Boolean = value >= x
  protected def >=(x: scala.Double): scala.Boolean = value >= x

  protected def |(x: scala.Byte): scala.Int = value | x
  protected def |(x: scala.Short): scala.Int = value | x
  protected def |(x: scala.Char): scala.Int = value | x
  protected def |(x: scala.Int): scala.Int = value | x
  protected def |(x: scala.Long): scala.Long = value | x

  protected def &(x: scala.Byte): scala.Int = value & x
  protected def &(x: scala.Short): scala.Int = value & x
  protected def &(x: scala.Char): scala.Int = value & x
  protected def &(x: scala.Int): scala.Int = value & x
  protected def &(x: scala.Long): scala.Long = value & x

  protected def ^(x: scala.Byte): scala.Int = value ^ x
  protected def ^(x: scala.Short): scala.Int = value ^ x
  protected def ^(x: scala.Char): scala.Int = value ^ x
  protected def ^(x: scala.Int): scala.Int = value ^ x
  protected def ^(x: scala.Long): scala.Long = value ^ x

  protected def +(x: scala.Byte): scala.Int = value + x
  protected def +(x: scala.Short): scala.Int = value + x
  protected def +(x: scala.Char): scala.Int = value + x
  protected def +(x: scala.Int): scala.Int = value + x
  protected def +(x: scala.Long): scala.Long = value + x
  protected def +(x: scala.Float): scala.Float = value + x
  protected def +(x: scala.Double): scala.Double = value + x

  protected def -(x: scala.Byte): scala.Int = value - x
  protected def -(x: scala.Short): scala.Int = value - x
  protected def -(x: scala.Char): scala.Int = value - x
  protected def -(x: scala.Int): scala.Int = value - x
  protected def -(x: scala.Long): scala.Long = value - x
  protected def -(x: scala.Float): scala.Float = value - x
  protected def -(x: scala.Double): scala.Double = value - x

  protected def *(x: scala.Byte): scala.Int = value * x
  protected def *(x: scala.Short): scala.Int = value * x
  protected def *(x: scala.Char): scala.Int = value * x
  protected def *(x: scala.Int): scala.Int = value * x
  protected def *(x: scala.Long): scala.Long = value * x
  protected def *(x: scala.Float): scala.Float = value * x
  protected def *(x: scala.Double): scala.Double = value * x

  protected def /(x: scala.Byte): scala.Int = value / x
  protected def /(x: scala.Short): scala.Int = value / x
  protected def /(x: scala.Char): scala.Int = value / x
  protected def /(x: scala.Int): scala.Int = value / x
  protected def /(x: scala.Long): scala.Long = value / x
  protected def /(x: scala.Float): scala.Float = value / x
  protected def /(x: scala.Double): scala.Double = value / x

  protected def %(x: scala.Byte): scala.Int = value % x
  protected def %(x: scala.Short): scala.Int = value % x
  protected def %(x: scala.Char): scala.Int = value % x
  protected def %(x: scala.Int): scala.Int = value % x
  protected def %(x: scala.Long): scala.Long = value % x
  protected def %(x: scala.Float): scala.Float = value % x
  protected def %(x: scala.Double): scala.Double = value % x

}

object Character {
  final val TYPE = classOf[scala.Char]
  final val MIN_VALUE = '\u0000'
  final val MAX_VALUE = '\uffff'
  final val SIZE = 16
  final val BYTES = 2

  def valueOf(charValue: scala.Char): Character = new Character(charValue)

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

  def getType(ch: scala.Char): Int = getType(ch.toInt)

  def getType(codePoint: Int): Int = {
    if (codePoint < 0) UNASSIGNED.toInt
    else if (codePoint < 256) getTypeLT256(codePoint)
    else getTypeGE256(codePoint)
  }

  @inline
  private[this] def getTypeLT256(codePoint: Int): scala.Byte =
    charTypesFirst256(codePoint)

  private[this] def getTypeGE256(codePoint: Int): scala.Byte = {
    // the idx is increased by 1 due to the differences in indexing
    // between charTypeIndices and charType
    val idx = Arrays.binarySearch(charTypeIndices, codePoint) + 1
    // in the case where idx is negative (-insertionPoint - 1)
    charTypes(Math.abs(idx))
  }

  def digit(c: scala.Char, radix: Int): Int = {
    if (radix > MAX_RADIX || radix < MIN_RADIX)
      -1
    else if (c >= '0' && c <= '9' && c - '0' < radix)
      c - '0'
    else if (c >= 'A' && c <= 'Z' && c - 'A' < radix - 10)
      c - 'A' + 10
    else if (c >= 'a' && c <= 'z' && c - 'a' < radix - 10)
      c - 'a' + 10
    else if (c >= '\uFF21' && c <= '\uFF3A' &&
      c - '\uFF21' < radix - 10)
      c - '\uFF21' + 10
    else if (c >= '\uFF41' && c <= '\uFF5A' &&
      c - '\uFF41' < radix - 10)
      c - '\uFF21' + 10
    else -1
  }

  // ported from https://github.com/gwtproject/gwt/blob/master/user/super/com/google/gwt/emul/java/lang/Character.java
  def forDigit(digit: Int, radix: Int): Char = {
    if (radix < MIN_RADIX || radix > MAX_RADIX || digit < 0 || digit >= radix) {
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

  // --- UTF-16 surrogate pairs handling ---
  // See http://en.wikipedia.org/wiki/UTF-16

  private final val HighSurrogateMask       = 0xfc00 // 111111 00  00000000
  private final val HighSurrogateID         = 0xd800 // 110110 00  00000000
  private final val LowSurrogateMask        = 0xfc00 // 111111 00  00000000
  private final val LowSurrogateID          = 0xdc00 // 110111 00  00000000
  private final val SurrogateUsefulPartMask = 0x03ff // 000000 11  11111111

  @inline def isHighSurrogate(c: scala.Char): scala.Boolean =
    (c & HighSurrogateMask) == HighSurrogateID
  @inline def isLowSurrogate(c: scala.Char): scala.Boolean =
    (c & LowSurrogateMask) == LowSurrogateID
  @inline def isSurrogatePair(high: scala.Char, low: scala.Char): scala.Boolean =
    isHighSurrogate(high) && isLowSurrogate(low)

  @inline def charCount(codePoint: Int): Int =
    if (codePoint >= MIN_SUPPLEMENTARY_CODE_POINT) 2 else 1

  @inline def toCodePoint(high: scala.Char, low: scala.Char): Int =
    ((high & SurrogateUsefulPartMask) << 10) + (low & SurrogateUsefulPartMask) + 0x10000

  // --- End of UTF-16 surrogate pairs handling ---

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

  @inline def isValidCodePoint(codePoint: Int): scala.Boolean =
    codePoint >= MIN_CODE_POINT && codePoint <= MAX_CODE_POINT

  @inline def isBmpCodePoint(codePoint: Int): scala.Boolean =
    codePoint >= MIN_VALUE && codePoint <= MAX_VALUE

  @inline def isSupplementaryCodePoint(codePoint: Int): scala.Boolean =
    codePoint >= MIN_SUPPLEMENTARY_CODE_POINT && codePoint <= MAX_CODE_POINT

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
    val idx = Arrays.binarySearch(isMirroredIndices, codePoint) + 1
    (Math.abs(idx) & 1) != 0
  }

  //def getDirectionality(c: scala.Char): scala.Byte

  /* Conversions */
  def toUpperCase(c: scala.Char): scala.Char = c.toString.toUpperCase()(0)
  def toLowerCase(c: scala.Char): scala.Char = c.toString.toLowerCase()(0)
  //def toTitleCase(c: scala.Char): scala.Char
  //def getNumericValue(c: scala.Char): Int

  /* Misc */
  //def reverseBytes(ch: scala.Char): scala.Char

  def toChars(codePoint: Int): Array[Char] = {
    if (!isValidCodePoint(codePoint))
      throw new IllegalArgumentException()

    if (isSupplementaryCodePoint(codePoint)) {
      val dst = new Array[Char](2)
      toSurrogate(codePoint, dst, 0)
      dst
    } else {
      Array(codePoint.toChar)
    }
  }

  def toChars(codePoint: Int, dst: Array[Char], dstIndex: Int): Int = {
    if (!isValidCodePoint(codePoint))
      throw new IllegalArgumentException()

    if (isSupplementaryCodePoint(codePoint)) {
      toSurrogate(codePoint, dst, dstIndex)
      2
    } else {
      dst(dstIndex) = codePoint.toChar
      1
    }
  }

  @inline private[this] def toSurrogate(codePoint: Int, dst: Array[Char], dstIndex: Int): Unit = {
    val cpPrime = codePoint - 0x10000
    val high = 0xD800 | ((cpPrime >> 10) & 0x3FF)
    val low = 0xDC00 | (cpPrime & 0x3FF)
    dst(dstIndex) = high.toChar
    dst(dstIndex + 1) = low.toChar
  }

  @inline def toString(c: scala.Char): String =
    js.Dynamic.global.String.fromCharCode(c.toInt).asInstanceOf[String]

  @inline def compare(x: scala.Char, y: scala.Char): Int =
    x - y

  // Based on Unicode 7.0.0

  // Types of characters from 0 to 255
  private[this] lazy val charTypesFirst256 = Array[scala.Byte](15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 12, 24, 24, 24, 26, 24, 24, 24,
    21, 22, 24, 25, 24, 20, 24, 24, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 24, 24, 25,
    25, 25, 24, 24, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 21, 24, 22, 27, 23, 27, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 21, 25, 22, 25, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 12, 24, 26, 26, 26,
    26, 28, 24, 27, 28, 5, 29, 25, 16, 28, 27, 28, 25, 11, 11, 27, 2, 24, 24,
    27, 11, 5, 30, 11, 11, 11, 24, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 25, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 25, 2, 2, 2, 2, 2, 2,
    2, 2)

  // Character type data by ranges of types
  // charTypeIndices: contains the index where the range ends
  // charType: contains the type of the carater in the range ends
  // note that charTypeIndices.length + 1 = charType.length and that the
  // range 0 to 255 is not included because it is contained in charTypesFirst256
  // They where generated with the following script:
  //
  //  val indicesAndTypes = (256 to Character.MAX_CODE_POINT)
  //    .map(i => (i, Character.getType(i)))
  //    .foldLeft[List[(Int, Int)]](Nil) {
  //      case (x :: xs, elem) if x._2 == elem._2 =>  x :: xs
  //      case (prevs, elem) => elem :: prevs
  //    }.reverse
  //  val charTypeIndices = indicesAndTypes.map(_._1).tail
  //  val charTypeIndicesDeltas = charTypeIndices.zip(0 :: charTypeIndices.init)
  //    .map(tup => tup._1 - tup._2)
  //  val charTypes = indicesAndTypes.map(_._2)
  //  println(charTypeIndicesDeltas.mkString(
  //    "charTypeIndices: val deltas = Array[Int](", ", ", ")"))
  //  println(charTypes.mkString("val charTypes = Array[scala.Byte](", ", ", ")"))
  //
  private[this] lazy val charTypeIndices = {
    val deltas = Array[Int](257, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 3, 2, 1, 1, 1, 2, 1, 3,
      2, 4, 1, 2, 1, 3, 3, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1,
      3, 1, 1, 1, 2, 2, 1, 1, 3, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 7, 2, 1, 2, 2, 1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 1, 69, 1, 27, 18,
      4, 12, 14, 5, 7, 1, 1, 1, 17, 112, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 3, 1,
      5, 2, 1, 1, 3, 1, 1, 1, 2, 1, 17, 1, 9, 35, 1, 2, 3, 3, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 1, 1, 1, 1,
      1, 2, 2, 51, 48, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 2, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9, 38, 2, 1, 6, 1, 39, 1, 1,
      1, 4, 1, 1, 45, 1, 1, 1, 2, 1, 2, 1, 1, 8, 27, 5, 3, 2, 11, 5, 1, 3,
      2, 1, 2, 2, 11, 1, 2, 2, 32, 1, 10, 21, 10, 4, 2, 1, 99, 1, 1, 7, 1,
      1, 6, 2, 2, 1, 4, 2, 10, 3, 2, 1, 14, 1, 1, 1, 1, 30, 27, 2, 89, 11,
      1, 14, 10, 33, 9, 2, 1, 3, 1, 5, 22, 4, 1, 9, 1, 3, 1, 5, 2, 15, 1,
      25, 3, 2, 1, 65, 1, 1, 11, 55, 27, 1, 3, 1, 54, 1, 1, 1, 1, 3, 8, 4,
      1, 2, 1, 7, 10, 2, 2, 10, 1, 1, 6, 1, 7, 1, 1, 2, 1, 8, 2, 2, 2, 22,
      1, 7, 1, 1, 3, 4, 2, 1, 1, 3, 4, 2, 2, 2, 2, 1, 1, 8, 1, 4, 2, 1, 3,
      2, 2, 10, 2, 2, 6, 1, 1, 5, 2, 1, 1, 6, 4, 2, 2, 22, 1, 7, 1, 2, 1, 2,
      1, 2, 2, 1, 1, 3, 2, 4, 2, 2, 3, 3, 1, 7, 4, 1, 1, 7, 10, 2, 3, 1, 11,
      2, 1, 1, 9, 1, 3, 1, 22, 1, 7, 1, 2, 1, 5, 2, 1, 1, 3, 5, 1, 2, 1, 1,
      2, 1, 2, 1, 15, 2, 2, 2, 10, 1, 1, 15, 1, 2, 1, 8, 2, 2, 2, 22, 1, 7,
      1, 2, 1, 5, 2, 1, 1, 1, 1, 1, 4, 2, 2, 2, 2, 1, 8, 1, 1, 4, 2, 1, 3,
      2, 2, 10, 1, 1, 6, 10, 1, 1, 1, 6, 3, 3, 1, 4, 3, 2, 1, 1, 1, 2, 3, 2,
      3, 3, 3, 12, 4, 2, 1, 2, 3, 3, 1, 3, 1, 2, 1, 6, 1, 14, 10, 3, 6, 1,
      1, 6, 3, 1, 8, 1, 3, 1, 23, 1, 10, 1, 5, 3, 1, 3, 4, 1, 3, 1, 4, 7, 2,
      1, 2, 6, 2, 2, 2, 10, 8, 7, 1, 2, 2, 1, 8, 1, 3, 1, 23, 1, 10, 1, 5,
      2, 1, 1, 1, 1, 5, 1, 1, 2, 1, 2, 2, 7, 2, 7, 1, 1, 2, 2, 2, 10, 1, 2,
      15, 2, 1, 8, 1, 3, 1, 41, 2, 1, 3, 4, 1, 3, 1, 3, 1, 1, 8, 1, 8, 2, 2,
      2, 10, 6, 3, 1, 6, 2, 2, 1, 18, 3, 24, 1, 9, 1, 1, 2, 7, 3, 1, 4, 3,
      3, 1, 1, 1, 8, 18, 2, 1, 12, 48, 1, 2, 7, 4, 1, 6, 1, 8, 1, 10, 2, 37,
      2, 1, 1, 2, 2, 1, 1, 2, 1, 6, 4, 1, 7, 1, 3, 1, 1, 1, 1, 2, 2, 1, 4,
      1, 2, 6, 1, 2, 1, 2, 5, 1, 1, 1, 6, 2, 10, 2, 4, 32, 1, 3, 15, 1, 1,
      3, 2, 6, 10, 10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 8, 1, 36, 4, 14, 1,
      5, 1, 2, 5, 11, 1, 36, 1, 8, 1, 6, 1, 2, 5, 4, 2, 37, 43, 2, 4, 1, 6,
      1, 2, 2, 2, 1, 10, 6, 6, 2, 2, 4, 3, 1, 3, 2, 7, 3, 4, 13, 1, 2, 2, 6,
      1, 1, 1, 10, 3, 1, 2, 38, 1, 1, 5, 1, 2, 43, 1, 1, 332, 1, 4, 2, 7, 1,
      1, 1, 4, 2, 41, 1, 4, 2, 33, 1, 4, 2, 7, 1, 1, 1, 4, 2, 15, 1, 57, 1,
      4, 2, 67, 2, 3, 9, 20, 3, 16, 10, 6, 85, 11, 1, 620, 2, 17, 1, 26, 1,
      1, 3, 75, 3, 3, 15, 13, 1, 4, 3, 11, 18, 3, 2, 9, 18, 2, 12, 13, 1, 3,
      1, 2, 12, 52, 2, 1, 7, 8, 1, 2, 11, 3, 1, 3, 1, 1, 1, 2, 10, 6, 10, 6,
      6, 1, 4, 3, 1, 1, 10, 6, 35, 1, 52, 8, 41, 1, 1, 5, 70, 10, 29, 3, 3,
      4, 2, 3, 4, 2, 1, 6, 3, 4, 1, 3, 2, 10, 30, 2, 5, 11, 44, 4, 17, 7, 2,
      6, 10, 1, 3, 34, 23, 2, 3, 2, 2, 53, 1, 1, 1, 7, 1, 1, 1, 1, 2, 8, 6,
      10, 2, 1, 10, 6, 10, 6, 7, 1, 6, 82, 4, 1, 47, 1, 1, 5, 1, 1, 5, 1, 2,
      7, 4, 10, 7, 10, 9, 9, 3, 2, 1, 30, 1, 4, 2, 2, 1, 1, 2, 2, 10, 44, 1,
      1, 2, 3, 1, 1, 3, 2, 8, 4, 36, 8, 8, 2, 2, 3, 5, 10, 3, 3, 10, 30, 6,
      2, 64, 8, 8, 3, 1, 13, 1, 7, 4, 1, 4, 2, 1, 2, 9, 44, 63, 13, 1, 34,
      37, 39, 21, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9,
      8, 6, 2, 6, 2, 8, 8, 8, 8, 6, 2, 6, 2, 8, 1, 1, 1, 1, 1, 1, 1, 1, 8,
      8, 14, 2, 8, 8, 8, 8, 8, 8, 5, 1, 2, 4, 1, 1, 1, 3, 3, 1, 2, 4, 1, 3,
      4, 2, 2, 4, 1, 3, 8, 5, 3, 2, 3, 1, 2, 4, 1, 2, 1, 11, 5, 6, 2, 1, 1,
      1, 2, 1, 1, 1, 8, 1, 1, 5, 1, 9, 1, 1, 4, 2, 3, 1, 1, 1, 11, 1, 1, 1,
      10, 1, 5, 5, 6, 1, 1, 2, 6, 3, 1, 1, 1, 10, 3, 1, 1, 1, 13, 3, 27, 21,
      13, 4, 1, 3, 12, 15, 2, 1, 4, 1, 2, 1, 3, 2, 3, 1, 1, 1, 2, 1, 5, 6,
      1, 1, 1, 1, 1, 1, 4, 1, 1, 4, 1, 4, 1, 2, 2, 2, 5, 1, 4, 1, 1, 2, 1,
      1, 16, 35, 1, 1, 4, 1, 6, 5, 5, 2, 4, 1, 2, 1, 2, 1, 7, 1, 31, 2, 2,
      1, 1, 1, 31, 268, 8, 4, 20, 2, 7, 1, 1, 81, 1, 30, 25, 40, 6, 18, 12,
      39, 25, 11, 21, 60, 78, 22, 183, 1, 9, 1, 54, 8, 111, 1, 144, 1, 103,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 30, 44, 5, 1, 1, 31, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 16, 256, 131, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 63, 1, 1, 1, 1, 32, 1, 1, 258, 48,
      21, 2, 6, 3, 10, 166, 47, 1, 47, 1, 1, 1, 3, 2, 1, 1, 1, 1, 1, 1, 4,
      1, 1, 2, 1, 6, 2, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 6, 1, 1, 1, 1, 3, 1, 1, 5,
      4, 1, 2, 38, 1, 1, 5, 1, 2, 56, 7, 1, 1, 14, 1, 23, 9, 7, 1, 7, 1, 7,
      1, 7, 1, 7, 1, 7, 1, 7, 1, 7, 1, 32, 2, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1,
      9, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 1, 10, 2, 68,
      26, 1, 89, 12, 214, 26, 12, 4, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 9, 4, 2, 1, 5, 2, 3,
      1, 1, 1, 2, 1, 86, 2, 2, 2, 2, 1, 1, 90, 1, 3, 1, 5, 41, 3, 94, 1, 2,
      4, 10, 27, 5, 36, 12, 16, 31, 1, 10, 30, 8, 1, 15, 32, 10, 39, 15, 63,
      1, 256, 6582, 10, 64, 20941, 51, 21, 1, 1143, 3, 55, 9, 40, 6, 2, 268,
      1, 3, 16, 10, 2, 20, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 7, 1, 70, 10, 2, 6, 8,
      23, 9, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 8, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 12, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 77, 2, 1, 7, 1, 3, 1, 4, 1, 23, 2, 2, 1, 4, 4, 6, 2, 1, 1, 6,
      52, 4, 8, 2, 50, 16, 1, 9, 2, 10, 6, 18, 6, 3, 1, 4, 10, 28, 8, 2, 23,
      11, 2, 11, 1, 29, 3, 3, 1, 47, 1, 2, 4, 2, 1, 4, 13, 1, 1, 10, 4, 2,
      32, 41, 6, 2, 2, 2, 2, 9, 3, 1, 8, 1, 1, 2, 10, 2, 4, 16, 1, 6, 3, 1,
      1, 4, 48, 1, 1, 3, 2, 2, 5, 2, 1, 1, 1, 24, 2, 1, 2, 11, 1, 2, 2, 2,
      1, 2, 1, 1, 10, 6, 2, 6, 2, 6, 9, 7, 1, 7, 145, 35, 2, 1, 2, 1, 2, 1,
      1, 1, 2, 10, 6, 11172, 12, 23, 4, 49, 4, 2048, 6400, 366, 2, 106, 38,
      7, 12, 5, 5, 1, 1, 10, 1, 13, 1, 5, 1, 1, 1, 2, 1, 2, 1, 108, 16, 17,
      363, 1, 1, 16, 64, 2, 54, 40, 12, 1, 1, 2, 16, 7, 1, 1, 1, 6, 7, 9, 1,
      2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 4, 3,
      3, 1, 4, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 3, 1, 1, 1, 2, 4, 5, 1, 135, 2,
      1, 1, 3, 1, 3, 1, 1, 1, 1, 1, 1, 2, 10, 2, 3, 2, 26, 1, 1, 1, 1, 1, 1,
      26, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 10, 1, 45, 2, 31, 3, 6, 2, 6, 2, 6,
      2, 3, 3, 2, 1, 1, 1, 2, 1, 1, 4, 2, 10, 3, 2, 2, 12, 1, 26, 1, 19, 1,
      2, 1, 15, 2, 14, 34, 123, 5, 3, 4, 45, 3, 9, 53, 4, 17, 1, 5, 12, 52,
      45, 1, 130, 29, 3, 49, 47, 31, 1, 4, 12, 17, 1, 8, 1, 53, 30, 1, 1,
      36, 4, 8, 1, 5, 42, 40, 40, 78, 2, 10, 854, 6, 2, 1, 1, 44, 1, 2, 3,
      1, 2, 23, 1, 1, 8, 160, 22, 6, 3, 1, 26, 5, 1, 64, 56, 6, 2, 64, 1, 3,
      1, 2, 5, 4, 4, 1, 3, 1, 27, 4, 3, 4, 1, 8, 8, 9, 7, 29, 2, 1, 128, 54,
      3, 7, 22, 2, 8, 19, 5, 8, 128, 73, 535, 31, 385, 1, 1, 1, 53, 15, 7,
      4, 20, 10, 16, 2, 1, 45, 3, 4, 2, 2, 2, 1, 4, 14, 25, 7, 10, 6, 3, 36,
      5, 1, 8, 1, 10, 4, 60, 2, 1, 48, 3, 9, 2, 4, 4, 7, 10, 1190, 43, 1, 1,
      1, 2, 6, 1, 1, 8, 10, 2358, 879, 145, 99, 13, 4, 2956, 1071, 13265,
      569, 1223, 69, 11, 1, 46, 16, 4, 13, 16480, 2, 8190, 246, 10, 39, 2,
      60, 2, 3, 3, 6, 8, 8, 2, 7, 30, 4, 48, 34, 66, 3, 1, 186, 87, 9, 18,
      142, 26, 26, 26, 7, 1, 18, 26, 26, 1, 1, 2, 2, 1, 2, 2, 2, 4, 1, 8, 4,
      1, 1, 1, 7, 1, 11, 26, 26, 2, 1, 4, 2, 8, 1, 7, 1, 26, 2, 1, 4, 1, 5,
      1, 1, 3, 7, 1, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 28, 2,
      25, 1, 25, 1, 6, 25, 1, 25, 1, 6, 25, 1, 25, 1, 6, 25, 1, 25, 1, 6,
      25, 1, 25, 1, 6, 1, 1, 2, 50, 5632, 4, 1, 27, 1, 2, 1, 1, 2, 1, 1, 10,
      1, 4, 1, 1, 1, 1, 6, 1, 4, 1, 1, 1, 1, 1, 1, 3, 1, 2, 1, 1, 2, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 4, 1, 7, 1, 4, 1, 4, 1, 1, 1, 10,
      1, 17, 5, 3, 1, 5, 1, 17, 52, 2, 270, 44, 4, 100, 12, 15, 2, 14, 2,
      15, 1, 15, 32, 11, 5, 31, 1, 60, 4, 43, 75, 29, 13, 43, 5, 9, 7, 2,
      174, 33, 15, 6, 1, 70, 3, 20, 12, 37, 1, 5, 21, 17, 15, 63, 1, 1, 1,
      182, 1, 4, 3, 62, 2, 4, 12, 24, 147, 70, 4, 11, 48, 70, 58, 116, 2188,
      42711, 41, 4149, 11, 222, 16354, 542, 722403, 1, 30, 96, 128, 240,
      65040, 65534, 2, 65534)
    uncompressDeltas(deltas)
  }

  private[this] lazy val charTypes = Array[scala.Byte](1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 5, 1, 2, 5, 1, 3, 2,
    1, 3, 2, 1, 3, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 3, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 5, 2, 4, 27, 4, 27, 4, 27, 4, 27, 4, 27, 6, 1, 2, 1,
    2, 4, 27, 1, 2, 0, 4, 2, 24, 0, 27, 1, 24, 1, 0, 1, 0, 1, 2, 1, 0, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 25, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 28, 6, 7, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 0, 1, 0, 4, 24, 0, 2, 0, 24, 20, 0, 26, 0, 6, 20, 6, 24, 6, 24, 6,
    24, 6, 0, 5, 0, 5, 24, 0, 16, 0, 25, 24, 26, 24, 28, 6, 24, 0, 24, 5,
    4, 5, 6, 9, 24, 5, 6, 5, 24, 5, 6, 16, 28, 6, 4, 6, 28, 6, 5, 9, 5,
    28, 5, 24, 0, 16, 5, 6, 5, 6, 0, 5, 6, 5, 0, 9, 5, 6, 4, 28, 24, 4, 0,
    5, 6, 4, 6, 4, 6, 4, 6, 0, 24, 0, 5, 6, 0, 24, 0, 5, 0, 5, 0, 6, 0, 6,
    8, 5, 6, 8, 6, 5, 8, 6, 8, 6, 8, 5, 6, 5, 6, 24, 9, 24, 4, 5, 0, 5, 0,
    6, 8, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 6, 5, 8, 6, 0, 8, 0, 8,
    6, 5, 0, 8, 0, 5, 0, 5, 6, 0, 9, 5, 26, 11, 28, 26, 0, 6, 8, 0, 5, 0,
    5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 6, 0, 8, 6, 0, 6, 0, 6, 0, 6, 0,
    5, 0, 5, 0, 9, 6, 5, 6, 0, 6, 8, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5,
    0, 6, 5, 8, 6, 0, 6, 8, 0, 8, 6, 0, 5, 0, 5, 6, 0, 9, 24, 26, 0, 6, 8,
    0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 6, 5, 8, 6, 8, 6, 0, 8, 0, 8,
    6, 0, 6, 8, 0, 5, 0, 5, 6, 0, 9, 28, 5, 11, 0, 6, 5, 0, 5, 0, 5, 0, 5,
    0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 8, 6, 8, 0, 8, 0, 8, 6, 0, 5,
    0, 8, 0, 9, 11, 28, 26, 28, 0, 8, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5,
    6, 8, 0, 6, 0, 6, 0, 6, 0, 5, 0, 5, 6, 0, 9, 0, 11, 28, 0, 8, 0, 5, 0,
    5, 0, 5, 0, 5, 0, 5, 0, 6, 5, 8, 6, 8, 0, 6, 8, 0, 8, 6, 0, 8, 0, 5,
    0, 5, 6, 0, 9, 0, 5, 0, 8, 0, 5, 0, 5, 0, 5, 0, 5, 8, 6, 0, 8, 0, 8,
    6, 5, 0, 8, 0, 5, 6, 0, 9, 11, 0, 28, 5, 0, 8, 0, 5, 0, 5, 0, 5, 0, 5,
    0, 5, 0, 6, 0, 8, 6, 0, 6, 0, 8, 0, 8, 24, 0, 5, 6, 5, 6, 0, 26, 5, 4,
    6, 24, 9, 24, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0,
    5, 0, 5, 0, 5, 6, 5, 6, 0, 6, 5, 0, 5, 0, 4, 0, 6, 0, 9, 0, 5, 0, 5,
    28, 24, 28, 24, 28, 6, 28, 9, 11, 28, 6, 28, 6, 28, 6, 21, 22, 21, 22,
    8, 5, 0, 5, 0, 6, 8, 6, 24, 6, 5, 6, 0, 6, 0, 28, 6, 28, 0, 28, 24,
    28, 24, 0, 5, 8, 6, 8, 6, 8, 6, 8, 6, 5, 9, 24, 5, 8, 6, 5, 6, 5, 8,
    5, 8, 5, 6, 5, 6, 8, 6, 8, 6, 5, 8, 9, 8, 6, 28, 1, 0, 1, 0, 1, 0, 5,
    24, 4, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5,
    0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 6, 24, 11, 0, 5, 28, 0, 5, 0, 20, 5,
    24, 5, 12, 5, 21, 22, 0, 5, 24, 10, 0, 5, 0, 5, 6, 0, 5, 6, 24, 0, 5,
    6, 0, 5, 0, 5, 0, 6, 0, 5, 6, 8, 6, 8, 6, 8, 6, 24, 4, 24, 26, 5, 6,
    0, 9, 0, 11, 0, 24, 20, 24, 6, 12, 0, 9, 0, 5, 4, 5, 0, 5, 6, 5, 0, 5,
    0, 5, 0, 6, 8, 6, 8, 0, 8, 6, 8, 6, 0, 28, 0, 24, 9, 5, 0, 5, 0, 5, 0,
    8, 5, 8, 0, 9, 11, 0, 28, 5, 6, 8, 0, 24, 5, 8, 6, 8, 6, 0, 6, 8, 6,
    8, 6, 8, 6, 0, 6, 9, 0, 9, 0, 24, 4, 24, 0, 6, 8, 5, 6, 8, 6, 8, 6, 8,
    6, 8, 5, 0, 9, 24, 28, 6, 28, 0, 6, 8, 5, 8, 6, 8, 6, 8, 6, 8, 5, 9,
    5, 6, 8, 6, 8, 6, 8, 6, 8, 0, 24, 5, 8, 6, 8, 6, 0, 24, 9, 0, 5, 9, 5,
    4, 24, 0, 24, 0, 6, 24, 6, 8, 6, 5, 6, 5, 8, 6, 5, 0, 2, 4, 2, 4, 2,
    4, 6, 0, 6, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 0, 1, 0, 2, 1, 2, 1, 2, 0, 1, 0, 2, 0, 1, 0, 1, 0, 1, 0, 1, 2, 1,
    2, 0, 2, 3, 2, 3, 2, 3, 2, 0, 2, 1, 3, 27, 2, 27, 2, 0, 2, 1, 3, 27,
    2, 0, 2, 1, 0, 27, 2, 1, 27, 0, 2, 0, 2, 1, 3, 27, 0, 12, 16, 20, 24,
    29, 30, 21, 29, 30, 21, 29, 24, 13, 14, 16, 12, 24, 29, 30, 24, 23,
    24, 25, 21, 22, 24, 25, 24, 23, 24, 12, 16, 0, 16, 11, 4, 0, 11, 25,
    21, 22, 4, 11, 25, 21, 22, 0, 4, 0, 26, 0, 6, 7, 6, 7, 6, 0, 28, 1,
    28, 1, 28, 2, 1, 2, 1, 2, 28, 1, 28, 25, 1, 28, 1, 28, 1, 28, 1, 28,
    1, 28, 2, 1, 2, 5, 2, 28, 2, 1, 25, 1, 2, 28, 25, 28, 2, 28, 11, 10,
    1, 2, 10, 11, 0, 25, 28, 25, 28, 25, 28, 25, 28, 25, 28, 25, 28, 25,
    28, 25, 28, 25, 28, 25, 28, 25, 28, 25, 28, 21, 22, 28, 25, 28, 25,
    28, 25, 28, 0, 28, 0, 28, 0, 11, 28, 11, 28, 25, 28, 25, 28, 25, 28,
    25, 28, 0, 28, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22,
    11, 28, 25, 21, 22, 25, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22, 25,
    28, 25, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22, 21,
    22, 21, 22, 21, 22, 21, 22, 25, 21, 22, 21, 22, 25, 21, 22, 25, 28,
    25, 28, 25, 0, 28, 0, 1, 0, 2, 0, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 4, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 28, 1, 2, 1, 2, 6, 1, 2, 0, 24,
    11, 24, 2, 0, 2, 0, 2, 0, 5, 0, 4, 24, 0, 6, 5, 0, 5, 0, 5, 0, 5, 0,
    5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 6, 24, 29, 30, 29, 30, 24, 29, 30, 24,
    29, 30, 24, 20, 24, 20, 24, 29, 30, 24, 29, 30, 21, 22, 21, 22, 21,
    22, 21, 22, 24, 4, 24, 20, 0, 28, 0, 28, 0, 28, 0, 28, 0, 12, 24, 28,
    4, 5, 10, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22, 28, 21, 22, 21, 22,
    21, 22, 21, 22, 20, 21, 22, 28, 10, 6, 8, 20, 4, 28, 10, 4, 5, 24, 28,
    0, 5, 0, 6, 27, 4, 5, 20, 5, 24, 4, 5, 0, 5, 0, 5, 0, 28, 11, 28, 5,
    0, 28, 0, 5, 28, 0, 11, 28, 11, 28, 11, 28, 11, 28, 11, 28, 0, 28, 5,
    0, 28, 5, 0, 5, 4, 5, 0, 28, 0, 5, 4, 24, 5, 4, 24, 5, 9, 5, 0, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 5, 6,
    7, 24, 6, 24, 4, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 0, 6, 5, 10, 6, 24, 0, 27, 4, 27, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
    1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
    2, 4, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 4, 27, 1, 2, 1, 2,
    0, 1, 2, 1, 2, 0, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 0, 4, 2, 5, 6, 5,
    6, 5, 6, 5, 8, 6, 8, 28, 0, 11, 28, 26, 28, 0, 5, 24, 0, 8, 5, 8, 6,
    0, 24, 9, 0, 6, 5, 24, 5, 0, 9, 5, 6, 24, 5, 6, 8, 0, 24, 5, 0, 6, 8,
    5, 6, 8, 6, 8, 6, 8, 24, 0, 4, 9, 0, 24, 0, 5, 6, 8, 6, 8, 6, 0, 5, 6,
    5, 6, 8, 0, 9, 0, 24, 5, 4, 5, 28, 5, 8, 0, 5, 6, 5, 6, 5, 6, 5, 6, 5,
    6, 5, 0, 5, 4, 24, 5, 8, 6, 8, 24, 5, 4, 8, 6, 0, 5, 0, 5, 0, 5, 0, 5,
    0, 5, 0, 5, 8, 6, 8, 6, 8, 24, 8, 6, 0, 9, 0, 5, 0, 5, 0, 5, 0, 19,
    18, 5, 0, 5, 0, 2, 0, 2, 0, 5, 6, 5, 25, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0,
    5, 27, 0, 5, 21, 22, 0, 5, 0, 5, 0, 5, 26, 28, 0, 6, 24, 21, 22, 24,
    0, 6, 0, 24, 20, 23, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22, 21, 22,
    21, 22, 21, 22, 24, 21, 22, 24, 23, 24, 0, 24, 20, 21, 22, 21, 22, 21,
    22, 24, 25, 20, 25, 0, 24, 26, 24, 0, 5, 0, 5, 0, 16, 0, 24, 26, 24,
    21, 22, 24, 25, 24, 20, 24, 9, 24, 25, 24, 1, 21, 24, 22, 27, 23, 27,
    2, 21, 25, 22, 25, 21, 22, 24, 21, 22, 24, 5, 4, 5, 4, 5, 0, 5, 0, 5,
    0, 5, 0, 5, 0, 26, 25, 27, 28, 26, 0, 28, 25, 28, 0, 16, 28, 0, 5, 0,
    5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 24, 0, 11, 0, 28, 10, 11, 28, 11,
    0, 28, 0, 28, 6, 0, 5, 0, 5, 0, 5, 0, 11, 0, 5, 10, 5, 10, 0, 5, 0,
    24, 5, 0, 5, 24, 10, 0, 1, 2, 5, 0, 9, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5,
    0, 5, 0, 24, 11, 0, 5, 11, 0, 24, 5, 0, 24, 0, 5, 0, 5, 0, 5, 6, 0, 6,
    0, 6, 5, 0, 5, 0, 5, 0, 6, 0, 6, 11, 0, 24, 0, 5, 11, 24, 0, 5, 0, 24,
    5, 0, 11, 5, 0, 11, 0, 5, 0, 11, 0, 8, 6, 8, 5, 6, 24, 0, 11, 9, 0, 6,
    8, 5, 8, 6, 8, 6, 24, 16, 24, 0, 5, 0, 9, 0, 6, 5, 6, 8, 6, 0, 9, 24,
    0, 6, 8, 5, 8, 6, 8, 5, 24, 0, 9, 0, 5, 6, 8, 6, 8, 6, 8, 6, 0, 9, 0,
    5, 0, 10, 0, 24, 0, 5, 0, 5, 0, 5, 0, 5, 8, 0, 6, 4, 0, 5, 0, 28, 0,
    28, 0, 28, 8, 6, 28, 8, 16, 6, 28, 6, 28, 6, 28, 0, 28, 6, 28, 0, 28,
    0, 11, 0, 1, 2, 1, 2, 0, 2, 1, 2, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 2,
    0, 2, 0, 2, 0, 2, 1, 2, 1, 0, 1, 0, 1, 0, 1, 0, 2, 1, 0, 1, 0, 1, 0,
    1, 0, 1, 0, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0, 1, 25, 2, 25, 2,
    1, 25, 2, 25, 2, 1, 25, 2, 25, 2, 1, 25, 2, 25, 2, 1, 25, 2, 25, 2, 1,
    2, 0, 9, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5,
    0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0,
    5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5,
    0, 25, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 11, 0, 28, 0, 28,
    0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28,
    0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28, 0, 28,
    0, 28, 0, 28, 0, 28, 0, 5, 0, 5, 0, 5, 0, 5, 0, 16, 0, 16, 0, 6, 0,
    18, 0, 18, 0)

  // Indices representing the start of ranges of codePoint that have the same
  // `isMirrored` result. It is true for the first range
  // (i.e. isMirrored(40)==true, isMirrored(41)==true, isMirrored(42)==false)
  // They where generated with the following script:
  //
  //  val indicesAndRes = (0 to Character.MAX_CODE_POINT)
  //    .map(i => (i, Character.isMirrored(i))).foldLeft[List[(Int, Boolean)]](Nil) {
  //      case (x :: xs, elem) if x._2 == elem._2 =>  x :: xs
  //      case (prevs, elem) => elem :: prevs
  //    }.reverse
  //  val isMirroredIndices = indicesAndRes.map(_._1).tail
  //  val isMirroredIndicesDeltas = isMirroredIndices.zip(
  //       0 :: isMirroredIndices.init).map(tup => tup._1 - tup._2)
  //  println(isMirroredIndicesDeltas.mkString(
  //     "isMirroredIndices: val deltas = Array[Int](", ", ", ")"))
  private[this] lazy val isMirroredIndices = {
    val deltas = Array[Int](40, 2, 18, 1, 1, 1, 28, 1, 1, 1, 29, 1, 1, 1,
      45, 1, 15, 1, 3710, 4, 1885, 2, 2460, 2, 10, 2, 54, 2, 14, 2, 177, 1,
      192, 4, 3, 6, 3, 1, 3, 2, 3, 4, 1, 4, 1, 1, 1, 1, 4, 9, 5, 1, 1, 18,
      5, 4, 9, 2, 1, 1, 1, 8, 2, 31, 2, 4, 5, 1, 9, 2, 2, 19, 5, 2, 9, 5, 2,
      2, 4, 24, 2, 16, 8, 4, 20, 2, 7, 2, 1085, 14, 74, 1, 2, 4, 1, 2, 1, 3,
      5, 4, 5, 3, 3, 14, 403, 22, 2, 21, 8, 1, 7, 6, 3, 1, 4, 5, 1, 2, 2, 5,
      4, 1, 1, 3, 2, 2, 10, 6, 2, 2, 12, 19, 1, 4, 2, 1, 1, 1, 2, 1, 1, 4,
      5, 2, 6, 3, 24, 2, 11, 2, 4, 4, 1, 2, 2, 2, 4, 43, 2, 8, 1, 40, 5, 1,
      1, 1, 3, 5, 5, 3, 4, 1, 3, 5, 1, 1, 772, 4, 3, 2, 1, 2, 14, 2, 2, 10,
      478, 10, 2, 8, 52797, 6, 5, 2, 162, 2, 18, 1, 1, 1, 28, 1, 1, 1, 29,
      1, 1, 1, 1, 2, 1, 2, 55159, 1, 57, 1, 57, 1, 57, 1, 57, 1)
    uncompressDeltas(deltas)
  }

  private[this] def uncompressDeltas(deltas: Array[Int]): Array[Int] = {
    for (i <- 1 until deltas.length)
      deltas(i) += deltas(i - 1)
    deltas
  }
}
