package java.lang

import scala.scalajs.js

class Character(value: scala.Char) {
  def charValue(): scala.Char = value

  override def equals(that: Any) =
    that.isInstanceOf[Character] && (value == that.asInstanceOf[Character].charValue)

  override def toString: String =
    js.Dynamic.global.String.fromCharCode(value.toInt).asInstanceOf[js.String]
}

object Character {
  val TYPE = classOf[scala.Char]
  val MIN_VALUE: scala.Char = 0
  val MAX_VALUE: scala.Char = 0xff

  def valueOf(charValue: scala.Char) = new Character(charValue)

  val LOWERCASE_LETTER: scala.Byte = 0
  val UPPERCASE_LETTER: scala.Byte = 0
  val OTHER_LETTER: scala.Byte = 0
  val TITLECASE_LETTER: scala.Byte = 0
  val LETTER_NUMBER: scala.Byte = 0
  val COMBINING_SPACING_MARK: scala.Byte = 0
  val ENCLOSING_MARK: scala.Byte = 0
  val NON_SPACING_MARK: scala.Byte = 0
  val MODIFIER_LETTER: scala.Byte = 0
  val DECIMAL_DIGIT_NUMBER: scala.Byte = 0
  val SURROGATE: scala.Byte = 0

  val MIN_RADIX: scala.Int = 2
  val MAX_RADIX: scala.Int = 36

  val MIN_HIGH_SURROGATE: scala.Char = '\uD800'
  val MAX_HIGH_SURROGATE: scala.Char = '\uDBFF'
  val MIN_LOW_SURROGATE: scala.Char = '\uDC00'
  val MAX_LOW_SURROGATE: scala.Char = '\uDFFF'
  val MIN_SURROGATE: scala.Char = MIN_HIGH_SURROGATE
  val MAX_SURROGATE: scala.Char = MAX_LOW_SURROGATE

  /* Tests */
  def getType(ch: scala.Char): scala.Int = sys.error("unimplemented")
  def getType(codePoint: scala.Int): scala.Int = sys.error("unimplemented")
  def digit(c: scala.Char, radix: scala.Int): scala.Int = {
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

  def isISOControl(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isDigit(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isLetter(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isLetterOrDigit(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isWhitespace(c: scala.Char): scala.Boolean = js.RegExp("^\\s$").test(c.toString)
  def isSpaceChar(c: scala.Char): scala.Boolean = sys.error("unimplemented")

  def isHighSurrogate(c: scala.Char): scala.Boolean =
    (c >= MIN_HIGH_SURROGATE) && (c <= MAX_HIGH_SURROGATE)
  def isLowSurrogate(c: scala.Char): scala.Boolean =
    (c >= MIN_LOW_SURROGATE) && (c <= MAX_LOW_SURROGATE)
  def isSurrogatePair(high: scala.Char, low: scala.Char): scala.Boolean =
    isHighSurrogate(high) && isLowSurrogate(low)

  def isUnicodeIdentifierStart(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isUnicodeIdentifierPart(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isIdentifierIgnorable(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isMirrored(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isLowerCase(c: scala.Char): scala.Boolean = toLowerCase(c) == c
  def isUpperCase(c: scala.Char): scala.Boolean = toUpperCase(c) == c
  def isTitleCase(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isJavaIdentifierPart(c: scala.Char): scala.Boolean = sys.error("unimplemented")

  def getDirectionality(c: scala.Char): scala.Byte = sys.error("unimplemented")

  /* Conversions */
  def toUpperCase(c: scala.Char): scala.Char = c.toString.toUpperCase()(0)
  def toLowerCase(c: scala.Char): scala.Char = c.toString.toLowerCase()(0)
  def toTitleCase(c: scala.Char): scala.Char = sys.error("unimplemented")
  def getNumericValue(c: scala.Char): scala.Int = sys.error("unimplemented")

  /* Misc */
  def reverseBytes(ch: scala.Char): scala.Char = sys.error("unimplemented")

  def toString(c: scala.Char) = valueOf(c).toString
}
