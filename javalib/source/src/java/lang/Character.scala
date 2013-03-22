package java.lang

import scala.js._

class Character(value: scala.Char) {
  def charValue(): scala.Char = value

  override def equals(that: Any) =
    that.isInstanceOf[Character] && (value == that.asInstanceOf[Character].charValue)

  override def toString: String =
    JSDynamic.window.String.fromCharCode(value.toInt).asInstanceOf[JSString]
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

  val MAX_RADIX: scala.Int = 0

  /* Tests */
  def getType(ch: scala.Char): scala.Int = sys.error("unimplemented")
  def getType(codePoint: scala.Int): scala.Int = sys.error("unimplemented")
  def digit(c: scala.Char, radix: scala.Int): scala.Int = sys.error("unimplemented")
  def isISOControl(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isDigit(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isLetter(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isLetterOrDigit(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isWhitespace(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isSpaceChar(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isHighSurrogate(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isLowSurrogate(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isUnicodeIdentifierStart(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isUnicodeIdentifierPart(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isIdentifierIgnorable(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isMirrored(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isLowerCase(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isUpperCase(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isTitleCase(c: scala.Char): scala.Boolean = sys.error("unimplemented")
  def isJavaIdentifierPart(c: scala.Char): scala.Boolean = sys.error("unimplemented")

  def getDirectionality(c: scala.Char): scala.Byte = sys.error("unimplemented")

  /* Conversions */
  def toUpperCase(c: scala.Char): scala.Char = sys.error("unimplemented")
  def toLowerCase(c: scala.Char): scala.Char = sys.error("unimplemented")
  def toTitleCase(c: scala.Char): scala.Char = sys.error("unimplemented")
  def getNumericValue(c: scala.Char): scala.Int = sys.error("unimplemented")

  /* Misc */
  def reverseBytes(ch: scala.Char): scala.Char = sys.error("unimplemented")

  def toString(c: scala.Char) = valueOf(c).toString
}
