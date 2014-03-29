/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


/**
 * All doc-comments marked as "MDN" are by Mozilla Contributors,
 * distributed under the Creative Commons Attribution-ShareAlike license from
 * https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js.prim

import scala.language.implicitConversions

import scala.scalajs.js.{Any, Dynamic, Object, Array, RegExp, prim}

/** Primitive JavaScript number. */
sealed trait Number extends Any {
  def unary_+(): Number
  def unary_-(): Number
  def unary_~(): Number

  def +(that: Number): Number
  def -(that: Number): Number
  def *(that: Number): Number
  def /(that: Number): Number
  def %(that: Number): Number
  def <<(that: Number): Number
  def >>(that: Number): Number
  def >>>(that: Number): Number
  def &(that: Number): Number
  def |(that: Number): Number
  def ^(that: Number): Number

  def +(that: Dynamic): Dynamic // could be a String if that is a String
  def -(that: Dynamic): Number
  def *(that: Dynamic): Number
  def /(that: Dynamic): Number
  def %(that: Dynamic): Number
  def <<(that: Dynamic): Number
  def >>(that: Dynamic): Number
  def >>>(that: Dynamic): Number
  def &(that: Dynamic): Number
  def |(that: Dynamic): Number
  def ^(that: Dynamic): Number

  def <(that: Number): Boolean
  def >(that: Number): Boolean
  def <=(that: Number): Boolean
  def >=(that: Number): Boolean

  def <(that: Dynamic): Boolean
  def >(that: Dynamic): Boolean
  def <=(that: Dynamic): Boolean
  def >=(that: Dynamic): Boolean

  def toString(radix: Number): String = ???

  /**
   * Returns a string representation of number that does not use exponential
   * notation and has exactly digits digits after the decimal place. The number
   * is rounded if necessary, and the fractional part is padded with zeros if
   * necessary so that it has the specified length. If number is greater than
   * 1e+21, this method simply calls Number.prototype.toString() and returns
   * a string in exponential notation.
   *
   * MDN
   */
  def toFixed(fractionDigits: Number): String = ???
  def toFixed(): String = ???

  /**
   * Returns a string representing a Number object in exponential notation with one
   * digit before the decimal point, rounded to fractionDigits digits after the
   * decimal point. If the fractionDigits argument is omitted, the number of
   * digits after the decimal point defaults to the number of digits necessary
   * to represent the value uniquely.
   *
   * If a number has more digits that requested by the fractionDigits parameter,
   * the number is rounded to the nearest number represented by fractionDigits
   * digits. See the discussion of rounding in the description of the toFixed()
   * method, which also applies to toExponential().
   *
   * MDN
   */
  def toExponential(fractionDigits: Number): String = ???
  def toExponential(): String = ???

  /**
   * Returns a string representing a Number object in fixed-point or exponential
   * notation rounded to precision significant digits. See the discussion of
   * rounding in the description of the Number.prototype.toFixed() method, which
   * also applies to toPrecision.
   *
   * If the precision argument is omitted, behaves as Number.prototype.toString().
   * If it is a non-integer value, it is rounded to the nearest integer.
   *
   * MDN
   */
  def toPrecision(precision: Number): String = ???
  def toPrecision(): String = ???
}

/** The top-level `Number` JavaScript object */
object Number extends Object {
  implicit def toDouble(value: prim.Number): scala.Double = sys.error("stub")

  /**
   * The Number.MAX_VALUE property represents the maximum numeric value
   * representable in JavaScript.
   *
   * The MAX_VALUE property has a value of approximately 1.79E+308. Values
   * larger than MAX_VALUE are represented as "Infinity".
   *
   * MDN
   */
  val MAX_VALUE: Double = ???
  /**
   * The Number.MIN_VALUE property represents the smallest positive numeric
   * value representable in JavaScript.
   *
   * The MIN_VALUE property is the number closest to 0, not the most negative
   * number, that JavaScript can represent.
   *
   * MIN_VALUE has a value of approximately 5e-324. Values smaller than MIN_VALUE
   * ("underflow values") are converted to 0.
   *
   * MDN
   */
  val MIN_VALUE: Double = ???
  /**
   * The Number.NaN property represents Not-A-Number. Equivalent of NaN.
   *
   * MDN
   */
  val NaN: Double = ???

  /**
   * The Number.NEGATIVE_INFINITY property represents the negative Infinity value.
   *
   * MDN
   */
  val NEGATIVE_INFINITY: Double = ???
  /**
   * The Number.POSITIVE_INFINITY property represents the positive Infinity value.
   *
   * MDN
   */
  val POSITIVE_INFINITY: Double = ???
}

/** Primitive JavaScript boolean. */
sealed trait Boolean extends Any {
  def &&(that: Boolean): Boolean
  def ||(that: Boolean): Boolean

  // See the comment in `Dynamic` for the rationale of returning Boolean here.
  def &&(that: Dynamic): Boolean
  def ||(that: Dynamic): Boolean
}

/** The top-level `Boolean` JavaScript object. */
object Boolean extends Object {
  implicit def toBoolean(value: prim.Boolean): scala.Boolean = sys.error("stub")
}

/** Primitive JavaScript string. */
sealed trait String extends Any {
  def +(that: String): String
  def +(that: Any): String
  def +(that: Dynamic): String

  def < (that: String): Boolean
  def < (that: Dynamic): Boolean

  def > (that: String): Boolean
  def > (that: Dynamic): Boolean

  def <=(that: String): Boolean
  def <=(that: Dynamic): Boolean

  def >=(that: String): Boolean
  def >=(that: Dynamic): Boolean

  /**
   * This property returns the number of code units in the string. UTF-16,
   * the string format used by JavaScript, uses a single 16-bit code unit to
   * represent the most common characters, but needs to use two code units for
   * less commonly-used characters, so it's possible for the value returned by
   * length to not match the actual number of characters in the string.
   *
   * For an empty string, length is 0.
   *
   * MDN
   */
  val length: Number = ???

  /**
   * The chartAt() method returns the specified character from a string.
   *
   * Characters in a string are indexed from left to right. The index of the
   * first character is 0, and the index of the last character in a string
   * called stringName is stringName.length - 1. If the index you supply is out
   * of range, JavaScript returns an empty string.
   *
   * MDN
   */
  def charAt(pos: Number): String = ???

  /**
   * The charCodeAt() method returns the numeric Unicode value of the character
   * at the given index (except for unicode codepoints > 0x10000).
   *
   * MDN
   */
  def charCodeAt(index: Number): Number = ???

  /**
   * concat combines the text from one or more strings and returns a new string.
   * Changes to the text in one string do not affect the other string.
   * MDN
   */
  def concat(strings: String*): String = ???

  /**
   * Returns the index within the calling String object of the first occurrence
   * of the specified value, starting the search at fromIndex,
   *
   * returns -1 if the value is not found.
   *
   * MDN
   */
  def indexOf(searchString: String, position: Number): Number = ???
  def indexOf(searchString: String): Number = ???

  /**
   * Returns the index within the calling String object of the last occurrence
   * of the specified value, or -1 if not found. The calling string is searched
   * backward, starting at fromIndex.
   *
   * MDN
   */
  def lastIndexOf(searchString: String, position: Number): Number = ???
  def lastIndexOf(searchString: String): Number = ???

  /**
   * Returns a number indicating whether a reference string comes before or
   * after or is the same as the given string in sort order. The new locales
   * and options arguments let applications specify the language whose sort
   * order should be used and customize the behavior of the function. In older
   * implementations, which ignore the locales and options arguments, the locale
   * and sort order used are entirely implementation dependent.
   *
   * MDN
   */
  def localeCompare(that: String): Number = ???

  /**
   * Used to retrieve the matches when matching a string against a regular
   * expression.
   *
   * If the regular expression does not include the g flag, returns the same
   * result as regexp.exec(string). The returned Array has an extra input
   * property, which contains the original string that was parsed. In addition,
   * it has an index property, which represents the zero-based index of the
   * match in the string.
   *
   * If the regular expression includes the g flag, the method returns an Array
   * containing all matches. If there were no matches, the method returns null.
   *
   * MDN
   */
  def `match`(regexp: String): Array[String] = ???
  def `match`(regexp: RegExp): Array[String] = ???

  /**
   * Returns a new string with some or all matches of a pattern replaced by a
   * replacement.  The pattern can be a string or a RegExp, and the replacement
   * can be a string or a function to be called for each match.
   *
   * This method does not change the String object it is called on. It simply
   * returns a new string.
   *
   * To perform a global search and replace, either include the g switch in the
   * regular expression or if the first parameter is a string, include g in the
   * flags parameter.
   *
   * MDN
   */
  def replace(searchValue: String, replaceValue: String): String = ???
  def replace(searchValue: String, replaceValue: Any): String = ???
  def replace(searchValue: RegExp, replaceValue: String): String = ???
  def replace(searchValue: RegExp, replaceValue: Any): String = ???

  /**
   * If successful, search returns the index of the regular expression inside
   * the string. Otherwise, it returns -1.
   *
   * When you want to know whether a pattern is found in a string use search
   * (similar to the regular expression test method); for more information
   * (but slower execution) use match (similar to the regular expression exec
   * method).
   *
   * MDN
   */
  def search(regexp: String): Number = ???
  def search(regexp: RegExp): Number = ???

  /**
   * slice extracts the text from one string and returns a new string. Changes
   * to the text in one string do not affect the other string.
   *
   * slice extracts up to but not including endSlice. string.slice(1,4) extracts
   * the second character through the fourth character (characters indexed 1, 2,
   * and 3).
   *
   * As an example, string.slice(2,-1) extracts the third character through the
   * second to last character in the string.
   *
   * MDN
   */
  def slice(start: Number, end: Number): String = ???
  def slice(start: Number): String = ???

  /**
   * Splits a String object into an array of strings by separating the string
   * into substrings.
   *
   * When found, separator is removed from the string and the substrings are
   * returned in an array. If separator is omitted, the array contains one
   * element consisting of the entire string. If separator is an empty string,
   * string is converted to an array of characters.
   *
   * If separator is a regular expression that contains capturing parentheses,
   * then each time separator is matched, the results (including any undefined
   * results) of the capturing parentheses are spliced into the output array.
   * However, not all browsers support this capability.
   *
   * Note: When the string is empty, split returns an array containing one
   * empty string, rather than an empty array.
   *
   * MDN
   */
  def split(separator: String, limit: Number): Array[String] = ???
  def split(separator: String): Array[String] = ???
  def split(separator: RegExp, limit: Number): Array[String] = ???
  def split(separator: RegExp): Array[String] = ???

  /**
   * Returns a subset of a string between one index and another, or through
   * the end of the string.
   *
   * MDN
   */
  def substring(start: Number, end: Number): String = ???
  def substring(start: Number): String = ???

  /**
   * Returns the calling string value converted to lowercase.
   *
   * MDN
   */
  def toLowerCase(): String = ???

  /**
   * The toLocaleLowerCase method returns the value of the string converted to
   * lower case according to any locale-specific case mappings. toLocaleLowerCase
   * does not affect the value of the string itself. In most cases, this will
   * produce the same result as toLowerCase(), but for some locales, such as
   * Turkish, whose case mappings do not follow the default case mappings in Unicode,
   * there may be a different result.
   *
   * MDN
   */
  def toLocaleLowerCase(): String = ???

  /**
   * Returns the calling string value converted to uppercase.
   *
   * MDN
   */
  def toUpperCase(): String = ???

  /**
   * The toLocaleUpperCase method returns the value of the string converted to
   * upper case according to any locale-specific case mappings. toLocaleUpperCase
   * does not affect the value of the string itself. In most cases, this will
   * produce the same result as toUpperCase(), but for some locales, such as
   * Turkish, whose case mappings do not follow the default case mappings in Unicode,
   * there may be a different result.
   *
   * MDN
   */
  def toLocaleUpperCase(): String = ???

  /**
   * Removes whitespace from both ends of the string.
   *
   * MDN
   */
  def trim(): String = ???
}

/** The top-level `String` JavaScript object. */
object String extends Object {
  implicit def toScalaString(value: prim.String): java.lang.String = sys.error("stub")

  def fromCharCode(codes: Int*): java.lang.String = ???
}

/** Primitive JavaScript undefined value. */
sealed trait Undefined extends Any
