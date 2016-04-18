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
package scala.scalajs.js

import scala.language.implicitConversions

import scala.scalajs.js.annotation._

/** Operations on JavaScript strings.
 *
 *  The methods with an equivalent signature in [[java.lang.String String]] but
 *  with a different meaning are prefixed by `js` in this trait.
 */
@native
trait JSStringOps extends Any {

  /**
   * Returns the index within the calling String object of the first occurrence
   * of the specified value, starting the search at fromIndex,
   *
   * returns -1 if the value is not found.
   *
   * MDN
   */
  @JSName("indexOf")
  def jsIndexOf(searchString: String, position: Int): Int = native
  @JSName("indexOf")
  def jsIndexOf(searchString: String): Int = native

  /**
   * Returns the index within the calling String object of the last occurrence
   * of the specified value, or -1 if not found. The calling string is searched
   * backward, starting at fromIndex.
   *
   * MDN
   */
  @JSName("lastIndexOf")
  def jsLastIndexOf(searchString: String, position: Int): Int = native
  @JSName("lastIndexOf")
  def jsLastIndexOf(searchString: String): Int = native

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
  def localeCompare(that: String): Int = native

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
  def `match`(regexp: String): Array[String] = native
  def `match`(regexp: RegExp): Array[String] = native

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
  @JSName("replace")
  def jsReplace(searchValue: String, replaceValue: String): String = native
  @JSName("replace")
  def jsReplace(searchValue: String, replaceValue: Any): String = native
  @JSName("replace")
  def jsReplace(searchValue: RegExp, replaceValue: String): String = native
  @JSName("replace")
  def jsReplace(searchValue: RegExp, replaceValue: Any): String = native

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
  def search(regexp: String): Int = native
  def search(regexp: RegExp): Int = native

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
  @JSName("slice")
  def jsSlice(start: Int, end: Int): String = native
  @JSName("slice")
  def jsSlice(start: Int): String = native

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
  @JSName("split")
  def jsSplit(separator: String, limit: Int): Array[String] = native
  @JSName("split")
  def jsSplit(separator: String): Array[String] = native
  @JSName("split")
  def jsSplit(separator: RegExp, limit: Int): Array[String] = native
  @JSName("split")
  def jsSplit(separator: RegExp): Array[String] = native

  /**
   * Returns a subset of a string between one index and another, or through
   * the end of the string.
   *
   * MDN
   */
  @JSName("substring")
  def jsSubstring(start: Int, end: Int): String = native
  @JSName("substring")
  def jsSubstring(start: Int): String = native

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
  def toLocaleLowerCase(): String = native

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
  def toLocaleUpperCase(): String = native

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  Returns the Unicode Normalization Form of this string.
   */
  def normalize(
      form: UnicodeNormalizationForm = UnicodeNormalizationForm.NFC): String = native
}

object JSStringOps {
  implicit def enableJSStringOps(x: String): JSStringOps =
    x.asInstanceOf[JSStringOps]
}
