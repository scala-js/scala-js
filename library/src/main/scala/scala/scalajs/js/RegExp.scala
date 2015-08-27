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

/**
 * The RegExp constructor creates a regular expression object for matching
 * text with a pattern.
 *
 * MDN
 */
@native
class RegExp(pattern: String, flags: String = "") extends Object {
  /** Creates a new RegExp with the same pattern and flags as the given one. */
  def this(pattern: RegExp) = this("", "")

  /**
   * The source property returns a String containing the text of the pattern,
   * excluding the forward slashes. It is a read-only property of an individual
   * regular expression instance. source does not contain any flags (like "g",
   * "i" or "m") of the regular expression.
   *
   * MDN
   */
  val source: String = native
  /**
   * The value of global is a Boolean and true if the "g" flag was used;
   * otherwise, false. The "g" flag indicates that the regular expression
   * should be tested against all possible matches in a string.
   *
   * MDN
   */
  val global: Boolean = native
  /**
   * The value of ignoreCase is a Boolean and true if the "i" flag was used;
   * otherwise, false. The "i" flag indicates that case should be ignored while
   * attempting a match in a string.
   *
   * MDN
   */
  val ignoreCase: Boolean = native
  /**
   * The value of multiline is a Boolean and is true if the "m" flag was used;
   * otherwise, false. The "m" flag indicates that a multiline input string
   * should be treated as multiple lines. For example, if "m" is used, "^" and
   * "$" change from matching at only the start or end of the entire string to
   * the start or end of any line within the string.

   * MDN
   */
  val multiline: Boolean = native

  /**
   * The lastIndex is a read/write integer property of regular expressions that
   * specifies the index at which to start the next match.
   *
   * MDN
   */
  var lastIndex: Int = native

  /**
   * The exec() method executes a search for a match in a specified string.
   * Returns a result array, or null.
   *
   * If you are executing a match simply to find true or false, use the
   * RegExp.test() method or the String.search() method.
   *
   * If the match succeeds, the exec method returns an array and updates properties
   * of the regular expression object. The returned array has the matched text
   * as the first item, and then one item for each capturing parenthesis that
   * matched containing the text that was captured.
   *
   * If the match fails, the exec method returns null.
   *
   * MDN
   */
  def exec(string: String): RegExp.ExecResult = native

  /**
   * The test() method executes a search for a match between a regular expression
   * and a specified string. Returns true or false.
   *
   * You can use test() whenever want to know whether a pattern is found in a
   * string (similar to the String.search method); for more information (but
   * slower execution) use the exec method (similar to the String.match method).
   * As with exec (or in combination with it), test called multiple times on the
   * same global regular expression instance will advance past the previous match.
   *
   * MDN
   */
  def test(string: String): Boolean = native
}

@native
object RegExp extends Object {
  def apply(pattern: String, flags: String = ""): RegExp = native

  @native
  trait ExecResult extends Array[UndefOr[String]] {
    var index: Int = native
    var input: String = native
  }
}
