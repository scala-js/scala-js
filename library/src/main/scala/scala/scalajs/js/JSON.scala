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
 * The JSON object contains methods for converting values to JavaScript Object
 * Notation (JSON) and for converting JSON to values.
 *
 * MDN
 */
@native
object JSON extends Object {
  /**
   * Parse a string as JSON, optionally transforming the value produced by parsing.
   * @param text The string to parse as JSON.  See the JSON object for a
   *             description of JSON syntax.
   * @param reviver If a function, prescribes how the value originally produced
   *                by parsing is transformed, before being returned.
   *
   * MDN
   */
  def parse(text: String, reviver: Function2[Any, Any, Any] = ???): Dynamic = native

  /**
   * Convert a value to JSON, optionally replacing values if a replacer function
   * is specified, or optionally including only the specified properties if a
   * replacer array is specified.
   *
   * @param value The value to convert to a JSON string.
   * @param replacer If a function, transforms values and properties encountered
   *                 while stringifying; if an array, specifies the set of
   *                 properties included in objects in the final string.
   * @param space Causes the resulting string to be pretty-printed.
   *
   * MDN
   */
  def stringify(value: Any, replacer: Function2[String, Any, Any] = ???, space: Any = ???): String = native
  def stringify(value: Any, replacer: Array[Any]): String = native
  def stringify(value: Any, replacer: Array[Any], space: Any): String = native
}
