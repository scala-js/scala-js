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

@native
class Error(message0: String = "") extends Object {
  val name: String = native
  /**
   * Human-readable description of the error
   *
   * MDN
   */
  val message: String = native
}

@native
object Error extends Object {
  def apply(message: String = ""): Error = native
}

/**
 * An instance representing an error that occurs regarding the global function
 * eval()
 *
 * MDN
 */
@native
class EvalError(message: String = "") extends Error

@native
object EvalError extends Object {
  def apply(message: String = ""): EvalError = native
}

/**
 * An instance representing an error that occurs when a numeric variable or
 * parameter is outside of its valid range.
 *
 * A RangeError is thrown when trying to pass a number as an argument to a
 * function that does not allow a range that includes that number. This can
 * be encountered when to create an array of an illegal length with the Array
 * constructor, or when passing bad values to the numeric methods toExponential,
 * toFixed, or toPrecision.
 *
 * MDN
 */
@native
class RangeError(message: String = "") extends Error

@native
object RangeError extends Object {
  def apply(message: String = ""): RangeError = native
}

/**
 * Represents an error when a non-existent variable is referenced.
 *
 * A ReferenceError is thrown when trying to dereference a variable that has
 * not been declared.
 *
 * MDN
 */
@native
class ReferenceError(message: String = "") extends Error

@native
object ReferenceError extends Object {
  def apply(message: String = ""): ReferenceError = native
}

/**
 * Represents an error when trying to interpret syntactically invalid code.
 *
 * A SyntaxError is thrown when the JavaScript engine encounters tokens or
 * token order that does not conform to the syntax of the language when parsing code.
 *
 * MDN
 */
@native
class SyntaxError(message: String = "") extends Error

@native
object SyntaxError extends Object {
  def apply(message: String = ""): SyntaxError = native
}

/**
 * Represents an error when a value is not of the expected type.
 *
 * A TypeError is thrown when an operand or argument passed to a function is
 * incompatible with the type expected by that operator or function.
 *
 * MDN
 */
@native
class TypeError(message: String = "") extends Error

@native
object TypeError extends Object {
  def apply(message: String = ""): TypeError = native
}

/**
 * Represents an error when a malformed URI is encountered.
 *
 * A URIError is thrown when the URI handling functions are passed a malformed URI.
 *
 * MDN
 */
@native
class URIError(message: String = "") extends Error

@native
object URIError extends Object {
  def apply(message: String = ""): URIError = native
}
