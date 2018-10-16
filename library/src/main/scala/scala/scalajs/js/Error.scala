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

/**
 * All doc-comments marked as "MDN" are by Mozilla Contributors,
 * distributed under the Creative Commons Attribution-ShareAlike license from
 * https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js

import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
@JSGlobal
class Error(message0: String = "") extends js.Object {
  val name: String = js.native

  /**
   * Human-readable description of the error
   *
   * MDN
   */
  val message: String = js.native
}

@js.native
@JSGlobal
object Error extends js.Object {
  def apply(message: String = ""): js.Error = js.native
}

/**
 * An instance representing an error that occurs regarding the global function
 * `eval()`.
 *
 * MDN
 */
@js.native
@JSGlobal
class EvalError(message: String = "") extends js.Error

@js.native
@JSGlobal
object EvalError extends js.Object {
  def apply(message: String = ""): js.EvalError = js.native
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
@js.native
@JSGlobal
class RangeError(message: String = "") extends js.Error

@js.native
@JSGlobal
object RangeError extends js.Object {
  def apply(message: String = ""): js.RangeError = js.native
}

/**
 * Represents an error when a non-existent variable is referenced.
 *
 * A ReferenceError is thrown when trying to dereference a variable that has
 * not been declared.
 *
 * MDN
 */
@js.native
@JSGlobal
class ReferenceError(message: String = "") extends js.Error

@js.native
@JSGlobal
object ReferenceError extends js.Object {
  def apply(message: String = ""): js.ReferenceError = js.native
}

/**
 * Represents an error when trying to interpret syntactically invalid code.
 *
 * A SyntaxError is thrown when the JavaScript engine encounters tokens or
 * token order that does not conform to the syntax of the language when parsing code.
 *
 * MDN
 */
@js.native
@JSGlobal
class SyntaxError(message: String = "") extends js.Error

@js.native
@JSGlobal
object SyntaxError extends js.Object {
  def apply(message: String = ""): js.SyntaxError = js.native
}

/**
 * Represents an error when a value is not of the expected type.
 *
 * A TypeError is thrown when an operand or argument passed to a function is
 * incompatible with the type expected by that operator or function.
 *
 * MDN
 */
@js.native
@JSGlobal
class TypeError(message: String = "") extends js.Error

@js.native
@JSGlobal
object TypeError extends js.Object {
  def apply(message: String = ""): js.TypeError = js.native
}

/**
 * Represents an error when a malformed URI is encountered.
 *
 * A URIError is thrown when the URI handling functions are passed a malformed URI.
 *
 * MDN
 */
@js.native
@JSGlobal
class URIError(message: String = "") extends js.Error

@js.native
@JSGlobal
object URIError extends js.Object {
  def apply(message: String = ""): js.URIError = js.native
}
