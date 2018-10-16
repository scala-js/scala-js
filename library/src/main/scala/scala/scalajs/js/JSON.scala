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

/**
 * The JSON object contains methods for converting values to JavaScript Object
 * Notation (JSON) and for converting JSON to values.
 *
 * MDN
 */
@js.native
@JSGlobal
object JSON extends js.Object {
  /**
   * Parse a string as JSON, optionally transforming the value produced by parsing.
   * @param text The string to parse as JSON.  See the JSON object for a
   *             description of JSON syntax.
   * @param reviver If a function, prescribes how the value originally produced
   *                by parsing is transformed, before being returned.
   *
   * MDN
   */
  def parse(text: String,
      reviver: js.Function2[js.Any, js.Any, js.Any] = ???): js.Dynamic = js.native

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
  def stringify(value: js.Any,
      replacer: js.Function2[String, js.Any, js.Any] = ???,
      space: js.Any = ???): String = js.native
  def stringify(value: js.Any, replacer: js.Array[Any]): String = js.native
  def stringify(value: js.Any, replacer: js.Array[Any],
      space: js.Any): String = js.native
}
