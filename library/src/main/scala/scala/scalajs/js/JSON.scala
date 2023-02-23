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
      reviver: js.Function2[js.Any, js.Any, js.Any] = js.native): js.Dynamic = js.native

  // scalastyle:off line.size.limit
  /**
   * Convert a value to JSON, optionally replacing values if a replacer function
   * is specified, or optionally including only the specified properties if a
   * replacer array is specified.
   *
   *   - If the value has a
   *     [[https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify#toJSON_behavior toJSON()]]
   *     method, it is responsible to define what data will be serialized.
   *   - [[Boolean]], [[Double Number]], and [[java.lang.String String]] objects are converted to
   *     the corresponding primitive values during stringification, in accord
   *     with the traditional conversion semantics.
   *   - [[undefined]], [[Function]]s, and [[Symbol]]s are not valid JSON
   *     values. If any such values are encountered during conversion they are
   *     either omitted (when found in an [[Object]]) or changed to null (when
   *     found in an [[Array]]). In Scala.js it is undefined behavior to directly
   *     pass any of these values to stringify.
   *   - All [[Symbol]]-keyed properties will be completely ignored, even when
   *     using the replacer function.
   *   - The instances of [[Date]] implement the [[Date#toJSON()*]] function by
   *     returning a string (the same as [[Date#toISOString]]). Thus, they are
   *     treated as strings.
   *   - The numbers Infinity and NaN, as well as the value null, are all
   *     considered null.
   *   - All the other [[Object]] instances (including Map, Set, WeakMap, and
   *     WeakSet) will have only their enumerable properties serialized.
   *
   * @param value The value to convert to a JSON string.
   * @param replacer If a function, transforms values and properties encountered
   *                 while stringifying; if an array, specifies the set of
   *                 properties included in objects in the final string.
   * @param space A String or Int that's used to insert white space into the
   *              output JSON string for readability purposes. If this is an
   *              Int, it indicates the number of space characters to use as
   *              white space; this number is capped at 10; values less than 1
   *              indicate that no space should be used. If this is a String,
   *              the string (or the first 10 characters of the string, if it's
   *              longer than that) is used as white space. If this parameter is
   *              not provided (or is null), no white space is used.
   *
   * MDN
   */
  // scalastyle:on line.size.limit
  def stringify(value: js.Any,
      replacer: js.Function2[String, js.Any, js.Any] = js.native,
      space: Int | String = js.native): String = js.native
  def stringify(value: js.Any, replacer: js.Array[Any]): String = js.native
  def stringify(value: js.Any, replacer: js.Array[Any],
      space: Int | String): String = js.native
}
