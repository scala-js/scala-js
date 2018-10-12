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

import annotation.JSBracketAccess

/** Dictionary "view" of a JavaScript value.
 *
 *  Using objects as dictionaries (maps from strings to values) through their
 *  properties is a common idiom in JavaScript. This trait lets you treat an
 *  object as such a dictionary, with the familiar API of a Map.
 *
 *  To use it, cast your object, say `x`, into a [[Dictionary]] using
 *  {{{
 *  val xDict = x.asInstanceOf[js.Dictionary[Int]]
 *  }}}
 *  then use it as
 *  {{{
 *  xDict("prop") = 5
 *  println(xDict.get("prop")) // displays Some(5)
 *  xDict -= "prop"            // removes the property "prop"
 *  println(xDict.get("prop")) // displays None
 *  }}}
 *
 *  To enumerate all the keys of a dictionary, use collection methods or
 *  for comprehensions. For example:
 *  {{{
 *  for ((prop, value) <- xDict) {
 *    println(prop + " -> " + value)
 *  }
 *  }}}
 *  Note that this does not enumerate properties in the prototype chain of
 *  `xDict`.
 *
 *  This trait extends [[Any js.Any]] directly, because it is not safe to
 *  call methods of [[Object js.Object]] on it, given that the name of these
 *  methods could be used as keys in the dictionary.
 */
@native
sealed trait Dictionary[A] extends Any {
  /** Writes a field of this object by its name. */
  @JSBracketAccess
  def update(key: String, value: A): Unit = native

  /* Note: `delete` cannot be replaced by a user-land implementation in
   * WrappedDictionary, because it would break forward compiler-library
   * compatibility. If someone uses an 0.6.16 compiler with an 0.6.17 library
   * (which can easily happen if depending on a third-party library compiled
   * with 0.6.17), the compiler will look for this symbol and crash.
   */

  /** Deletes a property of this object by its name.
   *
   *  The property must be configurable.
   *  This method is equivalent to the "delete" keyword in JavaScript.
   *
   *  Since we are using strict mode, this throws an exception, if the property
   *  isn't configurable.
   */
  @deprecated("Use -= instead.", "0.6.17")
  def delete(key: String): Unit = throw new java.lang.Error("stub")
}

/** Factory for [[Dictionary]] instances. */
object Dictionary {
  /** Returns a new empty dictionary */
  @inline def empty[A]: Dictionary[A] =
    (new Object).asInstanceOf[Dictionary[A]]

  def apply[A](properties: (String, A)*): Dictionary[A] = {
    val result = empty[A]
    for ((key, value) <- properties)
      result(key) = value
    result
  }
}
