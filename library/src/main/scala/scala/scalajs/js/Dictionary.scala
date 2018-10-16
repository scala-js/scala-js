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
sealed trait Dictionary[A] extends js.Any

/** Factory for [[js.Dictionary]] instances. */
object Dictionary {
  /** Returns a new empty dictionary */
  @inline def empty[A]: js.Dictionary[A] =
    (new js.Object).asInstanceOf[js.Dictionary[A]]

  @inline
  def apply[A](properties: (String, A)*): js.Dictionary[A] =
    js.special.objectLiteral(properties: _*).asInstanceOf[js.Dictionary[A]]
}
