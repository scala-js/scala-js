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
  /** Reads a field of this object by its name.
   *
   *  This must not be called if the dictionary does not contain the key.
   */
  @JSBracketAccess
  private[js] def rawApply(key: String): A = native

  /** Writes a field of this object by its name. */
  @JSBracketAccess
  def update(key: String, value: A): Unit = native

  /** Deletes a property of this object by its name.
   *
   *  The property must be configurable.
   *  This method is equivalent to the "delete" keyword in JavaScript.
   *
   *  Since we are using strict mode, this throws an exception, if the property
   *  isn't configurable.
   */
  def delete(key: String): Unit = sys.error("stub")
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
