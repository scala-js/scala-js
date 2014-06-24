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
 *  object as such a dictionary.
 *
 *  To use it, cast your object, say `x`, into a [[Dictionary]] using
 *  {{{
 *  val xDict = x.asInstanceOf[js.Dictionary[Int]]
 *  }}}
 *  then use it as
 *  {{{
 *  xDict("prop") = 5
 *  println(xDict("prop")) // displays 5
 *  xDict.delete("prop")   // removes the property "prop"
 *  println(xDict("prop")) // displays undefined
 *  }}}
 *
 *  To enumerate all the keys of a dictionary, use [[js.Object.keys]], which
 *  returns a [[js.Array]] of the properties. It can be used in a for
 *  comprehension like this:
 *  {{{
 *  for (prop <- js.Object.keys(xDict)) {
 *    val value = xDict(prop)
 *    println(prop + " -> " + value)
 *  }
 *  }}}
 */
sealed trait Dictionary[A] extends Object {
  /** Reads a field of this object by its name.
   *
   *  This will fail with a ClassCastException if the key doesn't exist and
   *  the return type doesn't allow js.undefined as value. If the return type
   *  does allow js.undefined, applying with a non-existent key will return
   *  js.undefined.
   */
  @JSBracketAccess
  def apply(key: String): A

  /** Reads a field of this object by its name.
   *
   *  This will return undefined if the key doesn't exist. It will also return
   *  undefined if the value associated with the key is undefined. To truly
   *  check for the existence of a property, use [[js.Object.hasOwnProperty]].
   */
  @JSBracketAccess
  def get(key: String): UndefOr[A]

  /** Writes a field of this object by its name. */
  @JSBracketAccess
  def update(key: String, value: A): Unit

  /** Deletes a property of this object by its name.
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
  def empty[A]: Dictionary[A] = (new Object).asInstanceOf[Dictionary[A]]

  def apply[A](properties: (String, A)*): Dictionary[A] = {
    val result = empty[A]
    for ((key, value) <- properties)
      result(key) = value
    result
  }

  /** Returns the names of all the enumerable properties of this object. */
  @deprecated("Use js.Object.properties(obj) instead", "0.5.0")
  def propertiesOf(obj: Any): Array[String] =
    Object.properties(obj.asInstanceOf[Object])
}
