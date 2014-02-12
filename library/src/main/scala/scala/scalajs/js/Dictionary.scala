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

/** Dictionary "view" of a JavaScript value. */
sealed trait Dictionary[A] extends Object {
  /** Reads a field of this object by its name. */
  @JSBracketAccess
  def apply(key: String): A

  /** Writes a field of this object by its name. */
  @JSBracketAccess
  def update(key: String, value: A): Unit

  /** Deletes a property of this object by its name.
   *  The property must be configurable.
   *  This method is equivalent to the "delete" keyword in JavaScript.
   *  @return true on success (the property did not exist or was configurable),
   *          false otherwise
   */
  def delete(key: String): Boolean = sys.error("stub")
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
  def propertiesOf(obj: Any): Array[String] = sys.error("stub")
}
