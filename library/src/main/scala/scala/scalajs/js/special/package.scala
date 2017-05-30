/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js

/** Contains special primitives of interoperability with JavaScript which are
 *  of limited importance or rare usefulness.
 *
 *  In theory, all of the members of this package could equally well be part of
 *  the `scala.scalajs.js` package. They are sligthly "hidden" in this
 *  `special` package so that they are not used on a daily basis, but only when
 *  absolutely necessary.
 *
 *  Everything in this package is a "I-know-what-I-am-doing" API. Notably, no
 *  attempt is made to guide the user with types that are not hard
 *  requirements.
 */
package object special {

  /** Deletes a property of an object.
   *
   *  This method is the exact equivalent of the `delete obj[key]` statement
   *  of JavaScript (and by extension of `delete obj.key` if `key` is a
   *  constant string).
   *
   *  The property must be configurable. Otherwise, this method throws a
   *  [[js.TypeError]].
   *
   *  Rather than using this method, it is often preferable to use a
   *  [[js.Dictionary]] and its `-=` method.
   */
  def delete(obj: scala.Any, key: scala.Any): Unit =
    throw new java.lang.Error("stub")

  /** Exact equivalent of the `debugger` keyword of JavaScript.
   *
   *  `debugger()` invokes any available debugging functionality.
   *  If no debugging functionality is available, this method has no effect.
   *
   *  MDN
   *
   *  Browser support:
   *  - Has no effect in Rhino nor, apparently, in Firefox
   *  - In Chrome, it has no effect unless the developer tools are opened
   *    beforehand.
   */
  def debugger(): Unit =
    throw new java.lang.Error("stub")

}
