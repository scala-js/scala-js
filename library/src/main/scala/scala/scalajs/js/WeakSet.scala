/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js

import scala.scalajs.js

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  JavaScript WeakSet.
 *
 *  A WeakSet cannot contain primitive values.
 */
@js.native
class WeakSet[A] extends js.Object {
  def this(iterable: js.Iterable[A]) = this()

  def has(value: A): Boolean = js.native

  def add(value: A): this.type = js.native

  def delete(value: A): Boolean = js.native
}
