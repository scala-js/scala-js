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
 *  JavaScript WeakMap.
 *
 *  A WeakMap cannot contain primitive values as keys.
 */
@js.native
class WeakMap[K <: AnyRef, V] extends js.Object {
  def this(iterable: js.Iterable[js.Tuple2[K, V]]) = this()

  def has(key: K): Boolean = js.native

  def get(key: K): js.UndefOr[V] = js.native

  def set(key: K, value: V): this.type = js.native

  def delete(key: K): Boolean = js.native
}
