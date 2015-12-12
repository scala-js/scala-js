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
 *  JavaScript Map.
 *
 *  In a JavaScript Map, keys are *not* compared according to the `==`
 *  equality of Scala. They are compared according to reference equality, with
 *  two notable special-cases: `NaN` is equal to `NaN`, and `+0.0` is equal to
 *  `-0.0`.
 *
 *  In fact, `-0.0` is inserted in the map as `+0.0`.
 */
@js.native
class Map[K, V] extends js.Object with js.Iterable[js.Tuple2[K, V]] {
  def this(iterable: js.Iterable[js.Tuple2[K, V]]) = this()

  def size: Int = js.native

  def clear(): Unit = js.native

  def has(key: K): Boolean = js.native

  def get(key: K): js.UndefOr[V] = js.native

  def set(key: K, value: V): this.type = js.native

  def delete(key: K): Boolean = js.native

  def entries(): js.Iterator[js.Tuple2[K, V]] = js.native

  def keys(): js.Iterator[K] = js.native

  def values(): js.Iterator[V] = js.native
}
