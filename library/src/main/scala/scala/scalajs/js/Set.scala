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
 *  JavaScript Set.
 *
 *  In a JavaScript Set, values are *not* compared according to the `==`
 *  equality of Scala. They are compared according to reference equality, with
 *  two notable special-cases: `NaN` is equal to `NaN`, and `+0.0` is equal to
 *  `-0.0`.
 *
 *  In fact, `-0.0` is inserted in the set as `+0.0`.
 */
@js.native
class Set[A] extends js.Object with js.Iterable[A] {
  def this(iterable: js.Iterable[A]) = this()

  def size: Int = js.native

  def clear(): Unit = js.native

  def has(value: A): Boolean = js.native

  def add(value: A): this.type = js.native

  def delete(value: A): Boolean = js.native

  def entries(): js.Iterator[js.Tuple2[A, A]] = js.native

  def keys(): js.Iterator[A] = js.native

  def values(): js.Iterator[A] = js.native
}
