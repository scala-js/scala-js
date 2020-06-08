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

package scala.scalajs.js

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobal, JSName}

/** <span class="badge badge-ecma2015" style="float: right;">ECMAScript 2015</span>
 *
 *  The Set object lets you store unique values of any type, whether primitive
 *  values or object references.
 *
 *  @tparam T A type of element.
 *  @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set
 */
@js.native
@JSGlobal
class Set[T]() extends js.Object with js.Iterable[T] {
  def this(array: js.Iterable[T]) = this()

  def clear(): Unit = js.native

  @JSName(js.Symbol.iterator)
  override def jsIterator(): js.Iterator[T] = js.native

  def size: Int = js.native
}

/** Factory for [[js.Set]] instances. */
object Set {
  /** Returns a new empty map */
  @inline def empty[V]: js.Set[V] = new Set[V]()

  @js.native
  private[js] trait Raw[T] extends js.Object {
    def add(value: T): this.type = js.native
    def has(value: T): Boolean = js.native
    def delete(value: T): Boolean = js.native
  }

  @inline
  def apply[V](values: V*): js.Set[V] = new js.Set(js.Array(values: _*))
}
