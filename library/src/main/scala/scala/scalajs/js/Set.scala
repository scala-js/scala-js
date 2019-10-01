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
import scala.scalajs.js.annotation.{ JSGlobal, JSName }

/** The Set object lets you store unique values of any type, whether primitive
 *  values or object references.
 *
 *  @tparam T A type of element.
 *  @note The class [[js.Set]] is added in ES6 (ES2015).
 *  @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set
 */
@js.native
@JSGlobal
class Set[T]() extends js.Object with js.Iterable[js.Tuple2[T, T]] {
  def this(array: js.Iterable[T]) = this()

  def clear(): Unit = js.native

  def delete(key: T): Boolean = js.native

  def entries(): js.Iterator[js.Tuple2[T,T]] = js.native

  @JSName("forEach")
  def jsForEach[T](callbackfn: js.ThisFunction3[T, T, T, Set[T], _],
                   thisArg: T): Unit = js.native
  @JSName("forEach")
  def jsForEach(callbackfn: js.Function3[T, T, Set[T], _]): Unit = js.native

  def keys(): js.Iterator[T] = js.native

  @JSName(js.Symbol.iterator)
  override def jsIterator(): Iterator[Tuple2[T, T]] = js.native

  def size: Int = js.native

  def has(value: T): Boolean = js.native

  def add(value: T): this.type = js.native

  def values(): js.Iterator[T] = js.native
}

/** Factory for [[js.Set]] instances. */
object Set {
  /** Returns a new empty map */
  @inline def empty[V]: js.Set[V] = new Set[V]()

  @inline
  def apply[V](values: V*): js.Set[V] = {
    val set = empty[V]
    for (value <- values)
      set.add(value)
    set
  }
}
