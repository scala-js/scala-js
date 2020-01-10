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

/** <span class="badge badge-ecma2015" style="float: right;">ECMAScript 2015</span>
 *
 *  The Map object holds key-value pairs and remembers the original insertion
 *  order of the keys. Any value (both objects and primitive values) may be used
 *  as either a key or a value.
 *
 *  @tparam K A type of key.
 *  @tparam V A type of value.
 *  @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
 */
@js.native
@JSGlobal
class Map[K, V]() extends js.Object with js.Iterable[js.Tuple2[K, V]] {
  def this(array: js.Iterable[js.Tuple2[K, V]]) = this()

  def clear(): Unit = js.native

  def delete(key: K): Boolean = js.native

  @JSName(js.Symbol.iterator)
  override def jsIterator(): Iterator[Tuple2[K, V]] = js.native

  def size: Int = js.native
}


/** Factory for [[js.Map]] instances. */
object Map {
  /** Returns a new empty map */
  @inline def empty[K, V]: js.Map[K, V] = new Map[K, V]()

  @js.native
  private[js] trait Raw[K, V] extends js.Object {
    def has(key: K): Boolean = js.native
    def keys(): js.Iterator[K] = js.native
    def set(key: K, value: V): js.Map[K, V] = js.native
    def get(key: K): V = js.native
  }

  @inline
  def apply[K, V](properties: (K, V)*): js.Map[K, V] = {
    val map = empty[K, V]
    val rawMap = map.asInstanceOf[js.Map.Raw[K, V]]
    for (pair <- properties)
      rawMap.set(pair._1, pair._2)
    map
  }
}
