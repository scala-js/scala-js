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

package java.util

class HashSet[E] private[util] (inner: HashMap[E, Any])
    extends AbstractSet[E] with Set[E] with Cloneable with Serializable {

  /* Note: in practice, the values of `inner` are always `()` (aka `undefined`).
   * We use `Any` because we need to deal with `null`s, and referencing
   * `scala.runtime.BoxedUnit` in this code would be really ugly.
   */

  def this(initialCapacity: Int, loadFactor: Float) =
    this(new HashMap[E, Any](initialCapacity, loadFactor))

  def this(initialCapacity: Int) =
    this(new HashMap[E, Any](initialCapacity))

  def this() =
    this(new HashMap[E, Any]())

  def this(c: Collection[_ <: E]) = {
    this(c.size())
    addAll(c)
  }

  private val innerKeySet = inner.keySet()

  override def contains(o: Any): Boolean =
    inner.containsKey(o)

  override def remove(o: Any): Boolean =
    inner.remove(o) != null

  override def containsAll(c: Collection[_]): Boolean =
    innerKeySet.containsAll(c)

  override def removeAll(c: Collection[_]): Boolean =
    innerKeySet.removeAll(c)

  override def retainAll(c: Collection[_]): Boolean =
    innerKeySet.retainAll(c)

  override def add(e: E): Boolean =
    inner.put(e, ()) == null

  override def addAll(c: Collection[_ <: E]): Boolean = {
    val iter = c.iterator()
    var changed = false
    while (iter.hasNext)
      changed = add(iter.next()) || changed
    changed
  }

  override def clear(): Unit = inner.clear()

  override def size(): Int = inner.size()

  def iterator(): Iterator[E] =
    innerKeySet.iterator()

}
