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

/** A subclass of `HashMap` that systematically rejects `null` keys and values.
 *
 *  This class is used as the implementation of some other hashtable-like data
 *  structures that require non-`null` keys and values to correctly implement
 *  their specifications.
 */
private[util] class NullRejectingHashMap[K, V](
    initialCapacity: Int, loadFactor: Float)
    extends HashMap[K, V](initialCapacity, loadFactor) {

  def this() =
    this(HashMap.DEFAULT_INITIAL_CAPACITY, HashMap.DEFAULT_LOAD_FACTOR)

  def this(initialCapacity: Int) =
    this(initialCapacity, HashMap.DEFAULT_LOAD_FACTOR)

  def this(m: Map[_ <: K, _ <: V]) = {
    this(m.size())
    putAll(m)
  }

  // Use Nodes that will reject `null`s in `setValue()`
  override private[util] def newNode(key: K, hash: Int, value: V,
      previous: HashMap.Node[K, V], next: HashMap.Node[K, V]): HashMap.Node[K, V] = {
    new NullRejectingHashMap.Node(key, hash, value, previous, next)
  }

  override def get(key: Any): V = {
    if (key == null)
      throw new NullPointerException()
    super.get(key)
  }

  override def containsKey(key: Any): Boolean = {
    if (key == null)
      throw new NullPointerException()
    super.containsKey(key)
  }

  override def put(key: K, value: V): V = {
    if (key == null || value == null)
      throw new NullPointerException()
    super.put(key, value)
  }

  override def putIfAbsent(key: K, value: V): V = {
    if (value == null)
      throw new NullPointerException()
    val old = get(key) // throws if `key` is null
    if (old == null)
      super.put(key, value)
    old
  }

  @noinline
  override def putAll(m: Map[_ <: K, _ <: V]): Unit = {
    /* The only purpose of `impl` is to capture the wildcards as named types,
     * so that we prevent type inference from inferring deprecated existential
     * types.
     */
    @inline
    def impl[K1 <: K, V1 <: V](m: Map[K1, V1]): Unit = {
      val iter = m.entrySet().iterator()
      while (iter.hasNext()) {
        val entry = iter.next()
        put(entry.getKey(), entry.getValue())
      }
    }
    impl(m)
  }

  override def remove(key: Any): V = {
    if (key == null)
      throw new NullPointerException()
    super.remove(key)
  }

  override def remove(key: Any, value: Any): Boolean = {
    val old = get(key) // throws if `key` is null
    if (old != null && old.equals(value)) { // false if `value` is null
      super.remove(key)
      true
    } else {
      false
    }
  }

  override def replace(key: K, oldValue: V, newValue: V): Boolean = {
    if (oldValue == null || newValue == null)
      throw new NullPointerException()
    val old = get(key) // throws if `key` is null
    if (oldValue.equals(old)) { // false if `old` is null
      super.put(key, newValue)
      true
    } else {
      false
    }
  }

  override def replace(key: K, value: V): V = {
    if (value == null)
      throw new NullPointerException()
    val old = get(key) // throws if `key` is null
    if (old != null)
      super.put(key, value)
    old
  }

  override def containsValue(value: Any): Boolean = {
    if (value == null)
      throw new NullPointerException()
    super.containsValue(value)
  }

  override def clone(): AnyRef =
    new NullRejectingHashMap[K, V](this)
}

private object NullRejectingHashMap {
  private final class Node[K, V](key: K, hash: Int, value: V,
      previous: HashMap.Node[K, V], next: HashMap.Node[K, V])
      extends HashMap.Node[K, V](key, hash, value, previous, next) {

    override def setValue(v: V): V = {
      if (v == null)
        throw new NullPointerException()
      super.setValue(v)
    }
  }
}
