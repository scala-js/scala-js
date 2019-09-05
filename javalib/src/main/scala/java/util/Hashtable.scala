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

import java.{util => ju}

/* This implementation allows `null` keys and values, although the JavaDoc
 * specifies that operations should throw `NullPointerException`s if `null`
 * keys or values are used. This makes the implementation easier, notably by
 * allowing to reuse the implementation of `j.u.HashMap`, and is acceptable
 * given that NPEs are undefined behavior in Scala.js.
 */
class Hashtable[K, V] private (inner: ju.HashMap[K, V])
    extends ju.Dictionary[K,V] with ju.Map[K, V] with Cloneable with Serializable {

  def this() =
    this(new ju.HashMap[K, V]())

  def this(initialCapacity: Int) =
    this(new ju.HashMap[K, V](initialCapacity))

  def this(initialCapacity: Int, loadFactor: Float) =
    this(new ju.HashMap[K, V](initialCapacity, loadFactor))

  def this(t: ju.Map[_ <: K, _ <: V]) =
    this(new ju.HashMap[K, V](t))

  def size(): Int =
    inner.size()

  def isEmpty(): Boolean =
    inner.isEmpty()

  def keys(): ju.Enumeration[K] =
    Collections.enumeration(keySet())

  def elements(): ju.Enumeration[V] =
    Collections.enumeration(values())

  def contains(value: Any): Boolean =
    containsValue(value)

  def containsValue(value: Any): Boolean =
    inner.containsValue(value)

  def containsKey(key: Any): Boolean =
    inner.containsKey(key)

  def get(key: Any): V =
    inner.get(key)

  // Not implemented
  // protected def rehash(): Unit

  def put(key: K, value: V): V =
    inner.put(key, value)

  def remove(key: Any): V =
    inner.remove(key)

  def putAll(m: ju.Map[_ <: K, _ <: V]): Unit =
    inner.putAll(m)

  def clear(): Unit =
    inner.clear()

  override def clone(): AnyRef =
    new ju.Hashtable[K, V](this)

  override def toString(): String =
    inner.toString()

  def keySet(): ju.Set[K] =
    inner.keySet()

  def entrySet(): ju.Set[ju.Map.Entry[K, V]] =
    inner.entrySet()

  def values(): ju.Collection[V] =
    inner.values()
}
