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

import java.util.function.{BiConsumer, BiFunction, Function}

import ScalaOps._

trait Map[K, V] {
  def size(): Int
  def isEmpty(): Boolean
  def containsKey(key: Any): Boolean
  def containsValue(value: Any): Boolean
  def get(key: Any): V
  def put(key: K, value: V): V
  def remove(key: Any): V
  def putAll(m: Map[_ <: K, _ <: V]): Unit
  def clear(): Unit
  def keySet(): Set[K]
  def values(): Collection[V]
  def entrySet(): Set[Map.Entry[K, V]]
  def equals(o: Any): Boolean
  def hashCode(): Int

  def getOrDefault(key: Any, defaultValue: V): V =
    if (containsKey(key)) get(key)
    else defaultValue

  def forEach(action: BiConsumer[_ >: K, _ >: V]): Unit = {
    for (entry <- entrySet().scalaOps)
      action.accept(entry.getKey(), entry.getValue())
  }

  def replaceAll(function: BiFunction[_ >: K, _ >: V, _ <: V]): Unit = {
    for (entry <- entrySet().scalaOps)
      entry.setValue(function.apply(entry.getKey(), entry.getValue()))
  }

  def putIfAbsent(key: K, value: V): V = {
    val prevValue = get(key)
    if (prevValue == null)
      put(key, value) // will return null
    else
      prevValue
  }

  def remove(key: Any, value: Any): Boolean = {
    if (containsKey(key) && Objects.equals(get(key), value)) {
      remove(key)
      true
    } else {
      false
    }
  }

  def replace(key: K, oldValue: V, newValue: V): Boolean = {
    if (containsKey(key) && Objects.equals(get(key), oldValue)) {
      put(key, newValue)
      true
    } else {
      false
    }
  }

  def replace(key: K, value: V): V =
    if (containsKey(key)) put(key, value)
    else null.asInstanceOf[V]

  def computeIfAbsent(key: K, mappingFunction: Function[_ >: K, _ <: V]): V = {
    val oldValue = get(key)
    if (oldValue != null) {
      oldValue
    } else {
      val newValue = mappingFunction.apply(key)
      if (newValue != null)
        put(key, newValue)
      newValue
    }
  }

  def computeIfPresent(key: K, remappingFunction: BiFunction[_ >: K, _ >: V, _ <: V]): V = {
    val oldValue = get(key)
    if (oldValue == null) {
      oldValue
    } else {
      val newValue = remappingFunction.apply(key, oldValue)
      putOrRemove(key, newValue)
      newValue
    }
  }

  def compute(key: K, remappingFunction: BiFunction[_ >: K, _ >: V, _ <: V]): V = {
    val oldValue = get(key)
    val newValue = remappingFunction.apply(key, oldValue)

    /* The "Implementation Requirements" section of the JavaDoc for this method
     * does not correspond to the textual specification in the case where both
     * a) there was a null mapping, and
     * b) the remapping function returned null.
     *
     * The Implementation Requirements would leave the null mapping, whereas
     * the specification says to remove it.
     *
     * We implement the specification, as it appears that the actual Map
     * implementations on the JVM behave like the spec.
     */
    putOrRemove(key, newValue)

    newValue
  }

  def merge(key: K, value: V, remappingFunction: BiFunction[_ >: V, _ >: V, _ <: V]): V = {
    Objects.requireNonNull(value)

    val oldValue = get(key)
    val newValue =
      if (oldValue == null) value
      else remappingFunction.apply(oldValue, value)
    putOrRemove(key, newValue)
    newValue
  }

  private def putOrRemove(key: K, value: V): Unit = {
    if (value != null)
      put(key, value)
    else
      remove(key)
  }
}

object Map {

  trait Entry[K, V] {
    def getKey(): K
    def getValue(): V
    def setValue(value: V): V
    def equals(o: Any): Boolean
    def hashCode(): Int
  }

}
