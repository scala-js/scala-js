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

abstract class Dictionary[K, V] {
  def size(): Int
  def isEmpty(): Boolean
  def keys(): Enumeration[K]
  def elements(): Enumeration[V]
  def get(key: Any): V
  def put(key: K, value: V): V
  def remove(key: Any): V
}
