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

trait SortedMap[K, V] extends Map[K, V] {
  def firstKey(): K
  def comparator(): Comparator[_ >: K]
  def lastKey(): K
  def subMap(fromKey: K, toKey: K): SortedMap[K, V]
  def headMap(toKey: K): SortedMap[K, V]
  def tailMap(fromKey: K): SortedMap[K, V]
}
