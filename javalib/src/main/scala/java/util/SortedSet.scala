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

trait SortedSet[E] extends Set[E] {
  def comparator(): Comparator[_ >: E]
  def subSet(fromElement: E, toElement: E): SortedSet[E]
  def headSet(toElement: E): SortedSet[E]
  def tailSet(fromElement: E): SortedSet[E]
  def first(): E
  def last(): E
}
