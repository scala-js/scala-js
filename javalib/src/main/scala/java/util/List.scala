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

trait List[E] extends Collection[E] {
  def get(index: Int): E
  def set(index: Int, element: E): E
  def add(index: Int, element: E): Unit
  def remove(index: Int): E
  def indexOf(o: Any): Int
  def lastIndexOf(o: Any): Int
  def listIterator(): ListIterator[E]
  def listIterator(index: Int): ListIterator[E]
  def subList(fromIndex: Int, toIndex: Int): List[E]
  def addAll(index: Int, c: Collection[_ <: E]): Boolean
}
