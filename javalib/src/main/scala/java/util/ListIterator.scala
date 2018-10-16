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

trait ListIterator[E] extends Iterator[E] {
  def add(e: E): Unit
  def hasPrevious(): Boolean
  def previous(): E
  def previousIndex(): Int
  def nextIndex(): Int
  def set(e: E): Unit
}
