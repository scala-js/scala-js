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

trait Queue[E] extends Collection[E] {
  def add(e: E): Boolean
  def offer(e: E): Boolean
  def remove(): E
  def poll(): E
  def element(): E
  def peek(): E
}
