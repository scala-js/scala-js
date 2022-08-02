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

package java.lang

trait CharSequence {
  def length(): scala.Int
  def charAt(index: scala.Int): scala.Char
  def subSequence(start: scala.Int, end: scala.Int): CharSequence
  def toString(): String
}
