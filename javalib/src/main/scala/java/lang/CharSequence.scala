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

private[lang] object CharSequence {
  /** Wraps an `Array[Char]` as a `CharSequence` to reuse algorithms.
   *
   *  `subSequence` has an inefficient implementation. Avoid using this class
   *  for algorithms that use that method.
   */
  @inline
  private[lang] def ofArray(array: Array[Char]): OfArray = new OfArray(array)

  /** Wraps an `Array[Char]` as a `CharSequence` to reuse algorithms.
   *
   *  `subSequence` has an inefficient implementation. Avoid using this class
   *  for algorithms that use that method.
   */
  @inline
  private[lang] final class OfArray(array: Array[Char]) extends CharSequence {
    def length(): Int = array.length
    def charAt(index: Int): Char = array(index)

    // This is not efficient but we do not actually use it
    def subSequence(start: Int, end: Int): CharSequence =
      new OfArray(java.util.Arrays.copyOfRange(array, start, end))

    override def toString(): String =
      String.valueOf(array)
  }
}
