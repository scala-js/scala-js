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

import scala.language.implicitConversions

import java.util.function._

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Utilities to perform bounds checks. */
private[java] object BoundsChecks {

  /* All the tests optimize for the happy path, where all values are valid.
   *
   * Throwing the exceptions is always extracted in a separate @noinline
   * helper. We do this so that we can construct good error message without
   * fear of duplicating that code everywhere.
   *
   * We explicitly write `BoundsChecks.*` so that the outer `LoadModule` for
   * `BoundsChecks` can be removed, and the module is only loaded inside the
   * failure path.
   */

  /** Checks that `capacity >= 0`.
   *
   *  If the conditions do not hold, throw an `IllegalArgumentException`.
   */
  @inline
  def checkCapacity(capacity: Int): Unit = {
    if (capacity < 0)
      BoundsChecks.throwIllegalCapacityException(capacity)
  }

  @noinline
  private def throwIllegalCapacityException(capacity: Int): Nothing =
    throw new IllegalArgumentException(s"Illegal capacity: $capacity")

  /** Checks that `0 <= index < length` hold.
   *
   *  If the conditions do not hold, throw an `IndexOutOfBoundsException`.
   *
   *  Assumes that `length >= 0`.
   */
  @inline
  def checkIndex(index: Int, length: Int): Unit = {
    if (isIndexInvalid(index, length))
      BoundsChecks.throwIOOBE(index, length)
  }

  @noinline
  private def throwIOOBE(index: Int, length: Int): Nothing =
    throw new IndexOutOfBoundsException(s"Index $index out of bounds [0, $length)")

  /** Returns true if the inequalities `0 <= index < length` do *not* hold.
   *
   *  Assumes `length >= 0`.
   */
  @inline
  def isIndexInvalid(index: Int, length: Int): scala.Boolean =
    Integer.unsigned_>=(index, length)

  /** Checks that `0 <= index <= length` hold.
   *
   *  If the conditions do not hold, throw an `IndexOutOfBoundsException`.
   *
   *  Assumes that `length >= 0`.
   */
  @inline
  def checkIndexInclusive(index: Int, length: Int): Unit = {
    if (isIndexInclusiveInvalid(index, length))
      BoundsChecks.throwInclusiveIOOBE(index, length)
  }

  @noinline
  private def throwInclusiveIOOBE(index: Int, length: Int): Nothing =
    throw new IndexOutOfBoundsException(s"Index $index out of bounds [0, $length]")

  /** Returns true if the inequalities `0 <= index <= length` do *not* hold.
   *
   *  Assumes `length >= 0`.
   */
  @inline
  def isIndexInclusiveInvalid(index: Int, length: Int): scala.Boolean =
    Integer.unsigned_>(index, length)

  /** Checks that `0 <= start <= end <= length` hold.
   *
   *  If the conditions do not hold, throw an `IndexOutOfBoundsException`.
   *
   *  Assumes that `length >= 0`.
   *
   *  @return `end - start`, the number of elements in the range.
   */
  @inline
  def checkStartEnd(start: Int, end: Int, length: Int): Int = {
    val count = end - start
    if (isStartCountEndInvalid(start, count, end, length))
      BoundsChecks.throwStartEndOOBE(start, end, length)
    count
  }

  @noinline
  private def throwStartEndOOBE(start: Int, end: Int, length: Int): Nothing =
    throw new IndexOutOfBoundsException(s"Range [$start, $end) out of bounds [0, $length)")

  /** Checks that offset >= 0, count >= 0 and offset + count <= length.
   *
   *  Where `offset + count` is understood as the mathematical (non-wrapping)
   *  addition.
   *
   *  If the conditions do not hold, throw an `IndexOutOfBoundsException`.
   *
   *  Assumes that `length >= 0`.
   *
   *  @return `offset + count`, the end index of the range.
   */
  @inline
  def checkOffsetCount(offset: Int, count: Int, length: Int): Int = {
    val endOffset = offset + count
    if (isStartCountEndInvalid(offset, count, endOffset, length))
      BoundsChecks.throwOffsetCountOOBE(offset, count, length)
    endOffset
  }

  @noinline
  private def throwOffsetCountOOBE(offset: Int, count: Int, length: Int): Nothing = {
    throw new IndexOutOfBoundsException(
        s"Range [$offset, $offset + $count) out of bounds [0, $length)")
  }

  /** Returns true if any of the inequalities `0 <= start <= end <= length` or
   *  `0 <= count <= length - start` does *not* hold.
   *
   *  Assumes `length >= 0`, as well as `end == start + count` (equivalently,
   *  `count = end - start`).
   */
  @inline
  def isStartCountEndInvalid(start: Int, count: Int, end: Int, length: Int): scala.Boolean =
    (start | count | end | (length - end)) < 0
}
