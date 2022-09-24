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

package java.nio

object LongBuffer {
  private final val HashSeed = -1709696158 // "java.nio.LongBuffer".##

  def allocate(capacity: Int): LongBuffer = {
    GenBuffer.validateAllocateCapacity(capacity)
    wrap(new Array[Long](capacity))
  }

  def wrap(array: Array[Long], offset: Int, length: Int): LongBuffer =
    HeapLongBuffer.wrap(array, 0, array.length, offset, length, false)

  def wrap(array: Array[Long]): LongBuffer =
    wrap(array, 0, array.length)
}

abstract class LongBuffer private[nio] (
    _capacity: Int, private[nio] val _array: Array[Long],
    private[nio] val _arrayOffset: Int)
    extends Buffer(_capacity) with Comparable[LongBuffer] {

  private[nio] type ElementType = Long
  private[nio] type BufferType = LongBuffer
  private[nio] type TypedArrayType = Null

  def this(_capacity: Int) = this(_capacity, null, -1)

  def slice(): LongBuffer

  def duplicate(): LongBuffer

  def asReadOnlyBuffer(): LongBuffer

  def get(): Long

  def put(l: Long): LongBuffer

  def get(index: Int): Long

  def put(index: Int, l: Long): LongBuffer

  @noinline
  def get(dst: Array[Long], offset: Int, length: Int): LongBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  def get(dst: Array[Long]): LongBuffer =
    get(dst, 0, dst.length)

  @noinline
  def put(src: LongBuffer): LongBuffer =
    GenBuffer(this).generic_put(src)

  @noinline
  def put(src: Array[Long], offset: Int, length: Int): LongBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  final def put(src: Array[Long]): LongBuffer =
    put(src, 0, src.length)

  @inline final def hasArray(): Boolean =
    GenBuffer(this).generic_hasArray()

  @inline final def array(): Array[Long] =
    GenBuffer(this).generic_array()

  @inline final def arrayOffset(): Int =
    GenBuffer(this).generic_arrayOffset()

  @inline override def position(newPosition: Int): LongBuffer = {
    super.position(newPosition)
    this
  }

  @inline override def limit(newLimit: Int): LongBuffer = {
    super.limit(newLimit)
    this
  }

  @inline override def mark(): LongBuffer = {
    super.mark()
    this
  }

  @inline override def reset(): LongBuffer = {
    super.reset()
    this
  }

  @inline override def clear(): LongBuffer = {
    super.clear()
    this
  }

  @inline override def flip(): LongBuffer = {
    super.flip()
    this
  }

  @inline override def rewind(): LongBuffer = {
    super.rewind()
    this
  }

  def compact(): LongBuffer

  def isDirect(): Boolean

  // toString(): String inherited from Buffer

  @noinline
  override def hashCode(): Int =
    GenBuffer(this).generic_hashCode(LongBuffer.HashSeed)

  override def equals(that: Any): Boolean = that match {
    case that: LongBuffer => compareTo(that) == 0
    case _                => false
  }

  @noinline
  def compareTo(that: LongBuffer): Int =
    GenBuffer(this).generic_compareTo(that)(java.lang.Long.compare(_, _))

  def order(): ByteOrder

  // Internal API

  private[nio] def load(index: Int): Long

  private[nio] def store(index: Int, elem: Long): Unit

  @inline
  private[nio] def load(startIndex: Int,
      dst: Array[Long], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  private[nio] def store(startIndex: Int,
      src: Array[Long], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}
