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

import scala.scalajs.js.typedarray._

object IntBuffer {
  private final val HashSeed = 39599817 // "java.nio.IntBuffer".##

  def allocate(capacity: Int): IntBuffer = {
    GenBuffer.validateAllocateCapacity(capacity)
    wrap(new Array[Int](capacity))
  }

  def wrap(array: Array[Int], offset: Int, length: Int): IntBuffer =
    HeapIntBuffer.wrap(array, 0, array.length, offset, length, false)

  def wrap(array: Array[Int]): IntBuffer =
    wrap(array, 0, array.length)

  // Extended API

  def wrapInt32Array(array: Int32Array): IntBuffer =
    TypedArrayIntBuffer.wrapInt32Array(array)
}

abstract class IntBuffer private[nio] (
    _capacity: Int, private[nio] val _array: Array[Int],
    private[nio] val _arrayOffset: Int)
    extends Buffer(_capacity) with Comparable[IntBuffer] {

  private[nio] type ElementType = Int
  private[nio] type BufferType = IntBuffer
  private[nio] type TypedArrayType = Int32Array

  def this(_capacity: Int) = this(_capacity, null, -1)

  def slice(): IntBuffer

  def duplicate(): IntBuffer

  def asReadOnlyBuffer(): IntBuffer

  def get(): Int

  def put(i: Int): IntBuffer

  def get(index: Int): Int

  def put(index: Int, i: Int): IntBuffer

  @noinline
  def get(dst: Array[Int], offset: Int, length: Int): IntBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  def get(dst: Array[Int]): IntBuffer =
    get(dst, 0, dst.length)

  @noinline
  def put(src: IntBuffer): IntBuffer =
    GenBuffer(this).generic_put(src)

  @noinline
  def put(src: Array[Int], offset: Int, length: Int): IntBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  final def put(src: Array[Int]): IntBuffer =
    put(src, 0, src.length)

  @inline final def hasArray(): Boolean =
    GenBuffer(this).generic_hasArray()

  @inline final def array(): Array[Int] =
    GenBuffer(this).generic_array()

  @inline final def arrayOffset(): Int =
    GenBuffer(this).generic_arrayOffset()

  @inline override def position(newPosition: Int): IntBuffer = {
    super.position(newPosition)
    this
  }

  @inline override def limit(newLimit: Int): IntBuffer = {
    super.limit(newLimit)
    this
  }

  @inline override def mark(): IntBuffer = {
    super.mark()
    this
  }

  @inline override def reset(): IntBuffer = {
    super.reset()
    this
  }

  @inline override def clear(): IntBuffer = {
    super.clear()
    this
  }

  @inline override def flip(): IntBuffer = {
    super.flip()
    this
  }

  @inline override def rewind(): IntBuffer = {
    super.rewind()
    this
  }

  def compact(): IntBuffer

  def isDirect(): Boolean

  // toString(): String inherited from Buffer

  @noinline
  override def hashCode(): Int =
    GenBuffer(this).generic_hashCode(IntBuffer.HashSeed)

  override def equals(that: Any): Boolean = that match {
    case that: IntBuffer => compareTo(that) == 0
    case _               => false
  }

  @noinline
  def compareTo(that: IntBuffer): Int =
    GenBuffer(this).generic_compareTo(that)(Integer.compare(_, _))

  def order(): ByteOrder

  // Internal API

  private[nio] def load(index: Int): Int

  private[nio] def store(index: Int, elem: Int): Unit

  @inline
  private[nio] def load(startIndex: Int,
      dst: Array[Int], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  private[nio] def store(startIndex: Int,
      src: Array[Int], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}
