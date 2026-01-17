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

object ByteBuffer {
  private final val HashSeed = -547316498 // "java.nio.ByteBuffer".##

  def allocate(capacity: Int): ByteBuffer = {
    GenBuffer.validateAllocateCapacity(capacity)
    wrap(new Array[Byte](capacity))
  }

  def allocateDirect(capacity: Int): ByteBuffer = {
    GenBuffer.validateAllocateCapacity(capacity)
    TypedArrayByteBuffer.allocate(capacity)
  }

  def wrap(array: Array[Byte], offset: Int, length: Int): ByteBuffer =
    HeapByteBuffer.wrap(array, 0, array.length, offset, length, false)

  def wrap(array: Array[Byte]): ByteBuffer =
    wrap(array, 0, array.length)

  // Extended API

  def wrapInt8Array(array: Int8Array): ByteBuffer =
    TypedArrayByteBuffer.wrapInt8Array(array)
}

abstract class ByteBuffer private[nio] (
    _capacity: Int, private[nio] val _array: Array[Byte],
    private[nio] val _arrayOffset: Int)
    extends Buffer(_capacity) with Comparable[ByteBuffer] {

  private[nio] type ElementType = Byte
  private[nio] type BufferType = ByteBuffer
  private[nio] type TypedArrayType = Int8Array

  def this(_capacity: Int) = this(_capacity, null, -1)

  private[nio] var _isBigEndian: Boolean = true

  def slice(): ByteBuffer

  def duplicate(): ByteBuffer

  def asReadOnlyBuffer(): ByteBuffer

  def get(): Byte

  def put(b: Byte): ByteBuffer

  def get(index: Int): Byte

  def put(index: Int, b: Byte): ByteBuffer

  @noinline
  def get(dst: Array[Byte], offset: Int, length: Int): ByteBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  def get(dst: Array[Byte]): ByteBuffer =
    get(dst, 0, dst.length)

  @noinline
  def put(src: ByteBuffer): ByteBuffer =
    GenBuffer(this).generic_put(src)

  @noinline
  def put(src: Array[Byte], offset: Int, length: Int): ByteBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  final def put(src: Array[Byte]): ByteBuffer =
    put(src, 0, src.length)

  @inline final def hasArray(): Boolean =
    GenBuffer(this).generic_hasArray()

  @inline final def array(): Array[Byte] =
    GenBuffer(this).generic_array()

  @inline final def arrayOffset(): Int =
    GenBuffer(this).generic_arrayOffset()

  @inline override def position(newPosition: Int): ByteBuffer = {
    super.position(newPosition)
    this
  }

  @inline override def limit(newLimit: Int): ByteBuffer = {
    super.limit(newLimit)
    this
  }

  @inline override def mark(): ByteBuffer = {
    super.mark()
    this
  }

  @inline override def reset(): ByteBuffer = {
    super.reset()
    this
  }

  @inline override def clear(): ByteBuffer = {
    super.clear()
    this
  }

  @inline override def flip(): ByteBuffer = {
    super.flip()
    this
  }

  @inline override def rewind(): ByteBuffer = {
    super.rewind()
    this
  }

  def compact(): ByteBuffer

  def isDirect(): Boolean

  // toString(): String inherited from Buffer

  @noinline
  override def hashCode(): Int =
    GenBuffer(this).generic_hashCode(ByteBuffer.HashSeed)

  override def equals(that: Any): Boolean = that match {
    case that: ByteBuffer => compareTo(that) == 0
    case _                => false
  }

  @noinline
  def compareTo(that: ByteBuffer): Int =
    GenBuffer(this).generic_compareTo(that)(java.lang.Byte.compare(_, _))

  final def order(): ByteOrder =
    if (_isBigEndian) ByteOrder.BIG_ENDIAN
    else ByteOrder.LITTLE_ENDIAN

  final def order(bo: ByteOrder): ByteBuffer = {
    if (bo == null)
      throw new NullPointerException
    _isBigEndian = bo == ByteOrder.BIG_ENDIAN
    this
  }

  def getChar(): Char
  def putChar(value: Char): ByteBuffer
  def getChar(index: Int): Char
  def putChar(index: Int, value: Char): ByteBuffer

  def asCharBuffer(): CharBuffer

  def getShort(): Short
  def putShort(value: Short): ByteBuffer
  def getShort(index: Int): Short
  def putShort(index: Int, value: Short): ByteBuffer

  def asShortBuffer(): ShortBuffer

  def getInt(): Int
  def putInt(value: Int): ByteBuffer
  def getInt(index: Int): Int
  def putInt(index: Int, value: Int): ByteBuffer

  def asIntBuffer(): IntBuffer

  def getLong(): Long
  def putLong(value: Long): ByteBuffer
  def getLong(index: Int): Long
  def putLong(index: Int, value: Long): ByteBuffer

  def asLongBuffer(): LongBuffer

  def getFloat(): Float
  def putFloat(value: Float): ByteBuffer
  def getFloat(index: Int): Float
  def putFloat(index: Int, value: Float): ByteBuffer

  def asFloatBuffer(): FloatBuffer

  def getDouble(): Double
  def putDouble(value: Double): ByteBuffer
  def getDouble(index: Int): Double
  def putDouble(index: Int, value: Double): ByteBuffer

  def asDoubleBuffer(): DoubleBuffer

  // Internal API

  override private[nio] def isBigEndian: Boolean =
    _isBigEndian

  private[nio] def load(index: Int): Byte

  private[nio] def store(index: Int, elem: Byte): Unit

  @inline
  private[nio] def load(startIndex: Int,
      dst: Array[Byte], offset: Int, length: Int): Unit = {
    GenBuffer(this).generic_load(startIndex, dst, offset, length)
  }

  @inline
  private[nio] def store(startIndex: Int,
      src: Array[Byte], offset: Int, length: Int): Unit = {
    GenBuffer(this).generic_store(startIndex, src, offset, length)
  }
}
