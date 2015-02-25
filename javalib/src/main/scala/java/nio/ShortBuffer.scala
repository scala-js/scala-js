package java.nio

import scala.scalajs.js.typedarray._

object ShortBuffer {
  private final val HashSeed = 383731478 // "java.nio.ShortBuffer".##

  def allocate(capacity: Int): ShortBuffer =
    wrap(new Array[Short](capacity))

  def wrap(array: Array[Short], offset: Int, length: Int): ShortBuffer =
    HeapShortBuffer.wrap(array, 0, array.length, offset, length, false)

  def wrap(array: Array[Short]): ShortBuffer =
    wrap(array, 0, array.length)

  // Extended API

  def wrap(array: Int16Array): ShortBuffer =
    TypedArrayShortBuffer.wrap(array)
}

abstract class ShortBuffer private[nio] (
    _capacity: Int, private[nio] val _array: Array[Short],
    private[nio] val _arrayOffset: Int)
    extends Buffer(_capacity) with Comparable[ShortBuffer] {

  private[nio] type ElementType = Short
  private[nio] type BufferType = ShortBuffer
  private[nio] type TypedArrayType = Int16Array

  def this(_capacity: Int) = this(_capacity, null, -1)

  def slice(): ShortBuffer

  def duplicate(): ShortBuffer

  def asReadOnlyBuffer(): ShortBuffer

  def get(): Short

  def put(s: Short): ShortBuffer

  def get(index: Int): Short

  def put(index: Int, s: Short): ShortBuffer

  @noinline
  def get(dst: Array[Short], offset: Int, length: Int): ShortBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  def get(dst: Array[Short]): ShortBuffer =
    get(dst, 0, dst.length)

  @noinline
  def put(src: ShortBuffer): ShortBuffer =
    GenBuffer(this).generic_put(src)

  @noinline
  def put(src: Array[Short], offset: Int, length: Int): ShortBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  final def put(src: Array[Short]): ShortBuffer =
    put(src, 0, src.length)

  @inline final def hasArray(): Boolean =
    GenBuffer(this).generic_hasArray()

  @inline final def array(): Array[Short] =
    GenBuffer(this).generic_array()

  @inline final def arrayOffset(): Int =
    GenBuffer(this).generic_arrayOffset()

  def compact(): ShortBuffer

  def isDirect(): Boolean

  // toString(): String inherited from Buffer

  @noinline
  override def hashCode(): Int =
    GenBuffer(this).generic_hashCode(ShortBuffer.HashSeed)

  override def equals(that: Any): Boolean = that match {
    case that: ShortBuffer => compareTo(that) == 0
    case _                 => false
  }

  @noinline
  def compareTo(that: ShortBuffer): Int =
    GenBuffer(this).generic_compareTo(that)(_.compareTo(_))

  def order(): ByteOrder

  // Internal API

  private[nio] def load(index: Int): Short

  private[nio] def store(index: Int, elem: Short): Unit

  @inline
  private[nio] def load(startIndex: Int,
      dst: Array[Short], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  private[nio] def store(startIndex: Int,
      src: Array[Short], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}
