package java.nio

import scala.scalajs.js.typedarray._

object FloatBuffer {
  private final val HashSeed = 1920204022 // "java.nio.FloatBuffer".##

  def allocate(capacity: Int): FloatBuffer =
    wrap(new Array[Float](capacity))

  def wrap(array: Array[Float], offset: Int, length: Int): FloatBuffer =
    HeapFloatBuffer.wrap(array, 0, array.length, offset, length, false)

  def wrap(array: Array[Float]): FloatBuffer =
    wrap(array, 0, array.length)

  // Extended API

  def wrap(array: Float32Array): FloatBuffer =
    TypedArrayFloatBuffer.wrap(array)
}

abstract class FloatBuffer private[nio] (
    _capacity: Int, private[nio] val _array: Array[Float],
    private[nio] val _arrayOffset: Int)
    extends Buffer(_capacity) with Comparable[FloatBuffer] {

  private[nio] type ElementType = Float
  private[nio] type BufferType = FloatBuffer
  private[nio] type TypedArrayType = Float32Array

  def this(_capacity: Int) = this(_capacity, null, -1)

  def slice(): FloatBuffer

  def duplicate(): FloatBuffer

  def asReadOnlyBuffer(): FloatBuffer

  def get(): Float

  def put(f: Float): FloatBuffer

  def get(index: Int): Float

  def put(index: Int, f: Float): FloatBuffer

  @noinline
  def get(dst: Array[Float], offset: Int, length: Int): FloatBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  def get(dst: Array[Float]): FloatBuffer =
    get(dst, 0, dst.length)

  @noinline
  def put(src: FloatBuffer): FloatBuffer =
    GenBuffer(this).generic_put(src)

  @noinline
  def put(src: Array[Float], offset: Int, length: Int): FloatBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  final def put(src: Array[Float]): FloatBuffer =
    put(src, 0, src.length)

  @inline final def hasArray(): Boolean =
    GenBuffer(this).generic_hasArray()

  @inline final def array(): Array[Float] =
    GenBuffer(this).generic_array()

  @inline final def arrayOffset(): Int =
    GenBuffer(this).generic_arrayOffset()

  def compact(): FloatBuffer

  def isDirect(): Boolean

  // toString(): String inherited from Buffer

  @noinline
  override def hashCode(): Int =
    GenBuffer(this).generic_hashCode(FloatBuffer.HashSeed)

  override def equals(that: Any): Boolean = that match {
    case that: FloatBuffer => compareTo(that) == 0
    case _                 => false
  }

  @noinline
  def compareTo(that: FloatBuffer): Int =
    GenBuffer(this).generic_compareTo(that)(_.compareTo(_))

  def order(): ByteOrder

  // Internal API

  private[nio] def load(index: Int): Float

  private[nio] def store(index: Int, elem: Float): Unit

  @inline
  private[nio] def load(startIndex: Int,
      dst: Array[Float], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  private[nio] def store(startIndex: Int,
      src: Array[Float], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}
