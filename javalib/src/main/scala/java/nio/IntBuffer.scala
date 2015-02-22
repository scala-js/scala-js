package java.nio

import scala.scalajs.js.typedarray._

object IntBuffer {
  private final val HashSeed = 39599817 // "java.nio.IntBuffer".##

  def allocate(capacity: Int): IntBuffer =
    wrap(new Array[Int](capacity))

  def wrap(array: Array[Int], offset: Int, length: Int): IntBuffer =
    HeapIntBuffer.wrap(array, 0, array.length, offset, length, false)

  def wrap(array: Array[Int]): IntBuffer =
    wrap(array, 0, array.length)

  // Extended API

  def wrap(array: Int32Array): IntBuffer =
    TypedArrayIntBuffer.wrap(array)
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
    GenBuffer(this).generic_compareTo(that)(_.compareTo(_))

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
