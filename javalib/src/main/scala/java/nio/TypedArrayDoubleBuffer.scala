package java.nio

import scala.scalajs.js.typedarray._

private[nio] final class TypedArrayDoubleBuffer private (
    override private[nio] val _typedArray: Float64Array,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends DoubleBuffer(_typedArray.length, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newTypedArrayDoubleBuffer =
    TypedArrayDoubleBuffer.NewTypedArrayDoubleBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = true

  @noinline
  def slice(): DoubleBuffer =
    GenTypedArrayBuffer(this).generic_slice()

  @noinline
  def duplicate(): DoubleBuffer =
    GenTypedArrayBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): DoubleBuffer =
    GenTypedArrayBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Double =
    GenBuffer(this).generic_get()

  @noinline
  def put(c: Double): DoubleBuffer =
    GenBuffer(this).generic_put(c)

  @noinline
  def get(index: Int): Double =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, c: Double): DoubleBuffer =
    GenBuffer(this).generic_put(index, c)

  @noinline
  override def get(dst: Array[Double], offset: Int, length: Int): DoubleBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Double], offset: Int, length: Int): DoubleBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): DoubleBuffer =
    GenTypedArrayBuffer(this).generic_compact()

  def order(): ByteOrder =
    ByteOrder.nativeOrder()

  // Internal API

  @inline
  override private[nio] def _arrayBuffer: ArrayBuffer =
    GenTypedArrayBuffer(this).generic_arrayBuffer

  @inline
  override private[nio] def _arrayBufferOffset: Int =
    GenTypedArrayBuffer(this).generic_arrayBufferOffset

  @inline
  override private[nio] def _dataView: DataView =
    GenTypedArrayBuffer(this).generic_dataView

  @inline
  private[nio] def load(index: Int): Double =
    _typedArray(index)

  @inline
  private[nio] def store(index: Int, elem: Double): Unit =
    _typedArray(index) = elem

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Double], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Double], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object TypedArrayDoubleBuffer {
  private[nio] implicit object NewTypedArrayDoubleBuffer
      extends GenTypedArrayBuffer.NewTypedArrayBuffer[DoubleBuffer] {
    def bytesPerElem: Int = 8

    def apply(typedArray: Float64Array,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean): TypedArrayDoubleBuffer = {
      new TypedArrayDoubleBuffer(typedArray,
          initialPosition, initialLimit, readOnly)
    }

    @inline
    def newTypedArray(buffer: ArrayBuffer,
        byteOffset: Int, length: Int): Float64Array = {
      new Float64Array(buffer, byteOffset, length)
    }
  }

  @inline
  def fromTypedArrayByteBuffer(byteBuffer: TypedArrayByteBuffer): DoubleBuffer =
    GenTypedArrayBuffer.generic_fromTypedArrayByteBuffer(byteBuffer)

  def wrap(array: Float64Array): DoubleBuffer =
    new TypedArrayDoubleBuffer(array, 0, array.length, false)
}
