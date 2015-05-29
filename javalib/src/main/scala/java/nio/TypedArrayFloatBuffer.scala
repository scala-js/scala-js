package java.nio

import scala.scalajs.js.typedarray._

private[nio] final class TypedArrayFloatBuffer private (
    override private[nio] val _typedArray: Float32Array,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends FloatBuffer(_typedArray.length, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newTypedArrayFloatBuffer =
    TypedArrayFloatBuffer.NewTypedArrayFloatBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = true

  @noinline
  def slice(): FloatBuffer =
    GenTypedArrayBuffer(this).generic_slice()

  @noinline
  def duplicate(): FloatBuffer =
    GenTypedArrayBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): FloatBuffer =
    GenTypedArrayBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Float =
    GenBuffer(this).generic_get()

  @noinline
  def put(c: Float): FloatBuffer =
    GenBuffer(this).generic_put(c)

  @noinline
  def get(index: Int): Float =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, c: Float): FloatBuffer =
    GenBuffer(this).generic_put(index, c)

  @noinline
  override def get(dst: Array[Float], offset: Int, length: Int): FloatBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Float], offset: Int, length: Int): FloatBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): FloatBuffer =
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
  private[nio] def load(index: Int): Float =
    _typedArray(index)

  @inline
  private[nio] def store(index: Int, elem: Float): Unit =
    _typedArray(index) = elem

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Float], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Float], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object TypedArrayFloatBuffer {
  private[nio] implicit object NewTypedArrayFloatBuffer
      extends GenTypedArrayBuffer.NewTypedArrayBuffer[FloatBuffer] {
    def bytesPerElem: Int = 4

    def apply(typedArray: Float32Array,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean): TypedArrayFloatBuffer = {
      new TypedArrayFloatBuffer(typedArray,
          initialPosition, initialLimit, readOnly)
    }

    @inline
    def newTypedArray(buffer: ArrayBuffer,
        byteOffset: Int, length: Int): Float32Array = {
      new Float32Array(buffer, byteOffset, length)
    }
  }

  @inline
  def fromTypedArrayByteBuffer(byteBuffer: TypedArrayByteBuffer): FloatBuffer =
    GenTypedArrayBuffer.generic_fromTypedArrayByteBuffer(byteBuffer)

  def wrap(array: Float32Array): FloatBuffer =
    new TypedArrayFloatBuffer(array, 0, array.length, false)
}
