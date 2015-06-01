package java.nio

import scala.scalajs.js.typedarray._

private[nio] final class TypedArrayShortBuffer private (
    override private[nio] val _typedArray: Int16Array,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends ShortBuffer(_typedArray.length, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newTypedArrayShortBuffer =
    TypedArrayShortBuffer.NewTypedArrayShortBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = true

  @noinline
  def slice(): ShortBuffer =
    GenTypedArrayBuffer(this).generic_slice()

  @noinline
  def duplicate(): ShortBuffer =
    GenTypedArrayBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): ShortBuffer =
    GenTypedArrayBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Short =
    GenBuffer(this).generic_get()

  @noinline
  def put(c: Short): ShortBuffer =
    GenBuffer(this).generic_put(c)

  @noinline
  def get(index: Int): Short =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, c: Short): ShortBuffer =
    GenBuffer(this).generic_put(index, c)

  @noinline
  override def get(dst: Array[Short], offset: Int, length: Int): ShortBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Short], offset: Int, length: Int): ShortBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): ShortBuffer =
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
  private[nio] def load(index: Int): Short =
    _typedArray(index)

  @inline
  private[nio] def store(index: Int, elem: Short): Unit =
    _typedArray(index) = elem

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Short], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Short], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object TypedArrayShortBuffer {
  private[nio] implicit object NewTypedArrayShortBuffer
      extends GenTypedArrayBuffer.NewTypedArrayBuffer[ShortBuffer] {
    def bytesPerElem: Int = 2

    def apply(typedArray: Int16Array,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean): TypedArrayShortBuffer = {
      new TypedArrayShortBuffer(typedArray,
          initialPosition, initialLimit, readOnly)
    }

    @inline
    def newTypedArray(buffer: ArrayBuffer,
        byteOffset: Int, length: Int): Int16Array = {
      new Int16Array(buffer, byteOffset, length)
    }
  }

  @inline
  def fromTypedArrayByteBuffer(byteBuffer: TypedArrayByteBuffer): ShortBuffer =
    GenTypedArrayBuffer.generic_fromTypedArrayByteBuffer(byteBuffer)

  def wrap(array: Int16Array): ShortBuffer =
    new TypedArrayShortBuffer(array, 0, array.length, false)
}
