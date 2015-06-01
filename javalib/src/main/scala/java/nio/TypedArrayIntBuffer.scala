package java.nio

import scala.scalajs.js.typedarray._

private[nio] final class TypedArrayIntBuffer private (
    override private[nio] val _typedArray: Int32Array,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends IntBuffer(_typedArray.length, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newTypedArrayIntBuffer =
    TypedArrayIntBuffer.NewTypedArrayIntBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = true

  @noinline
  def slice(): IntBuffer =
    GenTypedArrayBuffer(this).generic_slice()

  @noinline
  def duplicate(): IntBuffer =
    GenTypedArrayBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): IntBuffer =
    GenTypedArrayBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Int =
    GenBuffer(this).generic_get()

  @noinline
  def put(c: Int): IntBuffer =
    GenBuffer(this).generic_put(c)

  @noinline
  def get(index: Int): Int =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, c: Int): IntBuffer =
    GenBuffer(this).generic_put(index, c)

  @noinline
  override def get(dst: Array[Int], offset: Int, length: Int): IntBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Int], offset: Int, length: Int): IntBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): IntBuffer =
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
  private[nio] def load(index: Int): Int =
    _typedArray(index)

  @inline
  private[nio] def store(index: Int, elem: Int): Unit =
    _typedArray(index) = elem

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Int], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Int], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object TypedArrayIntBuffer {
  private[nio] implicit object NewTypedArrayIntBuffer
      extends GenTypedArrayBuffer.NewTypedArrayBuffer[IntBuffer] {
    def bytesPerElem: Int = 4

    def apply(typedArray: Int32Array,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean): TypedArrayIntBuffer = {
      new TypedArrayIntBuffer(typedArray,
          initialPosition, initialLimit, readOnly)
    }

    @inline
    def newTypedArray(buffer: ArrayBuffer,
        byteOffset: Int, length: Int): Int32Array = {
      new Int32Array(buffer, byteOffset, length)
    }
  }

  @inline
  def fromTypedArrayByteBuffer(byteBuffer: TypedArrayByteBuffer): IntBuffer =
    GenTypedArrayBuffer.generic_fromTypedArrayByteBuffer(byteBuffer)

  def wrap(array: Int32Array): IntBuffer =
    new TypedArrayIntBuffer(array, 0, array.length, false)
}
