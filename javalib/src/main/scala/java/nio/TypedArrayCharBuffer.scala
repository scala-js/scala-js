package java.nio

import scala.scalajs.js.typedarray._

private[nio] final class TypedArrayCharBuffer private (
    override private[nio] val _typedArray: Uint16Array,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean)
    extends CharBuffer(_typedArray.length, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newTypedArrayCharBuffer =
    TypedArrayCharBuffer.NewTypedArrayCharBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = true

  @noinline
  def slice(): CharBuffer =
    GenTypedArrayBuffer(this).generic_slice()

  @noinline
  def duplicate(): CharBuffer =
    GenTypedArrayBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): CharBuffer =
    GenTypedArrayBuffer(this).generic_asReadOnlyBuffer()

  def subSequence(start: Int, end: Int): CharBuffer = {
    if (start < 0 || end < start || end > remaining)
      throw new IndexOutOfBoundsException
    new TypedArrayCharBuffer(_typedArray,
        position + start, position + end, isReadOnly)
  }

  @noinline
  def get(): Char =
    GenBuffer(this).generic_get()

  @noinline
  def put(c: Char): CharBuffer =
    GenBuffer(this).generic_put(c)

  @noinline
  def get(index: Int): Char =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, c: Char): CharBuffer =
    GenBuffer(this).generic_put(index, c)

  @noinline
  override def get(dst: Array[Char], offset: Int, length: Int): CharBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Char], offset: Int, length: Int): CharBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): CharBuffer =
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
  private[nio] def load(index: Int): Char =
    _typedArray(index).toChar

  @inline
  private[nio] def store(index: Int, elem: Char): Unit =
    _typedArray(index) = elem.toInt

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Char], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Char], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object TypedArrayCharBuffer {
  private[nio] implicit object NewTypedArrayCharBuffer
      extends GenTypedArrayBuffer.NewTypedArrayBuffer[CharBuffer] {
    def bytesPerElem: Int = 2

    def apply(typedArray: Uint16Array,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean): TypedArrayCharBuffer = {
      new TypedArrayCharBuffer(typedArray,
          initialPosition, initialLimit, readOnly)
    }

    @inline
    def newTypedArray(buffer: ArrayBuffer,
        byteOffset: Int, length: Int): Uint16Array = {
      new Uint16Array(buffer, byteOffset, length)
    }
  }

  @inline
  def fromTypedArrayByteBuffer(byteBuffer: TypedArrayByteBuffer): CharBuffer =
    GenTypedArrayBuffer.generic_fromTypedArrayByteBuffer(byteBuffer)

  def wrap(array: Uint16Array): CharBuffer =
    new TypedArrayCharBuffer(array, 0, array.length, false)
}
