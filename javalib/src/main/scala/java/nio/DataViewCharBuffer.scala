package java.nio

import scala.scalajs.js.typedarray._

private[nio] final class DataViewCharBuffer private (
    override private[nio] val _dataView: DataView,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean,
    override private[nio] val isBigEndian: Boolean)
    extends CharBuffer(_dataView.byteLength / 2, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newDataViewCharBuffer =
    DataViewCharBuffer.NewDataViewCharBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = true

  @noinline
  def slice(): CharBuffer =
    GenDataViewBuffer(this).generic_slice()

  @noinline
  def duplicate(): CharBuffer =
    GenDataViewBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): CharBuffer =
    GenDataViewBuffer(this).generic_asReadOnlyBuffer()

  def subSequence(start: Int, end: Int): CharBuffer = {
    if (start < 0 || end < start || end > remaining)
      throw new IndexOutOfBoundsException
    new DataViewCharBuffer(_dataView,
        position + start, position + end, isReadOnly, isBigEndian)
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
    GenDataViewBuffer(this).generic_compact()

  def order(): ByteOrder =
    GenDataViewBuffer(this).generic_order()

  // Internal API

  @inline
  override private[nio] def _arrayBuffer: ArrayBuffer =
    GenDataViewBuffer(this).generic_arrayBuffer

  @inline
  override private[nio] def _arrayBufferOffset: Int =
    GenDataViewBuffer(this).generic_arrayBufferOffset

  @inline
  private[nio] def load(index: Int): Char =
    _dataView.getUint16(2 * index, !isBigEndian).toChar

  @inline
  private[nio] def store(index: Int, elem: Char): Unit =
    _dataView.setUint16(2 * index, elem.toInt, !isBigEndian)

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Char], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Char], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object DataViewCharBuffer {
  private[nio] implicit object NewDataViewCharBuffer
      extends GenDataViewBuffer.NewDataViewBuffer[CharBuffer] {
    def bytesPerElem: Int = 2

    def apply(dataView: DataView,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean, isBigEndian: Boolean): CharBuffer = {
      new DataViewCharBuffer(dataView,
          initialPosition, initialLimit, readOnly, isBigEndian)
    }
  }

  @inline
  def fromTypedArrayByteBuffer(byteBuffer: TypedArrayByteBuffer): CharBuffer =
    GenDataViewBuffer.generic_fromTypedArrayByteBuffer(byteBuffer)
}
