package java.nio

import scala.scalajs.js.typedarray._

private[nio] final class DataViewShortBuffer private (
    override private[nio] val _dataView: DataView,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean,
    override private[nio] val isBigEndian: Boolean)
    extends ShortBuffer(_dataView.byteLength / 2, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newDataViewShortBuffer =
    DataViewShortBuffer.NewDataViewShortBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = true

  @noinline
  def slice(): ShortBuffer =
    GenDataViewBuffer(this).generic_slice()

  @noinline
  def duplicate(): ShortBuffer =
    GenDataViewBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): ShortBuffer =
    GenDataViewBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Short =
    GenBuffer(this).generic_get()

  @noinline
  def put(s: Short): ShortBuffer =
    GenBuffer(this).generic_put(s)

  @noinline
  def get(index: Int): Short =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, s: Short): ShortBuffer =
    GenBuffer(this).generic_put(index, s)

  @noinline
  override def get(dst: Array[Short], offset: Int, length: Int): ShortBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Short], offset: Int, length: Int): ShortBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): ShortBuffer =
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
  private[nio] def load(index: Int): Short =
    _dataView.getInt16(2 * index, !isBigEndian)

  @inline
  private[nio] def store(index: Int, elem: Short): Unit =
    _dataView.setInt16(2 * index, elem, !isBigEndian)

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Short], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Short], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object DataViewShortBuffer {
  private[nio] implicit object NewDataViewShortBuffer
      extends GenDataViewBuffer.NewDataViewBuffer[ShortBuffer] {
    def bytesPerElem: Int = 2

    def apply(dataView: DataView,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean, isBigEndian: Boolean): ShortBuffer = {
      new DataViewShortBuffer(dataView,
          initialPosition, initialLimit, readOnly, isBigEndian)
    }
  }

  @inline
  def fromTypedArrayByteBuffer(byteBuffer: TypedArrayByteBuffer): ShortBuffer =
    GenDataViewBuffer.generic_fromTypedArrayByteBuffer(byteBuffer)
}
