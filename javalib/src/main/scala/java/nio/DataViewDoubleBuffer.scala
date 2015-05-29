package java.nio

import scala.scalajs.js.typedarray._

private[nio] final class DataViewDoubleBuffer private (
    override private[nio] val _dataView: DataView,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean,
    override private[nio] val isBigEndian: Boolean)
    extends DoubleBuffer(_dataView.byteLength / 8, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newDataViewDoubleBuffer =
    DataViewDoubleBuffer.NewDataViewDoubleBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = true

  @noinline
  def slice(): DoubleBuffer =
    GenDataViewBuffer(this).generic_slice()

  @noinline
  def duplicate(): DoubleBuffer =
    GenDataViewBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): DoubleBuffer =
    GenDataViewBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Double =
    GenBuffer(this).generic_get()

  @noinline
  def put(d: Double): DoubleBuffer =
    GenBuffer(this).generic_put(d)

  @noinline
  def get(index: Int): Double =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, d: Double): DoubleBuffer =
    GenBuffer(this).generic_put(index, d)

  @noinline
  override def get(dst: Array[Double], offset: Int, length: Int): DoubleBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Double], offset: Int, length: Int): DoubleBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): DoubleBuffer =
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
  private[nio] def load(index: Int): Double =
    _dataView.getFloat64(8 * index, !isBigEndian)

  @inline
  private[nio] def store(index: Int, elem: Double): Unit =
    _dataView.setFloat64(8 * index, elem, !isBigEndian)

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Double], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Double], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object DataViewDoubleBuffer {
  private[nio] implicit object NewDataViewDoubleBuffer
      extends GenDataViewBuffer.NewDataViewBuffer[DoubleBuffer] {
    def bytesPerElem: Int = 8

    def apply(dataView: DataView,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean, isBigEndian: Boolean): DoubleBuffer = {
      new DataViewDoubleBuffer(dataView,
          initialPosition, initialLimit, readOnly, isBigEndian)
    }
  }

  @inline
  def fromTypedArrayByteBuffer(byteBuffer: TypedArrayByteBuffer): DoubleBuffer =
    GenDataViewBuffer.generic_fromTypedArrayByteBuffer(byteBuffer)
}
