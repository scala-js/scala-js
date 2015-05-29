package java.nio

import scala.scalajs.js.typedarray._

private[nio] final class DataViewIntBuffer private (
    override private[nio] val _dataView: DataView,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean,
    override private[nio] val isBigEndian: Boolean)
    extends IntBuffer(_dataView.byteLength / 4, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newDataViewIntBuffer =
    DataViewIntBuffer.NewDataViewIntBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = true

  @noinline
  def slice(): IntBuffer =
    GenDataViewBuffer(this).generic_slice()

  @noinline
  def duplicate(): IntBuffer =
    GenDataViewBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): IntBuffer =
    GenDataViewBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Int =
    GenBuffer(this).generic_get()

  @noinline
  def put(i: Int): IntBuffer =
    GenBuffer(this).generic_put(i)

  @noinline
  def get(index: Int): Int =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, i: Int): IntBuffer =
    GenBuffer(this).generic_put(index, i)

  @noinline
  override def get(dst: Array[Int], offset: Int, length: Int): IntBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Int], offset: Int, length: Int): IntBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): IntBuffer =
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
  private[nio] def load(index: Int): Int =
    _dataView.getInt32(4 * index, !isBigEndian)

  @inline
  private[nio] def store(index: Int, elem: Int): Unit =
    _dataView.setInt32(4 * index, elem, !isBigEndian)

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Int], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Int], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object DataViewIntBuffer {
  private[nio] implicit object NewDataViewIntBuffer
      extends GenDataViewBuffer.NewDataViewBuffer[IntBuffer] {
    def bytesPerElem: Int = 4

    def apply(dataView: DataView,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean, isBigEndian: Boolean): IntBuffer = {
      new DataViewIntBuffer(dataView,
          initialPosition, initialLimit, readOnly, isBigEndian)
    }
  }

  @inline
  def fromTypedArrayByteBuffer(byteBuffer: TypedArrayByteBuffer): IntBuffer =
    GenDataViewBuffer.generic_fromTypedArrayByteBuffer(byteBuffer)
}
