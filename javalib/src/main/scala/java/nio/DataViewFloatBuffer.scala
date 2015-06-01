package java.nio

import scala.scalajs.js.typedarray._

private[nio] final class DataViewFloatBuffer private (
    override private[nio] val _dataView: DataView,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean,
    override private[nio] val isBigEndian: Boolean)
    extends FloatBuffer(_dataView.byteLength / 4, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newDataViewFloatBuffer =
    DataViewFloatBuffer.NewDataViewFloatBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = true

  @noinline
  def slice(): FloatBuffer =
    GenDataViewBuffer(this).generic_slice()

  @noinline
  def duplicate(): FloatBuffer =
    GenDataViewBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): FloatBuffer =
    GenDataViewBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Float =
    GenBuffer(this).generic_get()

  @noinline
  def put(f: Float): FloatBuffer =
    GenBuffer(this).generic_put(f)

  @noinline
  def get(index: Int): Float =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, f: Float): FloatBuffer =
    GenBuffer(this).generic_put(index, f)

  @noinline
  override def get(dst: Array[Float], offset: Int, length: Int): FloatBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Float], offset: Int, length: Int): FloatBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): FloatBuffer =
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
  private[nio] def load(index: Int): Float =
    _dataView.getFloat32(4 * index, !isBigEndian)

  @inline
  private[nio] def store(index: Int, elem: Float): Unit =
    _dataView.setFloat32(4 * index, elem, !isBigEndian)

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Float], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Float], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object DataViewFloatBuffer {
  private[nio] implicit object NewDataViewFloatBuffer
      extends GenDataViewBuffer.NewDataViewBuffer[FloatBuffer] {
    def bytesPerElem: Int = 4

    def apply(dataView: DataView,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean, isBigEndian: Boolean): FloatBuffer = {
      new DataViewFloatBuffer(dataView,
          initialPosition, initialLimit, readOnly, isBigEndian)
    }
  }

  @inline
  def fromTypedArrayByteBuffer(byteBuffer: TypedArrayByteBuffer): FloatBuffer =
    GenDataViewBuffer.generic_fromTypedArrayByteBuffer(byteBuffer)
}
