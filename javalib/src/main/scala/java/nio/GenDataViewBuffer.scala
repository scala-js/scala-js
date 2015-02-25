package java.nio

import scala.scalajs.js.Dynamic.{literal => lit}
import scala.scalajs.js.typedarray._

private[nio] object GenDataViewBuffer {
  def apply[B <: Buffer](self: B): GenDataViewBuffer[B] =
    new GenDataViewBuffer(self)

  trait NewDataViewBuffer[BufferType <: Buffer] {
    def bytesPerElem: Int

    def apply(dataView: DataView,
        initialPosition: Int, initialLimit: Int, readOnly: Boolean,
        isBigEndian: Boolean): BufferType
  }

  @inline
  def generic_fromTypedArrayByteBuffer[BufferType <: Buffer](
      byteBuffer: TypedArrayByteBuffer)(
      implicit newDataViewBuffer: NewDataViewBuffer[BufferType]): BufferType = {
    val byteArray = byteBuffer._typedArray
    val byteBufferPos = byteBuffer.position
    val byteBufferLimit = byteBuffer.limit
    val viewCapacity =
      (byteBufferLimit - byteBufferPos) / newDataViewBuffer.bytesPerElem
    val byteLength = viewCapacity * newDataViewBuffer.bytesPerElem
    val dataView = newDataView(
        byteArray.buffer, byteArray.byteOffset + byteBufferPos, byteLength)
    newDataViewBuffer(dataView,
        0, viewCapacity, byteBuffer.isReadOnly, byteBuffer.isBigEndian)
  }

  /* Work around for https://github.com/joyent/node/issues/6051
   * node 0.10 does not like creating a DataView whose byteOffset is equal to
   * the buffer's length, even if byteLength == 0.
   */
  @inline
  private def newDataView(buffer: ArrayBuffer, byteOffset: Int, byteLength: Int): DataView = {
    if (byteLength == 0)
      lit(buffer = buffer, byteOffset = byteOffset, byteLength = byteLength).asInstanceOf[DataView]
    else
      new DataView(buffer, byteOffset, byteLength)
  }
}

private[nio] final class GenDataViewBuffer[B <: Buffer](val self: B) extends AnyVal {
  import self._

  import GenDataViewBuffer.newDataView

  type NewThisDataViewBuffer = GenDataViewBuffer.NewDataViewBuffer[BufferType]

  @inline
  def generic_slice()(
      implicit newDataViewBuffer: NewThisDataViewBuffer): BufferType = {
    val bytesPerElem = newDataViewBuffer.bytesPerElem
    val dataView = _dataView
    val pos = position
    val newCapacity = limit - pos
    val slicedDataView = newDataView(dataView.buffer,
        dataView.byteOffset + bytesPerElem*pos, bytesPerElem*newCapacity)
    newDataViewBuffer(slicedDataView,
        0, newCapacity, isReadOnly, isBigEndian)
  }

  @inline
  def generic_duplicate()(
      implicit newDataViewBuffer: NewThisDataViewBuffer): BufferType = {
    val result = newDataViewBuffer(_dataView,
        position, limit, isReadOnly, isBigEndian)
    result._mark = _mark
    result
  }

  @inline
  def generic_asReadOnlyBuffer()(
      implicit newDataViewBuffer: NewThisDataViewBuffer): BufferType = {
    val result = newDataViewBuffer(_dataView,
        position, limit, true, isBigEndian)
    result._mark = _mark
    result
  }

  @inline
  def generic_compact()(
      implicit newDataViewBuffer: NewThisDataViewBuffer): BufferType = {
    if (isReadOnly)
      throw new ReadOnlyBufferException

    val dataView = _dataView
    val bytesPerElem = newDataViewBuffer.bytesPerElem
    val byteArray = new Int8Array(dataView.buffer,
        dataView.byteOffset, dataView.byteLength)
    val pos = position
    val lim = limit
    byteArray.set(byteArray.subarray(bytesPerElem * pos, bytesPerElem * lim))
    _mark = -1
    limit(capacity)
    position(lim - pos)
    self
  }

  @inline
  def generic_order(): ByteOrder =
    if (isBigEndian) ByteOrder.BIG_ENDIAN
    else             ByteOrder.LITTLE_ENDIAN

  @inline
  def generic_arrayBuffer: ArrayBuffer =
    _dataView.buffer

  @inline
  def generic_arrayBufferOffset: Int =
    _dataView.byteOffset

}
