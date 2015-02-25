package java.nio

import scala.scalajs.js.typedarray._

private[nio] object GenTypedArrayBuffer {
  def apply[B <: Buffer](self: B): GenTypedArrayBuffer[B] =
    new GenTypedArrayBuffer(self)

  trait NewTypedArrayBuffer[BufferType <: Buffer] {
    def bytesPerElem: Int

    def apply(typedArray: BufferType#TypedArrayType,
        initialPosition: Int, initialLimit: Int, readOnly: Boolean): BufferType

    def newTypedArray(buffer: ArrayBuffer,
        byteOffset: Int, length: Int): BufferType#TypedArrayType
  }

  @inline
  def generic_fromTypedArrayByteBuffer[BufferType <: Buffer](
      byteBuffer: TypedArrayByteBuffer)(
      implicit newTypedArrayBuffer: NewTypedArrayBuffer[BufferType]): BufferType = {
    val byteArray = byteBuffer._typedArray
    val byteBufferPos = byteBuffer.position
    val byteBufferLimit = byteBuffer.limit
    val viewCapacity =
      (byteBufferLimit - byteBufferPos) / newTypedArrayBuffer.bytesPerElem
    val viewTypedArray = newTypedArrayBuffer.newTypedArray(
        byteArray.buffer, byteArray.byteOffset + byteBufferPos, viewCapacity)
    newTypedArrayBuffer(viewTypedArray,
        0, viewCapacity, byteBuffer.isReadOnly)
  }
}

private[nio] final class GenTypedArrayBuffer[B <: Buffer](
    val self: B) extends AnyVal {
  import self._

  type NewThisTypedArrayBuffer =
    GenTypedArrayBuffer.NewTypedArrayBuffer[BufferType]

  @inline
  def generic_slice()(
      implicit newTypedArrayBuffer: NewThisTypedArrayBuffer): BufferType = {
    val slicedTypedArray = _typedArray.subarray(position, limit)
    newTypedArrayBuffer(slicedTypedArray,
        0, slicedTypedArray.length, isReadOnly)
  }

  @inline
  def generic_duplicate()(
      implicit newTypedArrayBuffer: NewThisTypedArrayBuffer): BufferType = {
    val result = newTypedArrayBuffer(_typedArray, position, limit, isReadOnly)
    result._mark = _mark
    result
  }

  @inline
  def generic_asReadOnlyBuffer()(
      implicit newTypedArrayBuffer: NewThisTypedArrayBuffer): BufferType = {
    val result = newTypedArrayBuffer(_typedArray, position, limit, true)
    result._mark = _mark
    result
  }

  @inline
  def generic_compact(): BufferType = {
    ensureNotReadOnly()

    val typedArray = _typedArray
    val pos = position
    val lim = limit
    typedArray.set(typedArray.subarray(pos, lim))
    _mark = -1
    limit(capacity)
    position(lim - pos)
    self
  }

  @inline
  def generic_arrayBuffer: ArrayBuffer =
    _typedArray.buffer

  @inline
  def generic_arrayBufferOffset: Int =
    _typedArray.byteOffset

  @inline
  def generic_dataView(
      implicit newTypedArrayBuffer: NewThisTypedArrayBuffer): DataView = {
    val bytesPerElem = newTypedArrayBuffer.bytesPerElem
    val array = _typedArray
    new DataView(array.buffer, array.byteOffset, capacity * bytesPerElem)
  }

}
