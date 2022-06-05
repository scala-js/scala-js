/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
    val byteBufferPos = byteBuffer.position()
    val byteBufferLimit = byteBuffer.limit()
    val viewCapacity =
      (byteBufferLimit - byteBufferPos) / newDataViewBuffer.bytesPerElem
    val byteLength = viewCapacity * newDataViewBuffer.bytesPerElem
    val dataView = new DataView(
        byteArray.buffer, byteArray.byteOffset + byteBufferPos, byteLength)
    newDataViewBuffer(dataView,
        0, viewCapacity, byteBuffer.isReadOnly(), byteBuffer.isBigEndian)
  }
}

/* The underlying `val self` is intentionally public because
 * `self.BufferType` appears in signatures.
 * It's tolerable because the class is `private[nio]` anyway.
 */
private[nio] final class GenDataViewBuffer[B <: Buffer] private (val self: B)
    extends AnyVal {

  import self._

  type NewThisDataViewBuffer = GenDataViewBuffer.NewDataViewBuffer[BufferType]

  @inline
  def generic_slice()(
      implicit newDataViewBuffer: NewThisDataViewBuffer): BufferType = {
    val bytesPerElem = newDataViewBuffer.bytesPerElem
    val dataView = _dataView
    val pos = position()
    val newCapacity = limit() - pos
    val slicedDataView = new DataView(dataView.buffer,
        dataView.byteOffset + bytesPerElem*pos, bytesPerElem*newCapacity)
    newDataViewBuffer(slicedDataView,
        0, newCapacity, isReadOnly(), isBigEndian)
  }

  @inline
  def generic_duplicate()(
      implicit newDataViewBuffer: NewThisDataViewBuffer): BufferType = {
    val result = newDataViewBuffer(_dataView,
        position(), limit(), isReadOnly(), isBigEndian)
    result._mark = _mark
    result
  }

  @inline
  def generic_asReadOnlyBuffer()(
      implicit newDataViewBuffer: NewThisDataViewBuffer): BufferType = {
    val result = newDataViewBuffer(_dataView,
        position(), limit(), true, isBigEndian)
    result._mark = _mark
    result
  }

  @inline
  def generic_compact()(
      implicit newDataViewBuffer: NewThisDataViewBuffer): BufferType = {
    if (isReadOnly())
      throw new ReadOnlyBufferException

    val dataView = _dataView
    val bytesPerElem = newDataViewBuffer.bytesPerElem
    val byteArray = new Int8Array(dataView.buffer,
        dataView.byteOffset, dataView.byteLength)
    val pos = position()
    val lim = limit()
    byteArray.set(byteArray.subarray(bytesPerElem * pos, bytesPerElem * lim))
    _mark = -1
    limit(capacity())
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
