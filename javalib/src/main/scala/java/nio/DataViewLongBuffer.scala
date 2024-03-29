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

import java.nio.DataViewExt._

import scala.scalajs.js.typedarray._

private[nio] final class DataViewLongBuffer private (
    override private[nio] val _dataView: DataView,
    _initialPosition: Int, _initialLimit: Int, _readOnly: Boolean,
    override private[nio] val isBigEndian: Boolean)
    extends LongBuffer(_dataView.byteLength / 8, null, -1) {

  position(_initialPosition)
  limit(_initialLimit)

  private[this] implicit def newDataViewLongBuffer =
    DataViewLongBuffer.NewDataViewLongBuffer

  def isReadOnly(): Boolean = _readOnly

  def isDirect(): Boolean = true

  @noinline
  def slice(): LongBuffer =
    GenDataViewBuffer(this).generic_slice()

  @noinline
  def duplicate(): LongBuffer =
    GenDataViewBuffer(this).generic_duplicate()

  @noinline
  def asReadOnlyBuffer(): LongBuffer =
    GenDataViewBuffer(this).generic_asReadOnlyBuffer()

  @noinline
  def get(): Long =
    GenBuffer(this).generic_get()

  @noinline
  def put(l: Long): LongBuffer =
    GenBuffer(this).generic_put(l)

  @noinline
  def get(index: Int): Long =
    GenBuffer(this).generic_get(index)

  @noinline
  def put(index: Int, l: Long): LongBuffer =
    GenBuffer(this).generic_put(index, l)

  @noinline
  override def get(dst: Array[Long], offset: Int, length: Int): LongBuffer =
    GenBuffer(this).generic_get(dst, offset, length)

  @noinline
  override def put(src: Array[Long], offset: Int, length: Int): LongBuffer =
    GenBuffer(this).generic_put(src, offset, length)

  @noinline
  def compact(): LongBuffer =
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
  private[nio] def load(index: Int): Long =
    dataViewGetInt64(_dataView, 8 * index, !isBigEndian)

  @inline
  private[nio] def store(index: Int, elem: Long): Unit =
    dataViewSetInt64(_dataView, 8 * index, elem, !isBigEndian)

  @inline
  override private[nio] def load(startIndex: Int,
      dst: Array[Long], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_load(startIndex, dst, offset, length)

  @inline
  override private[nio] def store(startIndex: Int,
      src: Array[Long], offset: Int, length: Int): Unit =
    GenBuffer(this).generic_store(startIndex, src, offset, length)
}

private[nio] object DataViewLongBuffer {
  private[nio] implicit object NewDataViewLongBuffer
      extends GenDataViewBuffer.NewDataViewBuffer[LongBuffer] {
    def bytesPerElem: Int = 8

    def apply(dataView: DataView,
        initialPosition: Int, initialLimit: Int,
        readOnly: Boolean, isBigEndian: Boolean): LongBuffer = {
      new DataViewLongBuffer(dataView,
          initialPosition, initialLimit, readOnly, isBigEndian)
    }
  }

  @inline
  def fromTypedArrayByteBuffer(byteBuffer: TypedArrayByteBuffer): LongBuffer =
    GenDataViewBuffer.generic_fromTypedArrayByteBuffer(byteBuffer)
}
