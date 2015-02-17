package org.scalajs.testsuite.niobuffer

import java.nio._

abstract class BufferAdapter[BT <: Buffer, ET] {
  type BufferType = BT
  type ElementType = ET

  def slice(): BufferType
  def duplicate(): BufferType
  def asReadOnlyBuffer(): BufferType
  def get(): ElementType
  def put(e: ElementType): BufferType
  def get(index: Int): ElementType
  def put(index: Int, e: ElementType): BufferType
  def get(dst: Array[ElementType], offset: Int, length: Int): BufferType
  def get(dst: Array[ElementType]): BufferType
  def put(src: BufferType): BufferType
  def put(src: Array[ElementType], offset: Int, length: Int): BufferType
  def put(src: Array[ElementType], dummy: Boolean = false): BufferType
  def hasArray(): Boolean
  def array(): Array[ElementType]
  def arrayOffset(): Int
  def compact(): BufferType
  def order(): ByteOrder
}

object BufferAdapter {
  class ByteBufferAdapater(val buffer: ByteBuffer)
      extends BufferAdapter[ByteBuffer, Byte] {
    def slice(): BufferType = buffer.slice()
    def duplicate(): BufferType = buffer.duplicate()
    def asReadOnlyBuffer(): BufferType = buffer.asReadOnlyBuffer()
    def get(): ElementType = buffer.get()
    def put(e: ElementType): BufferType = buffer.put(e)
    def get(index: Int): ElementType = buffer.get(index)
    def put(index: Int, e: ElementType): BufferType = buffer.put(index, e)
    def get(dst: Array[ElementType], offset: Int, length: Int): BufferType =
      buffer.get(dst, offset, length)
    def get(dst: Array[ElementType]): BufferType = buffer.get(dst)
    def put(src: BufferType): BufferType = buffer.put(src)
    def put(src: Array[ElementType], offset: Int, length: Int): BufferType =
      buffer.put(src, offset, length)
    def put(src: Array[ElementType], dummy: Boolean = false): BufferType =
      buffer.put(src)
    def hasArray(): Boolean = buffer.hasArray()
    def array(): Array[ElementType] = buffer.array()
    def arrayOffset(): Int = buffer.arrayOffset()
    def compact(): BufferType = buffer.compact()
    def order(): ByteOrder = buffer.order()
  }

  class CharBufferAdapater(val buffer: CharBuffer)
      extends BufferAdapter[CharBuffer, Char] {
    def slice(): BufferType = buffer.slice()
    def duplicate(): BufferType = buffer.duplicate()
    def asReadOnlyBuffer(): BufferType = buffer.asReadOnlyBuffer()
    def get(): ElementType = buffer.get()
    def put(e: ElementType): BufferType = buffer.put(e)
    def get(index: Int): ElementType = buffer.get(index)
    def put(index: Int, e: ElementType): BufferType = buffer.put(index, e)
    def get(dst: Array[ElementType], offset: Int, length: Int): BufferType =
      buffer.get(dst, offset, length)
    def get(dst: Array[ElementType]): BufferType = buffer.get(dst)
    def put(src: BufferType): BufferType = buffer.put(src)
    def put(src: Array[ElementType], offset: Int, length: Int): BufferType =
      buffer.put(src, offset, length)
    def put(src: Array[ElementType], dummy: Boolean = false): BufferType =
      buffer.put(src)
    def hasArray(): Boolean = buffer.hasArray()
    def array(): Array[ElementType] = buffer.array()
    def arrayOffset(): Int = buffer.arrayOffset()
    def compact(): BufferType = buffer.compact()
    def order(): ByteOrder = buffer.order()
  }
}
