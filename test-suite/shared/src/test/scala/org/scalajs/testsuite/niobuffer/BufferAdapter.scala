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

package org.scalajs.testsuite.niobuffer

import java.nio._

sealed abstract class BufferAdapter[BT <: Buffer, ET] {
  type BufferType = BT
  type ElementType = ET

  /* Some methods have a Chain suffix because they are declared as abstract in
   * java.nio.Buffer since Java 9, but with a result type of `Buffer` instead
   * of the more specific `BufferType`. We use the `Chain` variant to be able
   * to chain their application with further operations on the specific
   * `BufferType`.
   */

  def sliceChain(): BufferType
  def duplicateChain(): BufferType
  def asReadOnlyBuffer(): BufferType
  def get(): ElementType
  def put(e: ElementType): BufferType
  def get(index: Int): ElementType
  def put(index: Int, e: ElementType): BufferType
  def get(dst: Array[ElementType], offset: Int, length: Int): BufferType
  def get(dst: Array[ElementType]): BufferType
  def put(src: BufferType): BufferType
  def put(src: Array[ElementType], offset: Int, length: Int): BufferType
  def put(src: Array[ElementType])(implicit dummy: DummyImplicit): BufferType
  def hasArray(): Boolean
  def array(): Array[ElementType]
  def arrayOffset(): Int
  def compact(): BufferType
  def order(): ByteOrder
}

object BufferAdapter {
  class ByteBufferAdapater(val buffer: ByteBuffer) extends BufferAdapter[ByteBuffer, Byte] {
    def sliceChain(): BufferType = buffer.slice()
    def duplicateChain(): BufferType = buffer.duplicate()
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
    def put(src: Array[ElementType])(implicit dummy: DummyImplicit): BufferType =
      buffer.put(src)
    def hasArray(): Boolean = buffer.hasArray()
    def array(): Array[ElementType] = buffer.array()
    def arrayOffset(): Int = buffer.arrayOffset()
    def compact(): BufferType = buffer.compact()
    def order(): ByteOrder = buffer.order()
  }

  class CharBufferAdapater(val buffer: CharBuffer) extends BufferAdapter[CharBuffer, Char] {
    def sliceChain(): BufferType = buffer.slice()
    def duplicateChain(): BufferType = buffer.duplicate()
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
    def put(src: Array[ElementType])(implicit dummy: DummyImplicit): BufferType =
      buffer.put(src)
    def hasArray(): Boolean = buffer.hasArray()
    def array(): Array[ElementType] = buffer.array()
    def arrayOffset(): Int = buffer.arrayOffset()
    def compact(): BufferType = buffer.compact()
    def order(): ByteOrder = buffer.order()
  }

  class ShortBufferAdapater(val buffer: ShortBuffer) extends BufferAdapter[ShortBuffer, Short] {
    def sliceChain(): BufferType = buffer.slice()
    def duplicateChain(): BufferType = buffer.duplicate()
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
    def put(src: Array[ElementType])(implicit dummy: DummyImplicit): BufferType =
      buffer.put(src)
    def hasArray(): Boolean = buffer.hasArray()
    def array(): Array[ElementType] = buffer.array()
    def arrayOffset(): Int = buffer.arrayOffset()
    def compact(): BufferType = buffer.compact()
    def order(): ByteOrder = buffer.order()
  }

  class IntBufferAdapater(val buffer: IntBuffer) extends BufferAdapter[IntBuffer, Int] {
    def sliceChain(): BufferType = buffer.slice()
    def duplicateChain(): BufferType = buffer.duplicate()
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
    def put(src: Array[ElementType])(implicit dummy: DummyImplicit): BufferType =
      buffer.put(src)
    def hasArray(): Boolean = buffer.hasArray()
    def array(): Array[ElementType] = buffer.array()
    def arrayOffset(): Int = buffer.arrayOffset()
    def compact(): BufferType = buffer.compact()
    def order(): ByteOrder = buffer.order()
  }

  class LongBufferAdapater(val buffer: LongBuffer) extends BufferAdapter[LongBuffer, Long] {
    def sliceChain(): BufferType = buffer.slice()
    def duplicateChain(): BufferType = buffer.duplicate()
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
    def put(src: Array[ElementType])(implicit dummy: DummyImplicit): BufferType =
      buffer.put(src)
    def hasArray(): Boolean = buffer.hasArray()
    def array(): Array[ElementType] = buffer.array()
    def arrayOffset(): Int = buffer.arrayOffset()
    def compact(): BufferType = buffer.compact()
    def order(): ByteOrder = buffer.order()
  }

  class FloatBufferAdapater(val buffer: FloatBuffer) extends BufferAdapter[FloatBuffer, Float] {
    def sliceChain(): BufferType = buffer.slice()
    def duplicateChain(): BufferType = buffer.duplicate()
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
    def put(src: Array[ElementType])(implicit dummy: DummyImplicit): BufferType =
      buffer.put(src)
    def hasArray(): Boolean = buffer.hasArray()
    def array(): Array[ElementType] = buffer.array()
    def arrayOffset(): Int = buffer.arrayOffset()
    def compact(): BufferType = buffer.compact()
    def order(): ByteOrder = buffer.order()
  }

  class DoubleBufferAdapater(val buffer: DoubleBuffer) extends BufferAdapter[DoubleBuffer, Double] {
    def sliceChain(): BufferType = buffer.slice()
    def duplicateChain(): BufferType = buffer.duplicate()
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
    def put(src: Array[ElementType])(implicit dummy: DummyImplicit): BufferType =
      buffer.put(src)
    def hasArray(): Boolean = buffer.hasArray()
    def array(): Array[ElementType] = buffer.array()
    def arrayOffset(): Int = buffer.arrayOffset()
    def compact(): BufferType = buffer.compact()
    def order(): ByteOrder = buffer.order()
  }
}
