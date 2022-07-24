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

/* !!!!!
 * THIS FILE IS ALMOST COPY-PASTED IN javalib/ AND library/.
 * THEY MUST BE KEPT IN SYNC.
 *
 * This file acts as bridge for scala.scalajs.js.typedarray to be able to
 * access the additional public API provided by java.nio, but which is not
 * part of the JDK API. Because javalib/ does not export its .class files,
 * we cannot call this additional API directly from library/, even though the
 * members are public.
 *
 * In library/, this file has only the signatures, with stub implementations.
 * In javalib/, it has the proper implementations.
 * The build keeps the .class coming from library/ and the .sjsir file from
 * javalib/. This way, we bridge the library and javalib. But that means the
 * binary interface of TypedArrayBufferBridge must be strictly equivalent in
 * the two copies.
 *
 * Because of these copies, we must also explicitly use `Any` instead of all
 * JS types in the method signatures. The IR cleaner would replace any JS type
 * by `Any` in the javalib, so if we don't write them like that in the library
 * as well, there will be mismatches.
 *
 * (Yes, this is a hack.)
 * !!!!!
 */

package scala.scalajs.js.typedarray

import java.nio._

private[typedarray] object TypedArrayBufferBridge {
  def wrapArrayBuffer(array: Any): ByteBuffer =
    ByteBuffer.wrapArrayBuffer(array.asInstanceOf[ArrayBuffer])

  def wrapArrayBuffer(array: Any, byteOffset: Int, length: Int): ByteBuffer =
    ByteBuffer.wrapArrayBuffer(array.asInstanceOf[ArrayBuffer], byteOffset, length)

  def wrapInt8Array(array: Any): ByteBuffer =
    ByteBuffer.wrapInt8Array(array.asInstanceOf[Int8Array])

  def wrapUint16Array(array: Any): CharBuffer =
    CharBuffer.wrapUint16Array(array.asInstanceOf[Uint16Array])

  def wrapInt16Array(array: Any): ShortBuffer =
    ShortBuffer.wrapInt16Array(array.asInstanceOf[Int16Array])

  def wrapInt32Array(array: Any): IntBuffer =
    IntBuffer.wrapInt32Array(array.asInstanceOf[Int32Array])

  def wrapFloat32Array(array: Any): FloatBuffer =
    FloatBuffer.wrapFloat32Array(array.asInstanceOf[Float32Array])

  def wrapFloat64Array(array: Any): DoubleBuffer =
    DoubleBuffer.wrapFloat64Array(array.asInstanceOf[Float64Array])

  def Buffer_hasArrayBuffer(buffer: Buffer): Boolean =
    buffer.hasArrayBuffer()

  def Buffer_arrayBuffer(buffer: Buffer): Any =
    buffer.arrayBuffer()

  def Buffer_arrayBufferOffset(buffer: Buffer): Int =
    buffer.arrayBufferOffset()

  def Buffer_dataView(buffer: Buffer): Any =
    buffer.dataView()

  def Buffer_hasTypedArray(buffer: Buffer): Boolean =
    buffer.hasTypedArray()

  def Buffer_typedArray(buffer: Buffer): Any =
    buffer.typedArray()
}
