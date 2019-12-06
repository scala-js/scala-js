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
 * In javalib/, it has the proper the proper implementations.
 * The build keeps the .class coming from library/ and the .sjsir file from
 * javalib/. This way, we bridge the library and javalib. But that means the
 * binary interface of TypedArrayBufferBridge must be strictly equivalent in
 * the two copies.
 *
 * (Yes, this is a hack.)
 * !!!!!
 */

package scala.scalajs.js.typedarray

import java.nio._

private[typedarray] object TypedArrayBufferBridge {
  def wrap(array: ArrayBuffer): ByteBuffer = stub()

  def wrap(array: ArrayBuffer, byteOffset: Int, length: Int): ByteBuffer = stub()

  def wrap(array: Int8Array): ByteBuffer = stub()

  def wrap(array: Uint16Array): CharBuffer = stub()

  def wrap(array: Int16Array): ShortBuffer = stub()

  def wrap(array: Int32Array): IntBuffer = stub()

  def wrap(array: Float32Array): FloatBuffer = stub()

  def wrap(array: Float64Array): DoubleBuffer = stub()

  def Buffer_hasArrayBuffer(buffer: Buffer): Boolean = stub()

  def Buffer_arrayBuffer(buffer: Buffer): ArrayBuffer = stub()

  def Buffer_arrayBufferOffset(buffer: Buffer): Int = stub()

  def Buffer_dataView(buffer: Buffer): DataView = stub()

  def Buffer_hasTypedArray(buffer: Buffer): Boolean = stub()

  def Buffer_typedArray(buffer: Buffer): TypedArray[_, _] = stub()

  private def stub(): Nothing =
    throw new Error("stub")
}
