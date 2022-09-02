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

package org.scalajs.javalibintf

import java.nio._

import scala.scalajs.js.typedarray._

object TypedArrayBuffer {

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

  def hasArrayBuffer(buffer: Buffer): Boolean =
    buffer.hasArrayBuffer()

  def arrayBuffer(buffer: Buffer): Any =
    buffer.arrayBuffer()

  def arrayBufferOffset(buffer: Buffer): Int =
    buffer.arrayBufferOffset()

  def dataView(buffer: Buffer): Any =
    buffer.dataView()

  def hasTypedArray(buffer: Buffer): Boolean =
    buffer.hasTypedArray()

  def typedArray(buffer: Buffer): Any =
    buffer.typedArray()
}
