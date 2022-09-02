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

package scala.scalajs.js.typedarray

import scala.language.implicitConversions

import java.nio._

import org.scalajs.javalibintf.{TypedArrayBuffer => Intf}

/** Additional operations on a [[java.nio.Buffer Buffer]] with interoperability
 *  with JavaScript Typed Arrays.
 *
 *  All Scala.js implementations of [[java.nio.Buffer Buffer]] also implement
 *  this interface for some TypedArrayType, which depends on the type of
 *  elements in the buffer.
 */
final class TypedArrayBufferOps[ // scalastyle:ignore
    TypedArrayType <: TypedArray[_, TypedArrayType]] private (
    private val buffer: Buffer)
    extends AnyVal {

  /** Tests whether this buffer has a valid associated [[ArrayBuffer]].
   *
   *  This is true iff the buffer is direct and not read-only.
   */
  def hasArrayBuffer(): Boolean =
    Intf.hasArrayBuffer(buffer)

  /** [[ArrayBuffer]] backing this buffer _(optional operation)_.
   *
   *  @throws UnsupportedOperationException
   *    If this buffer has no backing [[ArrayBuffer]], i.e., !hasArrayBuffer()
   */
  def arrayBuffer(): ArrayBuffer =
    Intf.arrayBuffer(buffer).asInstanceOf[ArrayBuffer]

  /** Byte offset in the associated [[ArrayBuffer]] _(optional operation)_.
   *
   *  @throws UnsupportedOperationException
   *    If this buffer has no backing [[ArrayBuffer]], i.e., !hasArrayBuffer()
   */
  def arrayBufferOffset(): Int =
    Intf.arrayBufferOffset(buffer)

  /** [[DataView]] of the backing [[ArrayBuffer]] _(optional operation)_.
   *
   *  The [[DataView]] is sliced to the portion of the [[ArrayBuffer]] seen by
   *  this [[java.nio.Buffer Buffer]].
   *
   *  @throws UnsupportedOperationException
   *    If this buffer has no backing [[ArrayBuffer]], i.e., !hasArrayBuffer()
   */
  def dataView(): DataView =
    Intf.dataView(buffer).asInstanceOf[DataView]

  /** Tests whether this direct buffer has a valid associated [[TypedArray]].
   *
   *  If this buffer is read-only, returns false.
   *
   *  For read-write buffers:
   *
   *  * Direct Byte buffers always have an associated [[TypedArray]].
   *  * Long buffers never do.
   *  * Other kinds of direct buffers have an associated [[TypedArray]] if and
   *    only if their byte order is the native order of the platform.
   */
  def hasTypedArray(): Boolean =
    Intf.hasTypedArray(buffer)

  /** [[TypedArray]] backing this direct buffer _(optional operation)_.
   *
   *  The [[TypedArray]] is sliced to the portion of the [[ArrayBuffer]] seen
   *  by this [[java.nio.Buffer Buffer]].
   *
   *  @throws UnsupportedOperationException
   *    If this buffer does not have a backing [[TypedArray]], i.e., !hasTypedArray().
   */
  def typedArray(): TypedArrayType =
    Intf.typedArray(buffer).asInstanceOf[TypedArrayType]
}

/** Extensions to [[java.nio.Buffer Buffer]]s for interoperability with
 *  JavaScript Typed Arrays.
 */
object TypedArrayBufferOps {
  implicit def bufferOps(buffer: Buffer): TypedArrayBufferOps[_ <: TypedArray[_, _]] =
    new TypedArrayBufferOps(buffer)

  implicit def byteBufferOps(buffer: ByteBuffer): TypedArrayBufferOps[Int8Array] =
    new TypedArrayBufferOps(buffer)

  implicit def charBufferOps(buffer: CharBuffer): TypedArrayBufferOps[Uint16Array] =
    new TypedArrayBufferOps(buffer)

  implicit def shortBufferOps(buffer: ShortBuffer): TypedArrayBufferOps[Int16Array] =
    new TypedArrayBufferOps(buffer)

  implicit def intBufferOps(buffer: IntBuffer): TypedArrayBufferOps[Int32Array] =
    new TypedArrayBufferOps(buffer)

  implicit def longBufferOps(buffer: LongBuffer): TypedArrayBufferOps[Nothing] =
    new TypedArrayBufferOps(buffer)

  implicit def floatBufferOps(buffer: FloatBuffer): TypedArrayBufferOps[Float32Array] =
    new TypedArrayBufferOps(buffer)

  implicit def doubleBufferOps(buffer: DoubleBuffer): TypedArrayBufferOps[Float64Array] =
    new TypedArrayBufferOps(buffer)
}
