/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


/* !!!!!
 * THIS FILE IS ALMOST COPY-PASTED IN javalib/ AND library/.
 * THEY MUST BE KEPT IN SYNC.
 *
 * This file acts as bridge for scala.scalajs.js.typedarray to be able to
 * access the additional public API provided by java.nio, but which is not
 * part of the JDK API. Because javalib/ does not export its .class files,
 * we cannot call this additonal API directly from library/, even though the
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
  def wrap(array: ArrayBuffer): ByteBuffer =
    sys.error("stub")

  def wrap(array: ArrayBuffer, byteOffset: Int, length: Int): ByteBuffer =
    sys.error("stub")

  def wrap(array: Int8Array): ByteBuffer =
    sys.error("stub")

  def wrap(array: Uint16Array): CharBuffer =
    sys.error("stub")

  def wrap(array: Int16Array): ShortBuffer =
    sys.error("stub")

  def wrap(array: Int32Array): IntBuffer =
    sys.error("stub")

  def wrap(array: Float32Array): FloatBuffer =
    sys.error("stub")

  def wrap(array: Float64Array): DoubleBuffer =
    sys.error("stub")

  def Buffer_hasArrayBuffer(buffer: Buffer): Boolean =
    sys.error("stub")

  def Buffer_arrayBuffer(buffer: Buffer): ArrayBuffer =
    sys.error("stub")

  def Buffer_arrayBufferOffset(buffer: Buffer): Int =
    sys.error("stub")

  def Buffer_dataView(buffer: Buffer): DataView =
    sys.error("stub")

  def Buffer_hasTypedArray(buffer: Buffer): Boolean =
    sys.error("stub")

  def Buffer_typedArray(buffer: Buffer): TypedArray[_, _] =
    sys.error("stub")
}
