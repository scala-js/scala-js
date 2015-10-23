/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js.typedarray

import scala.language.implicitConversions

import java.nio._

/** Factory methods to create direct buffers from Typed Arrays.
 *
 *  All buffers created by the methods of this object are direct buffers with
 *  the native byte order of the platform.
 */
object TypedArrayBuffer {
  /** Wraps an [[ArrayBuffer]] in a direct [[java.nio.ByteBuffer ByteBuffer]]. */
  def wrap(array: ArrayBuffer): ByteBuffer =
    TypedArrayBufferBridge.wrap(array)

  /** Wraps an [[ArrayBuffer]] in a direct [[java.nio.ByteBuffer ByteBuffer]]. */
  def wrap(array: ArrayBuffer, byteOffset: Int, length: Int): ByteBuffer =
    TypedArrayBufferBridge.wrap(array, byteOffset, length)

  /** Wraps an [[Int8Array]] in a direct [[java.nio.ByteBuffer ByteBuffer]]. */
  def wrap(array: Int8Array): ByteBuffer =
    TypedArrayBufferBridge.wrap(array)

  /** Wraps a [[Uint16Array]] in a direct [[java.nio.CharBuffer CharBuffer]]. */
  def wrap(array: Uint16Array): CharBuffer =
    TypedArrayBufferBridge.wrap(array)

  /** Wraps an [[Int16Array]] in a direct [[java.nio.ShortBuffer ShortBuffer]]. */
  def wrap(array: Int16Array): ShortBuffer =
    TypedArrayBufferBridge.wrap(array)

  /** Wraps an [[Int32Array]] in a direct [[java.nio.IntBuffer IntBuffer]]. */
  def wrap(array: Int32Array): IntBuffer =
    TypedArrayBufferBridge.wrap(array)

  /** Wraps a [[Float32Array]] in a direct [[java.nio.FloatBuffer FloatBuffer]]. */
  def wrap(array: Float32Array): FloatBuffer =
    TypedArrayBufferBridge.wrap(array)

  /** Wraps a [[Float64Array]] in a direct [[java.nio.DoubleBuffer DoubleBuffer]]. */
  def wrap(array: Float64Array): DoubleBuffer =
    TypedArrayBufferBridge.wrap(array)
}
