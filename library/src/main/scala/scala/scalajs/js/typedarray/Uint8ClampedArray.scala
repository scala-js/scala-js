package scala.scalajs.js.typedarray

import scala.scalajs.js

class Uint8ClampedArray private extends TypedArray[Int, Uint8ClampedArray] {

  /** Constructs a Uint8ClampedArray with the given length. Initialized to all 0 */
  def this(length: Int) = this()

  /** Creates a new Uint8ClampedArray with the same elements than the given TypedArray
   *
   *  The elements are converted before being stored in the new Int8Array.
   */
  def this(typedArray: TypedArray[_, _]) = this()

  /** Creates a new Uint8ClampedArray with the elements in the given array */
  def this(array: js.Array[_]) = this()

  /** Creates a Uint8ClampedArray view on the given ArrayBuffer */
  def this(buffer: ArrayBuffer, byteOffset: Int = 0, length: Int = ???) = this()

}

object Uint8ClampedArray extends TypedArrayStatic
