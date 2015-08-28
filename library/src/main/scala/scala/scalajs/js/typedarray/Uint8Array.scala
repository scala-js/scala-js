package scala.scalajs.js.typedarray

import scala.scalajs.js

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  A [[TypedArray]] of unsigned 8-bit integers
 */
@js.native
class Uint8Array private extends TypedArray[Short, Uint8Array] {

  /** Constructs a Uint8Array with the given length. Initialized to all 0 */
  def this(length: Int) = this()

  /** Creates a new Uint8Array with the same elements than the given TypedArray
   *
   *  The elements are converted before being stored in the new Int8Array.
   */
  def this(typedArray: TypedArray[_, _]) = this()

  /** Creates a new Uint8Array with the elements in the given array */
  def this(array: js.Array[_]) = this()

  /** Creates a Uint8Array view on the given ArrayBuffer */
  def this(buffer: ArrayBuffer, byteOffset: Int = 0, length: Int = ???) = this()

}

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  [[Uint8Array]] companion
 */
@js.native
object Uint8Array extends TypedArrayStatic
