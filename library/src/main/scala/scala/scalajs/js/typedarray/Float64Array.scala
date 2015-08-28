package scala.scalajs.js.typedarray

import scala.scalajs.js

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  A [[TypedArray]] of double precision floats
 */
@js.native
class Float64Array private extends TypedArray[Double, Float64Array] {

  /** Constructs a Float64Array with the given length. Initialized to all 0 */
  def this(length: Int) = this()

  /** Creates a new Float64Array with the same elements than the given TypedArray
   *
   *  The elements are converted before being stored in the new Int8Array.
   */
  def this(typedArray: TypedArray[_, _]) = this()

  /** Creates a new Float64Array with the elements in the given array */
  def this(array: js.Array[_]) = this()

  /** Creates a Float64Array view on the given ArrayBuffer */
  def this(buffer: ArrayBuffer, byteOffset: Int = 0, length: Int = ???) = this()

}

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  [[Float64Array]] companion
 */
@js.native
object Float64Array extends TypedArrayStatic
