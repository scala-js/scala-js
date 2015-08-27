package scala.scalajs.js.typedarray

import scala.scalajs.js
import scala.scalajs.js.annotation.JSBracketAccess

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  A TypedArray allows to view an [[ArrayBuffer]] as an array of values of a
 *  particular numeric type.
 */
@js.native
trait TypedArray[T, Repr] extends ArrayBufferView {

  /** The number of elements in this TypedArray */
  val length: Int = js.native

  /** Retrieve element at index */
  @JSBracketAccess
  def apply(index: Int): T = js.native

  /** Set element at index */
  @JSBracketAccess
  def update(index: Int, value: T): Unit = js.native

  /** Retrieve element at index */
  @JSBracketAccess
  def get(index: Int): T = js.native

  /** Set element at index */
  @JSBracketAccess
  def set(index: Int, value: T): Unit = js.native

  /** Set the values of typedArray in this TypedArray */
  def set(typedArray: TypedArray[_, _]): Unit = js.native

  /** Set the values of typedArray in this TypedArray at given offset */
  def set(typedArray: TypedArray[_, _], offset: Int): Unit = js.native

  /** Set the values from array in this TypedArray */
  def set(array: js.Array[_]): Unit = js.native

  /** Set the values from array in this TypedArray at given offset */
  def set(array: js.Array[_], offset: Int): Unit = js.native

  /** Create a new TypedArray view of this TypedArray at given location */
  def subarray(begin: Int, end: Int = ???): Repr = js.native

}

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  Static information that exists for any concrete TypedArray
 */
@js.native
trait TypedArrayStatic extends js.Object {
  val BYTES_PER_ELEMENT: Int = js.native
}
