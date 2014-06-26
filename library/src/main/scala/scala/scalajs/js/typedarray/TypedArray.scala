package scala.scalajs.js.typedarray

import scala.scalajs.js
import scala.scalajs.js.annotation.JSBracketAccess

trait TypedArray[T, Repr] extends ArrayBufferView {

  /** The number of elements in this TypedArray */
  val length: Int = ???

  /** Retrieve element at index */
  @JSBracketAccess
  def apply(index: Int): T = ???

  /** Set element at index */
  @JSBracketAccess
  def update(index: Int, value: T): Unit = ???

  /** Retrieve element at index */
  @JSBracketAccess
  def get(index: Int): T = ???

  /** Set element at index */
  @JSBracketAccess
  def set(index: Int, value: T): Unit = ???

  /** Set the values of typedArray in this TypedArray */
  def set(typedArray: TypedArray[_, _]): Unit = ???

  /** Set the values of typedArray in this TypedArray at given offset */
  def set(typedArray: TypedArray[_, _], offset: Int): Unit = ???

  /** Set the values from array in this TypedArray */
  def set(array: js.Array[_]): Unit = ???

  /** Set the values from array in this TypedArray at given offset */
  def set(array: js.Array[_], offset: Int): Unit = ???

  /** Create a new TypedArray view of this TypedArray at given location */
  def subarray(begin: Int, end: Int = ???): Repr = ???

}

trait TypedArrayStatic extends js.Object {
  val BYTES_PER_ELEMENT: Int = ???
}
