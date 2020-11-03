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

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSBracketAccess, JSName}

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  A TypedArray allows to view an [[ArrayBuffer]] as an array of values of a
 *  particular numeric type.
 */
@js.native
trait TypedArray[T, Repr] extends ArrayBufferView with js.Iterable[T] {

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

  @JSName(js.Symbol.iterator)
  def jsIterator(): js.Iterator[T] = js.native

  /** Set element at index */
  @JSBracketAccess
  def set(index: Int, value: T): Unit = js.native

  /** Set the values of typedArray in this TypedArray */
  def set(typedArray: Repr): Unit = js.native

  /** Set the values of typedArray in this TypedArray at given offset */
  def set(typedArray: Repr, offset: Int): Unit = js.native

  /** Set the values from array in this TypedArray */
  def set(array: js.Array[_ <: T]): Unit = js.native

  /** Set the values from array in this TypedArray at given offset */
  def set(array: js.Array[_ <: T], offset: Int): Unit = js.native

  /** Create a new TypedArray view of this TypedArray at given location */
  def subarray(begin: Int, end: Int = ???): Repr = js.native

}

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  Static information that exists for any concrete TypedArray
 */
@js.native
trait TypedArrayStatic[T, Repr] extends js.Object {
  val BYTES_PER_ELEMENT: Int = js.native

  /** Returns a new array from a set of elements. */
  def of(items: T*): Repr = js.native

  /** Creates an array from an `iterable` object. */
  def from(iterable: js.Iterable[T]): Repr = js.native

  /** Creates an array from an `iterable` object. */
  def from[E](iterable: js.Iterable[E], mapFn: js.Function1[E, T]): Repr = js.native

  /** Creates an array from an `iterable` object. */
  def from[D, E](iterable: js.Iterable[E], mapFn: js.ThisFunction1[D, E, T],
      thisArg: D): Repr = js.native
}
