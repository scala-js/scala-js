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

/**
 * All doc-comments marked as "MDN" are by Mozilla Contributors,
 * distributed under the Creative Commons Attribution-ShareAlike license from
 * https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js

import scala.language.implicitConversions

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Discouraged native JavaScript Array methods.
 *
 *  In general, you should prefer the Scala collection methods available
 *  implicitly through [[ArrayOps]], because they are inlineable, and hence
 *  faster.
 *
 *  To enable the use of these functions on js.[[Array]]s, import the implicit
 *  conversion [[JSArrayOps.jsArrayOps]].
 */
@deprecated(
    "It almost never makes sense to call native JavaScript Array functions " +
    "from Scala.js. Use the normal Scala collection methods instead. " +
    "If this is really what you want, use js.Dynamic or write your own " +
    "facade type.",
    "0.6.25")
@native
trait JSArrayOps[A] extends js.Object {

  /**
   * The indexOf() method returns the first index at which a given element can
   * be found in the array, or -1 if it is not present.
   *
   * MDN
   */
  @JSName("indexOf")
  def jsIndexOf(searchElement: A, fromIndex: Int): Int
  @JSName("indexOf")
  def jsIndexOf(searchElement: A): Int

  /**
   * The lastIndexOf() method returns the last index at which a given element
   * can be found in the array, or -1 if it is not present. The array is
   * searched backwards, starting at fromIndex.
   *
   * MDN
   */
  @JSName("lastIndexOf")
  def jsLastIndexOf(searchElement: A, fromIndex: Int): Int
  @JSName("lastIndexOf")
  def jsLastIndexOf(searchElement: A): Int

  /**
   * The every method executes the provided callback function once for each
   * element present in the array until it finds one where callback returns
   * a falsy value (a value that becomes false when converted to a Boolean).
   * If such an element is found, the every method immediately returns false.
   * Otherwise, if callback returned a true value for all elements, every
   * will return true. callback is invoked only for indexes of the array
   * which have assigned values; it is not invoked for indexes which have been
   * deleted or which have never been assigned values.
   *
   * callback is invoked with three arguments:
   *
   * - the value of the element
   * - the index of the element
   * - and the Array object being traversed.
   *
   * If a thisObject parameter is provided to every, it will be used as the
   * this for each invocation of the callback. If it is not provided, or is
   * null, the global object associated with callback is used instead.
   *
   * every does not mutate the array on which it is called.
   *
   * every acts like the "for all" quantifier in mathematics. In particular, for
   * an empty array, it returns true. (It is vacuously true that all elements of
   * the empty set satisfy any given condition.)
   *
   * MDN
   */
  @JSName("every")
  def jsEvery[T](callbackfn: js.ThisFunction3[T, A, Int, js.Array[A], Boolean],
      thisArg: T): Boolean
  @JSName("every")
  def jsEvery(callbackfn: js.Function3[A, Int, js.Array[A], Boolean]): Boolean

  /**
   * some executes the callback function once for each element present in the
   * array until it finds one where callback returns a true value. If such an
   * element is found, some immediately returns true. Otherwise, some returns
   * false. callback is invoked only for indexes of the array which have assigned
   * values; it is not invoked for indexes which have been deleted or which
   * have never been assigned values.
   *
   * callback is invoked with three arguments: the value of the element, the index
   * of the element, and the Array object being traversed.
   *
   * If a thisObject parameter is provided to some, it will be used as the this
   * for each invocation of the callback. If it is not provided, or is null,
   * the global object associated with callback is used instead.
   *
   * some does not mutate the array on which it is called.
   *
   * MDN
   */
  @JSName("some")
  def jsSome[T](callbackfn: js.ThisFunction3[T, A, Int, js.Array[A], Boolean],
      thisArg: T): Boolean
  @JSName("some")
  def jsSome(callbackfn: js.Function3[A, Int, js.Array[A], Boolean]): Boolean
  @JSName("some")
  def jsSome(callbackfn: js.Function2[A, Int, Boolean]): Boolean
  @JSName("some")
  def jsSome(callbackfn: js.Function1[A, Boolean]): Boolean

  /**
   * forEach executes the provided callback once for each element of the array
   * with an assigned value. It is not invoked for indexes which have been deleted
   * or which have been initialized to undefined.
   *
   * callback is invoked with three arguments:
   *
   * - the element value
   * - the element index
   * - the array being traversed
   *
   * If a thisArg parameter is provided to forEach, it will be used as the
   * this value for each callback invocation as if callback.call(thisArg,
   * element, index, array) was called. If thisArg is undefined or null,
   * the this value within the function depends on whether the function
   * is in strict mode or not (passed value if in strict mode, global object
   * if in non-strict mode).
   *
   * MDN
   */
  @JSName("forEach")
  def jsForEach[T](callbackfn: js.ThisFunction3[T, A, Int, js.Array[A], _],
      thisArg: T): Unit
  @JSName("forEach")
  def jsForEach(callbackfn: js.Function3[A, Int, js.Array[A], _]): Unit
  @JSName("forEach")
  def jsForEach(callbackfn: js.Function2[A, Int, _]): Unit
  @JSName("forEach")
  def jsForEach(callbackfn: js.Function1[A, _]): Unit

  /**
   * map calls a provided callback function once for each element in an array,
   * in order, and constructs a new array from the results. callback is
   * invoked only for indexes of the array which have assigned values; it is
   * not invoked for indexes which have been deleted or which have never been
   * assigned values.
   *
   * callback is invoked with three arguments: the value of the element, the
   * index of the element, and the Array object being traversed.
   *
   * If a thisArg parameter is provided to map, it will be used as the this for
   * each invocation of the callback. If it is not provided, or is null, the
   * global object associated with callback is used instead.
   *
   * map does not mutate the array on which it is called.
   *
   * MDN
   */
  @JSName("map")
  def jsMap[B, T](callbackfn: js.ThisFunction3[T, A, Int, js.Array[A], B],
      thisArg: T): js.Array[B]
  @JSName("map")
  def jsMap[B](callbackfn: js.Function3[A, Int, js.Array[A], B]): js.Array[B]
  @JSName("map")
  def jsMap[B](callbackfn: js.Function2[A, Int, B]): js.Array[B]
  @JSName("map")
  def jsMap[B](callbackfn: js.Function1[A, B]): js.Array[B]

  /**
   * filter calls a provided callback function once for each element in an array,
   * and constructs a new array of all the values for which callback returns a true
   * value. callback is invoked only for indexes of the array which have assigned
   * values; it is not invoked for indexes which have been deleted or which have
   * never been assigned values. Array elements which do not pass the callback
   * test are simply skipped, and are not included in the new array.
   *
   * callback is invoked with three arguments:
   *
   * - the value of the element
   * - the index of the element
   * - the Array object being traversed
   *
   * If a thisObject parameter is provided to filter, it will be used as the this
   * for each invocation of the callback. If it is not provided, or is null, the
   * global object associated with callback is used instead.
   *
   * filter does not mutate the array on which it is called.
   *
   * MDN
   */
  @JSName("filter")
  def jsFilter[T](callbackfn: js.ThisFunction3[T, A, Int, js.Array[A], Boolean],
      thisArg: T): js.Array[A]
  @JSName("filter")
  def jsFilter(callbackfn: js.Function3[A, Int, js.Array[A], Boolean]): js.Array[A]
  @JSName("filter")
  def jsFilter(callbackfn: js.Function2[A, Int, Boolean]): js.Array[A]
  @JSName("filter")
  def jsFilter(callbackfn: js.Function1[A, Boolean]): js.Array[A]

  /**
   * reduce executes the callback function once for each element present in
   * the array, excluding holes in the array, receiving four arguments: the
   * initial value (or value from the previous callback call), the value of
   * the current element, the current index, and the array over which
   * iteration is occurring.
   *
   * The first time the callback is called, previousValue and currentValue can
   * be one of two values. If initialValue is provided in the call to reduce,
   * then previousValue will be equal to initialValue and currentValue will be
   * equal to the first value in the array. If no initialValue was provided,
   * then previousValue will be equal to the first value in the array and
   * currentValue will be equal to the second.
   *
   * MDN
   */
  @JSName("reduce")
  def jsReduce[B](callbackfn: js.Function4[B, A, Int, js.Array[A], B],
      initialValue: B): B
  @JSName("reduce")
  def jsReduce[B](callbackfn: js.Function3[B, A, Int, B], initialValue: B): B
  @JSName("reduce")
  def jsReduce[B](callbackfn: js.Function2[B, A, B], initialValue: B): B
  @JSName("reduce")
  def jsReduce[B](callbackfn: js.Function4[B, A, Int, js.Array[A], B]): B
  @JSName("reduce")
  def jsReduce[B](callbackfn: js.Function3[B, A, Int, B]): B
  @JSName("reduce")
  def jsReduce[B](callbackfn: js.Function2[B, A, B]): B

  /**
   * reduceRight executes the callback function once for each element present
   * in the array, excluding holes in the array, receiving four arguments:
   * the initial value (or value from the previous callback call), the value
   * of the current element, the current index, and the array over which
   * iteration is occurring.
   *
   * MDN
   */
  @JSName("reduceRight")
  def jsReduceRight[B](callbackfn: js.Function4[B, A, Int, js.Array[A], B],
      initialValue: B): B
  @JSName("reduceRight")
  def jsReduceRight[B](callbackfn: js.Function3[B, A, Int, B],
      initialValue: B): B
  @JSName("reduceRight")
  def jsReduceRight[B](callbackfn: js.Function2[B, A, B], initialValue: B): B
  @JSName("reduceRight")
  def jsReduceRight[B](callbackfn: js.Function4[B, A, Int, js.Array[A], B]): B
  @JSName("reduceRight")
  def jsReduceRight[B](callbackfn: js.Function3[B, A, Int, B]): B
  @JSName("reduceRight")
  def jsReduceRight[B](callbackfn: js.Function2[B, A, B]): B

}

@deprecated(
    "It almost never makes sense to call native JavaScript Array functions " +
    "from Scala.js. Use the normal Scala collection methods instead. " +
    "If this is really what you want, use js.Dynamic or write your own " +
    "facade type.",
    "0.6.25")
object JSArrayOps {
  @inline implicit def jsArrayOps[A](array: js.Array[A]): js.JSArrayOps[A] =
    array.asInstanceOf[js.JSArrayOps[A]]
}
