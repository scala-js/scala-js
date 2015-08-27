/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


/**
 * All doc-comments marked as "MDN" are by Mozilla Contributors,
 * distributed under the Creative Commons Attribution-ShareAlike license from
 * https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js

import scala.language.implicitConversions

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
@native
trait JSArrayOps[A] extends Object {

  /**
   * The indexOf() method returns the first index at which a given element can
   * be found in the array, or -1 if it is not present.
   *
   * MDN
   */
  @JSName("indexOf")
  def jsIndexOf(searchElement: A, fromIndex: Int): Int = native
  @JSName("indexOf")
  def jsIndexOf(searchElement: A): Int = native

  /**
   * The lastIndexOf() method returns the last index at which a given element
   * can be found in the array, or -1 if it is not present. The array is
   * searched backwards, starting at fromIndex.
   *
   * MDN
   */
  @JSName("lastIndexOf")
  def jsLastIndexOf(searchElement: A, fromIndex: Int): Int = native
  @JSName("lastIndexOf")
  def jsLastIndexOf(searchElement: A): Int = native

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
  def jsEvery[T](callbackfn: ThisFunction3[T, A, Int, Array[A], Boolean],
      thisArg: T): Boolean = native
  @JSName("every")
  def jsEvery(callbackfn: Function3[A, Int, Array[A], Boolean]): Boolean = native

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
  def jsSome[T](callbackfn: ThisFunction3[T, A, Int, Array[A], Boolean],
      thisArg: T): Boolean = native
  @JSName("some")
  def jsSome(callbackfn: Function3[A, Int, Array[A], Boolean]): Boolean = native
  @JSName("some")
  def jsSome(callbackfn: Function2[A, Int, Boolean]): Boolean = native
  @JSName("some")
  def jsSome(callbackfn: Function1[A, Boolean]): Boolean = native

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
  def jsForEach[T](callbackfn: ThisFunction3[T, A, Int, Array[A], _],
      thisArg: T): Unit = native
  @JSName("forEach")
  def jsForEach(callbackfn: Function3[A, Int, Array[A], _]): Unit = native
  @JSName("forEach")
  def jsForEach(callbackfn: Function2[A, Int, _]): Unit = native
  @JSName("forEach")
  def jsForEach(callbackfn: Function1[A, _]): Unit = native

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
  def jsMap[B, T](callbackfn: ThisFunction3[T, A, Int, Array[A], B],
      thisArg: T): Array[B] = native
  @JSName("map")
  def jsMap[B](callbackfn: Function3[A, Int, Array[A], B]): Array[B] = native
  @JSName("map")
  def jsMap[B](callbackfn: Function2[A, Int, B]): Array[B] = native
  @JSName("map")
  def jsMap[B](callbackfn: Function1[A, B]): Array[B] = native

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
  def jsFilter[T](callbackfn: ThisFunction3[T, A, Int, Array[A], Boolean],
      thisArg: T): Array[A] = native
  @JSName("filter")
  def jsFilter(callbackfn: Function3[A, Int, Array[A], Boolean]): Array[A] = native
  @JSName("filter")
  def jsFilter(callbackfn: Function2[A, Int, Boolean]): Array[A] = native
  @JSName("filter")
  def jsFilter(callbackfn: Function1[A, Boolean]): Array[A] = native

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
  def jsReduce[B](callbackfn: Function4[B, A, Int, Array[A], B], initialValue: B): B = native
  @JSName("reduce")
  def jsReduce[B](callbackfn: Function3[B, A, Int, B], initialValue: B): B = native
  @JSName("reduce")
  def jsReduce[B](callbackfn: Function2[B, A, B], initialValue: B): B = native
  @JSName("reduce")
  def jsReduce[B](callbackfn: Function4[B, A, Int, Array[A], B]): B = native
  @JSName("reduce")
  def jsReduce[B](callbackfn: Function3[B, A, Int, B]): B = native
  @JSName("reduce")
  def jsReduce[B](callbackfn: Function2[B, A, B]): B = native

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
  def jsReduceRight[B](callbackfn: Function4[B, A, Int, Array[A], B], initialValue: B): B = native
  @JSName("reduceRight")
  def jsReduceRight[B](callbackfn: Function3[B, A, Int, B], initialValue: B): B = native
  @JSName("reduceRight")
  def jsReduceRight[B](callbackfn: Function2[B, A, B], initialValue: B): B = native
  @JSName("reduceRight")
  def jsReduceRight[B](callbackfn: Function4[B, A, Int, Array[A], B]): B = native
  @JSName("reduceRight")
  def jsReduceRight[B](callbackfn: Function3[B, A, Int, B]): B = native
  @JSName("reduceRight")
  def jsReduceRight[B](callbackfn: Function2[B, A, B]): B = native

}

object JSArrayOps {
  @inline implicit def jsArrayOps[A](array: Array[A]): JSArrayOps[A] =
    array.asInstanceOf[JSArrayOps[A]]
}
