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

import annotation._

/**
 *  Arrays are list-like objects whose prototype has methods to perform
 *  traversal and mutation operations. Neither the length of a JavaScript
 *  array nor the types of its elements are fixed. Since an array's size
 *  length grow or shrink at any time, JavaScript arrays are not guaranteed
 *  to be dense. In general, these are convenient characteristics; but if
 *  these features are not desirable for your particular use, you might
 *  consider using typed arrays.
 *
 *  MDN
 *
 *  To construct a new array with uninitialized elements, use the constructor
 *  of this class. To construct a new array with specified elements, as if
 *  you used the array literal syntax in JavaScript, use the
 *  [[Array$.apply Array.apply]] method instead.
 *
 *  @tparam A Type of the elements of the array
 *
 *  @constructor Creates a new array of length 0.
 */
class Array[A] extends Object {
  /** Creates a new array with the given length.
   *  @param arrayLength Initial length of the array.
   */
  def this(arrayLength: Int) = this()

  // Do not expose this one - use js.Array(item1, item2, ...) instead
  // def this(items: A*) = this()

  /** Length of the array. */
  def length: Int = ???

  /** Access the element at the given index. */
  @JSBracketAccess
  def apply(index: Int): A = ???
  /** Set the element at the given index. */
  @JSBracketAccess
  def update(index: Int, value: A): Unit = ???

  /**
   * concat creates a new array consisting of the elements in the this object
   * on which it is called, followed in order by, for each argument, the
   * elements of that argument (if the argument is an array) or the argument
   * itself (if the argument is not an array).
   *
   * MDN
   */
  def concat[B >: A](items: Array[_ <: B]*): Array[B] = ???

  /**
   * The join() method joins all elements of an array into a string.
   *
   * separator Specifies a string to separate each element of the array.
   * The separator is converted to a string if necessary. If omitted, the
   * array elements are separated with a comma.
   */
  def join(seperator: String = ","): String = ???

  /**
   * The pop() method removes the last element from an array and returns that
   * element.
   *
   * MDN
   */
  def pop(): A = ???

  /**
   * The push() method mutates an array by appending the given elements and
   * returning the new length of the array.
   *
   * MDN
   */
  def push(items: A*): Int = ???

  /**
   * The reverse() method reverses an array in place. The first array element
   * becomes the last and the last becomes the first.
   *
   * MDN
   */
  def reverse(): Array[A] = ???

  /**
   * The shift() method removes the first element from an array and returns that
   * element. This method changes the length of the array.
   *
   * MDN
   */
  def shift(): A = ???

  /**
   * The slice() method returns a shallow copy of a portion of an array.
   *
   * MDN
   */
  def slice(start: Int = 0, end: Int = Int.MaxValue): Array[A] = ???

  /**
   * The sort() method sorts the elements of an array in place and returns the
   * array. The sort is not necessarily stable. The default sort order is
   * lexicographic (not numeric).
   *
   * If compareFunction is not supplied, elements are sorted by converting them
   * to strings and comparing strings in lexicographic ("dictionary" or "telephone
   * book," not numerical) order. For example, "80" comes before "9" in
   * lexicographic order, but in a numeric sort 9 comes before 80.
   *
   * MDN
   */
  def sort(compareFn: Function2[A, A, Int] = ???): Array[A] = ???

  /**
   * The splice() method changes the content of an array, adding new elements
   * while removing old elements.
   *
   * MDN
   */
  def splice(index: Int): Array[A] = ???
  /**
   * The splice() method changes the content of an array, adding new elements
   * while removing old elements.
   *
   * MDN
   */
  def splice(index: Int, deleteCount: Int, items: A*): Array[A] = ???

  /**
   * The unshift() method adds one or more elements to the beginning of an array
   * and returns the new length of the array.
   *
   * MDN
   */
  def unshift(items: A*): Int = ???

  /**
   * The indexOf() method returns the first index at which a given element can
   * be found in the array, or -1 if it is not present.
   *
   * MDN
   */
  def indexOf(searchElement: A, fromIndex: Int): Int = ???
  def indexOf(searchElement: A): Int = ???

  /**
   * The lastIndexOf() method returns the last index at which a given element
   * can be found in the array, or -1 if it is not present. The array is
   * searched backwards, starting at fromIndex.
   *
   * MDN
   */
  def lastIndexOf(searchElement: A, fromIndex: Int): Int = ???
  def lastIndexOf(searchElement: A): Int = ???

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
  def every[T](callbackfn: ThisFunction3[T, A, Int, Array[A], Boolean],
      thisArg: T): Boolean = ???
  def every(callbackfn: Function3[A, Int, Array[A], Boolean]): Boolean = ???

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
  def some[T](callbackfn: ThisFunction3[T, A, Int, Array[A], Boolean],
      thisArg: T): Boolean = ???
  def some(callbackfn: Function3[A, Int, Array[A], Boolean]): Boolean = ???
  def some(callbackfn: Function2[A, Int, Boolean]): Boolean = ???
  def some(callbackfn: Function1[A, Boolean]): Boolean = ???

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
  def forEach[T](callbackfn: ThisFunction3[T, A, Int, Array[A], _],
      thisArg: T): Unit = ???
  def forEach(callbackfn: Function3[A, Int, Array[A], _]): Unit = ???
  def forEach(callbackfn: Function2[A, Int, _]): Unit = ???
  def forEach(callbackfn: Function1[A, _]): Unit = ???

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
  def map[B, T](callbackfn: ThisFunction3[T, A, Int, Array[A], B],
      thisArg: T): Array[B] = ???
  def map[B](callbackfn: Function3[A, Int, Array[A], B]): Array[B] = ???
  def map[B](callbackfn: Function2[A, Int, B]): Array[B] = ???
  def map[B](callbackfn: Function1[A, B]): Array[B] = ???

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
  def filter[T](callbackfn: ThisFunction3[T, A, Int, Array[A], Boolean],
      thisArg: T): Array[A] = ???
  def filter(callbackfn: Function3[A, Int, Array[A], Boolean]): Array[A] = ???
  def filter(callbackfn: Function2[A, Int, Boolean]): Array[A] = ???
  def filter(callbackfn: Function1[A, Boolean]): Array[A] = ???

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
  def reduce[B](callbackfn: Function4[B, A, Int, Array[A], B], initialValue: B): B = ???
  def reduce[B](callbackfn: Function3[B, A, Int, B], initialValue: B): B = ???
  def reduce[B](callbackfn: Function2[B, A, B], initialValue: B): B = ???
  def reduce[B](callbackfn: Function4[B, A, Int, Array[A], B]): B = ???
  def reduce[B](callbackfn: Function3[B, A, Int, B]): B = ???
  def reduce[B](callbackfn: Function2[B, A, B]): B = ???

  /**
   * reduceRight executes the callback function once for each element present
   * in the array, excluding holes in the array, receiving four arguments:
   * the initial value (or value from the previous callback call), the value
   * of the current element, the current index, and the array over which
   * iteration is occurring.
   *
   * MDN
   */
  def reduceRight[B](callbackfn: Function4[B, A, Int, Array[A], B], initialValue: B): B = ???
  def reduceRight[B](callbackfn: Function3[B, A, Int, B], initialValue: B): B = ???
  def reduceRight[B](callbackfn: Function2[B, A, B], initialValue: B): B = ???
  def reduceRight[B](callbackfn: Function4[B, A, Int, Array[A], B]): B = ???
  def reduceRight[B](callbackfn: Function3[B, A, Int, B]): B = ???
  def reduceRight[B](callbackfn: Function2[B, A, B]): B = ???
}

/** Factory for [[js.Array]] objects. */
object Array extends Object {
  // Do not expose this one - use new Array(len) instead
  // def apply[A](arrayLength: Int): Array[A] = ???

  /** Creates a new array with the given items. */
  def apply[A](items: A*): Array[A] = sys.error("stub")

  /** Returns true if the given value is an array. */
  def isArray(arg: Any): Boolean = ???
}
