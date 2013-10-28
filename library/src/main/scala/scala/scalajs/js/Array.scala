/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.scalajs.js

import annotation.JSBracketAccess

/** JavaScript array
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
  def this(arrayLength: Number) = this()

  // Do not expose this one - use js.Array(item1, item2, ...) instead
  // def this(items: A*) = this()

  /** Length of the array. */
  def length: Number = ???

  /** Access the element at the given index. */
  @JSBracketAccess
  def apply(index: Number): A = ???
  /** Set the element at the given index. */
  @JSBracketAccess
  def update(index: Number, value: A): Unit = ???

  def concat(items: Array[A]*): Array[A] = ???
  def concat(item: A, items: A*): Array[A] = ???
  def join(seperator: String): String = ???
  def join(): String = ???
  def pop(): A = ???
  def push(items: A*): Number = ???
  def reverse(): Array[A] = ???
  def shift(): A = ???
  def slice(start: Number, end: Number): Array[A] = ???
  def slice(start: Number): Array[A] = ???
  def sort(compareFn: Function2[A, A, Number]): Array[A] = ???
  def sort(): Array[A] = ???
  def splice(start: Number): Array[A] = ???
  def splice(start: Number, deleteCount: Number, items: A*): Array[A] = ???
  def unshift(items: A*): Number = ???
  def indexOf(searchElement: A, fromIndex: Number): Number = ???
  def indexOf(searchElement: A): Number = ???
  def lastIndexOf(searchElement: A, fromIndex: Number): Number = ???
  def lastIndexOf(searchElement: A): Number = ???

  def every(callbackfn: Function3[A, Number, Array[A], Boolean], thisArg: Any): Boolean = ???
  def every(callbackfn: Function3[A, Number, Array[A], Boolean]): Boolean = ???
  def some(callbackfn: Function3[A, Number, Array[A], Boolean], thisArg: Any): Boolean = ???
  def some(callbackfn: Function3[A, Number, Array[A], Boolean]): Boolean = ???
  def forEach[U](callbackfn: Function3[A, Number, Array[A], U], thisArg: Any): Unit = ???
  def forEach[U](callbackfn: Function3[A, Number, Array[A], U]): Unit = ???
  def map[B](callbackfn: Function3[A, Number, Array[A], B], thisArg: Any): Array[B] = ???
  def map[B](callbackfn: Function3[A, Number, Array[A], B]): Array[B] = ???
  def filter(callbackfn: Function3[A, Number, Array[A], Boolean], thisArg: Any): Array[A] = ???
  def filter(callbackfn: Function3[A, Number, Array[A], Boolean]): Array[A] = ???
  def reduce[B](callbackfn: Function4[B, A, Number, Array[A], B], initialValue: B): B = ???
  def reduce[B](callbackfn: Function4[B, A, Number, Array[A], B]): B = ???
  def reduceRight[B](callbackfn: Function4[B, A, Number, Array[A], B], initialValue: B): B = ???
  def reduceRight[B](callbackfn: Function4[B, A, Number, Array[A], B]): B = ???
}

/** Factory for [[js.Array]] objects. */
object Array extends Object {
  // Do not expose this one - use new Array(len) instead
  // def apply[A](arrayLength: Number): Array[A] = ???

  /** Creates a new array with the given items. */
  def apply[A](items: A*): Array[A] = sys.error("stub")

  /** Returns true if the given value is an array. */
  def isArray(arg: Any): Boolean = ???
}
