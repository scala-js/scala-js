/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.js

class Array[A] protected () extends Object {
  def this(arrayLength: Number) = this()

  // Do not expose this one - use js.Array(item1, item2, ...) instead
  // def this(items: A*) = this()

  val length: Number = sys.error("stub")

  def apply(index: Number): A = sys.error("stub")
  def update(index: Number, value: A): Unit = sys.error("stub")

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

object Array extends Object {
  // Do not expose this one - use new Array(len) instead
  // def apply[A](arrayLength: Number): Array[A] = ???

  def apply[A](items: A*): Array[A] = sys.error("stub")

  def isArray(arg: Any): Boolean = ???
}
