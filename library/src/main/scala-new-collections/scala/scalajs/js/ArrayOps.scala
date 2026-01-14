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

package scala.scalajs.js

import scala.annotation.tailrec

import scala.collection.{immutable, mutable}
import scala.collection.{AbstractIndexedSeqView, AbstractIterator, IndexedSeqView, IterableOnce}

import scala.reflect.ClassTag

import java.lang.Math.{max, min}

import scala.scalajs.js

/** Equivalent of `scala.collection.js.ArrayOps` for `js.Array`. */
@inline
final class ArrayOps[A](private val xs: js.Array[A]) extends AnyVal {
  import ArrayOps._

  // Seq[A]

  /** The size of this array.
   *
   *  @return
   *    the number of elements in this array.
   */
  @inline def size: Int = xs.length

  /** The size of this array.
   *
   *  @return the number of elements in this array.
   */
  @inline def knownSize: Int = xs.length

  /** Tests whether the array is empty.
   *
   *  @return `true` if the array contains no elements, `false` otherwise.
   */
  @inline def isEmpty: Boolean = xs.length == 0

  /** Tests whether the array is not empty.
   *
   *  @return
   *    `true` if the array contains at least one element, `false` otherwise.
   */
  @inline def nonEmpty: Boolean = xs.length != 0

  /** Selects the first element of this array.
   *
   *  @return the first element of this array.
   *
   *  @throws NoSuchElementException
   *    if the array is empty.
   */
  def head: A = {
    if (isEmpty)
      throw new NoSuchElementException("head of empty array")
    xs.apply(0)
  }

  /** Selects the last element of this array.
   *
   *  @return the last element of this array.
   *
   *  @throws NoSuchElementException
   *    if the array is empty.
   */
  def last: A = {
    if (isEmpty)
      throw new NoSuchElementException("last of empty array")
    xs.apply(xs.length - 1)
  }

  /** Optionally selects the first element.
   *
   *  @return
   *    the first element of this array if it is nonempty, `None` if it is
   *    empty.
   */
  def headOption: Option[A] =
    if (isEmpty) None
    else Some(xs.apply(0))

  /** Optionally selects the last element.
   *
   *  @return
   *    the last element of this array$ if it is nonempty, `None` if it is
   *    empty.
   */
  def lastOption: Option[A] =
    if (isEmpty) None
    else Some(xs.apply(xs.length - 1))

  /** Compares the size of this array to a test value.
   *
   *  @param otherSize
   *    the test value that gets compared with the size.
   *  @return
   *    A value `x` where
   *    {{{
   *    x <  0       if this.size <  otherSize
   *    x == 0       if this.size == otherSize
   *    x >  0       if this.size >  otherSize
   *    }}}
   */
  @inline def sizeCompare(otherSize: Int): Int =
    Integer.compare(xs.length, otherSize)

  /** Compares the length of this array to a test value.
   *
   *  @param len
   *    the test value that gets compared with the length.
   *
   *  @return
   *    A value `x` where
   *    {{{
   *    x <  0       if this.length <  len
   *    x == 0       if this.length == len
   *    x >  0       if this.length >  len
   *    }}}
   */
  @inline def lengthCompare(len: Int): Int =
    Integer.compare(xs.length, len)

  /** Method mirroring
   *  [[scala.collection.IterableOps.sizeIs IterableOps.sizeIs]] for
   *  consistency, except it returns an `Int` because `size` is known and
   *  comparison is constant-time.
   *
   *  These operations are equivalent to
   *  [[sizeCompare `sizeCompare(Int)`]], and allow the following more
   *  readable usages:
   *
   *  {{{
   *  this.sizeIs < size     // this.sizeCompare(size) < 0
   *  this.sizeIs <= size    // this.sizeCompare(size) <= 0
   *  this.sizeIs == size    // this.sizeCompare(size) == 0
   *  this.sizeIs != size    // this.sizeCompare(size) != 0
   *  this.sizeIs >= size    // this.sizeCompare(size) >= 0
   *  this.sizeIs > size     // this.sizeCompare(size) > 0
   *  }}}
   */
  def sizeIs: Int = xs.length

  /** Method mirroring [[scala.collection.SeqOps.lengthIs SeqOps.lengthIs]] for
   *  consistency, except it returns an `Int` because `length` is known and
   *  comparison is constant-time.
   *
   *  These operations are equivalent to
   *  [[lengthCompare `lengthCompare(Int)`]], and allow the following more
   *  readable usages:
   *
   *  {{{
   *  this.lengthIs < len     // this.lengthCompare(len) < 0
   *  this.lengthIs <= len    // this.lengthCompare(len) <= 0
   *  this.lengthIs == len    // this.lengthCompare(len) == 0
   *  this.lengthIs != len    // this.lengthCompare(len) != 0
   *  this.lengthIs >= len    // this.lengthCompare(len) >= 0
   *  this.lengthIs > len     // this.lengthCompare(len) > 0
   *  }}}
   */
  def lengthIs: Int = xs.length

  /** Selects an interval of elements.
   *
   *  The returned array is made up of all elements `x` which satisfy the
   *  invariant:
   *  {{{
   *  from <= indexOf(x) < until
   *  }}}
   *
   *  @param from
   *    the lowest index to include from this array.
   *  @param until
   *    the lowest index to EXCLUDE from this array.
   *  @return
   *    an array containing the elements greater than or equal to index `from`
   *    extending up to (but not including) index `until` of this array.
   */
  @inline def slice(from: Int, until: Int): js.Array[A] =
    xs.jsSlice(max(from, 0), max(until, 0))

  /** The rest of the array without its first element. */
  def tail: js.Array[A] = {
    if (isEmpty)
      throw new UnsupportedOperationException("tail of empty array")
    xs.jsSlice(1)
  }

  /** The initial part of the array without its last element. */
  def init: js.Array[A] = {
    if (isEmpty)
      throw new UnsupportedOperationException("init of empty array")
    xs.jsSlice(0, -1)
  }

  /** Iterates over the tails of this array.
   *
   *  The first value will be this array and the final one will be an empty
   *  array, with the intervening values the results of successive applications
   *  of `tail`.
   *
   *  @return an iterator over all the tails of this array
   */
  def tails: scala.collection.Iterator[js.Array[A]] =
    iterateUntilEmpty(_.tail)

  /** Iterates over the inits of this array.
   *
   *  The first value will be this array and the final one will be an empty
   *  array, with the intervening values the results of successive applications
   *  of `init`.
   *
   *  @return an iterator over all the inits of this array
   */
  def inits: scala.collection.Iterator[js.Array[A]] =
    iterateUntilEmpty(_.init)

  // A helper for tails and inits.
  private[this] def iterateUntilEmpty(
      f: js.Array[A] => js.Array[A]): scala.collection.Iterator[js.Array[A]] = {
    scala.collection.Iterator.iterate(xs)(f).takeWhile(_.nonEmpty) ++
    scala.collection.Iterator.single(js.Array[A]())
  }

  /** An array containing the first `n` elements of this array. */
  @inline def take(n: Int): js.Array[A] =
    xs.jsSlice(0, max(n, 0))

  /** The rest of the array without its `n` first elements. */
  @inline def drop(n: Int): js.Array[A] =
    xs.jsSlice(max(n, 0))

  /** An array containing the last `n` elements of this array. */
  def takeRight(n: Int): js.Array[A] =
    if (n <= 0) js.Array[A]()
    else xs.jsSlice(-n)

  /** The rest of the array without its `n` last elements. */
  def dropRight(n: Int): js.Array[A] =
    if (n <= 0) xs.jsSlice()
    else xs.jsSlice(0, -n)

  /** Takes the longest prefix of elements that satisfy a predicate.
   *
   *  @param p
   *    The predicate used to test elements.
   *  @return
   *    the longest prefix of this array whose elements all satisfy the
   *    predicate `p`.
   */
  def takeWhile(p: A => Boolean): js.Array[A] = {
    val i = indexWhere(x => !p(x))
    if (i < 0) xs.jsSlice()
    else xs.jsSlice(0, i)
  }

  /** Drops the longest prefix of elements that satisfy a predicate.
   *
   *  @param p
   *    The predicate used to test elements.
   *  @return
   *    the longest suffix of this array whose first element does not satisfy
   *    the predicate `p`.
   */
  def dropWhile(p: A => Boolean): js.Array[A] = {
    val i = indexWhere(x => !p(x))
    if (i < 0) js.Array()
    else xs.jsSlice(i)
  }

  @inline def iterator: scala.collection.Iterator[A] =
    new js.ArrayOps.ArrayIterator[A](xs)

  /** Partitions elements in fixed size arrays.
   *
   *  @see [[scala.collection.Iterator]], method `grouped`
   *
   *  @param size
   *    the number of elements per group
   *  @return
   *    An iterator producing arrays of size `size`, except the last will be
   *    less than size `size` if the elements don't divide evenly.
   */
  @inline def grouped(size: Int): scala.collection.Iterator[js.Array[A]] =
    new js.ArrayOps.GroupedIterator[A](xs, size)

  /** Splits this array into a prefix/suffix pair according to a predicate.
   *
   *  Note: `c.span(p)` is equivalent to (but more efficient than)
   *  `(c.takeWhile(p), c.dropWhile(p))`, provided the evaluation of the
   *  predicate `p` does not cause any side-effects.
   *
   *  @param p
   *    the test predicate
   *  @return
   *    a pair consisting of the longest prefix of this array whose chars all
   *    satisfy `p`, and the rest of this array.
   */
  def span(p: A => Boolean): (js.Array[A], js.Array[A]) = {
    val i = indexWhere(x => !p(x))
    val idx = if (i < 0) xs.length else i
    (xs.jsSlice(0, idx), xs.jsSlice(idx))
  }

  /** Splits this array into two at a given position.
   *
   *  Note: `c.splitAt(n)` is equivalent to `(c.take(n), c.drop(n))`.
   *
   *  @param n
   *    the position at which to split.
   *  @return
   *    a pair of arrays consisting of the first `n` elements of this array,
   *    and the other elements.
   */
  def splitAt(n: Int): (js.Array[A], js.Array[A]) =
    (take(n), drop(n))

  /** A pair of, first, all elements that satisfy predicate `p` and, second,
   *  all elements that do not.
   */
  def partition(p: A => Boolean): (js.Array[A], js.Array[A]) = {
    val res1 = js.Array[A]()
    val res2 = js.Array[A]()
    for (x <- xs)
      (if (p(x)) res1 else res2).push(x)
    (res1, res2)
  }

  /** Applies a function `f` to each element of the array and returns a pair of
   *  arrays: the first one made of those values returned by `f` that were
   *  wrapped in [[scala.util.Left]], and the second one made of those wrapped
   *  in [[scala.util.Right]].
   *
   *  Example:
   *  {{{
   *  val xs = js.Array(1, "one", 2, "two", 3, "three").partitionMap {
   *    case i: Int    => Left(i)
   *    case s: String => Right(s)
   *  }
   *  // xs == (js.Array(1, 2, 3),
   *  //        js.Array("one", "two", "three"))
   *  }}}
   *
   *  @tparam A1
   *    the element type of the first resulting collection
   *  @tparam A2
   *    the element type of the second resulting collection
   *  @param f
   *    the 'split function' mapping the elements of this array to an [[scala.util.Either]]
   *  @return
   *    a pair of arrays: the first one made of those values returned by `f`
   *    that were wrapped in [[scala.util.Left]],  and the second one made of
   *    those wrapped in [[scala.util.Right]].
   */
  def partitionMap[A1, A2](f: A => Either[A1, A2]): (Array[A1], Array[A2]) = {
    val res1 = js.Array[A1]()
    val res2 = js.Array[A2]()
    for (x <- xs) {
      f(x) match {
        case Left(y)  => res1 += y
        case Right(y) => res2 += y
      }
    }
    (res1, res2)
  }

  /** Returns a new array with the elements in reversed order. */
  def reverse: js.Array[A] = {
    val len = xs.length
    val res = new js.Array[A](len)
    var i = 0
    while (i < len) {
      res(len - i - 1) = xs(i)
      i += 1
    }
    res
  }

  /** An iterator yielding elements in reversed order.
   *
   *  Note: `xs.reverseIterator` is the same as `xs.reverse.iterator` but
   *  implemented more efficiently.
   *
   *  @return
   *    an iterator yielding the elements of this array in reversed order
   */
  def reverseIterator: scala.collection.Iterator[A] =
    new js.ArrayOps.ReverseIterator[A](xs)

  /** Selects all elements of this array which satisfy a predicate.
   *
   *  @param p
   *    the predicate used to test elements.
   *  @return
   *    a new array consisting of all elements of this array that satisfy the
   *    given predicate `p`.
   */
  def filter(p: A => Boolean): js.Array[A] = {
    val res = js.Array[A]()
    for (x <- xs) {
      if (p(x))
        res.push(x)
    }
    res
  }

  /** Selects all elements of this array which do not satisfy a predicate.
   *
   *  @param pred
   *    the predicate used to test elements.
   *  @return
   *    a new array consisting of all elements of this array that do not
   *    satisfy the given predicate `pred`.
   */
  def filterNot(p: A => Boolean): js.Array[A] =
    filter(x => !p(x))

  /** Sorts this array according to an Ordering.
   *
   *  The sort is stable. That is, elements that are equal (as determined by
   *  `lt`) appear in the same order in the sorted sequence as in the original.
   *
   *  @see [[scala.math.Ordering]]
   *
   *  @param ord
   *    the ordering to be used to compare elements.
   *  @return
   *    an array consisting of the elements of this array sorted according to
   *    the ordering `ord`.
   */
  def sorted[B >: A](implicit ord: Ordering[B]): js.Array[A] =
    new js.WrappedArray(xs).sorted(ord.asInstanceOf[Ordering[A]])

  /** Sorts this array according to a comparison function.
   *
   *  The sort is stable. That is, elements that are equal (as determined by
   *  `lt`) appear in the same order in the sorted sequence as in the original.
   *
   *  @param lt
   *    the comparison function which tests whether its first argument precedes
   *    its second argument in the desired ordering.
   *  @return
   *    an array consisting of the elements of this array sorted according to
   *    the comparison function `lt`.
   */
  def sortWith(lt: (A, A) => Boolean): js.Array[A] =
    sorted(Ordering.fromLessThan(lt))

  /** Sorts this array according to the Ordering which results from
   *  transforming an implicitly given Ordering with a transformation function.
   *
   *  @see [[scala.math.Ordering]]
   *
   *  @param f
   *    the transformation function mapping elements to some other domain `B`.
   *  @param ord
   *    the ordering assumed on domain `B`.
   *  @tparam B
   *    the target type of the transformation `f`, and the type where the
   *    ordering `ord` is defined.
   *  @return
   *    an array consisting of the elements of this array sorted according to
   *    the ordering where `x < y` if `ord.lt(f(x), f(y))`.
   */
  def sortBy[B](f: A => B)(implicit ord: Ordering[B]): js.Array[A] =
    sorted(ord.on(f))

  /** Creates a non-strict filter of this array.
   *
   *  Note: the difference between `c.filter(p)` and `c.withFilter(p)` is that
   *  the former creates a new array, whereas the latter only restricts the
   *  domain of subsequent `map`, `flatMap`, `foreach`, and `withFilter`
   *  operations.
   *
   *  @param p
   *    the predicate used to test elements.
   *  @return
   *    an object of class `js.ArrayOps.WithFilter`, which supports `map`,
   *    `flatMap`, `foreach`, and `withFilter` operations. All these operations
   *    apply to those elements of this array which satisfy the predicate `p`.
   */
  def withFilter(p: A => Boolean): js.ArrayOps.WithFilter[A] =
    new js.ArrayOps.WithFilter[A](p, xs)

  /** Finds the index of the first occurrence of some value in this array after
   *  or at some start index.
   *
   *  @param elem
   *    the element value to search for.
   *  @param from
   *    the start index
   *  @return
   *    the index `>= from` of the first element of this array that is equal
   *    (as determined by `==`) to `elem`, or `-1`, if none exists.
   */
  def indexOf(elem: A, from: Int = 0): Int =
    indexWhere(elem == _, from)

  /** Finds the index of the first element satisfying some predicate after or
   *  at some start index.
   *
   *  @param p
   *    the predicate used to test elements.
   *  @param from
   *    the start index
   *  @return
   *    the index `>= from` of the first element of this array that satisfies
   *    the predicate `p`, or `-1`, if none exists.
   */
  def indexWhere(f: A => Boolean, from: Int = 0): Int = {
    // scalastyle:off return
    val len = xs.length
    var i = from
    while (i < len) {
      if (f(xs(i)))
        return i
      i += 1
    }
    -1
    // scalastyle:on return
  }

  /** Finds the index of the last occurrence of some value in this array before
   *  or at a given end index.
   *
   *  @param elem
   *    the element value to search for.
   *  @param end
   *    the end index.
   *  @return
   *    the index `<= end` of the last element of this array that is equal
   *    (as determined by `==`) to `elem`, or `-1`, if none exists.
   */
  def lastIndexOf(elem: A, end: Int = xs.length - 1): Int =
    lastIndexWhere(elem == _, end)

  /** Finds the index of the last element satisfying some predicate before or
   *  at given end index.
   *
   *  @param p
   *    the predicate used to test elements.
   *  @return
   *    the index `<= end` of the last element of this array that satisfies the
   *    predicate `p`, or `-1`, if none exists.
   */
  def lastIndexWhere(p: A => Boolean, end: Int = xs.length - 1): Int = {
    // scalastyle:off return
    var i = min(end, xs.length - 1)
    while (i >= 0) {
      if (p(xs(i)))
        return i
      i -= 1
    }
    -1
    // scalastyle:on return
  }

  /** Finds the first element of the array satisfying a predicate, if any.
   *
   *  @param p
   *    the predicate used to test elements.
   *  @return
   *    an option value containing the first element in the array that
   *    satisfies `p`, or `None` if none exists.
   */
  def find(f: A => Boolean): Option[A] = {
    val idx = indexWhere(f)
    if (idx == -1) None
    else Some(xs(idx))
  }

  /** Tests whether a predicate holds for at least one element of this array.
   *
   *  @param p
   *    the predicate used to test elements.
   *  @return
   *    `true` if the given predicate `p` is satisfied by at least one element
   *    of this array, otherwise `false`
   */
  def exists(f: A => Boolean): Boolean =
    indexWhere(f) >= 0

  /** Tests whether a predicate holds for all elements of this array.
   *
   *  @param p
   *    the predicate used to test elements.
   *  @return
   *    `true` if this array is empty or the given predicate `p` holds for all
   *    elements of this array, otherwise `false`.
   */
  def forall(f: A => Boolean): Boolean = {
    // scalastyle:off return
    val len = xs.length
    var i = 0
    while (i < len) {
      if (!f(xs(i)))
        return false
      i += 1
    }
    true
    // scalastyle:on return
  }

  /** Applies a binary operator to a start value and all elements of this
   *  array, going left to right.
   *
   *  @param z
   *    the start value.
   *  @param op
   *    the binary operator.
   *  @tparam B
   *    the result type of the binary operator.
   *  @return
   *    the result of inserting `op` between consecutive elements of this
   *    array, going left to right with the start value `z` on the left:
   *    {{{
   *    op(...op(z, x_1), x_2, ..., x_n)
   *    }}}
   *    where `x,,1,,, ..., x,,n,,` are the elements of this array. Returns `z`
   *    if this array is empty.
   */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var v = z
    for (x <- xs)
      v = op(v, x)
    v
  }

  /** Produces an array containing cumulative results of applying the binary
   *  operator going left to right.
   *
   *  @param z
   *    the start value.
   *  @param op
   *    the binary operator.
   *  @tparam B
   *    the result type of the binary operator.
   *  @return
   *     array with intermediate values.
   *
   *  Example:
   *  {{{
   *  js.Array(1, 2, 3, 4).scanLeft(0)(_ + _) == js.Array(0, 1, 3, 6, 10)
   *  }}}
   */
  def scanLeft[B](z: B)(op: (B, A) => B): js.Array[B] = {
    val len = xs.length
    var v = z
    var i = 0
    val res = new js.Array[B](len + 1)
    while (i < len) {
      res(i) = v
      v = op(v, xs(i))
      i += 1
    }
    res(i) = v
    res
  }

  /** Computes a prefix scan of the elements of the array.
   *
   *  Note: The neutral element `z` may be applied more than once.
   *
   *  @tparam B
   *    element type of the resulting array
   *  @param z
   *    neutral element for the operator `op`
   *  @param op
   *    the associative operator for the scan
   *  @return
   *    a new array containing the prefix scan of the elements in this array
   */
  def scan[B >: A](z: B)(op: (B, B) => B): js.Array[B] =
    scanLeft(z)(op)

  /** Produces an array containing cumulative results of applying the binary
   *  operator going right to left.
   *
   *  @param z
   *    the start value.
   *  @param op
   *    the binary operator.
   *  @tparam B
   *    the result type of the binary operator.
   *  @return
   *    array with intermediate values.
   *
   *  Example:
   *  {{{
   *  js.Array(4, 3, 2, 1).scanRight(0)(_ + _) == js.Array(10, 6, 3, 1, 0)
   *  }}}
   */
  def scanRight[B](z: B)(op: (A, B) => B): js.Array[B] = {
    val len = xs.length
    var v = z
    var i = len - 1
    val res = new js.Array[B](len + 1)
    res(len) = z
    while (i >= 0) {
      v = op(xs(i), v)
      res(i) = v
      i -= 1
    }
    res
  }

  /** Applies a binary operator to all elements of this array and a start
   *  value, going right to left.
   *
   *  @param z
   *    the start value.
   *  @param op
   *    the binary operator.
   *  @tparam B
   *    the result type of the binary operator.
   *  @return
   *    the result of inserting `op` between consecutive elements of this
   *    array, going right to left with the start value `z` on the right:
   *    {{{
   *    op(x_1, op(x_2, ... op(x_n, z)...))
   *    }}}
   *    where `x,,1,,, ..., x,,n,,` are the elements of this array. Returns `z`
   *    if this array is empty.
   */
  def foldRight[B](z: B)(op: (A, B) => B): B = {
    var v = z
    var i = xs.length - 1
    while (i >= 0) {
      v = op(xs(i), v)
      i -= 1
    }
    v
  }

  /** Folds the elements of this array using the specified associative binary
   *  operator.
   *
   *  @tparam A1
   *    a type parameter for the binary operator, a supertype of `A`.
   *  @param z
   *    a neutral element for the fold operation; may be added to the result an
   *    arbitrary number of times, and must not change the result (e.g., `Nil`
   *    for list concatenation, 0 for addition, or 1 for multiplication).
   *  @param op
   *    a binary operator that must be associative.
   *  @return
   *    the result of applying the fold operator `op` between all the elements,
   *    or `z` if this array is empty.
   */
  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = {
    val len = xs.length
    if (len > 0) {
      var v: A1 = xs(0)
      var i = 1
      while (i < len) {
        v = op(v, xs(i))
        i += 1
      }
      v
    } else {
      z
    }
  }

  /** Builds a new array by applying a function to all elements of this array.
   *
   *  @param f
   *    the function to apply to each element.
   *  @tparam B
   *    the element type of the returned array.
   *  @return
   *    a new aray resulting from applying the given function `f` to each
   *    element of this array and collecting the results.
   */
  def map[B](f: A => B): js.Array[B] = {
    val len = xs.length
    val res = new js.Array[B](len)
    var i = 0
    while (i < len) {
      res(i) = f(xs(i))
      i = i + 1
    }
    res
  }

  def mapInPlace(f: A => A): js.Array[A] = {
    val len = xs.length
    var i = 0
    while (i < len) {
      xs(i) = f(xs(i))
      i = i + 1
    }
    xs
  }

  /** Builds a new array by applying a function to all elements of this array
   *  and using the elements of the resulting collections.
   *
   *  @param f
   *    the function to apply to each element.
   *  @tparam B
   *    the element type of the returned array.
   *  @return
   *    a new array resulting from applying the given collection-valued
   *    function `f` to each element of this array and concatenating the
   *    results.
   */
  def flatMap[B](f: A => IterableOnce[B]): js.Array[B] = {
    val b = js.Array[B]()
    for (x <- xs)
      b ++= f(x)
    b
  }

  def flatMap[BS, B](f: A => BS)(
      implicit asIterable: BS => scala.collection.Iterable[B]): js.Array[B] = {
    flatMap[B](x => asIterable(f(x)))
  }

  /** Flattens a two-dimensional array by concatenating all its rows into a
   *  single array.
   *
   *  @tparam B
   *    Type of row elements.
   *  @param asIterable
   *    A function that converts elements of this array to rows - Iterables of
   *    type `B`.
   *  @return
   *    An array obtained by concatenating rows of this array.
   */
  def flatten[B](
      implicit asIterable: A => scala.collection.Iterable[B]): js.Array[B] = {
    flatMap(identity)
  }

  /** Builds a new array by applying a partial function to all elements of this
   *  array on which the function is defined.
   *
   *  @param pf
   *    the partial function which filters and maps the array.
   *  @tparam B
   *    the element type of the returned array.
   *  @return
   *    a new array resulting from applying the given partial function `pf` to
   *    each element on which it is defined and collecting the results. The
   *    order of the elements is preserved.
   */
  def collect[B](pf: PartialFunction[A, B]): js.Array[B] = {
    var matched = true
    def d(x: A): B = {
      matched = false
      null.asInstanceOf[B]
    }
    val b = js.Array[B]()
    for (x <- xs) {
      matched = true
      val v = pf.applyOrElse(x, d)
      if (matched)
        b.push(v)
    }
    b
  }

  /** Finds the first element of the array for which the given partial function
   *  is defined, and applies the partial function to it.
   */
  def collectFirst[B](f: PartialFunction[A, B]): Option[B] = {
    // scalastyle:off return
    var i = 0
    var matched = true
    def d(x: A): B = {
      matched = false
      null.asInstanceOf[B]
    }
    val len = xs.length
    while (i < len) {
      matched = true
      val v = f.applyOrElse(xs(i), d)
      if (matched)
        return Some(v)
      i += 1
    }
    None
    // scalastyle:on return
  }

  /** Returns an array formed from this array and another iterable collection
   *  by combining corresponding elements in pairs.
   *
   *  If one of the two collections is longer than the other, its remaining
   *  elements are ignored.
   *
   *  @param that
   *    The iterable providing the second half of each result pair
   *  @tparam B
   *    the type of the second half of the returned pairs
   *  @return
   *    a new array containing pairs consisting of corresponding elements of
   *    this array and `that`. The length of the returned array is the minimum
   *    of the lengths of this array and `that`.
   */
  def zip[B](that: IterableOnce[B]): js.Array[(A, B)] = {
    val b = js.Array[(A, B)]()
    val len = xs.length
    var i = 0
    val it = that.iterator
    while (i < len && it.hasNext) {
      b.push((xs(i), it.next()))
      i += 1
    }
    b
  }

  /** Returns an array formed from this array and another iterable collection
   *  by combining corresponding elements in pairs.
   *
   *  If one of the two collections is shorter than the other, placeholder
   *  elements are used to extend the shorter collection to the length of the
   *  longer.
   *
   *  @param that
   *    the iterable providing the second half of each result pair
   *  @param thisElem
   *    the element to be used to fill up the result if this array is shorter
   *    than `that`.
   *  @param thatElem
   *    the element to be used to fill up the result if `that` is shorter than
   *    this array.
   *  @return
   *    a new array containing pairs consisting of corresponding elements of
   *    this array and `that`. The length of the returned array is the maximum
   *    of the lengths of this array and `that`. If this array is shorter than
   *    `that`, `thisElem` values are used to pad the result. If `that` is
   *    shorter than this array, `thatElem` values are used to pad the result.
   */
  def zipAll[A1 >: A, B](that: scala.collection.Iterable[B], thisElem: A1,
      thatElem: B): js.Array[(A1, B)] = {

    val b = js.Array[(A1, B)]()
    val len = xs.length
    var i = 0
    val it = that.iterator
    while (i < len && it.hasNext) {
      b.push((xs(i), it.next()))
      i += 1
    }
    while (it.hasNext) {
      b.push((thisElem, it.next()))
      i += 1
    }
    while (i < len) {
      b.push((xs(i), thatElem))
      i += 1
    }
    b
  }

  /** Zips this array with its indices.
   *
   *  @return
   *    A new array containing pairs consisting of all elements of this array
   *    paired with their index. Indices start at `0`.
   */
  def zipWithIndex: js.Array[(A, Int)] = {
    val len = xs.length
    val b = new js.Array[(A, Int)](len)
    var i = 0
    while (i < len) {
      b(i) = ((xs(i), i))
      i += 1
    }
    b
  }

  /** A copy of this array with an element appended. */
  def appended[B >: A](x: B): js.Array[B] = {
    val dest = xs.jsSlice().asInstanceOf[js.Array[B]]
    dest.push(x)
    dest
  }

  @inline final def :+[B >: A](x: B): js.Array[B] =
    appended(x)

  /** A copy of this array with an element prepended. */
  def prepended[B >: A](x: B): js.Array[B] = {
    val dest = xs.jsSlice().asInstanceOf[js.Array[B]]
    dest.unshift(x)
    dest
  }

  @inline final def +:[B >: A](x: B): js.Array[B] =
    prepended(x)

  /** A copy of this array with all elements of a collection prepended. */
  def prependedAll[B >: A](prefix: IterableOnce[B]): js.Array[B] = {
    val b = js.Array[B]()
    b.addAll(prefix)
    b.addAll(xs)
    b
  }

  /** A copy of this array with all elements of an array prepended. */
  def prependedAll[B >: A](prefix: js.Array[_ <: B]): js.Array[B] =
    ArrayOpsCommon.concat(prefix, xs)

  @inline final def ++:[B >: A](prefix: IterableOnce[B]): js.Array[B] =
    prependedAll(prefix)

  @inline final def ++:[B >: A](prefix: js.Array[_ <: B]): js.Array[B] =
    prependedAll(prefix)

  /** A copy of this array with all elements of a collection appended. */
  def appendedAll[B >: A](suffix: IterableOnce[B]): js.Array[B] = {
    val b = xs.jsSlice().asInstanceOf[js.Array[B]]
    b.addAll(suffix)
    b
  }

  /** A copy of this array with all elements of an array appended. */
  def appendedAll[B >: A](suffix: js.Array[_ <: B]): js.Array[B] =
    ArrayOpsCommon.concat(xs, suffix)

  @inline final def :++[B >: A](suffix: IterableOnce[B]): js.Array[B] =
    appendedAll(suffix)

  @inline final def :++[B >: A](suffix: js.Array[_ <: B]): js.Array[B] =
    appendedAll(suffix)

  @inline final def ++[B >: A](ys: IterableOnce[B]): js.Array[B] =
    appendedAll(ys)

  @inline final def ++[B >: A](ys: js.Array[_ <: B]): js.Array[B] =
    appendedAll(ys)

  /** Tests whether this array contains a given value as an element.
   *
   *  @param elem
   *    the element to test.
   *  @return
   *    `true` if this array has an element that is equal (as determined by
   *    `==`) to `elem`, `false` otherwise.
   */
  def contains(elem: A): Boolean =
    indexOf(elem) >= 0

  /** Returns a copy of this array with patched values.
   *
   *  Patching at negative indices is the same as patching starting at 0.
   *  Patching at indices at or larger than the length of the original array
   *  appends the patch to the end. If more values are replaced than actually
   *  exist, the excess is ignored.
   *
   *  @param from
   *    The start index from which to patch
   *  @param other
   *    The patch values
   *  @param replaced
   *    The number of values in the original array that are replaced by the
   *    patch.
   */
  def patch[B >: A](from: Int, other: IterableOnce[B],
      replaced: Int): js.Array[B] = {
    val len = xs.length
    val cut = if (from > 0) min(from, len) else 0
    val b = xs.jsSlice(0, cut).asInstanceOf[js.Array[B]]
    b ++= other
    var i = cut + max(replaced, 0)
    while (i < len) {
      b += xs(i)
      i += 1
    }
    b
  }

  /** Converts an array of pairs into an array of first elements and an array
   *  of second elements.
   *
   *  @tparam A1
   *    the type of the first half of the element pairs
   *  @tparam A2
   *    the type of the second half of the element pairs
   *  @param asPair
   *    an implicit conversion which asserts that the element type of this
   *    array is a pair.
   *  @return
   *    a pair of Arrays, containing, respectively, the first and second half
   *    of each element pair of this array.
   */
  def unzip[A1, A2](
      implicit asPair: A => (A1, A2)): (js.Array[A1], js.Array[A2]) = {

    val len = xs.length
    val a1 = new js.Array[A1](len)
    val a2 = new js.Array[A2](len)
    var i = 0
    while (i < len) {
      val e = asPair(xs(i))
      a1(i) = e._1
      a2(i) = e._2
      i += 1
    }
    (a1, a2)
  }

  /** Converts an array of triples into three arrays, one containing the
   *  elements from each position of the triple.
   *
   *  @tparam A1
   *    the type of the first of three elements in the triple
   *  @tparam A2
   *    the type of the second of three elements in the triple
   *  @tparam A3
   *    the type of the third of three elements in the triple
   *  @param asTriple
   *    an implicit conversion which asserts that the element type of this
   *    array is a triple.
   *  @return
   *    a triple of Arrays, containing, respectively, the first, second, and
   *    third elements from each element triple of this array.
   */
  def unzip3[A1, A2, A3](
      implicit asTriple: A => (A1, A2, A3)): (js.Array[A1], js.Array[A2], js.Array[A3]) = {

    val len = xs.length
    val a1 = new js.Array[A1](len)
    val a2 = new js.Array[A2](len)
    val a3 = new js.Array[A3](len)
    var i = 0
    while (i < len) {
      val e = asTriple(xs(i))
      a1(i) = e._1
      a2(i) = e._2
      a3(i) = e._3
      i += 1
    }
    (a1, a2, a3)
  }

  /** Transposes a two dimensional array.
   *
   *   @tparam B
   *     Type of row elements.
   *   @param asArray
   *     A function that converts elements of this array to rows - arrays of
   *     type `B`.
   *   @return
   *     An array obtained by replacing elements of this arrays with rows the
   *     represent.
   */
  def transpose[B](
      implicit asArray: A => js.Array[B]): js.Array[js.Array[B]] = {

    val xsLen = xs.length
    if (xsLen > 0) {
      val bs = asArray(xs(0)).map((x: B) => js.Array[B]())
      var i = 0
      while (i < xsLen) {
        val ys = asArray(xs(i))
        val ysLen = ys.length
        var j = 0
        while (j < ysLen) {
          bs(j).push(ys(j))
          j += 1
        }
        i += 1
      }
      bs
    } else {
      js.Array[js.Array[B]]()
    }
  }

  /** Apply `f` to each element for its side effects.
   *
   *  Note: [U] parameter needed to help scalac's type inference.
   */
  def foreach[U](f: A => U): Unit = {
    val len = xs.length
    var i = 0
    while (i < len) {
      f(xs(i))
      i += 1
    }
  }

  /** Selects all the elements of this array ignoring the duplicates.
   *
   *  @return
   *    a new array consisting of all the elements of this array without
   *    duplicates.
   */
  def distinct: js.Array[A] =
    distinctBy(identity)

  /** Selects all the elements of this array ignoring the duplicates as
   *  determined by `==` after applying the transforming function `f`.
   *
   *  @param f
   *    The transforming function whose result is used to determine the
   *    uniqueness of each element
   *  @tparam B
   *    the type of the elements after being transformed by `f`
   *  @return
   *    a new array consisting of all the elements of this array without
   *    duplicates.
   */
  def distinctBy[B](f: A => B): js.Array[A] = {
    val result = js.Array[A]()
    result.addAll(iterator.distinctBy(f))
    result
  }

  /** A copy of this array with an element value appended until a given target
   *  length is reached.
   *
   *  @param len
   *    the target length
   *  @param elem
   *    the padding value
   *  @tparam B
   *    the element type of the returned array.
   *  @return
   *    a new array consisting of all elements of this array followed by the
   *    minimal number of occurrences of `elem` so that the resulting
   *    collection has a length of at least `len`.
   */
  def padTo[B >: A](len: Int, elem: B): js.Array[B] = {
    val dest = xs.jsSlice().asInstanceOf[js.Array[B]]
    var i = xs.length
    while (i < len) {
      dest.push(elem)
      i += 1
    }
    dest
  }

  /** Produces the range of all indices of this sequence.
   *
   *  @return
   *    a `Range` value from `0` to one less than the length of this array.
   */
  @inline def indices: Range =
    Range(0, xs.length)

  /** Partitions this array into a map of arrays according to some
   *  discriminator function.
   *
   *  @param f
   *    the discriminator function.
   *  @tparam K
   *    the type of keys returned by the discriminator function.
   *  @return
   *    A map from keys to arrays such that the following invariant holds:
   *    {{{
   *    (xs groupBy f)(k) = xs filter (x => f(x) == k)
   *    }}}
   *    That is, every key `k` is bound to an array of those elements `x` for
   *    which `f(x)` equals `k`.
   */
  def groupBy[K](f: A => K): immutable.Map[K, js.Array[A]] = {
    val m = mutable.Map.empty[K, js.Array[A]]
    for (x <- xs) {
      val key = f(x)
      m.getOrElseUpdate(key, js.Array[A]()).push(x)
    }
    m.toMap
  }

  /** Partitions this array into a map of arrays according to a discriminator
   *  function `key`.
   *
   *  Each element in a group is transformed into a value of type `B` using the
   *  `value` function.
   *
   *  It is equivalent to `groupBy(key).mapValues(_.map(f))`, but more
   *  efficient.
   *
   *  {{{
   *  case class User(name: String, age: Int)
   *
   *  def namesByAge(users: js.Array[User]): Map[Int, js.Array[String]] =
   *    users.groupMap(_.age)(_.name)
   *  }}}
   *
   *  @param key
   *    the discriminator function
   *  @param f
   *    the element transformation function
   *  @tparam K
   *    the type of keys returned by the discriminator function
   *  @tparam B
   *    the type of values returned by the transformation function
   */
  def groupMap[K, B](key: A => K)(f: A => B): immutable.Map[K, js.Array[B]] = {
    val m = mutable.Map.empty[K, js.Array[B]]
    for (x <- xs) {
      val k = key(x)
      m.getOrElseUpdate(k, js.Array[B]()).push(f(x))
    }
    m.toMap
  }

  @inline final def toSeq: immutable.Seq[A] =
    toIndexedSeq

  @inline def toIndexedSeq: immutable.IndexedSeq[A] =
    immutable.IndexedSeq.from(xs)

  /** Copy elements of this array to a Scala array.
   *
   *  Fills the given array `xs` starting at index 0. Copying will stop once
   *  either all the elements of this array have been copied, or the end of the
   *  array is reached.
   *
   *  @param xs
   *    the array to fill.
   *  @tparam B
   *    the type of the elements of the array.
   */
  def copyToArray[B >: A](xs: scala.Array[B]): Int =
    copyToArray(xs, 0)

  /** Copy elements of this array to a Scala array.
   *
   *  Fills the given array `xs` starting at index `start`. Copying will stop
   *  once either all the elements of this array have been copied, or the end
   *  of the array is reached.
   *
   *  @param xs
   *    the array to fill.
   *  @param start
   *    the starting index within the destination array.
   *  @tparam B
   *    the type of the elements of the array.
   */
  def copyToArray[B >: A](xs: scala.Array[B], start: Int): Int =
    copyToArray(xs, start, Int.MaxValue)

  /** Copy elements of this array to a Scala array.
   *
   *  Fills the given array `xs` starting at index `start` with at most `len`
   *  values. Copying will stop once either all the elements of this array have
   *  been copied, or the end of the array is reached, or `len` elements have
   *  been copied.
   *
   *  @param xs
   *    the array to fill.
   *  @param start
   *    the starting index within the destination array.
   *  @param len
   *    the maximal number of elements to copy.
   *  @tparam B
   *    the type of the elements of the array.
   */
  def copyToArray[B >: A](xs: scala.Array[B], start: Int, len: Int): Int = {
    val src = this.xs
    val dest = xs

    // Copied from IterableOnce.elemsToCopyToArray
    @inline
    def elemsToCopyToArray(srcLen: Int, destLen: Int, start: Int, len: Int): Int =
      max(min(min(len, srcLen), destLen - start), 0)

    val copied = elemsToCopyToArray(src.length, dest.length, start, len)
    var i = 0
    while (i < copied) {
      dest(i + start) = src(i)
      i += 1
    }
    copied
  }

  /** Create a copy of this array as a Scala array. */
  def toArray[B >: A: ClassTag]: scala.Array[B] = {
    val destination = new scala.Array[B](xs.length)
    copyToArray(destination, 0)
    destination
  }

  /** Counts the number of elements in this array which satisfy a predicate. */
  def count(p: A => Boolean): Int = {
    var res = 0
    for (x <- xs) {
      if (p(x))
        res += 1
    }
    res
  }

  // can't use a default arg because we already have another overload with a default arg
  /** Tests whether this array starts with the given array. */
  @inline def startsWith[B >: A](that: js.Array[B]): Boolean =
    startsWith(that, 0)

  /** Tests whether this array contains the given array at a given index.
   *
   *  @param that
   *    the array to test
   *  @param offset
   *    the index where the array is searched.
   *  @return
   *    `true` if the array `that` is contained in this array at index
   *    `offset`, otherwise `false`.
   */
  def startsWith[B >: A](that: js.Array[B], offset: Int): Boolean = {
    // scalastyle:off return
    val safeOffset = offset.max(0)
    val thatl = that.length
    if (thatl > xs.length - safeOffset) {
      thatl == 0
    } else {
      var i = 0
      while (i < thatl) {
        if (xs(i + safeOffset) != that(i))
          return false
        i += 1
      }
      true
    }
    // scalastyle:on return
  }

  /** Tests whether this array ends with the given array.
   *
   *  @param that
   *    the array to test
   *  @return
   *    `true` if this array has `that` as a suffix, `false` otherwise.
   */
  def endsWith[B >: A](that: js.Array[B]): Boolean = {
    // scalastyle:off return
    val thatl = that.length
    val off = xs.length - thatl
    if (off < 0) {
      false
    } else {
      var i = 0
      while (i < thatl) {
        if (xs(i + off) != that(i))
          return false
        i += 1
      }
      true
    }
    // scalastyle:on return
  }

  /** A copy of this array with one single replaced element.
   *
   *  @param index
   *    the position of the replacement
   *  @param elem
   *    the replacing element
   *  @return
   *    a new array which is a copy of this array with the element at position
   *    `index` replaced by `elem`.
   *  @throws IndexOutOfBoundsException
   *    if `index` does not satisfy `0 <= index < length`.
   */
  def updated[B >: A](index: Int, elem: B): js.Array[B] = {
    if (index < 0 || index >= xs.length)
      throw new IndexOutOfBoundsException
    val dest = xs.jsSlice().asInstanceOf[js.Array[B]]
    dest(index) = elem
    dest
  }

  @inline def view: IndexedSeqView[A] =
    new js.ArrayOps.ArrayView[A](xs)

  /** Computes the multiset difference between this array and another sequence.
   *
   *  @param that
   *    the sequence of elements to remove
   *  @return
   *    a new array which contains all elements of this array except some of
   *    occurrences of elements that also appear in `that`. If an element value
   *    `x` appears ''n'' times in `that`, then the first ''n'' occurrences of
   *    `x` will not form part of the result, but any following occurrences
   *    will.
   */
  def diff[B >: A](that: Seq[B]): js.Array[A] =
    new js.WrappedArray(xs).diff(that)

  /** Computes the multiset intersection between this array and another
   *  sequence.
   *
   *  @param that
   *    the sequence of elements to intersect with.
   *  @return
   *    a new array which contains all elements of this array which also appear
   *    in `that`. If an element value `x` appears ''n'' times in `that`, then
   *    the first ''n'' occurrences of `x` will be retained in the result, but
   *    any following occurrences will be omitted.
   */
  def intersect[B >: A](that: Seq[B]): js.Array[A] =
    new js.WrappedArray(xs).intersect(that)

  /** Groups elements in fixed size blocks by passing a "sliding window" over
   *  them (as opposed to partitioning them, as is done in grouped).
   *
   *  @see [[scala.collection.Iterator]], method `sliding`
   *
   *  @param size
   *    the number of elements per group
   *  @param step
   *    the distance between the first elements of successive groups
   *  @return
   *    An iterator producing arrays of size `size`, except the last element
   *    (which may be the only element) will be truncated if there are fewer
   *    than `size` elements remaining to be grouped.
   */
  def sliding(size: Int, step: Int = 1): scala.collection.Iterator[js.Array[A]] =
    new js.WrappedArray(xs).sliding(size, step).map(js.WrappedArray.toJSArray _)

  /** Iterates over combinations. */
  def combinations(n: Int): scala.collection.Iterator[js.Array[A]] =
    new js.WrappedArray(xs).combinations(n).map(js.WrappedArray.toJSArray _)

  /** Iterates over distinct permutations. */
  def permutations: scala.collection.Iterator[js.Array[A]] =
    new js.WrappedArray(xs).permutations.map(js.WrappedArray.toJSArray _)

  // we have another overload here, so we need to duplicate this method
  /** Tests whether this array contains the given sequence at a given index.
   *
   *  @param that
   *    the sequence to test
   *  @param offset
   *    the index where the sequence is searched.
   *  @return
   *    `true` if the sequence `that` is contained in this array at index
   *    `offset`, otherwise `false`.
   */
  def startsWith[B >: A](that: IterableOnce[B], offset: Int = 0): Boolean =
    new js.WrappedArray(xs).startsWith(that, offset)

  // we have another overload here, so we need to duplicate this method
  /** Tests whether this array ends with the given sequence.
   *
   *  @param that
   *    the sequence to test
   *  @return
   *    `true` if this array has `that` as a suffix, `false` otherwise.
   */
  def endsWith[B >: A](that: scala.collection.Iterable[B]): Boolean =
    new js.WrappedArray(xs).endsWith(that)

  // Clearable

  /** Clears the array's contents.
   *
   *  After this operation, the array is empty.
   */
  @inline def clear(): Unit =
    xs.length = 0

  // Growable[A]

  /** Appends a single element to this array.
   *
   *  @param elem
   *    the element to add.
   *  @return
   *    the array itself
   */
  @inline def addOne(elem: A): js.Array[A] = {
    xs.push(elem)
    xs
  }

  /** Alias for `addOne` */
  @inline final def +=(elem: A): js.Array[A] =
    addOne(elem)

  /** Adds two or more elements to this array.
   *
   *  @param elem1
   *    the first element to add.
   *  @param elem2
   *    the second element to add.
   *  @param elems
   *    the remaining elements to add.
   *  @return
   *    the array itself
   */
  @deprecated("Use `++=` (addAll) instead of varargs `+=`", "2.13.0")
  @inline final def +=(elem1: A, elem2: A, elems: A*): js.Array[A] =
    this += elem1 += elem2 ++= (elems: IterableOnce[A])

  /** Adds all elements produced by an IterableOnce to this array.
   *
   *  @param xs
   *    the IterableOnce producing the elements to add.
   *  @return
   *    the array itself.
   */
  def addAll(ys: IterableOnce[A]): js.Array[A] = {
    ys.iterator.foreach(addOne(_))
    xs
  }

  /** Alias for `addAll` */
  @inline final def ++=(ys: IterableOnce[A]): js.Array[A] =
    addAll(ys)

  // Shrinkable

  /** Removes a single element from this array.
   *
   *  @param elem
   *    the element to remove.
   *  @return
   *    the array itself
   */
  def subtractOne(elem: A): js.Array[A] = {
    val i = indexOf(elem)
    if (i != -1)
      remove(i)
    xs
  }

  /** Alias for `subtractOne` */
  @inline final def -=(elem: A): js.Array[A] =
    subtractOne(elem)

  /** Removes two or more elements from this array.
   *
   *  @param elem1
   *    the first element to remove.
   *  @param elem2
   *    the second element to remove.
   *  @param elems
   *    the remaining elements to remove.
   *  @return
   *    the array itself
   */
  @deprecated("Use `--=` (subtractAll) instead of varargs `-=`", "2.13.0")
  def -=(elem1: A, elem2: A, elems: A*): js.Array[A] =
    this -= elem1 -= elem2 --= elems

  /** Removes all elements produced by an iterator from this array.
   *
   *  @param xs
   *    the elements to remove.
   *  @return
   *    the array itself
   */
  @inline
  def subtractAll(ys: scala.collection.IterableOnce[A]): js.Array[A] = {
    ys.iterator.foreach(subtractOne)
    xs
  }

  /** Alias for `subtractAll` */
  @inline final def --=(ys: collection.IterableOnce[A]): js.Array[A] =
    subtractAll(ys)

  // Buffer[A]

  /** Prepends a single element at the front of this array.
   *
   *  @param elem
   *    the element to add.
   *  @return
   *    the array itself
   */
  @inline
  def prepend(elem: A): js.Array[A] = {
    xs.unshift(elem)
    xs
  }

  /** Appends the given element to this array.
   *
   *  @param elem  the element to append.
   */
  @inline final def append(elem: A): js.Array[A] =
    addOne(elem)

  @deprecated("Use appendAll instead", "2.13.0")
  @inline final def append(elems: A*): js.Array[A] =
    addAll(elems)

  /** Appends the elements contained in an iterable object to this array.
   *
   *  @param xs  the iterable object containing the elements to append.
   */
  @inline final def appendAll(ys: IterableOnce[A]): js.Array[A] =
    addAll(ys)

  /** Alias for `prepend` */
  @inline final def +=:(elem: A): js.Array[A] =
    prepend(elem)

  @inline def prependAll(elems: IterableOnce[A]): js.Array[A] = {
    insertAll(0, elems)
    xs
  }

  @deprecated("Use prependAll instead", "2.13.0")
  @inline final def prepend(elems: A*): js.Array[A] =
    prependAll(elems)

  /** Alias for `prependAll` */
  @inline final def ++=:(elems: IterableOnce[A]): js.Array[A] =
    prependAll(elems)

  /** Inserts a new element at a given index into this array.
   *
   *  @param idx
   *    the index where the new elements is inserted.
   *  @param elem
   *    the element to insert.
   *  @throws IndexOutOfBoundsException
   *    if the index `idx` is not in the valid range `0 <= idx <= length`.
   */
  def insert(idx: Int, elem: A): Unit = {
    if (idx < 0 || idx > xs.length)
      throw new IndexOutOfBoundsException
    xs.splice(idx, 0, elem)
  }

  /** Inserts new elements at the index `idx`.
   *
   *  As opposed to the method `update`, this method will not replace an
   *  element with a new one. Instead, it will insert a new element at index
   *  `idx`.
   *
   *  @param idx
   *    the index where a new element will be inserted.
   *  @param elems
   *    the iterable object providing all elements to insert.
   *  @throws IndexOutOfBoundsException
   *    if `idx` is out of bounds.
   */
  def insertAll(idx: Int, elems: IterableOnce[A]): Unit = {
    val len = xs.length
    if (idx < 0 || idx > len)
      throw new IndexOutOfBoundsException

    val (count, elems0) = elems.knownSize match {
      case -1 =>
        import js.JSConverters._
        val elemsArray = elems.toJSArray
        (elemsArray.length, elemsArray.iterator)
      case count =>
        (count, elems)
    }

    val newlen = len + count
    xs.length = newlen
    var i = newlen - 1
    val endOfInserted = idx + count
    while (i >= endOfInserted) {
      xs(i) = xs(i - count)
      i -= 1
    }
    i = idx
    val iter = elems0.iterator
    while (iter.hasNext) {
      xs(i) = iter.next()
      i += 1
    }
  }

  /** Removes the element at a given index position.
   *
   *  @param idx
   *    the index which refers to the element to delete.
   *  @return
   *    the element that was formerly at index `idx`.
   *  @throws IndexOutOfBoundsException
   *    if `idx` is out of bounds.
   */
  def remove(idx: Int): A = {
    if (idx < 0 || idx >= xs.length)
      throw new IndexOutOfBoundsException
    xs.splice(idx, 1)(0)
  }

  /** Removes consecutive elements starting at a given index position.
   *
   *  @param idx
   *    the index which refers to the first element to remove.
   *  @param count
   *    the number of elements to remove.
   *  @throws IndexOutOfBoundsException
   *    if the index `idx` is not in the valid range
   *    `0 <= idx <= length - count` (with `count > 0`).
   *  @throws IllegalArgumentException
   *    if `count < 0`.
   */
  def remove(idx: Int, count: Int): Unit = {
    if (count < 0)
      throw new IllegalArgumentException
    if (idx < 0 || (count > 0 && idx > xs.length - count))
      throw new IndexOutOfBoundsException
    xs.splice(idx, count)
  }

  /** Removes the first ''n'' elements of this array.
   *
   *  @param n
   *    the number of elements to remove from the beginning of this array.
   */
  def trimStart(n: Int): Unit =
    xs.splice(0, clampIndex(n))

  /** Removes the last ''n'' elements of this array.
   *
   *  @param n
   *    the number of elements to remove from the end of this array.
   */
  def trimEnd(n: Int): Unit =
    xs.length -= clampIndex(n)

  @noinline // js.WrappedArray itself is @inline, so the call below will produce a lot of code
  def patchInPlace(from: Int, patch: scala.collection.IterableOnce[A],
      replaced: Int): js.Array[A] = {
    new js.WrappedArray(xs).patchInPlace(from, patch, replaced)
    xs
  }

  def dropInPlace(n: Int): js.Array[A] = {
    xs.splice(0, clampIndex(n))
    xs
  }

  @inline def dropRightInPlace(n: Int): js.Array[A] = {
    trimEnd(n)
    xs
  }

  def takeInPlace(n: Int): js.Array[A] = {
    xs.length = clampIndex(n)
    xs
  }

  def takeRightInPlace(n: Int): js.Array[A] = {
    xs.splice(0, xs.length - clampIndex(n))
    xs
  }

  def sliceInPlace(start: Int, end: Int): js.Array[A] =
    takeInPlace(end).dropInPlace(start)

  @inline private def clampIndex(n: Int): Int =
    min(max(n, 0), xs.length)

  def dropWhileInPlace(p: A => Boolean): js.Array[A] = {
    val idx = indexWhere(!p(_))
    if (idx < 0) {
      clear()
      xs
    } else {
      dropInPlace(idx)
    }
  }

  def takeWhileInPlace(p: A => Boolean): js.Array[A] = {
    val idx = indexWhere(!p(_))
    if (idx < 0)
      xs
    else
      takeInPlace(idx)
  }

  def padToInPlace(len: Int, elem: A): js.Array[A] = {
    while (xs.length < len)
      xs.push(elem)
    xs
  }

  // IndexedBuffer[A]

  def flatMapInPlace(f: A => IterableOnce[A]): js.Array[A] = {
    // There's scope for a better implementation which copies elements in place.
    var i = 0
    val copy = xs.jsSlice()
    xs.length = 0
    for {
      x <- copy
      y <- f(x).iterator
    } {
      xs.push(y)
    }
    xs
  }

  def filterInPlace(p: A => Boolean): js.Array[A] = {
    var i = 0
    var j = 0
    val len = xs.length
    while (i < len) {
      val x = xs(i)
      if (p(x)) {
        xs(j) = x
        j += 1
      }
      i += 1
    }

    if (j != len)
      xs.length = j
    xs
  }

}

object ArrayOps {
  private class ArrayView[A](xs: js.Array[A]) extends AbstractIndexedSeqView[A] {
    @inline def length: Int = xs.length

    @inline def apply(n: Int): A = xs(n)

    override protected[this] def className = "js.ArrayView"
  }

  /** A lazy filtered array.
   *
   *  No filtering is applied until one of `foreach`, `map` or `flatMap` is
   *  called.
   */
  final class WithFilter[A](p: A => Boolean, xs: js.Array[A]) {

    /** Apply `f` to each element for its side effects.
     *
     *  Note: [U] parameter needed to help scalac's type inference.
     */
    def foreach[U](f: A => U): Unit = {
      val len = xs.length
      var i = 0
      while (i < len) {
        val x = xs(i)
        if (p(x))
          f(x)
        i += 1
      }
    }

    /** Builds a new array by applying a function to all elements of this array.
     *
     *  @param f
     *    the function to apply to each element.
     *  @tparam B
     *    the element type of the returned array.
     *  @return
     *    a new aray resulting from applying the given function `f` to each
     *    element of this array and collecting the results.
     */
    def map[B](f: A => B): js.Array[B] = {
      val b = js.Array[B]()
      val len = xs.length
      var i = 0
      while (i < len) {
        val x = xs(i)
        if (p(x))
          b.push(f(x))
        i = i + 1
      }
      b
    }

    /** Builds a new array by applying a function to all elements of this array
     *  and using the elements of the resulting collections.
     *
     *  @param f
     *    the function to apply to each element.
     *  @tparam B
     *    the element type of the returned array.
     *  @return
     *    a new array resulting from applying the given collection-valued
     *    function `f` to each element of this array and concatenating the
     *    results.
     */
    def flatMap[B](f: A => IterableOnce[B]): js.Array[B] = {
      val b = js.Array[B]()
      val len = xs.length
      var i = 0
      while (i < len) {
        val x = xs(i)
        if (p(x))
          b.addAll(f(x))
        i += 1
      }
      b
    }

    def flatMap[BS, B](f: A => BS)(
        implicit asIterable: BS => scala.collection.Iterable[B]): js.Array[B] = {
      flatMap[B](x => asIterable(f(x)))
    }

    /** Creates a new non-strict filter which combines this filter with the
     *  given predicate.
     */
    def withFilter(q: A => Boolean): WithFilter[A] =
      new WithFilter[A](a => p(a) && q(a), xs)
  }

  private class ArrayIterator[A](private[this] val xs: js.Array[A]) extends AbstractIterator[A] {

    private[this] var pos = 0

    def hasNext: Boolean = pos < xs.length

    def next(): A = {
      if (pos >= xs.length)
        throw new NoSuchElementException
      val r = xs(pos)
      pos += 1
      r
    }

    override def drop(n: Int): scala.collection.Iterator[A] = {
      if (n > 0)
        pos = Math.min(xs.length, pos + n)
      this
    }
  }

  private class ReverseIterator[A](private[this] val xs: js.Array[A]) extends AbstractIterator[A] {

    private[this] var pos = xs.length - 1

    def hasNext: Boolean = pos >= 0

    def next(): A = {
      if (pos < 0)
        throw new NoSuchElementException
      val r = xs(pos)
      pos -= 1
      r
    }

    override def drop(n: Int): scala.collection.Iterator[A] = {
      if (n > 0)
        pos = Math.max(-1, pos - n)
      this
    }
  }

  private class GroupedIterator[A](xs: js.Array[A], groupSize: Int)
      extends AbstractIterator[js.Array[A]] {

    private[this] var pos = 0

    def hasNext: Boolean = pos < xs.length

    def next(): js.Array[A] = {
      if (pos >= xs.length)
        throw new NoSuchElementException
      val r = xs.slice(pos, pos + groupSize)
      pos += groupSize
      r
    }
  }
}
