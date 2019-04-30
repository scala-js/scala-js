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

package java.util

import scala.collection.immutable
import scala.collection.mutable

/** Make some Scala collection APIs available on Java collections. */
private[util] object ScalaOps {

  implicit class ToScalaIterableOps[A] private[ScalaOps] (
      val __self: scala.collection.Iterable[A])
      extends AnyVal {
    def javaIterator(): Iterator[A] =
      new JavaIteratorAdapter(__self.iterator)
  }

  private class JavaIteratorAdapter[A](scalaIterator: scala.collection.Iterator[A])
      extends Iterator[A] {
    def hasNext(): Boolean = scalaIterator.hasNext
    def next(): A = scalaIterator.next()

    def remove(): Unit =
      throw new UnsupportedOperationException("remove")
  }

  implicit class ScalaIteratorOps[A] private[ScalaOps] (
      val __self: scala.collection.Iterator[A])
      extends AnyVal {

    def asJavaEnumeration(): Enumeration[A] =
      new JavaEnumerationAdapter(__self)
  }

  private class JavaEnumerationAdapter[A] private[ScalaOps] (
      val __self: scala.collection.Iterator[A])
      extends Enumeration[A] {

    def hasMoreElements(): Boolean = __self.hasNext

    def nextElement(): A = __self.next()
  }

  implicit class ToJavaIterableOps[A] private[ScalaOps] (
      val __self: java.lang.Iterable[A])
      extends AnyVal {
    def scalaOps: JavaIterableOps[A] = new JavaIterableOps[A](__self)
  }

  class JavaIterableOps[A] private[ScalaOps] (
      val __self: java.lang.Iterable[A])
      extends AnyVal {

    @inline def foreach[U](f: A => U): Unit =
      __self.iterator().scalaOps.foreach(f)

    @inline def count(f: A => Boolean): Int =
      __self.iterator().scalaOps.count(f)

    @inline def exists(f: A => Boolean): Boolean =
      __self.iterator().scalaOps.exists(f)

    @inline def forall(f: A => Boolean): Boolean =
      __self.iterator().scalaOps.forall(f)

    @inline def indexWhere(f: A => Boolean): Int =
      __self.iterator().scalaOps.indexWhere(f)

    @inline def find(f: A => Boolean): Option[A] =
      __self.iterator().scalaOps.find(f)

    @inline def foldLeft[B](z: B)(f: (B, A) => B): B =
      __self.iterator().scalaOps.foldLeft(z)(f)

    @inline def reduceLeft[B >: A](f: (B, A) => B): B =
      __self.iterator().scalaOps.reduceLeft(f)

    @inline def toList: immutable.List[A] =
      __self.iterator().scalaOps.toList

    @inline def toSeq: immutable.Seq[A] =
      __self.iterator().scalaOps.toSeq

    @inline def toSet: immutable.Set[A] =
      __self.iterator().scalaOps.toSet

    @inline def mkString(start: String, sep: String, end: String): String =
      __self.iterator().scalaOps.mkString(start, sep, end)
  }

  implicit class ToJavaIteratorOps[A] private[ScalaOps] (
      val __self: Iterator[A])
      extends AnyVal {
    def scalaOps: JavaIteratorOps[A] = new JavaIteratorOps[A](__self)
  }

  class JavaIteratorOps[A] private[ScalaOps] (val __self: Iterator[A])
      extends AnyVal {

    @inline def foreach[U](f: A => U): Unit = {
      while (__self.hasNext())
        f(__self.next())
    }

    @inline def count(f: A => Boolean): Int =
      foldLeft(0)((prev, x) => if (f(x)) prev + 1 else prev)

    @inline def exists(f: A => Boolean): Boolean = {
      // scalastyle:off return
      while (__self.hasNext()) {
        if (f(__self.next()))
          return true
      }
      false
      // scalastyle:on return
    }

    @inline def forall(f: A => Boolean): Boolean =
      !exists(x => !f(x))

    @inline def indexWhere(f: A => Boolean): Int = {
      // scalastyle:off return
      var i = 0
      while (__self.hasNext()) {
        if (f(__self.next()))
          return i
        i += 1
      }
      -1
      // scalastyle:on return
    }

    @inline def find(f: A => Boolean): Option[A] = {
      // scalastyle:off return
      while (__self.hasNext()) {
        val x = __self.next()
        if (f(x))
          return Some(x)
      }
      None
      // scalastyle:on return
    }

    @inline def foldLeft[B](z: B)(f: (B, A) => B): B = {
      var result: B = z
      while (__self.hasNext())
        result = f(result, __self.next())
      result
    }

    @inline def reduceLeft[B >: A](f: (B, A) => B): B = {
      if (!__self.hasNext())
        throw new NoSuchElementException("collection is empty")
      foldLeft[B](__self.next())(f)
    }

    @inline def toList: immutable.List[A] = {
      val builder = immutable.List.newBuilder[A]
      foreach(builder += _)
      builder.result()
    }

    @inline def toSeq: immutable.Seq[A] = toList

    @inline def toSet: immutable.Set[A] = {
      val builder = immutable.Set.newBuilder[A]
      foreach(builder += _)
      builder.result()
    }

    @inline def mkString(start: String, sep: String, end: String): String = {
      var result: String = start
      var first = true
      while (__self.hasNext()) {
        if (first)
          first = false
        else
          result += sep
        result += __self.next()
      }
      result + end
    }

    @inline def map[B](f: A => B): Iterator[B] =
      new MappedIterator(__self, f)

    @inline def withFilter(f: A => Boolean): IteratorWithFilter[A] =
      new IteratorWithFilter(__self, f)

    @inline def zipWithIndex: Iterator[(A, Int)] =
      new IteratorWithIndex(__self)
  }

  @inline
  private class MappedIterator[A, B](iter: Iterator[A], f: A => B)
      extends Iterator[B] {

    def hasNext(): Boolean = iter.hasNext()

    def next(): B = f(iter.next())

    def remove(): Unit = iter.remove()
  }

  @inline
  final class IteratorWithFilter[A] private[ScalaOps] (
      iter: Iterator[A], pred: A => Boolean) {

    def foreach[U](f: A => U): Unit = {
      for (x <- iter.scalaOps) {
        if (pred(x))
          f(x)
      }
    }

    def withFilter(f: A => Boolean): IteratorWithFilter[A] =
      new IteratorWithFilter(iter, x => pred(x) && f(x))
  }

  @inline
  private class IteratorWithIndex[A](iter: Iterator[A])
      extends Iterator[(A, Int)] {

    private var lastIndex = -1

    def hasNext(): Boolean = iter.hasNext()

    def next(): (A, Int) = {
      val index = lastIndex + 1
      lastIndex = index
      (iter.next(), index)
    }

    def remove(): Unit = iter.remove()
  }

  implicit class ToJavaEnumerationOps[A] private[ScalaOps] (
      val __self: Enumeration[A])
      extends AnyVal {
    def scalaOps: JavaEnumerationOps[A] = new JavaEnumerationOps[A](__self)
  }

  class JavaEnumerationOps[A] private[ScalaOps] (val __self: Enumeration[A])
      extends AnyVal {

    @inline def foreach[U](f: A => U): Unit = {
      while (__self.hasMoreElements())
        f(__self.nextElement())
    }
  }

}
