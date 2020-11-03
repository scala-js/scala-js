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

/** Make some Scala collection APIs available on Java collections. */
private[java] object ScalaOps {

  implicit class IntScalaOps private[ScalaOps] (val __self: Int) extends AnyVal {
    @inline def until(end: Int): SimpleRange =
      new SimpleRange(__self, end)
  }

  @inline
  final class SimpleRange(start: Int, end: Int) {
    @inline
    def foreach[U](f: Int => U): Unit = {
      var i = start
      while (i < end) {
        f(i)
        i += 1
      }
    }
  }

  implicit class ToJavaIterableOps[A] private[ScalaOps] (val __self: java.lang.Iterable[A])
      extends AnyVal {
    def scalaOps: JavaIterableOps[A] = new JavaIterableOps[A](__self)
  }

  class JavaIterableOps[A] private[ScalaOps] (val __self: java.lang.Iterable[A]) extends AnyVal {

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

    @inline def mkString(start: String, sep: String, end: String): String =
      __self.iterator().scalaOps.mkString(start, sep, end)
  }

  implicit class ToJavaIteratorOps[A] private[ScalaOps] (val __self: Iterator[A]) extends AnyVal {
    def scalaOps: JavaIteratorOps[A] = new JavaIteratorOps[A](__self)
  }

  class JavaIteratorOps[A] private[ScalaOps] (val __self: Iterator[A]) extends AnyVal {

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
  }

  implicit class ToJavaEnumerationOps[A] private[ScalaOps] (val __self: Enumeration[A])
      extends AnyVal {
    def scalaOps: JavaEnumerationOps[A] = new JavaEnumerationOps[A](__self)
  }

  class JavaEnumerationOps[A] private[ScalaOps] (val __self: Enumeration[A]) extends AnyVal {

    @inline def foreach[U](f: A => U): Unit = {
      while (__self.hasMoreElements())
        f(__self.nextElement())
    }
  }

}
