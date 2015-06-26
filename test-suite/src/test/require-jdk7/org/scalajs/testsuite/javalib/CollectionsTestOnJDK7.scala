/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import java.{util => ju}

import org.scalajs.jasminetest.JasmineTest

object CollectionsTestOnJDK7 extends JasmineTest with ExpectExceptions {

  describe("java.util.Collections.empty* on JDK7 or higher") {
    it(s"should implement emptyIterator[T](): Iterator[T]") {
      def freshIter: ju.Iterator[Int] = ju.Collections.emptyIterator[Int]

      expect(freshIter.hasNext).toBeFalsy
      expectThrows[NoSuchElementException](freshIter.next())
      expectThrows[IllegalStateException](freshIter.remove())
    }

    it(s"should implement emptyListIterator[T](): ListIterator[T]") {
      def test[E](toElem: Int => E): Unit = {
        def freshIter: ju.ListIterator[E] = ju.Collections.emptyListIterator[E]

        expect(freshIter.hasNext).toBeFalsy
        expect(freshIter.hasPrevious).toBeFalsy
        expectThrows[NoSuchElementException](freshIter.next())
        expectThrows[NoSuchElementException](freshIter.previous())
        expectThrows[IllegalStateException](freshIter.remove())
        expectThrows[UnsupportedOperationException](freshIter.add(toElem(0)))
        expectThrows[IllegalStateException](freshIter.set(toElem(0)))
      }

      test[Int](_.toInt)
      test[Long](_.toLong)
      test[Double](_.toDouble)
    }

    it(s"should implement emptyEnumeration[T](): Enumeration[T]") {
      def freshEnum: ju.Enumeration[Int] = ju.Collections.emptyEnumeration[Int]

      expect(freshEnum.hasMoreElements).toBeFalsy
      expectThrows[NoSuchElementException](freshEnum.nextElement())
    }
  }
}
