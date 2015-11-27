package org.scalajs.testsuite.javalib.util

import java.{lang => jl, util => ju}

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.javalib.util.concurrent.CopyOnWriteArrayListFactory
import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform.executingInJVM

import scala.collection.JavaConversions._

trait CollectionsOnListTest extends CollectionsOnCollectionsTest {

  def factory: ListFactory

  @Test def sort_on_comparables(): Unit = {
    // Test: sort[T<:Comparable[T]](List[T])
    def test[T <: AnyRef with Comparable[T]](toElem: Int => T,
        absoluteOrder: Boolean = true): Unit = {

      val list = factory.empty[T]

      def testIfSorted(rangeValues: Boolean): Unit = {
        for (i <- range.init)
          assertTrue(list(i).compareTo(list(i + 1)) <= 0)
        if (absoluteOrder && rangeValues) {
          for (i <- range)
            assertEquals(0, list(i).compareTo(toElem(i)))
        }
      }

      list.addAll(range.map(toElem))
      ju.Collections.sort(list)
      testIfSorted(true)

      list.clear()
      list.addAll(range.reverse.map(toElem))
      ju.Collections.sort(list)
      testIfSorted(true)

      for (seed <- List(0, 1, 42, -5432, 2341242)) {
        val rnd = new scala.util.Random(seed)
        list.clear()
        list.addAll(range.map(_ => toElem(rnd.nextInt())))
        ju.Collections.sort(list)
        testIfSorted(false)
      }
    }
    if (factory.sortableUsingCollections) { // Issue #2087
      test[CustomComparable](new CustomComparable(_), false)
      test[jl.Integer](jl.Integer.valueOf)
      test[jl.Long](_.toLong)
      test[jl.Double](_.toDouble)
    }
  }

  @Test def sort_with_comparator(): Unit = {
    // Test: sort[T](List[T], Comparator[T])
    def test[T](toElem: Int => T, cmpFun: (T, T) => Int,
        absoluteOrder: Boolean = true): Unit = {
      val list = factory.empty[T]

      def testIfSorted(rangeValues: Boolean): Unit = {
        for (i <- range.init)
          assertTrue(cmpFun(list(i), list(i + 1)) <= 0)
        if (absoluteOrder && rangeValues) {
          for (i <- range)
            assertEquals(0, cmpFun(list(i), toElem(i)))
        }
      }

      val cmp = new ju.Comparator[T] {
        override def compare(o1: T, o2: T): Int = cmpFun(o1, o2)
      }

      list.addAll(range.map(toElem))
      ju.Collections.sort(list, cmp)
      testIfSorted(true)

      list.clear()
      list.addAll(range.reverse.map(toElem))
      ju.Collections.sort(list, cmp)
      testIfSorted(true)

      for (seed <- List(0, 1, 42, -5432, 2341242)) {
        val rnd = new scala.util.Random(seed)
        list.clear()
        list.addAll(range.map(_ => toElem(rnd.nextInt())))
        ju.Collections.sort(list, cmp)
        testIfSorted(false)
      }
    }

    if (factory.sortableUsingCollections) { // Issue #2087
      test[CustomComparable](new CustomComparable(_), (x, y) => x.compareTo(y), false)
      test[Int](_.toInt, (x, y) => x.compareTo(y))
      test[Long](_.toLong, (x, y) => x.compareTo(y))
      test[Double](_.toDouble, (x, y) => x.compareTo(y))
    }
  }

  @Test def binarySearch_on_comparables(): Unit = {
    // Test: binarySearch[T](list: List[Comparable[T]], T)
    def test[T <: AnyRef with Comparable[T]](toElem: Int => T): Unit = {
      val list = factory.empty[T]

      list.addAll(range.map(toElem).sorted)

      for (i <- Seq(range.head, range.last, range(range.size/3),
        range(range.size/2), range(3*range.size/5))) {
        assertEquals(i, ju.Collections.binarySearch(list, toElem(i)))
      }

      // If not found it should return: -(insertion point) - 1
      assertEquals(-1, ju.Collections.binarySearch(list, toElem(-1)))
      assertEquals(-1, ju.Collections.binarySearch(list, toElem(-42)))
      assertEquals(-range.size - 1,
        ju.Collections.binarySearch(list, toElem(range.last + 1)))
      assertEquals(-range.size - 1,
        ju.Collections.binarySearch(list, toElem(range.last + 42)))
      list.remove(range.last / 2)
      assertEquals(-(range.last / 2) - 1,
        ju.Collections.binarySearch(list, toElem(range.last / 2)))
    }

    test[jl.Integer](jl.Integer.valueOf)
    test[jl.Long](_.toLong)
    test[jl.Double](_.toDouble)
  }

  @Test def binarySearch_with_comparator(): Unit = {
    // Test: binarySearch[T](List[T], key: T, Comparator[T]))
    def test[T](toElem: Int => T, cmpFun: (T, T) => Int): Unit = {
      val list = factory.empty[T]
      val cmp = new ju.Comparator[T] {
        override def compare(o1: T, o2: T): Int = cmpFun(o1, o2)
      }

      list.addAll(range.map(toElem).sortWith(cmpFun(_, _) < 0))

      for (i <- Seq(range.head, range.last, range(range.size/3),
        range(range.size/2), range(3*range.size/5))) {
        assertEquals(i, ju.Collections.binarySearch(list, toElem(i), cmp))
      }

      // If not found it should return: -(insertion point) - 1
      assertEquals(-1, ju.Collections.binarySearch(list, toElem(-1), cmp))
      assertEquals(-1, ju.Collections.binarySearch(list, toElem(-42), cmp))
      assertEquals(-range.size - 1,
          ju.Collections.binarySearch(list, toElem(range.last + 1), cmp))
      assertEquals(-range.size - 1,
          ju.Collections.binarySearch(list, toElem(range.last + 42), cmp))
      list.remove(range.last / 2)
      assertEquals(-(range.last / 2) - 1,
          ju.Collections.binarySearch(list, toElem(range.last / 2), cmp))
    }

    test[Int](_.toInt, (x, y) => x.compareTo(y))
    test[Long](_.toLong, (x, y) => x.compareTo(y))
    test[Double](_.toDouble, (x, y) => x.compareTo(y))
  }

  @Test def reverse(): Unit = {
    // Test: reverse(list: List[_])
    def test[T](toElem: Int => T): Unit = {
      val list = factory.empty[T]
      list.addAll(range.map(toElem))

      def testIfInOrder(reversed: Boolean): Unit = {
        for (i <- range) {
          val expected =
            if (reversed) range.last - i
            else i
          assertTrue(list(i) == toElem(expected))
        }
      }

      ju.Collections.reverse(list)
      testIfInOrder(true)

      ju.Collections.reverse(list)
      testIfInOrder(false)
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
    test[String](_.toString)
  }

  @Test def shuffle(): Unit = {
    def testShuffle(shuffle: ju.List[_] => Unit): Unit = {
      def test[E](toElem: Int => E): Unit = {
        val list = factory.empty[E]
        ju.Collections.shuffle(list)
        assertEquals(0, list.size)
        list.addAll(range.map(toElem))
        shuffle(list)
        assertEquals(range.size, list.size)
        assertTrue(list.containsAll(range.map(toElem)))
      }
      test[Int](_.toInt)
      test[Long](_.toLong)
      test[Double](_.toDouble)
      test[String](_.toString)
    }

    // Test: shuffle(list: List[_])
    // Relies on the correctness of shuffle(list: List[_], rnd: Random)
    // Tests for this version are omitted because they are not reproducible

    // Test: shuffle(list: List[_], rnd: Random)
    testShuffle(ju.Collections.shuffle(_, new ju.Random(0)))
    testShuffle(ju.Collections.shuffle(_, new ju.Random(42)))
    testShuffle(ju.Collections.shuffle(_, new ju.Random(-1243)))
    testShuffle(ju.Collections.shuffle(_, new ju.Random(94325)))
  }

  @Test def swap(): Unit = {
    // Test: swap(List[_], Int, Int)
    def test[T](toElem: Int => T): Unit = {
      val list = factory.empty[T]
      list.addAll(range.map(toElem(_)))

      ju.Collections.swap(list, 0, 1)
      assertTrue(list.get(0) == toElem(1))
      assertTrue(list.get(1) == toElem(0))
      for (i <- range.drop(2))
        assertTrue(list.get(i) == toElem(i))

      ju.Collections.swap(list, 0, range.last)
      assertTrue(list.get(0) == toElem(range.last))
      assertTrue(list.get(1) == toElem(0))
      for (i <- range.drop(2).init)
        assertTrue(list.get(i) == toElem(i))
      assertTrue(list.get(range.last) == toElem(1))

      ju.Collections.swap(list, 0, range.last)
      assertTrue(list.get(0) == toElem(1))
      assertTrue(list.get(1) == toElem(0))
      for (i <- range.drop(2))
        assertTrue(list.get(i) == toElem(i))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
    test[String](_.toString)
  }

  @Test def fill(): Unit = {
    // Test: fill[T](List[T], T)
    def test[T](toElem: Int => T): Unit = {
      val list = factory.empty[T]
      list.addAll(range.map(toElem(_)))

      ju.Collections.fill(list, toElem(0))
      for (i <- range)
        assertTrue(list.get(i) == toElem(0))

      ju.Collections.fill(list, toElem(42))
      for (i <- range)
        assertTrue(list.get(i) == toElem(42))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
    test[String](_.toString)
  }

  @Test def copy(): Unit = {
    // Test: copy[T](List[T], List[T])
    def test[T](toElem: Int => T): Unit = {
      val source = factory.empty[T]
      val dest = factory.empty[T]

      // Lists of same size
      range.foreach(i => source.add(toElem(i)))
      range.foreach(i => dest.add(toElem(-i)))
      ju.Collections.copy(dest, source)
      for (i <- range)
        assertTrue(dest(i) == toElem(i))

      // source.size < dest.size
      source.clear()
      dest.clear()
      range.take(range.size / 2).foreach(i => source.add(toElem(i)))
      range.foreach(i => dest.add(toElem(-i)))
      ju.Collections.copy(dest, source)
      for (i <- range.take(range.size / 2))
        assertTrue(dest(i) == toElem(i))
      for (i <- range.drop(range.size / 2))
        assertTrue(dest(i) == toElem(-i))

      // source.size > dest.size
      source.clear()
      dest.clear()
      range.foreach(i => source.add(toElem(i)))
      range.take(range.size / 2).foreach(i => dest.add(toElem(-i)))
      expectThrows(classOf[IndexOutOfBoundsException], ju.Collections.copy(dest, source))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
    test[String](_.toString)
  }

  @Test def rotate(): Unit = {
    def modulo(a: Int, b: Int): Int = ((a % b) + b) % b
    def test[E](toElem: Int => E): Unit = {
      val list = factory.empty[E]
      list.addAll(range.map(toElem))

      ju.Collections.rotate(list, 0)
      for (i <- range)
        assertTrue(list(i) == toElem(i))

      ju.Collections.rotate(list, list.size)
      for (i <- range)
        assertTrue(list(i) == toElem(i))

      ju.Collections.rotate(list, 1)
      for (i <- range)
        assertTrue(list(i) == toElem(modulo(i - 1, range.size)))

      ju.Collections.rotate(list, 1)
      for (i <- range)
        assertTrue(list(i) == toElem(modulo(i - 2, range.size)))

      ju.Collections.rotate(list, -5)
      for (i <- range)
        assertTrue(list(i) == toElem(modulo(i + 3, range.size)))

      list.clear()
      list.addAll((0 until 6).map(toElem))
      ju.Collections.rotate(list, 2)
      for (i <- 0 until 6)
        assertTrue(list(i) == toElem(modulo(i - 2, 6)))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
    test[String](_.toString)
  }

  @Test def replaceAll(): Unit = {
    def test[E](toElem: Int => E): Unit = {
      val list = factory.empty[E]
      list.addAll(range.map(toElem))

      ju.Collections.replaceAll(list, toElem(range.last), toElem(0))
      for (i <- range.init)
        assertTrue(list(i) == toElem(i))
      assertTrue(list.last == toElem(0))

      ju.Collections.replaceAll(list, toElem(range(range.size - 2)), toElem(0))
      for (i <- range.dropRight(2))
        assertTrue(list(i) == toElem(i))
      assertTrue(list.dropRight(1).last == toElem(0))
      assertTrue(list.last == toElem(0))

      ju.Collections.replaceAll(list, toElem(0), toElem(-1))
      for (i <- range.tail.dropRight(2))
        assertTrue(list(i) == toElem(i))
      assertTrue(list.head == toElem(-1))
      assertTrue(list.dropRight(1).last == toElem(-1))
      assertTrue(list.last == toElem(-1))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
    test[String](_.toString)
  }

  @Test def indexOfSubList(): Unit = {
    def test[E](toElem: Int => E): Unit = {
      val source = factory.empty[E]
      val target = factory.empty[E]

      assertEquals(0, ju.Collections.indexOfSubList(source, target))

      source.addAll(range.map(toElem))
      assertEquals(0, ju.Collections.indexOfSubList(source, target))

      target.addAll(range.map(toElem))
      assertEquals(0, ju.Collections.indexOfSubList(source, target))

      source.addAll(range.map(toElem))
      assertEquals(0, ju.Collections.indexOfSubList(source, target))

      source.addAll(range.map(toElem))
      assertEquals(0, ju.Collections.indexOfSubList(source, target))

      source.remove(0)
      assertEquals(range.size - 1, ju.Collections.indexOfSubList(source, target))

      target.add(0, toElem(-5))
      assertEquals(-1, ju.Collections.indexOfSubList(source, target))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
    test[String](_.toString)
  }

  @Test def lastIndexOfSubList(): Unit = {
    def test[E](toElem: Int => E): Unit = {
      val source = factory.empty[E]
      val target = factory.empty[E]

      assertEquals(0, ju.Collections.lastIndexOfSubList(source, target))

      if (!executingInJVM) { // Issue #2079
        source.addAll(range.map(toElem))
        assertEquals(0, ju.Collections.lastIndexOfSubList(source, target))

        target.addAll(range.map(toElem))
        assertEquals(0, ju.Collections.lastIndexOfSubList(source, target))

        source.addAll(range.map(toElem))
        assertEquals(range.size, ju.Collections.lastIndexOfSubList(source, target))

        source.addAll(range.map(toElem))
        assertEquals(2 * range.size, ju.Collections.lastIndexOfSubList(source, target))

        source.remove(source.size - 1)
        assertEquals(range.size, ju.Collections.lastIndexOfSubList(source, target))

        target.add(0, toElem(-5))
        assertEquals(-1, ju.Collections.lastIndexOfSubList(source, target))
      }
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
    test[String](_.toString)
  }

  @Test def unmodifiableList(): Unit = {
    def test[E](toElem: Int => E): Unit = {
      val immuList = ju.Collections.unmodifiableList(factory.empty[E])
      testListImmutability(immuList, toElem(0))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
    test[String](_.toString)
  }
}

class CollectionsOnAbstractListTest extends CollectionsOnListTest {
  def factory: ListFactory = new AbstractListFactory
}

class CollectionsOnArrayListTest extends CollectionsOnListTest {
  def factory: ListFactory = new ArrayListFactory
}

class CollectionsOnLinkedListTest extends CollectionsOnListTest {
  def factory: ListFactory = new LinkedListFactory
}

class CollectionsOnCopyOnWriteArrayListTest extends CollectionsOnListTest {
  def factory: ListFactory = new CopyOnWriteArrayListFactory
}
