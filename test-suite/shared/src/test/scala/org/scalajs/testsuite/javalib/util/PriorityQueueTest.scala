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

package org.scalajs.testsuite.javalib.util

import scala.reflect.ClassTag

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import java.util.PriorityQueue
import java.util.Comparator

import org.scalajs.testsuite.utils.Platform.executingInJVM

class PriorityQueueTest extends CollectionTest {
  def factory: PriorityQueueFactory = new PriorityQueueFactory

  @Test def addAndRemoveInt(): Unit = {
    val pq = new PriorityQueue[Int]()

    assertEquals(0, pq.size())
    assertTrue(pq.add(111))
    assertEquals(1, pq.size())
    assertTrue(pq.add(222))
    assertEquals(2, pq.size())
    assertEquals(111, pq.poll())
    assertEquals(1, pq.size())
    assertEquals(222, pq.poll())
    assertTrue(pq.add(222))
    assertTrue(pq.add(222))
    assertTrue(pq.remove(222))
    assertTrue(pq.remove(222))
    assertFalse(pq.remove(222))
  }

  @Test def addAndRemoveString(): Unit = {
    val pq = new PriorityQueue[String]()

    assertEquals(0, pq.size())
    assertTrue(pq.add("aaa"))
    assertEquals(1, pq.size())
    assertTrue(pq.add("bbb"))
    assertEquals(2, pq.size())
    assertEquals("aaa", pq.poll())
    assertEquals(1, pq.size())
    assertEquals("bbb", pq.poll())
    assertTrue(pq.add("bbb"))
    assertTrue(pq.add("bbb"))
    assertTrue(pq.remove("bbb"))
    assertTrue(pq.remove("bbb"))
    assertFalse(pq.remove("bbb"))
    assertNull(pq.poll())
  }

  @Test def addAndRemoveObjectWithCustomComparator(): Unit = {
    case class Rect(x: Int, y: Int)

    val areaComp = new Comparator[Rect] {
      def compare(a: Rect, b: Rect): Int = (a.x*a.y) - (b.x*b.y)
    }

    val pq = new PriorityQueue[Rect](11, areaComp)

    assertTrue(pq.add(Rect(1,2)))
    assertTrue(pq.add(Rect(2,3)))
    assertTrue(pq.add(Rect(1,3)))

    val first = pq.poll()
    assertEquals(1, first.x)
    assertEquals(2, first.y)

    assertFalse(pq.remove(first))

    val second = pq.peek()
    assertEquals(1, second.x)
    assertEquals(3, second.y)

    assertTrue(pq.remove(second))
    assertFalse(pq.remove(second))

    val third = pq.peek()
    assertEquals(2, third.x)
    assertEquals(3, third.y)

    assertTrue(pq.remove(third))
    assertFalse(pq.remove(third))

    assertTrue(pq.isEmpty)

    assertTrue(pq.peek() eq null)
    assertTrue(pq.poll() eq null)
  }

  @Test def addAndRemoveDoubleCornerCases(): Unit = {
    val pq = new PriorityQueue[Double]()

    assertTrue(pq.add(1.0))
    assertTrue(pq.add(+0.0))
    assertTrue(pq.add(-0.0))
    assertTrue(pq.add(Double.NaN))

    assertTrue(pq.poll.equals(-0.0))

    assertTrue(pq.poll.equals(+0.0))

    assertTrue(pq.poll.equals(1.0))

    assertTrue(pq.peek.isNaN)

    assertTrue(pq.remove(Double.NaN))

    assertTrue(pq.isEmpty)
  }

  @Test def ctorCollectionInt(): Unit = {
    val l = TrivialImmutableCollection(1, 5, 2, 3, 4)
    val pq = new PriorityQueue[Int](l)

    assertEquals(5, pq.size())
    for (i <- 1 to 5) {
      assertEquals(i, pq.poll())
    }
    assertTrue(pq.isEmpty)
  }

  @Test def ctorPriorityQueueInt(): Unit = {
    val l = TrivialImmutableCollection(1, 5, 2, 3, 4)
    val pq1 = new PriorityQueue[Int](l)
    val pq2 = new PriorityQueue[Int](pq1)

    assertEquals(5, pq1.size())
    assertEquals(5, pq2.size())
    for (i <- 1 to 5) {
      assertEquals(pq2.poll(), pq1.poll())
    }
    assertTrue(pq1.isEmpty)
    assertTrue(pq2.isEmpty)
  }

  @Test def ctorSortedSetInt(): Unit = {
    val l = TrivialImmutableCollection(1, 5, 2, 3, 4)
    val ss = new java.util.concurrent.ConcurrentSkipListSet[Int](l)
    val pq1 = new PriorityQueue[Int](l)
    val pq2 = new PriorityQueue[Int](ss)

    assertEquals(5, pq1.size())
    assertEquals(5, pq2.size())
    for (i <- 1 to 5) {
      assertEquals(pq2.poll(), pq1.poll())
    }
    assertTrue(pq1.isEmpty)
    assertTrue(pq2.isEmpty)
  }

  @Test def testClear(): Unit = {
    val l = TrivialImmutableCollection(1, 5, 2, 3, 4)
    val pq = new PriorityQueue[Int](l)

    assertEquals(5, pq.size())
    pq.clear()
    assertEquals(0, pq.size())
  }

  @Test def addAllCollectionIntAndAddInt(): Unit = {
    val l = TrivialImmutableCollection(1, 5, 2, 3, 4)
    val pq = new PriorityQueue[Int]()

    assertEquals(0, pq.size())
    pq.addAll(l)
    assertEquals(5, pq.size())
    pq.add(6)
    assertEquals(6, pq.size())
  }

  @Test def containsDoubleCornerCasesPriorityQueue(): Unit = {
    val pq = new PriorityQueue[Double]()

    assertTrue(pq.add(11111.0))
    assertEquals(1, pq.size())
    assertTrue(pq.contains(11111.0))
    assertEquals(11111.0, pq.iterator.next(), 0.0)

    assertTrue(pq.add(Double.NaN))
    assertEquals(2, pq.size())
    assertTrue(pq.contains(Double.NaN))
    assertFalse(pq.contains(+0.0))
    assertFalse(pq.contains(-0.0))

    assertTrue(pq.remove(Double.NaN))
    assertTrue(pq.add(+0.0))
    assertEquals(2, pq.size())
    assertFalse(pq.contains(Double.NaN))
    assertTrue(pq.contains(+0.0))
    assertFalse(pq.contains(-0.0))

    assertTrue(pq.remove(+0.0))
    assertTrue(pq.add(-0.0))
    assertEquals(2, pq.size())
    assertFalse(pq.contains(Double.NaN))
    assertFalse(pq.contains(+0.0))
    assertTrue(pq.contains(-0.0))

    assertTrue(pq.add(+0.0))
    assertTrue(pq.add(Double.NaN))
    assertTrue(pq.contains(Double.NaN))
    assertTrue(pq.contains(+0.0))
    assertTrue(pq.contains(-0.0))
  }

  @Test def pollIntStringDouble(): Unit = {
    val pqInt = new PriorityQueue[Int]()

    assertTrue(pqInt.add(1000))
    assertTrue(pqInt.add(10))
    assertEquals(10, pqInt.poll())

    val pqString = new PriorityQueue[String]()

    assertTrue(pqString.add("pluto"))
    assertTrue(pqString.add("pippo"))
    assertEquals("pippo", pqString.poll())

    val pqDouble = new PriorityQueue[Double]()

    assertTrue(pqDouble.add(+10000.987))
    assertTrue(pqDouble.add(-0.987))
    assertEquals(-0.987, pqDouble.poll(), 0.0)
  }

  @Test def pollIntEntireQueue(): Unit = {
    val pq = newPriorityQueueWith0Until100()

    var nextExpected = 0
    while (!pq.isEmpty()) {
      assertEquals(nextExpected, pq.poll())
      nextExpected += 1
    }
    assertEquals(100, nextExpected)
  }

  @Test def removePreservesPriorities(): Unit = {
    for (itemToRemove <- 0 until 100) {
      val pq = newPriorityQueueWith0Until100()

      assertTrue(pq.remove(itemToRemove))

      var nextExpected = 0
      while (!pq.isEmpty()) {
        if (nextExpected == itemToRemove)
          nextExpected += 1
        assertEquals(s"after removing $itemToRemove", nextExpected, pq.poll())
        nextExpected += 1
      }

      if (itemToRemove == 99)
        assertEquals(99, nextExpected)
      else
        assertEquals(100, nextExpected)
    }
  }

  @Test def iteratorRemovePreservesPriorities(): Unit = {
    for (itemToRemove <- 0 until 100) {
      val pq = newPriorityQueueWith0Until100()

      val iter = pq.iterator()
      while (iter.next() != itemToRemove) {}
      iter.remove()

      var nextExpected = 0
      while (!pq.isEmpty()) {
        if (nextExpected == itemToRemove)
          nextExpected += 1
        assertEquals(s"after removing $itemToRemove", nextExpected, pq.poll())
        nextExpected += 1
      }

      if (itemToRemove == 99)
        assertEquals(99, nextExpected)
      else
        assertEquals(100, nextExpected)
    }
  }

  // --- Begin Whitebox Tests ---

  /* The following tests are meant to test the behavior of `Iterator.remove()`
   * and subsequent iteration in very specific scenarios that are corner cases
   * of the current implementation.
   *
   * We could write the same tests as blackbox, but then we would not be able
   * to verify that they are indeed testing the scenarios they are supposed to
   * test.
   *
   * Therefore, these tests are whitebox, and rely on details of our own
   * implementation. They verify that elements are iterated in a very specific
   * order, and that the internal representation of the heap evolves in the way
   * we expect.
   *
   * Since the following tests are whitebox, they may have to be changed if the
   * internal data structure and/or algorithms of PriorityQueue are modified in
   * the future.
   */

  @Test def iteratorRemovePreservesPrioritiesIntCornerCase(): Unit = {
    /* This tests the specific scenario where `Iterator.remove()` causes the
     * array to be reordered in such a way that a) elements yet to be iterated
     * are moved before the iteration cursor, and b) elements already iteratoed
     * are moved after the cursor.
     *
     * Due to the nature of a binary heap, triggering this scenario is quite
     * difficult, and does not easily happen by chance. The arrangement of
     * nodes has to be engineered in a specific way for the test to be
     * meaningful.
     */

    assumeFalse("whitebox test of our own implementation", executingInJVM)

    val pq = new java.util.PriorityQueue[Int]()
    for (x <- List(1, 2, 30, 4, 3, 40, 35, 10))
      pq.add(x)

    assertEquals("[1, 2, 30, 4, 3, 40, 35, 10]", pq.toString())

    val iter = pq.iterator()
    assertEquals(1, iter.next())
    assertEquals(2, iter.next())
    assertEquals(30, iter.next())
    assertEquals(4, iter.next())
    assertEquals(3, iter.next())
    assertEquals(40, iter.next())

    iter.remove()
    assertEquals("[1, 2, 10, 4, 3, 30, 35]", pq.toString())

    assertEquals(35, iter.next())
    assertEquals(10, iter.next())

    iter.remove()
    assertEquals("[1, 2, 30, 4, 3, 35]", pq.toString())

    assertFalse(iter.hasNext())
  }

  @Test def iteratorRemoveDoubleCornerCase(): Unit = {
    /* This tests that when `Iterator.remove()` is supposed to remove a zero,
     * it does not accidentally remove a zero of the opposite sign.
     *
     * To be meaningful, this tests requires that the zero we are trying to
     * remove be positioned further in the internal array than the other one.
     */

    assumeFalse("whitebox test of our own implementation", executingInJVM)

    val pq = new java.util.PriorityQueue[Double]()
    for (x <- List(-1.0, -0.0, 0.0, 3.5, Double.NaN))
      pq.add(x)

    assertEquals("[-1, 0, 0, 3.5, NaN]", pq.toString())

    val iter = pq.iterator()
    assertEquals(-1.0: Any, iter.next())
    assertEquals(-0.0: Any, iter.next())
    assertEquals(0.0: Any, iter.next())

    iter.remove()
    assertEquals("+0.0 must have been removed, not -0.0",
        "[-1, 0, NaN, 3.5]", pq.toString())
    assertTrue("+0.0 must have been removed, not -0.0",
        pq.contains(-0.0) && !pq.contains(0.0))

    assertEquals(3.5: Any, iter.next())
    assertEquals(Double.NaN: Any, iter.next())

    iter.remove()
    assertEquals("NaN must have been removed", "[-1, 0, 3.5]", pq.toString())

    assertFalse(iter.hasNext())
  }

  @Test def iteratorRemoveCustomObjectCornerCase(): Unit = {
    /* This tests that when `Iterator.remove()` is supposed to remove an
     * object, it does not accidentally remove an other object that happens to
     * be `equals` to it (but with a different identity).
     *
     * To be meaningful, this tests requires that the object we are trying to
     * remove be positioned further in the internal array than the other one.
     */

    assumeFalse("whitebox test of our own implementation", executingInJVM)

    final case class TestObj(i: Int)(val id: Int) extends Comparable[TestObj] {
      def compareTo(o: TestObj): Int = Integer.compare(this.i, o.i)

      override def toString(): String = s"TestObj@$id"
    }

    val first = TestObj(1)(10)
    val second = TestObj(2)(20)
    val third = TestObj(2)(21)
    val fourth = TestObj(3)(30)
    val fifth = TestObj(4)(40)

    val pq = new java.util.PriorityQueue[TestObj]()
    for (x <- List(first, second, third, fourth, fifth))
      pq.add(x)

    assertEquals(
        "[TestObj@10, TestObj@20, TestObj@21, TestObj@30, TestObj@40]",
        pq.toString())

    val iter = pq.iterator()
    assertSame(first, iter.next())
    assertSame(second, iter.next())
    assertSame(third, iter.next())

    iter.remove()
    assertEquals("third must have been removed, not second",
        "[TestObj@10, TestObj@20, TestObj@40, TestObj@30]", pq.toString())

    assertSame(fourth, iter.next())
    assertSame(fifth, iter.next())

    iter.remove()
    assertEquals("[TestObj@10, TestObj@20, TestObj@30]", pq.toString())

    assertFalse(iter.hasNext())
  }

  // --- End Whitebox Tests ---

  /** Built with `scala.util.Random.shuffle((0 until 100).toList)`. */
  private val listOfShuffled0Until100 = List(
      89, 26, 23, 9, 96, 81, 34, 79, 37, 90, 45, 66, 16, 49, 70, 77, 5, 19, 39,
      98, 44, 15, 1, 6, 43, 27, 40, 3, 68, 91, 76, 20, 54, 87, 85, 12, 86, 31,
      67, 24, 95, 0, 38, 22, 97, 28, 59, 2, 94, 7, 51, 30, 72, 56, 18, 13, 14,
      75, 53, 64, 47, 46, 58, 93, 74, 32, 57, 83, 60, 73, 11, 88, 69, 65, 33,
      52, 29, 80, 50, 63, 10, 62, 48, 55, 41, 35, 21, 42, 61, 36, 99, 78, 82,
      8, 4, 71, 25, 84, 92, 17
  )

  /** Creates a new priority queue in which the integers 0 until 100 have been
   *  added in a random order.
   *
   *  The random order is important to avoid creating a degenerate heap where
   *  the array is in strict increasing order. Such degenerate heaps tend to
   *  pass tests too easily, even with broken implementations.
   */
  private def newPriorityQueueWith0Until100(): PriorityQueue[Int] = {
    val pq = new PriorityQueue[Int]()
    for (i <- listOfShuffled0Until100)
      pq.add(i)
    pq
  }

}

class PriorityQueueFactory extends CollectionFactory {

  override def implementationName: String =
    "java.util.PriorityQueue"

  override def empty[E: ClassTag]: PriorityQueue[E] =
    new PriorityQueue()

  override def allowsNullElement: Boolean = false
}
