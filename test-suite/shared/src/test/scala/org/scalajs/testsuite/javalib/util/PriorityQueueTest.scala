/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import scala.language.implicitConversions

import scala.collection.JavaConversions._

import org.junit.Test
import org.junit.Assert._

import java.util.PriorityQueue
import java.util.Comparator

class PriorityQueueTest {

  @Test def should_store_and_remove_ordered_integers(): Unit = {
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

  @Test def should_store_and_remove_ordered_strings(): Unit = {
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

  @Test def should_store_objects_with_custom_comparables(): Unit = {
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

  @Test def should_store_ordered_Double_even_in_corner_cases(): Unit = {
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

  @Test def could_be_instantiated_with_a_prepopulated_Collection(): Unit = {
    val l = asJavaCollection(Set(1, 5, 2, 3, 4))
    val pq = new PriorityQueue[Int](l)

    assertEquals(5, pq.size())
    for (i <- 1 to 5) {
      assertEquals(i, pq.poll())
    }
    assertTrue(pq.isEmpty)
  }

  @Test def could_be_instantiated_with_a_prepopulated_PriorityQueue(): Unit = {
    val l = asJavaCollection(Set(1, 5, 2, 3, 4))
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

  @Test def could_be_instantiated_with_a_prepopulated_SortedSet(): Unit = {
    val l = asJavaCollection(Set(1, 5, 2, 3, 4))
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

  @Test def should_be_cleared_in_a_single_operation(): Unit = {
    val l = asJavaCollection(Set(1, 5, 2, 3, 4))
    val pq = new PriorityQueue[Int](l)

    assertEquals(5, pq.size())
    pq.clear()
    assertEquals(0, pq.size())
  }

  @Test def should_add_multiple_elemnt_in_one_operation(): Unit = {
    val l = asJavaCollection(Set(1, 5, 2, 3, 4))
    val pq = new PriorityQueue[Int]()

    assertEquals(0, pq.size())
    pq.addAll(l)
    assertEquals(5, pq.size())
    pq.add(6)
    assertEquals(6, pq.size())
  }

  @Test def should_check_contained_values_even_in_double_corner_cases(): Unit = {
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

  @Test def should_retrieve_the_first_element(): Unit = {
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

}
