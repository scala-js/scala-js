/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.util

import org.junit.Test
import org.junit.Assert._

import scala.collection.JavaConversions._

import java.{util => ju}

import scala.reflect.ClassTag

class ArrayDequeTest extends AbstractCollectionTest with DequeTest {

  override def factory: ArrayDequeFactory = new ArrayDequeFactory

  @Test def should_add_and_remove_head_and_last(): Unit = {
    val ad = factory.empty[Int]

    ad.addLast(1)
    ad.removeFirst()
    ad.addLast(2)
    assertEquals(ad.peekFirst(), 2)

    ad.clear()

    ad.addFirst(1)
    ad.removeLast()
    ad.addFirst(2)
    assertEquals(ad.peekLast(), 2)
  }

  @Test def could_be_instantiated_with_a_prepopulated_Collection(): Unit = {
    val s = Seq(1, 5, 2, 3, 4)
    val l = asJavaCollection(s)
    val ad = factory.from[Int](l)

    assertEquals(ad.size(), 5)

    for (i <- 0 until s.size)
      assertEquals(ad.poll(), s(i))

    assertTrue(ad.isEmpty)
  }

  @Test def should_add_multiple_element_in_one_operation(): Unit = {
    val l = asJavaCollection(Set(1, 5, 2, 3, 4))
    val ad = factory.empty[Int]

    assertEquals(ad.size(), 0)
    ad.addAll(l)
    assertEquals(ad.size(), 5)
    ad.add(6)
    assertEquals(ad.size(), 6)
  }

  @Test def should_retrieve_last_element(): Unit = {
    val adInt = factory.empty[Int]

    assertTrue(adInt.add(1000))
    assertTrue(adInt.add(10))
    assertEquals(adInt.pollLast(), 10)

    val adString = factory.empty[String]

    assertTrue(adString.add("pluto"))
    assertTrue(adString.add("pippo"))
    assertEquals(adString.pollLast(), "pippo")

    val adDouble = factory.empty[Double]

    assertTrue(adDouble.add(+10000.987))
    assertTrue(adDouble.add(-0.987))
    assertEquals(adDouble.pollLast(), -0.987, 0.0)
  }

  @Test def should_perform_as_a_stack_with_push_and_pop(): Unit = {
    val adInt = factory.empty[Int]

    adInt.push(1000)
    adInt.push(10)
    assertEquals(adInt.pop(), 10)
    assertEquals(adInt.pop(), 1000)
    assertTrue(adInt.isEmpty())

    val adString = factory.empty[String]

    adString.push("pluto")
    adString.push("pippo")
    assertEquals(adString.pop(), "pippo")
    assertEquals(adString.pop(), "pluto")
    assertTrue(adString.isEmpty())

    val adDouble = factory.empty[Double]

    adDouble.push(+10000.987)
    adDouble.push(-0.987)
    assertEquals(adDouble.pop(), -0.987, 0.0)
    assertEquals(adDouble.pop(), +10000.987, 0.0)
    assertTrue(adString.isEmpty())
  }

  @Test def should_poll_and_peek_elements(): Unit = {
    val pq = factory.empty[String]

    assertTrue(pq.add("one"))
    assertTrue(pq.add("two"))
    assertTrue(pq.add("three"))

    assertTrue(pq.peek.equals("one"))
    assertTrue(pq.poll.equals("one"))

    assertTrue(pq.peekFirst.equals("two"))
    assertTrue(pq.pollFirst.equals("two"))

    assertTrue(pq.peekLast.equals("three"))
    assertTrue(pq.pollLast.equals("three"))

    assertNull(pq.peekFirst)
    assertNull(pq.pollFirst)

    assertNull(pq.peekLast)
    assertNull(pq.pollLast)
  }

  @Test def should_remove_occurrences_of_provided_elements(): Unit = {
    val l = asJavaCollection(Seq("one", "two", "three", "two", "one"))
    val ad = factory.from[String](l)

    assertTrue(ad.removeFirstOccurrence("one"))
    assertTrue(ad.removeLastOccurrence("two"))
    assertTrue(ad.removeFirstOccurrence("one"))
    assertTrue(ad.removeLastOccurrence("two"))
    assertTrue(ad.removeFirstOccurrence("three"))
    assertFalse(ad.removeLastOccurrence("three"))
    assertTrue(ad.isEmpty)
  }

  @Test def should_iterate_over_elements_in_both_directions(): Unit = {
    val s = Seq("one", "two", "three")
    val l = asJavaCollection(s)
    val ad = factory.from[String](l)

    val iter = ad.iterator()
    for (i <- 0 until l.size()) {
      assertTrue(iter.hasNext())
      assertEquals(iter.next(), s(i))
    }
    assertFalse(iter.hasNext())

    val diter = ad.descendingIterator()
    for (i <- (0 until l.size()).reverse) {
      assertTrue(diter.hasNext())
      assertEquals(diter.next(), s(i))
    }
    assertFalse(diter.hasNext())
  }
}

object ArrayDequeFactory {
  def allFactories: Iterator[ArrayDequeFactory] =
    Iterator(new ArrayDequeFactory())
}

class ArrayDequeFactory extends AbstractCollectionFactory with DequeFactory {
  override def implementationName: String =
    "java.util.ArrayDeque"

  override def empty[E: ClassTag]: ju.ArrayDeque[E] =
    new ju.ArrayDeque[E]

  def from[E](coll: ju.Collection[E]): ju.ArrayDeque[E] =
    new ju.ArrayDeque[E](coll)
}
