/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util.concurrent

import java.util.concurrent.ConcurrentLinkedQueue
import java.{util => ju}

import org.junit.Assert._
import org.junit.Test
import org.scalajs.testsuite.javalib.util.{AbstractCollectionFactory, AbstractCollectionTest}

import scala.collection.JavaConversions._
import scala.language.implicitConversions
import scala.reflect.ClassTag

class ConcurrentLinkedQueueTest extends AbstractCollectionTest {

  override def factory: ConcurrentLinkedQueueFactory = new ConcurrentLinkedQueueFactory

  @Test def should_store_and_remove_ordered_integers(): Unit = {
    val pq = factory.empty[Int]

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

  @Test def should_store_and_remove_strings(): Unit = {
    val pq = factory.empty[String]

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

  @Test def should_store_Double_even_in_corner_cases(): Unit = {
    val pq = factory.empty[Double]

    assertTrue(pq.add(1.0))
    assertTrue(pq.add(+0.0))
    assertTrue(pq.add(-0.0))
    assertTrue(pq.add(Double.NaN))

    assertTrue(pq.poll.equals(1.0))

    assertTrue(pq.poll.equals(+0.0))

    assertTrue(pq.poll.equals(-0.0))

    assertTrue(pq.peek.isNaN)

    assertTrue(pq.remove(Double.NaN))

    assertTrue(pq.isEmpty)
  }

  @Test def could_be_instantiated_with_a_prepopulated_Collection(): Unit = {
    val l = asJavaCollection(Set(1, 5, 2, 3, 4))
    val pq = factory.newFrom(l)

    assertEquals(5, pq.size())
    for (i <- l) {
      assertEquals(i, pq.poll())
    }
    assertTrue(pq.isEmpty)
  }

  @Test def should_be_cleared_in_a_single_operation(): Unit = {
    val l = asJavaCollection(Set(1, 5, 2, 3, 4))
    val pq = factory.newFrom(l)

    assertEquals(5, pq.size())
    pq.clear()
    assertEquals(0, pq.size())
  }

  @Test def should_add_multiple_elemnt_in_one_operation(): Unit = {
    val l = asJavaCollection(Set(1, 5, 2, 3, 4))
    val pq = factory.empty[Int]

    assertEquals(0, pq.size())
    pq.addAll(l)
    assertEquals(5, pq.size())
    pq.add(6)
    assertEquals(6, pq.size())
  }

  @Test def should_check_contained_values_even_in_double_corner_cases(): Unit = {
    val pq = factory.empty[Double]

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

  @Test def should_provide_a_weakly_consistent_iterator(): Unit = {
    val queue = factory.empty[Int]
    queue.add(1)
    queue.add(2)
    val iter1 = queue.iterator()
    assertEquals(1, iter1.next())
    assertTrue(iter1.hasNext)
    queue.remove(2)
    assertTrue(iter1.hasNext)
    assertEquals(2, iter1.next())
    assertFalse(iter1.hasNext)

    val queue2 = factory.empty[Int]
    queue2.add(1)
    queue2.add(2)
    queue2.add(3)
    val iter2 = queue2.iterator()
    assertEquals(1, iter2.next())
    iter2.remove()
    assertEquals(2, iter2.next())
    assertEquals(3, iter2.next())
  }
}

class ConcurrentLinkedQueueFactory extends AbstractCollectionFactory {
  override def implementationName: String =
    "java.util.concurrent.ConcurrentLinkedQueue"

  override def empty[E: ClassTag]: ConcurrentLinkedQueue[E] =
    new ConcurrentLinkedQueue[E]()

  def newFrom[E](coll: ju.Collection[E]): ConcurrentLinkedQueue[E] =
    new ConcurrentLinkedQueue[E](coll)
}
