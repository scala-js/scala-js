/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util.concurrent

import java.{util => ju}

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.javalib.util.{ListFactory, ListTest}

import scala.collection.JavaConversions._

import org.scalajs.testsuite.utils.Platform.executingInJVM

import scala.reflect.ClassTag

class CopyOnWriteArrayListTest extends ListTest {

  def factory: CopyOnWriteArrayListFactory = new CopyOnWriteArrayListFactory

  @Test def should_implement_addIfAbsent(): Unit = {
    val list = factory.empty[Int]

    assertTrue(list.addIfAbsent(0))
    assertEquals(1, list.size)
    assertEquals(0, list.get(0))

    assertFalse(list.addIfAbsent(0))
    assertEquals(1, list.size)
    assertEquals(0, list.get(0))

    assertTrue(list.addIfAbsent(1))
    assertEquals(2, list.size)
    assertEquals(0, list.get(0))
    assertEquals(1, list.get(1))
  }

  @Test def should_implement_addAllAbsent(): Unit = {
    val list = factory.empty[Int]

    assertEquals(3, list.addAllAbsent(0 until 3))
    assertEquals(3, list.size)
    for (i <- 0 until 3)
      assertEquals(i, list.get(i))

    assertEquals(0, list.addAllAbsent(0 until 2))
    assertEquals(3, list.size)
    for (i <- 0 until 3)
      assertEquals(i, list.get(i))

    assertEquals(3, list.addAllAbsent(3 until 6))
    assertEquals(6, list.size)
    for (i <- 0 until 6)
      assertEquals(i, list.get(i))

    assertEquals(4, list.addAllAbsent(0 until 10))
    assertEquals(10, list.size)
    for (i <- 0 until 10)
      assertEquals(i, list.get(i))

    assertEquals(1, list.addAllAbsent(Seq(42, 42, 42)))
    assertEquals(11, list.size)
    for (i <- 0 until 10)
      assertEquals(i, list.get(i))
    assertEquals(42, list.get(10))
  }

  @Test def should_implement_a_snapshot_iterator(): Unit = {
    val list = factory.empty[Int]
    list.addAll(0 to 10)

    val iter = list.iterator()
    list.clear()
    val iter2 = list.iterator()
    list.addAll(0 to 5)

    for (i <- 0 to 10) {
      assertTrue(iter.hasNext)
      if (iter.hasNext)
        assertEquals(i, iter.next())
    }
    assertFalse(iter2.hasNext)
  }

  @Test def `should_have_accessible_array_constructor_-_#2023`(): Unit = {
    def test[T <: AnyRef](arr: Array[T]): Unit = {
      val cowal1 = factory.newFrom(arr)
      assertEquals(arr.length, cowal1.length)
      for (i <- arr.indices)
        assertEquals(arr(i), cowal1.get(i))
    }

    test(Array("a", "", "da", "23"))
    test(Array[Integer](1, 7, 2, 5, 3))
    test(Array[Character]('a', '3', '5', 'g', 'a'))
  }
}

class CopyOnWriteArrayListFactory extends ListFactory {

  override def allowsMutationThroughIterator: Boolean = false

  override def implementationName: String =
    "java.util.concurrent.CopyOnWriteArrayList"

  override def empty[E: ClassTag]: ju.concurrent.CopyOnWriteArrayList[E] =
    new ju.concurrent.CopyOnWriteArrayList[E]

  def newFrom[E <: AnyRef](arr: Array[E]): ju.concurrent.CopyOnWriteArrayList[E] =
    new ju.concurrent.CopyOnWriteArrayList[E](arr)

  // Sorting a CopyOnWriteArrayListFactory was not supported until JDK8.
  // See CollectionsOnCopyOnWriteArrayListTestOnJDK8.
  override def sortableUsingCollections: Boolean = false
}
