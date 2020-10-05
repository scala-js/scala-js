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

package org.scalajs.testsuite.javalib.util.concurrent

import java.{util => ju}

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.javalib.util.{ListFactory, ListTest}
import org.scalajs.testsuite.javalib.util.TrivialImmutableCollection

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

    assertEquals(3, list.addAllAbsent(TrivialImmutableCollection((0 until 3): _*)))
    assertEquals(3, list.size)
    for (i <- 0 until 3)
      assertEquals(i, list.get(i))

    assertEquals(0, list.addAllAbsent(TrivialImmutableCollection((0 until 2): _*)))
    assertEquals(3, list.size)
    for (i <- 0 until 3)
      assertEquals(i, list.get(i))

    assertEquals(3, list.addAllAbsent(TrivialImmutableCollection((3 until 6): _*)))
    assertEquals(6, list.size)
    for (i <- 0 until 6)
      assertEquals(i, list.get(i))

    assertEquals(4, list.addAllAbsent(TrivialImmutableCollection((0 until 10): _*)))
    assertEquals(10, list.size)
    for (i <- 0 until 10)
      assertEquals(i, list.get(i))

    assertEquals(1, list.addAllAbsent(TrivialImmutableCollection(42, 42, 42)))
    assertEquals(11, list.size)
    for (i <- 0 until 10)
      assertEquals(i, list.get(i))
    assertEquals(42, list.get(10))
  }

  @Test def should_implement_a_snapshot_iterator(): Unit = {
    val list = factory.empty[Int]
    list.addAll(TrivialImmutableCollection((0 to 10): _*))

    val iter = list.iterator()
    list.clear()
    val iter2 = list.iterator()
    list.addAll(TrivialImmutableCollection((0 to 5): _*))

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
      assertEquals(arr.length, cowal1.size)
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
}
