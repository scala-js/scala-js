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

import java.{util => ju, lang => jl}

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

import scala.reflect.ClassTag

import Utils._

trait CollectionTest {

  def factory: CollectionFactory

  @Test def shouldStoreStrings(): Unit = {
    val coll = factory.empty[String]

    assertEquals(0, coll.size())
    coll.add("one")
    assertEquals(1, coll.size())

    coll.clear()
    assertEquals(0, coll.size())
    assertFalse(coll.addAll(TrivialImmutableCollection[String]()))
    assertEquals(0, coll.size())

    assertTrue(coll.addAll(TrivialImmutableCollection("one")))
    assertEquals(1, coll.size())

    coll.clear()
    assertTrue(coll.addAll(TrivialImmutableCollection("one", "two", "one")))
    assertTrue(coll.size() >= 1)
  }

  @Test def shouldStoreIntegers(): Unit = {
    val coll = factory.empty[Int]

    assertEquals(0, coll.size())
    coll.add(1)
    assertEquals(1, coll.size())

    coll.clear()
    assertEquals(0, coll.size())
    assertFalse(coll.addAll(TrivialImmutableCollection[Int]()))
    assertEquals(0, coll.size())

    assertTrue(coll.addAll(TrivialImmutableCollection(1)))
    assertEquals(1, coll.size())

    coll.clear()
    assertTrue(coll.addAll(TrivialImmutableCollection(1, 2, 1)))
    assertTrue(coll.size() >= 1)
  }

  @Test def shouldStoreDoubles(): Unit = {
    val coll = factory.empty[Double]

    assertEquals(0, coll.size())
    coll.add(1.234)
    assertEquals(1, coll.size())

    coll.clear()
    assertEquals(0, coll.size())
    assertFalse(coll.addAll(TrivialImmutableCollection[Double]()))
    assertEquals(0, coll.size())

    assertTrue(coll.addAll(TrivialImmutableCollection(1.234)))
    assertEquals(1, coll.size())

    coll.clear()
    assertTrue(coll.addAll(TrivialImmutableCollection(1.234, 2.345, 1.234)))
    assertTrue(coll.size() >= 1)

    coll.clear()
    coll.add(+0.0)
    assertTrue(coll.contains(+0.0))
    assertFalse(coll.contains(-0.0))

    coll.clear()
    coll.add(-0.0)
    assertFalse(coll.contains(+0.0))
    assertTrue(coll.contains(-0.0))

    coll.clear()
    coll.add(Double.NaN)
    assertEquals(1, coll.size())
    assertTrue(coll.contains(Double.NaN))
  }

  @Test def shouldStoreCustomObjects(): Unit = {
    case class TestObj(num: Int) extends jl.Comparable[TestObj] {
      def compareTo(o: TestObj): Int =
        o.num.compareTo(num)
    }

    val coll = factory.empty[TestObj]

    coll.add(TestObj(100))
    assertEquals(1, coll.size())
    assertTrue(coll.contains(TestObj(100)))
    assertFalse(coll.contains(TestObj(200)))
  }

  @Test def shouldRemoveStoredElements(): Unit = {
    val coll = factory.empty[String]

    coll.add("one")
    coll.add("two")
    coll.add("three")
    coll.add("two")

    val initialSize = coll.size()
    assertFalse(coll.remove("four"))
    assertEquals(initialSize, coll.size())
    assertTrue(coll.remove("two"))
    assertEquals(initialSize - 1, coll.size())
    assertTrue(coll.remove("one"))
    assertEquals(initialSize - 2, coll.size())
  }

  @Test def shouldRemoveStoredElementsOnDoubleCornerCases(): Unit = {
    val coll = factory.empty[Double]

    coll.add(1.234)
    coll.add(2.345)
    coll.add(Double.NaN)
    coll.add(+0.0)
    coll.add(-0.0)

    // coll == ArrayCollection(1.234, 2.345, NaN, +0.0, -0.0)
    assertTrue(coll.remove(Double.NaN))
    // coll == ArrayCollection(1.234, 2.345, +0.0, -0.0)
    assertEquals(4, coll.size())
    assertTrue(coll.remove(2.345))
    // coll == ArrayCollection(1.234, +0.0, -0.0)
    assertEquals(3, coll.size())
    assertTrue(coll.remove(1.234))
    // coll == ArrayCollection(+0.0, -0.0)
    assertEquals(2, coll.size())
    assertTrue(coll.remove(-0.0))
    // coll == ArrayCollection(NaN, +0.0)
    assertEquals(1, coll.size())

    coll.clear()

    assertTrue(coll.isEmpty)
  }

  @Test def shouldBeClearedWithOneOperation(): Unit = {
    val coll = factory.empty[String]

    coll.add("one")
    coll.add("two")
    assertEquals(2, coll.size)
    coll.clear()
    assertEquals(0, coll.size)
  }

  @Test def shouldCheckContainedPresence(): Unit = {
    val coll = factory.empty[String]

    coll.add("one")
    assertTrue(coll.contains("one"))
    assertFalse(coll.contains("two"))
    if (factory.allowsNullElementQuery) {
      assertFalse(coll.contains(null))
    } else {
      expectThrows(classOf[Exception], coll.contains(null))
    }
  }

  @Test def shouldCheckContainedPresenceForDoubleCornerCases(): Unit = {
    val coll = factory.empty[Double]

    coll.add(-0.0)
    assertTrue(coll.contains(-0.0))
    assertFalse(coll.contains(+0.0))

    coll.clear()

    coll.add(+0.0)
    assertFalse(coll.contains(-0.0))
    assertTrue(coll.contains(+0.0))
  }

  @Test def shouldGiveProperIteratorOverElements(): Unit = {
    val coll = factory.empty[String]
    coll.add("one")
    coll.add("two")
    coll.add("three")
    coll.add("three")
    coll.add("three")

    assertIteratorSameElementsAsSetDupesAllowed("one", "two", "three")(
        coll.iterator())
  }

  @Test def removeIf(): Unit = {
    val coll = factory.fromElements[Int](42, 50, 12, 0, -45, 102, 32, 75)
    assertEquals(8, coll.size())

    assertTrue(coll.removeIf(new java.util.function.Predicate[Int] {
      def test(x: Int): Boolean = x >= 50
    }))
    assertEquals(5, coll.size())
    assertIteratorSameElementsAsSet(-45, 0, 12, 32, 42)(coll.iterator())

    assertFalse(coll.removeIf(new java.util.function.Predicate[Int] {
      def test(x: Int): Boolean = x >= 45
    }))
    assertEquals(5, coll.size())
    assertIteratorSameElementsAsSet(-45, 0, 12, 32, 42)(coll.iterator())
  }
}

trait CollectionFactory {
  def implementationName: String
  def empty[E: ClassTag]: ju.Collection[E]
  def allowsMutationThroughIterator: Boolean = true
  def allowsNullElementQuery: Boolean = true

  def fromElements[E: ClassTag](elems: E*): ju.Collection[E] = {
    val coll = empty[E]
    coll.addAll(TrivialImmutableCollection(elems: _*))
    coll
  }
}
