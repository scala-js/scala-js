/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju, lang => jl}

import org.junit.Test
import org.junit.Assert._

import scala.collection.JavaConversions._

import org.scalajs.testsuite.utils.AssertThrows._

import scala.reflect.ClassTag

trait CollectionTest {

  def factory: CollectionFactory

  def testCollectionApi(): Unit = {
    shouldStoreStrings()
    shouldStoreIntegers()
    shouldStoreDoubles()
    shouldStoreCustomObjects()
    shouldRemoveStoredElements()
    shouldRemoveStoredElementsOnDoubleCornerCases()
    shouldBeClearedWithOneOperation()
    shouldCheckContainedPresence()
    shouldCheckContainedPresenceForDoubleCornerCases()
    shouldGiveProperIteratorOverElements()
  }

  @Test def shouldStoreStrings(): Unit = {
    val coll = factory.empty[String]

    assertEquals(0, coll.size())
    coll.add("one")
    assertEquals(1, coll.size())

    coll.clear()
    assertEquals(0, coll.size())
    assertFalse(coll.addAll(Seq.empty[String]))
    assertEquals(0, coll.size())

    assertTrue(coll.addAll(Seq("one")))
    assertEquals(1, coll.size())

    coll.clear()
    assertTrue(coll.addAll(Seq("one", "two", "one")))
    assertTrue(coll.size() >= 1)
  }

  @Test def shouldStoreIntegers(): Unit = {
    val coll = factory.empty[Int]

    assertEquals(0, coll.size())
    coll.add(1)
    assertEquals(1, coll.size())

    coll.clear()
    assertEquals(0, coll.size())
    assertFalse(coll.addAll(Seq.empty[Int]))
    assertEquals(0, coll.size())

    assertTrue(coll.addAll(Seq(1)))
    assertEquals(1, coll.size())

    coll.clear()
    assertTrue(coll.addAll(Seq(1, 2, 1)))
    assertTrue(coll.size() >= 1)
  }

  @Test def shouldStoreDoubles(): Unit = {
    val coll = factory.empty[Double]

    assertEquals(0, coll.size())
    coll.add(1.234)
    assertEquals(1, coll.size())

    coll.clear()
    assertEquals(0, coll.size())
    assertFalse(coll.addAll(Seq.empty[Double]))
    assertEquals(0, coll.size())

    assertTrue(coll.addAll(Seq(1.234)))
    assertEquals(1, coll.size())

    coll.clear()
    assertTrue(coll.addAll(Seq(1.234, 2.345, 1.234)))
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

    assertEquals(coll.iterator().toSet, Set("one", "two", "three"))
  }
}

object CollectionFactory {
  def allFactories: Iterator[CollectionFactory] =
    ListFactory.allFactories ++ SetFactory.allFactories
}

trait CollectionFactory {
  def implementationName: String
  def empty[E: ClassTag]: ju.Collection[E]
  def allowsMutationThroughIterator: Boolean = true
  def allowsNullElementQuery: Boolean = true
}
