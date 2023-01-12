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

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows.{assertThrows, _}

import java.{util => ju, lang => jl}

import scala.reflect.ClassTag

import Utils._

trait SetTest extends CollectionTest {

  def factory: SetFactory

  @Test def size(): Unit = {
    val hs = factory.empty[String]

    assertEquals(0, hs.size())
    assertTrue(hs.add("ONE"))
    assertEquals(1, hs.size())
    assertTrue(hs.add("TWO"))
    assertEquals(2, hs.size())
  }

  @Test def addInt(): Unit = {
    val hs = factory.empty[Int]

    assertTrue(hs.add(100))
    assertEquals(1, hs.size())
    assertTrue(hs.contains(100))
    assertEquals(100, hs.iterator.next())
  }

  @Test def addAnyRefCustomObjectsWithSameHashCode(): Unit = {
    val hs = factory.empty[AnyRef]
    trait A extends Comparable[A] {
      def compareTo(o: A): Int = toString.compareTo(o.toString)
    }
    object B extends A {
      override def hashCode(): Int = 42
    }
    object C extends A {
      override def hashCode(): Int = 42
    }

    assertTrue(hs.add(B))
    assertTrue(hs.add(C))
    assertEquals(2, hs.size())
  }

  @Test def addDoubleCornerCases(): Unit = {
    val hs = factory.empty[Double]

    assertTrue(hs.add(11111.0))
    assertEquals(1, hs.size())
    assertTrue(hs.contains(11111.0))
    assertEquals(11111.0, hs.iterator.next(), 0.0)

    assertTrue(hs.add(Double.NaN))
    assertEquals(2, hs.size())
    assertTrue(hs.contains(Double.NaN))
    assertFalse(hs.contains(+0.0))
    assertFalse(hs.contains(-0.0))

    assertTrue(hs.remove(Double.NaN))
    assertTrue(hs.add(+0.0))
    assertEquals(2, hs.size())
    assertFalse(hs.contains(Double.NaN))
    assertTrue(hs.contains(+0.0))
    assertFalse(hs.contains(-0.0))

    assertTrue(hs.remove(+0.0))
    assertTrue(hs.add(-0.0))
    assertEquals(2, hs.size())
    assertFalse(hs.contains(Double.NaN))
    assertFalse(hs.contains(+0.0))
    assertTrue(hs.contains(-0.0))

    assertTrue(hs.add(+0.0))
    assertTrue(hs.add(Double.NaN))
    assertTrue(hs.contains(Double.NaN))
    assertTrue(hs.contains(+0.0))
    assertTrue(hs.contains(-0.0))
  }

  @Test def addCustomClass(): Unit = {
    case class TestObj(num: Int) extends jl.Comparable[TestObj] {
      override def compareTo(o: TestObj): Int = o.num - num
    }

    val hs = factory.empty[TestObj]

    assertTrue(hs.add(TestObj(100)))
    assertEquals(1, hs.size())
    assertTrue(hs.contains(TestObj(100)))
    assertEquals(100, hs.iterator.next().num)
  }

  @Test def removeRemoveAllRetainAll(): Unit = {
    val hs = factory.empty[String]

    assertEquals(0, hs.size())
    assertTrue(hs.add("ONE"))
    assertFalse(hs.add("ONE"))
    assertEquals(1, hs.size())
    assertTrue(hs.add("TWO"))
    assertEquals(2, hs.size())
    assertTrue(hs.remove("ONE"))
    assertFalse(hs.remove("ONE"))
    assertEquals(1, hs.size())
    assertTrue(hs.remove("TWO"))
    assertEquals(0, hs.size())

    assertTrue(hs.add("ONE"))
    assertTrue(hs.add("TWO"))
    assertEquals(2, hs.size())
    assertTrue(hs.removeAll(TrivialImmutableCollection("ONE", "TWO")))
    assertEquals(0, hs.size())

    assertTrue(hs.add("ONE"))
    assertTrue(hs.add("TWO"))
    assertEquals(2, hs.size())
    assertTrue(hs.retainAll(TrivialImmutableCollection("ONE", "THREE")))
    assertEquals(1, hs.size())
    assertTrue(hs.contains("ONE"))
    assertFalse(hs.contains("TWO"))
  }

  @Test def clearSet(): Unit = {
    val hs = factory.empty[String]

    assertTrue(hs.add("ONE"))
    assertTrue(hs.add("TWO"))
    assertEquals(2, hs.size())

    hs.clear()
    assertEquals(0, hs.size())
    assertTrue(hs.isEmpty)
  }

  @Test def contains(): Unit = {
    val hs = factory.empty[String]

    assertTrue(hs.add("ONE"))
    assertTrue(hs.contains("ONE"))
    assertFalse(hs.contains("TWO"))
    if (factory.allowsNullElement) {
      assertFalse(hs.contains(null))
      assertTrue(hs.add(null))
      assertTrue(hs.contains(null))
    } else {
      assertThrowsNPEIfCompliant(hs.add(null))
    }
  }

  @Test def addAllCollectionStringSet(): Unit = {
    val hs = factory.empty[String]

    val l = TrivialImmutableCollection("ONE", "TWO", null)

    if (factory.allowsNullElement) {
      assertTrue(hs.addAll(l))
      assertEquals(3, hs.size)
      assertTrue(hs.contains("ONE"))
      assertTrue(hs.contains("TWO"))
      assertTrue(hs.contains(null))
    } else {
      assertThrowsNPEIfCompliant(hs.addAll(l))
    }
  }

  @Test def iterator(): Unit = {
    val hs = factory.empty[String]

    val l = {
      if (factory.allowsNullElement)
        List("ONE", "TWO", null)
      else
        List("ONE", "TWO", "THREE")
    }
    assertTrue(hs.addAll(TrivialImmutableCollection(l: _*)))
    assertEquals(3, hs.size)

    assertIteratorSameElementsAsSet(l: _*)(hs.iterator())
  }
}

trait SetFactory extends CollectionFactory {
  def empty[E: ClassTag]: ju.Set[E]
}
