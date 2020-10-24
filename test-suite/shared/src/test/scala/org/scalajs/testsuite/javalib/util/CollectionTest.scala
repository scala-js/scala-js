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

import org.scalajs.testsuite.javalib.lang.IterableFactory
import org.scalajs.testsuite.javalib.lang.IterableTest
import org.scalajs.testsuite.utils.AssertThrows._

import scala.reflect.ClassTag

import Utils._

trait CollectionTest extends IterableTest {

  def factory: CollectionFactory

  @Test def testWithString(): Unit = {
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

  @Test def testWithInt(): Unit = {
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

  @Test def testWithDouble(): Unit = {
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

  @Test def testWithCustomClass(): Unit = {
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

  @Test def removeString(): Unit = {
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

  @Test def removeDoubleCornerCases(): Unit = {
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

  @Test def clear(): Unit = {
    val coll = factory.empty[String]

    coll.add("one")
    coll.add("two")
    assertEquals(2, coll.size)
    coll.clear()
    assertEquals(0, coll.size)
  }

  @Test def containsString(): Unit = {
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

  @Test def containsDoubleCornerCases(): Unit = {
    val coll = factory.empty[Double]

    coll.add(-0.0)
    assertTrue(coll.contains(-0.0))
    assertFalse(coll.contains(+0.0))

    coll.clear()

    coll.add(+0.0)
    assertFalse(coll.contains(-0.0))
    assertTrue(coll.contains(+0.0))
  }

  @Test def iteratorString(): Unit = {
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

  @Test def toStringCollectionDoubleEmpty(): Unit = {
    val coll = factory.empty[Double]
    assertEquals("[]", coll.toString())
  }

  @Test def toStringCollectionDoubleOneElement(): Unit = {
    val coll = factory.fromElements[Double](1.01)
    // JavaScript displays n.0 as n, so one trailing digit must be non-zero.
    assertEquals("[1.01]", coll.toString())
  }

  @Test def toStringCollectionDoubleHasCommaSpace(): Unit = {
    // Choose Doubles which display the same in Java and Scala.js.
    // JavaScript displays n.0 as n, so one trailing digit must be non-zero.
    val elements = Seq(88.42, -23.36, 60.173)

    val coll = factory.fromElements[Double](elements: _*)

    val result = coll.toString()

    // The order of elements returned by each collection is defined
    // by the collection. Be prepared to handle the general case of any
    // order here. Specific collections should test the order they specify.
    val expected = elements.permutations.map(_.mkString("[", ", ", "]")).toSet

    assertTrue(s"result '${result}' not in expected set '${expected}'",
        expected.contains(result))
  }

  @Test def toStringCollectionAnyWithNull(): Unit = {
    if (factory.allowsNullElement) {
      val elements = Seq(-1, -2, null, -3)

      val coll = factory.fromElements[Any](elements: _*)

      val result = coll.toString()

      val expected = elements.permutations.map(_.mkString("[", ", ", "]")).toSet
      assertTrue(s"result '${result}' not in expected set '${expected}'",
          expected.contains(result))
    }
  }

  @Test def toStringCollectionCustomClass(): Unit = {
    case class Custom(name: String, id: Int) extends Ordered[Custom] {
      def compare(that: Custom): Int = this.id - that.id
    }

    val elements = Seq(Custom("A", 1), Custom("b", 2), Custom("C", 3))

    val coll = factory.fromElements[Custom](elements: _*)

    val result = coll.toString()
    val expected = elements.permutations.map(_.mkString("[", ", ", "]")).toSet
    assertTrue(s"result '${result}' not in expected set '${expected}'",
        expected.contains(result))
  }

}

trait CollectionFactory extends IterableFactory {
  def empty[E: ClassTag]: ju.Collection[E]
  def allowsMutationThroughIterator: Boolean = true
  def allowsNullElementQuery: Boolean = true
  def allowsNullElement: Boolean = true

  override def fromElements[E: ClassTag](elems: E*): ju.Collection[E] = {
    val coll = empty[E]
    coll.addAll(TrivialImmutableCollection(elems: _*))
    coll
  }
}
