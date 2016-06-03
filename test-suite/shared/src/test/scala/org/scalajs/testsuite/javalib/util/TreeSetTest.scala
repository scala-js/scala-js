/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import org.junit.Assert._

import scala.language.implicitConversions

import scala.collection.JavaConversions._

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

import java.{util => ju}
import ju.TreeSet
import ju.Comparator

import scala.reflect.ClassTag

class TreeSetWithoutNullTest extends TreeSetTest(new TreeSetFactory) {

  @Test def should_check_that_comparator_is_always_null(): Unit = {
    val ts1 = factory.empty[Int]

    assertNull(ts1.comparator())

    val ts2 = factory.empty[String]

    assertNull(ts2.comparator())
  }
}

class TreeSetWithNullTest extends TreeSetTest(new TreeSetWithNullFactory) {
  @Test def should_check_that_comparator_is_never_null(): Unit = {
    val ts1 = factory.empty[Int]

    assertFalse(ts1.comparator() == null)

    val ts2 = factory.empty[String]

    assertFalse(ts2.comparator() == null)
  }
}

abstract class TreeSetTest(val factory: TreeSetFactory)
    extends AbstractSetTest
    with SortedSetTest
    with NavigableSetTest {

  @Test def should_store_and_remove_ordered_integers(): Unit = {
    val ts = factory.empty[Int]

    assertEquals(0, ts.size())
    assertTrue(ts.add(222))
    assertEquals(1, ts.size())
    assertTrue(ts.add(111))
    assertEquals(2, ts.size())
    assertEquals(111, ts.first)
    assertTrue(ts.remove(111))

    assertEquals(1, ts.size())
    assertEquals(222, ts.first)

    assertTrue(ts.remove(222))
    assertEquals(0, ts.size())
    assertTrue(ts.isEmpty)
    assertFalse(ts.remove(333))
    expectThrows(classOf[NoSuchElementException], ts.first)

    if (factory.allowsNullElement) {
      assertTrue(ts.asInstanceOf[TreeSet[Any]].add(null))
      assertTrue(ts.contains(null))
      assertTrue(ts.remove(null))
      assertFalse(ts.contains(null))
    }
  }

  @Test def should_store_and_remove_ordered_strings(): Unit = {
    val ts = factory.empty[String]

    assertEquals(0, ts.size())
    assertTrue(ts.add("222"))
    assertEquals(1, ts.size())
    assertTrue(ts.add("111"))
    assertEquals(2, ts.size())
    assertEquals("111", ts.first)
    assertTrue(ts.remove("111"))

    assertEquals(1, ts.size())
    assertEquals("222", ts.first)

    assertTrue(ts.remove("222"))
    assertEquals(0, ts.size())
    assertFalse(ts.remove("333"))
    assertTrue(ts.isEmpty)

    if (factory.allowsNullElement) {
      assertTrue(ts.add(null))
      assertTrue(ts.contains(null))
      assertTrue(ts.remove(null))
      assertFalse(ts.contains(null))
    }
  }

  @Test def should_store_objects_with_custom_comparables(): Unit = {
    case class Rect(x: Int, y: Int)

    val areaComp = new ju.Comparator[Rect] {
      def compare(a: Rect, b: Rect): Int = (a.x * a.y) - (b.x * b.y)
    }

    val ts = factory.empty[Rect](areaComp)

    assertTrue(ts.add(Rect(1, 2)))
    assertTrue(ts.add(Rect(2, 3)))
    assertTrue(ts.add(Rect(1, 3)))

    val first = ts.first()
    assertEquals(1, first.x)
    assertEquals(2, first.y)

    assertTrue(ts.remove(first))
    assertFalse(ts.remove(first))

    val second = ts.first()
    assertEquals(1, second.x)
    assertEquals(3, second.y)

    assertTrue(ts.remove(second))

    val third = ts.first()
    assertEquals(2, third.x)
    assertEquals(3, third.y)

    assertTrue(ts.remove(third))

    assertTrue(ts.isEmpty)
  }

  @Test def should_store_ordered_Double_even_in_corner_cases(): Unit = {
    val ts = factory.empty[Double]

    assertTrue(ts.add(1.0))
    assertTrue(ts.add(+0.0))
    assertTrue(ts.add(-0.0))
    assertTrue(ts.add(Double.NaN))

    assertTrue(ts.first.equals(-0.0))

    assertTrue(ts.remove(-0.0))

    assertTrue(ts.first.equals(+0.0))

    assertTrue(ts.remove(+0.0))

    assertTrue(ts.first.equals(1.0))

    assertTrue(ts.remove(1.0))

    assertTrue(ts.first.isNaN)

    assertTrue(ts.remove(Double.NaN))

    assertTrue(ts.isEmpty)
  }

  @Test def could_be_instantiated_with_a_prepopulated_Collection(): Unit = {
    val l = asJavaCollection(Set(1, 5, 2, 3, 4))
    val ts = factory.newFrom(l)

    assertEquals(5, ts.size())
    for (i <- 1 to 5) {
      assertEquals(i, ts.first)
      assertTrue(ts.remove(i))
    }
    assertTrue(ts.isEmpty)
  }

  @Test def should_be_cleared_in_a_single_operation(): Unit = {
    val l = asJavaCollection(Set(1, 5, 2, 3, 4))
    val ts = factory.empty[Int]

    ts.addAll(l)

    assertEquals(5, ts.size())
    ts.clear()
    assertEquals(0, ts.size())
  }

  @Test def should_add_multiple_element_in_one_operation(): Unit = {
    val l = asJavaCollection(Set(1, 5, 2, 3, 4))
    val ts = factory.empty[Int]

    assertEquals(0, ts.size())
    ts.addAll(l)
    assertEquals(5, ts.size())
    ts.add(6)
    assertEquals(6, ts.size())
  }

  @Test def should_check_contained_values_even_in_double_corner_cases(): Unit = {
    val ts = factory.empty[Double]

    assertTrue(ts.add(11111.0))
    assertEquals(1, ts.size())
    assertTrue(ts.contains(11111.0))
    assertEquals(11111.0, ts.iterator.next(), 0.0)

    assertTrue(ts.add(Double.NaN))
    assertEquals(2, ts.size())
    assertTrue(ts.contains(Double.NaN))
    assertFalse(ts.contains(+0.0))
    assertFalse(ts.contains(-0.0))

    assertTrue(ts.remove(Double.NaN))
    assertTrue(ts.add(+0.0))
    assertEquals(2, ts.size())
    assertFalse(ts.contains(Double.NaN))
    assertTrue(ts.contains(+0.0))
    assertFalse(ts.contains(-0.0))

    assertTrue(ts.remove(+0.0))
    assertTrue(ts.add(-0.0))
    assertEquals(2, ts.size())
    assertFalse(ts.contains(Double.NaN))
    assertFalse(ts.contains(+0.0))
    assertTrue(ts.contains(-0.0))

    assertTrue(ts.add(+0.0))
    assertTrue(ts.add(Double.NaN))
    assertTrue(ts.contains(Double.NaN))
    assertTrue(ts.contains(+0.0))
    assertTrue(ts.contains(-0.0))
  }

  @Test def should_throws_exception_in_case_of_null_elements_and_default_ordering(): Unit = {
    val hs = factory.empty[String]

    assertTrue(hs.add("ONE"))
    assertTrue(hs.contains("ONE"))
    assertFalse(hs.contains("TWO"))

    if (factory.allowsNullElement) {
      assertTrue(hs.add(null))
      assertTrue(hs.contains(null))
    } else {
      expectThrows(classOf[Exception], hs.add(null))
    }
  }

  @Test def should_not_put_a_whole_Collection_with_null_elements_into(): Unit = {
    val l = List[String]("ONE", "TWO", (null: String))
    val ts1 = factory.empty[String]

    if (factory.allowsNullElement) {
      assertTrue(ts1.addAll(l))
      assertTrue(ts1.contains(null))
      assertTrue(ts1.contains("ONE"))
      assertFalse(ts1.contains("THREE"))
    } else {
      expectThrows(classOf[Exception], {
        ts1.addAll(asJavaCollection(l))
      })
    }
  }

  @Test def should_throw_exception_on_non_comparable_objects(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)
    assumeTrue("Assumed JDK8 implementation", !executingInJVMOnJDK6)

    class TestObj(num: Int)

    val ts1 = factory.empty[TestObj]
    assertEquals(0, ts1.size())
    expectThrows(classOf[ClassCastException], ts1.add(new TestObj(111)))
  }

  @Test def should_throw_exceptions_on_access_outside_bound_on_views(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    val l = asJavaCollection(Set(2, 3, 6))
    val ts = factory.empty[Int]
    ts.addAll(l)

    val hs1 = ts.headSet(5, true)
    assertTrue(hs1.add(4))
    assertTrue(hs1.add(5))
    expectThrows(classOf[IllegalArgumentException], hs1.add(6))

    ts.clear()
    ts.addAll(l)

    val hs2 = ts.headSet(5, false)
    assertTrue(hs2.add(4))
    expectThrows(classOf[IllegalArgumentException], hs2.add(5))

    ts.clear()
    ts.addAll(l)

    val ts1 = ts.tailSet(1, true)
    assertTrue(ts1.add(7))
    assertTrue(ts1.add(1))
    expectThrows(classOf[IllegalArgumentException], ts1.add(0))

    ts.clear()
    ts.addAll(l)

    val ts2 = ts.tailSet(1, false)
    assertTrue(ts2.add(7))
    expectThrows(classOf[IllegalArgumentException], ts2.add(1))

    ts.clear()
    ts.addAll(l)

    val ss1 = ts.subSet(1, true, 5, true)
    assertTrue(ss1.add(4))
    assertTrue(ss1.add(1))
    expectThrows(classOf[IllegalArgumentException], ss1.add(0))
    assertTrue(ss1.add(5))
    expectThrows(classOf[IllegalArgumentException], ss1.add(6))

    ts.clear()
    ts.addAll(l)

    val ss2 = ts.subSet(1, false, 5, false)
    assertTrue(ss2.add(4))
    expectThrows(classOf[IllegalArgumentException], ss2.add(1))
    expectThrows(classOf[IllegalArgumentException], ss2.add(5))
  }
}

object TreeSetFactory extends TreeSetFactory {
  def allFactories: Iterator[TreeSetFactory] =
    Iterator(new TreeSetFactory, new TreeSetWithNullFactory)
}

class TreeSetFactory extends AbstractSetFactory with NavigableSetFactory
    with SortedSetFactory {
  def implementationName: String =
    "java.util.TreeSet"

  def empty[E: ClassTag]: ju.TreeSet[E] =
    new TreeSet[E]

  def empty[E](cmp: ju.Comparator[E]): ju.TreeSet[E] =
    new TreeSet[E](cmp)

  def newFrom[E](coll: ju.Collection[E]): ju.TreeSet[E] =
    new TreeSet[E](coll)

  override def allowsNullElement: Boolean = false

  override def allowsNullElementQuery: Boolean = false
}

class TreeSetWithNullFactory extends TreeSetFactory {

  override def implementationName: String =
    super.implementationName + " {allows null}"

  case class EvenNullComp[E]() extends Comparator[E] {
    def compare(a: E, b: E): Int =
      (Option(a), Option(b)) match {
        case (Some(e1), Some(e2)) => e1.asInstanceOf[Comparable[E]].compareTo(e2)
        case (Some(e1), None) => -1
        case (None, Some(e2)) => 1
        case (None, None) => 0
      }
    }

  override def empty[E: ClassTag]: ju.TreeSet[E] =
    new TreeSet[E](EvenNullComp[E]())

  override def allowsNullElement: Boolean = true

  override def allowsNullElementQuery: Boolean = true
}
