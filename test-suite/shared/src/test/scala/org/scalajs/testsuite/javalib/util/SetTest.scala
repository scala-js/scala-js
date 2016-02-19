/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import scala.language.implicitConversions

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

import java.{util => ju, lang => jl}

import scala.collection.JavaConversions._
import scala.reflect.ClassTag

trait SetTest extends CollectionTest {

  def factory: SetFactory

  def testSetApi(): Unit = {
    testCollectionApi()
    shouldCheckSetSize()
    shouldStoreIntegers_Set()
    shouldStoreObjectsWithSameHashCodeButDifferentTypes()
    shouldStoreDoublesAlsoInCornerCases()
    shouldStoreCustomObjects_Set()
    shouldRemoveStoredElements_Set()
    shouldBeClearedWithOneOperation_Set()
    shouldCheckContainedElemsPresence()
    shouldPutAWholeCollectionInto()
    shouldIterateOverElements()
  }

  @Test def shouldCheckSetSize(): Unit = {
    val hs = factory.empty[String]

    assertEquals(0, hs.size())
    assertTrue(hs.add("ONE"))
    assertEquals(1, hs.size())
    assertTrue(hs.add("TWO"))
    assertEquals(2, hs.size())
  }

  @Test def shouldStoreIntegers_Set(): Unit = {
    val hs = factory.empty[Int]

    assertTrue(hs.add(100))
    assertEquals(1, hs.size())
    assertTrue(hs.contains(100))
    assertEquals(100, hs.iterator.next())
  }

  @Test def shouldStoreObjectsWithSameHashCodeButDifferentTypes(): Unit = {
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

  @Test def shouldStoreDoublesAlsoInCornerCases(): Unit = {
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

  @Test def shouldStoreCustomObjects_Set(): Unit = {
    case class TestObj(num: Int) extends jl.Comparable[TestObj] {
      override def compareTo(o: TestObj): Int = o.num - num
    }

    val hs = factory.empty[TestObj]

    assertTrue(hs.add(TestObj(100)))
    assertEquals(1, hs.size())
    assertTrue(hs.contains(TestObj(100)))
    assertEquals(100, hs.iterator.next().num)
  }

  @Test def shouldRemoveStoredElements_Set(): Unit = {
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
    val l1 = List[String]("ONE", "TWO")
    assertTrue(hs.removeAll(asJavaCollection(l1)))
    assertEquals(0, hs.size())

    assertTrue(hs.add("ONE"))
    assertTrue(hs.add("TWO"))
    assertEquals(2, hs.size())
    val l2 = List[String]("ONE", "THREE")
    assertTrue(hs.retainAll(asJavaCollection(l2)))
    assertEquals(1, hs.size())
    assertTrue(hs.contains("ONE"))
    assertFalse(hs.contains("TWO"))
  }

  @Test def shouldBeClearedWithOneOperation_Set(): Unit = {
    val hs = factory.empty[String]

    assertTrue(hs.add("ONE"))
    assertTrue(hs.add("TWO"))
    assertEquals(2, hs.size())

    hs.clear()
    assertEquals(0, hs.size())
    assertTrue(hs.isEmpty)
  }

  @Test def shouldCheckContainedElemsPresence(): Unit = {
    val hs = factory.empty[String]

    assertTrue(hs.add("ONE"))
    assertTrue(hs.contains("ONE"))
    assertFalse(hs.contains("TWO"))
    if (factory.allowsNullElement) {
      assertFalse(hs.contains(null))
      assertTrue(hs.add(null))
      assertTrue(hs.contains(null))
    } else {
      expectThrows(classOf[Exception], hs.add(null))
    }
  }

  @Test def shouldPutAWholeCollectionInto(): Unit = {
    val hs = factory.empty[String]

    if (factory.allowsNullElement) {
      val l = List[String]("ONE", "TWO", (null: String))
      assertTrue(hs.addAll(asJavaCollection(l)))
      assertEquals(3, hs.size)
      assertTrue(hs.contains("ONE"))
      assertTrue(hs.contains("TWO"))
      assertTrue(hs.contains(null))
    } else {
      expectThrows(classOf[Exception], {
        val l = List[String]("ONE", "TWO", (null: String))
        hs.addAll(asJavaCollection(l))
      })
    }
  }

  @Test def shouldIterateOverElements(): Unit = {
    val hs = factory.empty[String]

    val l = {
      if (factory.allowsNullElement)
        List[String]("ONE", "TWO", (null: String))
      else
        List[String]("ONE", "TWO", "THREE")
    }
    assertTrue(hs.addAll(asJavaCollection(l)))
    assertEquals(3, hs.size)

    val iter = hs.iterator()
    val result = {
      for (i <- 0 until 3) yield {
        assertTrue(iter.hasNext())
        iter.next()
      }
    }
    assertFalse(iter.hasNext())
    assertTrue(result.containsAll(l))
    assertTrue(l.containsAll(result))
  }
}

object SetFactory {
  def allFactories: Iterator[SetFactory] =
    AbstractSetFactory.allFactories ++ SortedSetFactory.allFactories ++ NavigableSetFactory.allFactories
}

trait SetFactory extends CollectionFactory {
  def empty[E: ClassTag]: ju.Set[E]

  def allowsNullElement: Boolean
}
