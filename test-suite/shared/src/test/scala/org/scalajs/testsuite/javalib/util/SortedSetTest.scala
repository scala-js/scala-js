/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import org.junit.Test
import org.junit.Assert._

import java.{util => ju}
import scala.collection.JavaConverters._
import scala.reflect.ClassTag

trait SortedSetTest extends SetTest {

  def factory: SortedSetFactory

  def testSortedSetApi(): Unit = {
    testSetApi()
    shouldRetrieveTheFirstElement()
    shouldRetrieveTheLastElement()
    shouldReturnAProperHeadSet()
    shouldReturnAProperTailSet()
    shouldReturnAProperSubSet()
  }

  @Test def shouldRetrieveTheFirstElement(): Unit = {
    val ssInt = factory.empty[Int]

    assertTrue(ssInt.add(1000))
    assertTrue(ssInt.add(10))
    assertEquals(10, ssInt.first)

    val ssString = factory.empty[String]

    assertTrue(ssString.add("pluto"))
    assertTrue(ssString.add("pippo"))
    assertEquals("pippo", ssString.first)

    val ssDouble = factory.empty[Double]

    assertTrue(ssDouble.add(+10000.987))
    assertTrue(ssDouble.add(-0.987))
    assertEquals(-0.987, ssDouble.first, 0.0)
  }

  @Test def shouldRetrieveTheLastElement(): Unit = {
    val ssInt = factory.empty[Int]

    assertTrue(ssInt.add(1000))
    assertTrue(ssInt.add(10))
    assertEquals(1000, ssInt.last)

    val ssString = factory.empty[String]

    assertTrue(ssString.add("pluto"))
    assertTrue(ssString.add("pippo"))
    assertEquals("pluto", ssString.last)

    val ssDouble = factory.empty[Double]

    assertTrue(ssDouble.add(+10000.987))
    assertTrue(ssDouble.add(-0.987))
    assertEquals(10000.987, ssDouble.last, 0.0)
  }

  val l = Set(1, 5, 2, 3, 4).asJavaCollection

  @Test def shouldReturnAProperHeadSet(): Unit = {
    val ss = factory.empty[Int]

    ss.addAll(l)

    val hs1 = ss.headSet(3)
    val l1 = Set(1,2).asJavaCollection
    assertTrue(hs1.containsAll(l1))
    assertTrue(hs1.removeAll(l1))
    assertTrue(hs1.isEmpty)
    assertEquals(3, ss.size)
    assertTrue(ss.containsAll(Set(3,4,5).asJavaCollection))

    ss.addAll(l)

    val hs2 = ss.headSet(4)
    val l2 = Set(1,2,3).asJavaCollection
    assertTrue(hs2.containsAll(l2))
    assertTrue(hs2.removeAll(l2))
    assertTrue(hs2.isEmpty)
    assertEquals(2, ss.size)
    assertTrue(ss.containsAll(Set(4,5).asJavaCollection))
  }

  @Test def shouldReturnAProperTailSet(): Unit = {
    val ss = factory.empty[Int]

    ss.addAll(l)

    val ts1 = ss.tailSet(3)
    val l3 = Set(3,4,5).asJavaCollection
    assertTrue(ts1.containsAll(l3))
    assertTrue(ts1.removeAll(l3))
    assertTrue(ts1.isEmpty)
    assertEquals(2, ss.size)
    assertTrue(ss.containsAll(Set(1,2).asJavaCollection))

    ss.addAll(l)

    val ts2 = ss.tailSet(4)
    val l4 = Set(4,5).asJavaCollection
    assertTrue(ts2.containsAll(l4))
    assertTrue(ts2.removeAll(l4))
    assertTrue(ts2.isEmpty)
    assertEquals(3, ss.size)
    assertTrue(ss.containsAll(Set(1,2,3).asJavaCollection))
  }

  @Test def shouldReturnAProperSubSet(): Unit = {
    val ss = factory.empty[Int]

    ss.addAll(l)

    val ss1 = ss.subSet(2, 4)
    val l5 = Set(2,3).asJavaCollection
    assertTrue(ss1.containsAll(l5))
    assertTrue(ss1.removeAll(l5))
    assertTrue(ss1.isEmpty)
    assertEquals(3, ss.size)
    assertTrue(ss.containsAll(Set(1,4,5).asJavaCollection))

    ss.addAll(l)

    val ss2 = ss.subSet(1, 5)
    assertTrue(ss2.containsAll(l5))
    assertTrue(ss2.removeAll(l5))
    assertFalse(ss2.isEmpty)
    assertEquals(3, ss.size)
    assertTrue(ss.containsAll(Set(1,4,5).asJavaCollection))
  }
}

object SortedSetFactory {
  def allFactories: Iterator[SortedSetFactory] =
    Iterator.empty
}

trait SortedSetFactory extends SetFactory {
  def empty[E: ClassTag]: ju.SortedSet[E]
}
