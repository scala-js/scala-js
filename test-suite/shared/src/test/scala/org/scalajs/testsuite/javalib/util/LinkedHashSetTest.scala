/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import org.junit.Test
import org.junit.Assert._

import scala.collection.JavaConversions._
import scala.reflect.ClassTag

class LinkedHashSetTest extends HashSetTest {

  override def factory: LinkedHashSetFactory = new LinkedHashSetFactory

  @Test def should_iterate_over_elements_in_an_ordered_manner(): Unit = {
    val hs = factory.empty[String]

    val l1 = List[String]("ONE", "TWO", (null: String))
    assertTrue(hs.addAll(asJavaCollection(l1)))
    assertEquals(3, hs.size)

    val iter1 = hs.iterator()
    val result1 = {
      for (i <- 0 until 3) yield {
        assertTrue(iter1.hasNext())
        val value = iter1.next()
        assertEquals(l1(i), value)
        value
      }
    }
    assertFalse(iter1.hasNext())
    assertEquals(l1, result1)

    val l2 = l1 :+ "THREE"
    assertTrue(hs.add(l2(3)))

    val iter2 = hs.iterator()
    val result2 = {
      for (i <- 0 until 4) yield {
        assertTrue(iter2.hasNext())
        val value = iter2.next()
        assertEquals(l2(i), value)
        value
      }
    }
    assertFalse(iter2.hasNext())
    assertTrue(result2.equals(l2))
  }

}

object LinkedHashSetFactory extends HashSetFactory {
  def allFactories: Iterator[LinkedHashSetFactory] =
    Iterator(new LinkedHashSetFactory)
}

class LinkedHashSetFactory extends HashSetFactory {
  override def implementationName: String =
    "java.util.LinkedHashSet"

  override def empty[E: ClassTag]: ju.LinkedHashSet[E] =
    new ju.LinkedHashSet[E]()
}
