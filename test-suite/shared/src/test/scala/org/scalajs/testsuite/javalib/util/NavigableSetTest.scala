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

import org.scalajs.testsuite.javalib.util.concurrent.ConcurrentSkipListSetFactory

import scala.collection.JavaConversions._
import scala.reflect.ClassTag

trait NavigableSetTest extends SetTest {

  def factory: NavigableSetFactory

  @Test def `should_retrieve_ceiling(ordered)_elements`(): Unit = {
    val lInt = asJavaCollection(Set(1, 5, 2, 3, 4))
    val nsInt = factory.empty[Int]

    nsInt.addAll(lInt)

    assertEquals(1, nsInt.ceiling(-10))
    assertEquals(1, nsInt.ceiling(0))
    assertEquals(1, nsInt.ceiling(1))
    assertEquals(5, nsInt.ceiling(5))

    val lString = asJavaCollection(Set("a", "e", "b", "c", "d"))
    val nsString = factory.empty[String]

    nsString.addAll(lString)

    assertEquals("a", nsString.ceiling("00000"))
    assertEquals("a", nsString.ceiling("0"))
    assertEquals("a", nsString.ceiling("a"))
    assertEquals("d", nsString.ceiling("d"))
    assertNull(nsString.ceiling("z"))
  }

  @Test def `should_retrieve_floor(ordered)_elements`(): Unit = {
    val lInt = asJavaCollection(Set(1, 5, 2, 3, 4))
    val nsInt = factory.empty[Int]

    nsInt.addAll(lInt)

    assertEquals(5, nsInt.floor(10))
    assertEquals(5, nsInt.floor(5))
    assertEquals(3, nsInt.floor(3))
    assertEquals(1, nsInt.floor(1))

    val lString = asJavaCollection(Set("a", "e", "b", "c", "d"))
    val nsString = factory.empty[String]

    nsString.addAll(lString)

    assertEquals("e", nsString.floor("zzzzz"))
    assertEquals("d", nsString.floor("d"))
    assertEquals("b", nsString.floor("b"))
    assertEquals("a", nsString.floor("a"))
    assertNull(nsString.floor("0"))
  }

  @Test def `should_retrieve_higher(ordered)_elements`(): Unit = {
    val lInt = asJavaCollection(Set(1, 5, 2, 3, 4))
    val nsInt = factory.empty[Int]

    nsInt.addAll(lInt)

    assertEquals(5, nsInt.higher(4))
    assertEquals(4, nsInt.higher(3))
    assertEquals(2, nsInt.higher(1))
    assertEquals(1, nsInt.higher(-10))

    val lString = asJavaCollection(Set("a", "e", "b", "c", "d"))
    val nsString = factory.empty[String]

    nsString.addAll(lString)

    assertNull(nsString.higher("zzzzz"))
    assertEquals("e", nsString.higher("d"))
    assertEquals("c", nsString.higher("b"))
    assertEquals("b", nsString.higher("a"))
    assertEquals("a", nsString.higher("0"))
  }

  @Test def `should_retrieve_lower(ordered)_elements`(): Unit = {
    val lInt = asJavaCollection(Set(1, 5, 2, 3, 4))
    val nsInt = factory.empty[Int]

    nsInt.addAll(lInt)

    assertEquals(4, nsInt.lower(5))
    assertEquals(3, nsInt.lower(4))
    assertEquals(2, nsInt.lower(3))
    assertEquals(5, nsInt.lower(10))

    val lString = asJavaCollection(Set("a", "e", "b", "c", "d"))
    val nsString = factory.empty[String]

    nsString.addAll(lString)

    assertEquals("e", nsString.lower("zzzzz"))
    assertEquals("c", nsString.lower("d"))
    assertEquals("a", nsString.lower("b"))
    assertNull(nsString.lower("a"))
    assertNull(nsString.lower("0"))
  }

  @Test def should_poll_first_and_last_elements(): Unit = {
    val lInt = asJavaCollection(Set(1, 5, 2, 3, 4))
    val ns = factory.empty[Int]

    ns.addAll(lInt)

    assertTrue(ns.contains(1))
    assertEquals(1, ns.pollFirst())
    assertFalse(ns.contains(1))
    assertEquals(5, ns.pollLast())
    assertEquals(2, ns.pollFirst())
    assertEquals(4, ns.pollLast())
    assertEquals(3, ns.pollFirst())
    assertTrue(ns.isEmpty())
  }
}

object NavigableSetFactory {
  def allFactories: Iterator[NavigableSetFactory] =
    ConcurrentSkipListSetFactory.allFactories
}

trait NavigableSetFactory extends SetFactory {
  def empty[E: ClassTag]: ju.NavigableSet[E]
}
