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

import java.{util => ju}

import org.junit.Test
import org.junit.Assert._

import scala.reflect.ClassTag

import org.scalajs.testsuite.utils.AssertThrows.{assertThrows, _}

trait NavigableMapTest extends SortedMapTest {

  def factory: NavigableMapFactory

  private def newMapForTest() = {
    val m = factory.empty[Int, String]
    m.putAll(TrivialImmutableMap(1 -> "a", 5 -> "b", 2 -> "c", 3 -> "d"))
    m
  }

  @Test
  def lowerEntry(): Unit = {
    val m = newMapForTest()

    assertNull(m.lowerEntry(1))

    val e = m.lowerEntry(2)

    assertEquals(1, e.getKey())
    assertEquals("a", e.getValue())

    assertEquals(3, m.lowerEntry(4).getKey())
  }

  @Test
  def lowerKey(): Unit = {
    val m = newMapForTest()

    assertNull(m.lowerKey(1))
    assertEquals(1, m.lowerKey(2))
    assertEquals(3, m.lowerKey(4))
  }

  @Test
  def floorEntry(): Unit = {
    val m = newMapForTest()

    assertNull(m.floorEntry(0))

    val e = m.floorEntry(2)

    assertEquals(2, e.getKey())
    assertEquals("c", e.getValue())

    assertEquals(3, m.floorEntry(4).getKey())
  }

  @Test
  def floorKey(): Unit = {
    val m = newMapForTest()

    assertNull(m.floorKey(0))
    assertEquals(2, m.floorKey(2))
    assertEquals(3, m.floorKey(4))
  }

  @Test
  def ceilingEntry(): Unit = {
    val m = newMapForTest()

    assertNull(m.ceilingEntry(6))

    val e = m.ceilingEntry(2)

    assertEquals(2, e.getKey())
    assertEquals("c", e.getValue())

    assertEquals(5, m.ceilingEntry(4).getKey())
  }

  @Test
  def ceilingKey(): Unit = {
    val m = newMapForTest()

    assertNull(m.ceilingKey(6))
    assertEquals(2, m.ceilingKey(2))
    assertEquals(5, m.ceilingKey(4))
  }

  @Test
  def higherEntry(): Unit = {
    val m = newMapForTest()

    assertNull(m.higherEntry(6))

    val e = m.higherEntry(2)

    assertEquals(3, e.getKey())
    assertEquals("d", e.getValue())

    assertEquals(5, m.higherEntry(4).getKey())
  }

  @Test
  def higherKey(): Unit = {
    val m = newMapForTest()

    assertNull(m.higherKey(6))
    assertEquals(3, m.higherKey(2))
    assertEquals(5, m.higherKey(4))
  }

  @Test
  def firstEntry(): Unit = {
    assertNull(factory.empty[String, String].firstEntry())

    val e = newMapForTest().firstEntry()
    assertEquals(1, e.getKey())
    assertEquals("a", e.getValue())
  }

  @Test
  def lastEntry(): Unit = {
    assertNull(factory.empty[String, String].lastEntry())

    val e = newMapForTest().lastEntry()
    assertEquals(5, e.getKey())
    assertEquals("b", e.getValue())
  }

  @Test
  def pollFirstEntry(): Unit = {
    val em = factory.empty[String, String]
    assertNull(em.pollFirstEntry())
    assertTrue(em.isEmpty())

    val m = newMapForTest()
    val e = m.pollFirstEntry()
    assertEquals(1, e.getKey())
    assertEquals("a", e.getValue())
    assertEquals(3, m.size())
  }

  @Test
  def pollLastEntry(): Unit = {
    val em = factory.empty[String, String]
    assertNull(em.pollLastEntry())
    assertTrue(em.isEmpty())

    val m = newMapForTest()
    val e = m.pollLastEntry()
    assertEquals(5, e.getKey())
    assertEquals("b", e.getValue())
    assertEquals(3, m.size())
  }

  @Test
  def descendingMap(): Unit = {
    val m = newMapForTest()
    val r = m.descendingMap()

    assertEquals(1, r.pollLastEntry().getKey())
    assertEquals(2, m.firstKey())
  }

  @Test
  def navigableKeySet(): Unit = {
    val m = newMapForTest()
    val s = m.navigableKeySet()

    assertEquals(5, s.pollLast())
    assertEquals(3, m.lastKey())
  }

  @Test
  def descendingKeySet(): Unit = {
    val m = newMapForTest()
    val s = m.descendingKeySet()

    assertEquals(1, s.pollLast())
    assertEquals(2, m.firstKey())
  }

  @Test def navigableSubMap(): Unit = {
    val m = newMapForTest()

    val sm = m.subMap(2, true, 4, false)
    assertEquals(2, sm.size())
    assertTrue(sm.containsKey(2))
    assertTrue(sm.containsKey(3))

    assertThrows(classOf[IllegalArgumentException], sm.put(4, "a"))
    assertThrows(classOf[IllegalArgumentException], sm.put(1, "a"))

    assertEquals("c", sm.remove(2))
    assertFalse(m.containsKey(2))

    assertEquals("d", sm.remove(3))
    assertFalse(m.containsKey(3))

    assertTrue(sm.isEmpty())
    assertEquals(2, m.size())

    assertEquals(1, newMapForTest().subMap(2, false, 4, false).size())
    assertEquals(1, newMapForTest().subMap(2, false, 4, true).size())
    assertEquals(2, newMapForTest().subMap(2, true, 4, true).size())
  }

  @Test def navigableHeadMap(): Unit = {
    val m = newMapForTest()

    val sm = m.headMap(4, false)
    assertEquals(3, sm.size())
    assertTrue(sm.containsKey(1))
    assertTrue(sm.containsKey(2))
    assertTrue(sm.containsKey(3))

    assertThrows(classOf[IllegalArgumentException], sm.put(4, "a"))

    assertEquals("c", sm.remove(2))
    assertFalse(m.containsKey(2))

    assertEquals("d", sm.remove(3))
    assertFalse(m.containsKey(3))

    assertEquals(1, sm.size())
    assertEquals(2, m.size)

    assertEquals(2, newMapForTest().headMap(3, false).size())
    assertEquals(3, newMapForTest().headMap(3, true).size())
  }

  @Test def navigableTailMap(): Unit = {
    val m = newMapForTest()

    val sm = m.tailMap(2, false)
    assertEquals(2, sm.size())
    assertTrue(sm.containsKey(3))
    assertTrue(sm.containsKey(5))

    assertThrows(classOf[IllegalArgumentException], sm.put(2, "a"))

    assertEquals("d", sm.remove(3))
    assertFalse(m.containsKey(3))

    assertEquals("b", sm.remove(5))
    assertFalse(m.containsKey(5))

    assertEquals(0, sm.size())
    assertEquals(2, m.size)

    assertEquals(3, newMapForTest().tailMap(2, true).size())
  }
}

trait NavigableMapFactory extends SortedMapFactory {
  def empty[K: ClassTag, V: ClassTag]: ju.NavigableMap[K, V]
}
