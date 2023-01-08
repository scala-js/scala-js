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

trait SortedMapTest extends MapTest {

  def factory: SortedMapFactory

  @Test def sorted(): Unit = {
    val m = factory.empty[Int, String]
    m.putAll(TrivialImmutableMap(1 -> "a", 5 -> "b", 2 -> "c", 3 -> "d", 4 -> "e"))

    assertArrayEquals(Array[AnyRef]("a", "c", "d", "e", "b"), m.values().toArray)
  }

  @Test def firstKey(): Unit = {
    val m = factory.empty[Int, String]
    m.put(1000, "a")
    m.put(10, "b")
    assertEquals(10, m.firstKey())
  }

  @Test def lastKey(): Unit = {
    val m = factory.empty[Int, String]
    m.put(1000, "a")
    m.put(10, "b")
    assertEquals(1000, m.lastKey())
  }

  val elems = TrivialImmutableMap(1 -> "a", 5 -> "e", 2 -> "b", 3 -> "c", 4 -> "d")

  @Test def headMap(): Unit = {
    val m = factory.empty[Int, String]

    m.putAll(elems)

    val sm = m.headMap(3)
    assertEquals(2, sm.size())
    assertTrue(sm.containsKey(1))
    assertTrue(sm.containsKey(2))

    assertEquals("a", sm.remove(1))
    assertFalse(m.containsKey(1))

    assertEquals("b", sm.remove(2))
    assertFalse(m.containsKey(2))

    assertTrue(sm.isEmpty())
    assertEquals(3, m.size)
  }

  @Test def tailMap(): Unit = {
    val m = factory.empty[Int, String]

    m.putAll(elems)

    val sm = m.tailMap(4)
    assertEquals(2, sm.size())
    assertTrue(sm.containsKey(4))
    assertTrue(sm.containsKey(5))

    assertEquals("d", sm.remove(4))
    assertFalse(m.containsKey(4))

    assertEquals("e", sm.remove(5))
    assertFalse(m.containsKey(5))

    assertTrue(sm.isEmpty())
    assertEquals(3, m.size)
  }

  @Test def subMap(): Unit = {
    val m = factory.empty[Int, String]

    m.putAll(elems)

    val sm = m.subMap(2, 4)
    assertEquals(2, sm.size())
    assertTrue(sm.containsKey(2))
    assertTrue(sm.containsKey(3))

    assertEquals("b", sm.remove(2))
    assertFalse(m.containsKey(2))

    assertEquals("c", sm.remove(3))
    assertFalse(m.containsKey(3))

    assertTrue(sm.isEmpty())
    assertEquals(3, m.size)
  }
}

trait SortedMapFactory extends MapFactory {
  def empty[K: ClassTag, V: ClassTag]: ju.SortedMap[K, V]
}
