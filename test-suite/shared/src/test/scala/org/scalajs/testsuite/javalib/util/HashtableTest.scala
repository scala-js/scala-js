package org.scalajs.testsuite.javalib.util

import org.junit.Test
import org.junit.Assert._

import java.{util => ju}

import scala.collection.JavaConversions._

class HashtableTest {

  @Test def size(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    assertEquals(0, ht.size())
    ht.put(1, 1)
    assertEquals(1, ht.size())
    ht.put(2, 1)
    ht.put(3, 1)
    assertEquals(3, ht.size())
    ht.put(3, 2)
    assertEquals(3, ht.size())
    ht.remove(1)
    assertEquals(2, ht.size())
    ht.clear()
    assertEquals(0, ht.size())
  }

  @Test def isEmpty(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    assertTrue(ht.isEmpty)
    ht.put(3, 1)
    assertFalse(ht.isEmpty)
    ht.clear()
    assertTrue(ht.isEmpty)
  }

  @Test def keys(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    assertEquals(Set.empty[Int], ht.keys().toSet)
    ht.put(1, 4)
    assertEquals(Set(1), ht.keys().toSet)
    ht.put(2, 5)
    ht.put(3, 6)
    assertEquals(Set(1, 2, 3), ht.keys().toSet)
  }

  @Test def elements(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    assertEquals(Set.empty[Int], ht.elements().toSet)
    ht.put(1, 4)
    assertEquals(Set(4), ht.elements().toSet)
    ht.put(2, 5)
    ht.put(3, 6)
    assertEquals(Set(4, 5, 6), ht.elements().toSet)
  }

  @Test def contains(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    assertFalse(ht.contains(4))
    assertFalse(ht.contains(5))
    assertFalse(ht.contains(6))
    ht.put(1, 4)
    assertTrue(ht.contains(4))
    assertFalse(ht.contains(5))
    assertFalse(ht.contains(6))
    ht.put(2, 5)
    ht.put(3, 6)
    assertTrue(ht.contains(4))
    assertTrue(ht.contains(5))
    assertTrue(ht.contains(6))
  }

  @Test def containsValue(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    assertFalse(ht.containsValue(4))
    assertFalse(ht.containsValue(5))
    assertFalse(ht.containsValue(6))
    ht.put(1, 4)
    assertTrue(ht.containsValue(4))
    assertFalse(ht.containsValue(5))
    assertFalse(ht.containsValue(6))
    ht.put(2, 5)
    ht.put(3, 6)
    assertTrue(ht.containsValue(4))
    assertTrue(ht.containsValue(5))
    assertTrue(ht.containsValue(6))
  }

  @Test def containsKey(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    assertFalse(ht.containsKey(1))
    assertFalse(ht.containsKey(2))
    assertFalse(ht.containsKey(3))
    ht.put(1, 4)
    assertTrue(ht.containsKey(1))
    assertFalse(ht.containsKey(2))
    assertFalse(ht.containsKey(3))
    ht.put(2, 5)
    ht.put(3, 6)
    assertTrue(ht.containsKey(1))
    assertTrue(ht.containsKey(2))
    assertTrue(ht.containsKey(3))
  }

  @Test def get(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    assertEquals(null, ht.get(1))
    assertEquals(null, ht.get(2))
    assertEquals(null, ht.get(3))
    ht.put(1, 4)
    assertEquals(4, ht.get(1))
    assertEquals(null, ht.get(2))
    assertEquals(null, ht.get(3))
    ht.put(2, 5)
    ht.put(3, 6)
    assertEquals(4, ht.get(1))
    assertEquals(5, ht.get(2))
    assertEquals(6, ht.get(3))
  }

  @Test def put(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    assertEquals(null, ht.put(1, 4))
    assertEquals(4, ht.put(1, 3))
    assertEquals(3, ht.put(1, 5))
    assertEquals(null, ht.put(2, 5))
    assertEquals(2, ht.size)
  }

  @Test def remove(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    ht.put(1, 4)
    ht.put(2, 5)
    ht.put(3, 6)
    assertEquals(null, ht.remove(0))
    assertEquals(4, ht.remove(1))
    assertEquals(null, ht.remove(1))
    assertEquals(5, ht.remove(2))
    assertEquals(6, ht.remove(3))
  }

  @Test def clear(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    ht.put(1, 4)
    ht.put(2, 5)
    ht.put(3, 6)
    assertFalse(ht.isEmpty)
    ht.clear()
    assertTrue(ht.isEmpty)
    ht.put(1, 4)
    ht.clear()
    assertTrue(ht.isEmpty)
  }

  @Test def cloneTest(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    ht.put(1, 4)
    ht.put(2, 5)
    ht.put(3, 6)

    assertTrue(ht.clone().isInstanceOf[ju.Hashtable[_, _]])
    val clone = ht.clone().asInstanceOf[ju.Hashtable[Int, Int]]
    ht.clear()
    assertEquals(3, clone.size)
    assertEquals(4, clone.get(1))
    assertEquals(5, clone.get(2))
    assertEquals(6, clone.get(3))
  }

  @Test def toStringTest(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    assertEquals("{}", ht.toString)
    ht.put(1, 4)
    assertEquals("{1=4}", ht.toString)
    ht.put(2, 4)
    assertTrue(ht.toString.matches("\\{\\d=\\d, \\d=\\d\\}"))
    ht.put(3, 5)
    assertTrue(ht.toString.matches("\\{\\d=\\d, \\d=\\d, \\d=\\d\\}"))
  }

  @Test def keySet(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    assertEquals(Set.empty[Int], ht.keySet().toSet)
    ht.put(1, 4)
    assertEquals(Set(1), ht.keySet().toSet)
    ht.put(2, 5)
    assertEquals(Set(1, 2), ht.keySet().toSet)
    ht.put(3, 6)
    assertEquals(Set(1, 2, 3), ht.keySet().toSet)
  }

  @Test def entrySet(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    assertTrue(ht.entrySet().isEmpty)
    ht.put(1, 4)
    assertEquals(Set(1), ht.entrySet().map(_.getKey))
    assertEquals(Set(4), ht.entrySet().map(_.getValue))
    ht.put(2, 5)
    assertEquals(Set(1, 2), ht.entrySet().map(_.getKey))
    assertEquals(Set(4, 5), ht.entrySet().map(_.getValue))
    ht.put(3, 6)
    assertEquals(Set(1, 2, 3), ht.entrySet().map(_.getKey))
    assertEquals(Set(4, 5, 6), ht.entrySet().map(_.getValue))
  }

  @Test def values(): Unit = {
    val ht = new ju.Hashtable[Int, Int]
    assertEquals(Set.empty[Int], ht.values().toSet)
    ht.put(1, 4)
    assertEquals(Set(4), ht.values().toSet)
    ht.put(2, 5)
    assertEquals(Set(4, 5), ht.values().toSet)
    ht.put(3, 6)
    assertEquals(Set(4, 5, 6), ht.values().toSet)
  }
}
