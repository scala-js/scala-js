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

package org.scalajs.testsuite.jsinterop

import org.junit.Assert._
import org.junit.Assume._
import org.junit.{BeforeClass, Test}

import scala.scalajs.js

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

object MapTest {
  @BeforeClass
  def assumeRuntimeSupportsMap(): Unit = {
    assumeTrue("Requires js.Map support", jsMaps)
  }
}

class MapTest {

  // scala.scalajs.js.Map

  @Test def testApply(): Unit = {
    val obj = js.Map("foo" -> "bar")
    assertThrows(classOf[NoSuchElementException], obj("bar"))
    assertEquals("bar", obj("foo"))
    assertEquals(1, obj.size)

    val empty = js.Map()
    assertEquals(0, empty.size)
  }

  @Test def testGet(): Unit = {
    val obj = js.Map.empty[String, Int]
    obj("hello") = 1

    assertTrue(obj.get("hello").get == 1)
    assertFalse(obj.get("world").isDefined)
  }

  @Test def testMinsuEqual(): Unit = {
    val obj = js.Map("a" -> "A")
    assert(!obj.delete("b"))
    assertEquals(1, obj.size)

    assert(obj.delete("a"))
    assertEquals(0, obj.size)
  }

  @Test def testIterator(): Unit = {
    val obj = js.Map("foo" -> 5, "bar" -> 42, "babar" -> 0)
    val elems = obj.iterator.toList
    assertEquals(List("foo" -> 5, "bar" -> 42, "babar" -> 0), elems)
  }

  @Test def testToJSMap(): Unit = {
    // scala.scalajs.js.JSConverters.JSRichGenMapKV
    import js.JSConverters._
    val map1 = Map(1 -> "one", 2 -> "two").toJSMap
    assertEquals("one", map1(1))
    assertEquals("two", map1(2))

    val map2 = Map("a" -> "foo", "b" -> "bar").toJSMap
    assertEquals("foo", map2("a"))
    assertEquals("bar", map2("b"))
  }

  @Test def testContains(): Unit = {
    val obj = js.Map(1 -> "foo")
    assertTrue(obj.contains(1))
    assertFalse(obj.contains(2))
  }

  @Test def testDelete(): Unit = {
    val obj = js.Map(1 -> "foo")
    assertTrue(obj.delete(1))
    assertFalse(obj.contains(1))
  }

  @Test def testUpdate(): Unit = {
    val obj = js.Map(1 -> "foo")
    obj.update(1, "bar")
    obj.update(2, "babar")
    assertEquals("bar", obj.get(1).get)
    assertEquals("babar", obj.get(2).get)
  }
}
