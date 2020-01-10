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
import org.junit.{ BeforeClass, Test }
import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform

import scala.scalajs.js

object MapTest {
  @BeforeClass
  def assumeRuntimeSupportsMap(): Unit = {
    assumeTrue("Assume Map exists in Global",
      js.typeOf(js.constructorOf[js.Map[_, _]]) != "undefined")
  }
}

class MapTest {

  // scala.scalajs.js.Map

  @Test def apply_should_throw_when_not_found(): Unit = {
    val obj = js.Map("foo" -> "bar")
    assertThrows(classOf[NoSuchElementException], obj("bar"))
  }

  @Test def should_provide_get(): Unit = {
    val obj = js.Map.empty[String, Int]
    obj("hello") = 1

    assertTrue(obj.get("hello").get == 1)
    assertFalse(obj.get("world").isDefined)
  }

  @Test def `-=_should_ignore_deleting_a_non_existent_key`(): Unit = {
    val obj = js.Map("a" -> "A")
    assert(!obj.delete("b"))
  }

  @Test def should_provide_an_iterator(): Unit = {
    val obj = js.Map("foo" -> 5, "bar" -> 42, "babar" -> 0)
    var elems: List[(String, Int)] = Nil
    for ((prop, value) <- obj) {
      elems ::= (prop, value)
    }
    assertEquals(3, elems.size)
    assertTrue(elems.contains(("foo", 5)))
    assertTrue(elems.contains(("bar", 42)))
    assertTrue(elems.contains(("babar", 0)))
  }

  // scala.scalajs.js.JSConverters.JSRichGenMapKV

  @Test def should_provide_toJSMap(): Unit = {
    import js.JSConverters._
    val map1 = Map(1 -> "one", 2 -> "two").toJSMap
    assertEquals("one", map1(1))
    assertEquals("two", map1(2))

    val map2 = Map("a" -> "foo", "b" -> "bar").toJSMap
    assertEquals("foo", map2("a"))
    assertEquals("bar", map2("b"))
  }

  @Test def should_provide_contains(): Unit = {
    val obj = js.Map(1 -> "foo")
    assertTrue(obj.contains(1))
    assertFalse(obj.contains(2))
  }

  @Test def should_provide_delete(): Unit = {
    val obj = js.Map(1 -> "foo")
    assertTrue(obj.delete(1))
    assertFalse(obj.contains(1))
  }

  @Test def should_provide_update(): Unit = {
    val obj = js.Map(1 -> "foo")
    obj.update(1, "bar")
    obj.update(2, "babar")
    assertTrue(obj.get(1).get == "bar")
    assertTrue(obj.get(2).get == "babar")
  }
}


