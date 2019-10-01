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
import org.junit.Assume.assumeTrue
import org.junit.{ BeforeClass, Test }
import org.scalajs.testsuite.utils.Platform

import scala.scalajs.js

object SetTest {
  @BeforeClass
  def assumeRuntimeSupportsSet(): Unit = {
    assumeTrue("Assume Set exists in Global",
      Platform.hasInGlobal("Set"))
  }
}

class SetTest {

  // scala.scalajs.js.Set

  @Test def should_provide_has(): Unit = {
    val obj = js.Set("foo")
    assertTrue(obj.has("foo"))
    assertFalse(obj.has("bar"))
  }

  @Test def should_provide_add(): Unit = {
    val obj = js.Set[String]()
    assertTrue(obj.add("foo").add("bar") == obj)
    assertTrue(obj.has("foo"))
    assertTrue(obj.has("bar"))
  }

  @Test def should_provide_delete(): Unit = {
    val obj = js.Set("foo")
    assertTrue(obj.delete("foo"))
    assertFalse(obj.has("foo"))
  }

  @Test def should_provide_clear(): Unit = {
    val obj = js.Set("foo",  "bar")
    assertTrue(obj.size == 2)
    obj.clear()
    assertTrue(obj.size == 0)
  }

  @Test def should_provide_keys(): Unit = {
    val obj = js.Set("a", "b")
    val keys = js.Array.from(obj.keys())
    assertEquals(2, keys.size)
    assertTrue(keys.contains("a"))
    assertTrue(keys.contains("b"))
  }

  @Test def should_provide_values(): Unit = {
    val obj = js.Set("a", "b")
    val values = js.Array.from(obj.values())
    assertEquals(2, values.size)
    assertTrue(values.contains("a"))
    assertTrue(values.contains("b"))
  }

  @Test def should_provide_entries(): Unit = {
    val obj = js.Set("a", "b")
    val entries = js.Array.from(obj.entries())
    assertEquals(2, entries.size)
    assertTrue(entries(0)._1 == "a")
    assertTrue(entries(1)._2 == "b")
  }

  @Test def should_provide_an_iterator(): Unit = {
    val obj = js.Set("foo", "bar", "babar")
    var elems: List[String] = Nil
    for (value <- obj) {
      elems ::= value
    }
    assertEquals(3, elems.size)
    assertTrue(elems.contains("foo"))
    assertTrue(elems.contains("bar"))
    assertTrue(elems.contains("babar"))
  }

  // scala.scalajs.js.JSConverters.JSRichGenSet

  @Test def should_provide_toJSSet(): Unit = {
    import js.JSConverters._
    val obj = Set(1, 2).toJSSet
    assertTrue(obj(1))
    assertTrue(obj(2))
    assertFalse(obj(3))
  }
}


