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
      js.typeOf(js.constructorOf[js.Set[_]]) != "undefined")
  }
}

class SetTest {

  // scala.scalajs.js.Set

  @Test def should_provide_clear(): Unit = {
    val obj = js.Set("foo",  "bar")
    assertTrue(obj.size == 2)
    obj.clear()
    assertTrue(obj.size == 0)
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

  @Test def should_provide_add(): Unit = {
    val obj = js.Set[String]()
    assertTrue(obj.size == 0)
    assertTrue(obj.add("foo"))
    assertTrue(obj.add("bar"))
    assertTrue(obj.size == 2)
  }

  @Test def should_provide_contains(): Unit = {
    val obj = js.Set("foo")
    assertTrue(obj.contains("foo"))
    assertFalse(obj.contains("bar"))
  }

  @Test def should_provide_delete(): Unit = {
    val obj = js.Set("foo")
    assertTrue(obj.remove("foo"))
    assertFalse(obj.contains("foo"))
  }
}


