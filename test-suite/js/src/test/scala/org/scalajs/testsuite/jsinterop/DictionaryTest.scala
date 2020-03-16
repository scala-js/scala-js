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

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

class DictionaryTest {
  import DictionaryTest._

  // scala.scalajs.js.Dictionary

  @Test def apply_should_throw_when_not_found(): Unit = {
    val obj = js.Dictionary("foo" -> "bar")
    assertThrows(classOf[NoSuchElementException], obj("bar"))
  }

  @Test def should_provide_get(): Unit = {
    val obj = js.Dictionary.empty[Int]
    obj("hello") = 1

    assertTrue(obj.get("hello") == Some(1))
    assertFalse(obj.get("world").isDefined)
  }

  @Test def `-=_should_ignore_deleting_a_non_existent_key`(): Unit = {
    val obj = js.Dictionary("a" -> "A")
    obj -= "b"
  }

  @Test def should_provide_keys(): Unit = {
    val obj = js.Dictionary("a" -> "A", "b" -> "B")
    val keys = obj.keys.toList
    assertEquals(2, keys.size)
    assertTrue(keys.contains("a"))
    assertTrue(keys.contains("b"))
  }

  @Test def should_survive_the_key_hasOwnProperty_issue_1414(): Unit = {
    val obj = js.Dictionary.empty[Int]
    assertFalse(obj.contains("hasOwnProperty"))
    obj("hasOwnProperty") = 5
    assertTrue(obj.contains("hasOwnProperty"))
    obj -= "hasOwnProperty"
    assertFalse(obj.contains("hasOwnProperty"))
  }

  @Test def should_provide_an_iterator(): Unit = {
    val obj = js.Dictionary("foo" -> 5, "bar" -> 42, "babar" -> 0)
    var elems: List[(String, Int)] = Nil
    for ((prop, value) <- obj) {
      elems ::= (prop, value)
    }
    assertEquals(3, elems.size)
    assertTrue(elems.contains(("foo", 5)))
    assertTrue(elems.contains(("bar", 42)))
    assertTrue(elems.contains(("babar", 0)))
  }

  // scala.scalajs.js.JSConverters.JSRichGenMap

  @Test def should_provide_toJSDictionary(): Unit = {
    import js.JSConverters._
    val dict1 = Map("a" -> 1, "b" -> 2).toJSDictionary
    assertEquals(1, dict1("a"))
    assertEquals(2, dict1("b"))

    val dict2 = Map("a" -> "foo", "b" -> "bar").toJSDictionary
    assertEquals("foo", dict2("a"))
    assertEquals("bar", dict2("b"))
  }

  @Test def should_provide_underlying_JSDictionary(): Unit = {
    val original = js.Dictionary("a" -> 1, "b" -> 2, "c" -> 3)
    val dict: js.Dictionary[Int] = original.filter(_._1 != "b")

    assertEquals(1, dict("a"))
    assertEquals(None, dict.get("b"))
    assertEquals(3, dict("c"))
  }
}

object DictionaryTest {
  @js.native
  trait KeyHolder extends js.Object {
    def key: String = js.native
  }
}
