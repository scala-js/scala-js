/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
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

  @Test def should_provide_an_equivalent_of_the_JS_delete_keyword_issue_255(): Unit = {
    val obj = js.Dictionary.empty[js.Any]
    obj("foo") = 42
    obj("bar") = "foobar"

    assertEquals(42, obj("foo"))
    assertEquals("foobar", obj("bar"))
    obj.delete("foo")
    assertFalse(obj.contains("foo"))
    assertFalse(obj.asInstanceOf[js.Object].hasOwnProperty("foo"))
    assertEquals("foobar", obj("bar"))
  }

  // This doesn't work on Rhino due to lack of full strict mode support - #679

  @Test def should_behave_as_specified_when_deleting_a_non_configurable_property_issue_461_issue_679(): Unit = {
    assumeFalse("Assumed not executing in Rhino", executingInRhino)
    val obj = js.Dictionary.empty[js.Any]
    js.Object.defineProperty(obj.asInstanceOf[js.Object], "nonconfig",
        js.Dynamic.literal(value = 4, writable = false).asInstanceOf[js.PropertyDescriptor])
    assertEquals(4, obj("nonconfig"))
    assertThrows(classOf[Exception], obj.delete("nonconfig"))
    assertEquals(4, obj("nonconfig"))
  }

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

  @Test def should_treat_delete_as_a_statement_issue_907(): Unit = {
    val obj = js.Dictionary("a" -> "A")
    obj.delete("a")
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
    obj.delete("hasOwnProperty")
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

  @Test def should_desugar_arguments_to_delete_statements_issue_908(): Unit = {
    val kh = js.Dynamic.literal(key = "a").asInstanceOf[KeyHolder]
    val dict = js.Dictionary[String]("a" -> "A")
    def a[T](foo: String): T = dict.asInstanceOf[T]
    a[js.Dictionary[String]]("foo").delete(kh.key)
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
}

object DictionaryTest {
  @js.native
  trait KeyHolder extends js.Object {
    def key: String = js.native
  }
}
