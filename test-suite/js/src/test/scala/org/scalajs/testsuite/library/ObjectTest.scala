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

package org.scalajs.testsuite.library

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test
import org.scalajs.testsuite.utils.Platform

import scala.scalajs.js
import scala.scalajs.js.annotation.{ JSBracketAccess, JSName }

class ObjectTest {
  import ObjectTest._

  private lazy val symA = js.Symbol("a")
  private lazy val symB = js.Symbol.forKey("b")

  @Test def getOwnPropertySymbols(): Unit = {
    assumeTrue(
        "Assuming newer methods existence, which are not honored by Rhino",
        Platform.executingInNodeJS)
    val obj = (new js.Object()).asInstanceOf[ObjectCreator]
    obj(symA) = "localSymbol"
    obj(symB) = "globalSymbol"
    val objectSymbols = js.Object.getOwnPropertySymbols(obj)
    assertArrayEquals(Array[AnyRef](symA, symB), objectSymbols.toArray[AnyRef])
  }

  @Test def is(): Unit = {
    assumeTrue(
        "Assuming newer methods existence, which are not honored by Rhino",
        Platform.executingInNodeJS)
    val a = new js.Object()
    assertTrue(js.Object.is(a, a))
    assertTrue(js.Object.is(Double.NaN, Double.NaN))
    assertTrue(js.Object.is(0.0, +0.0))
    assertTrue(js.Object.is(-0.0, -0.0))
    assertTrue(js.Object.is("foo", "foo"))

    val b = new js.Object()
    assertFalse(js.Object.is(a, b))
    assertFalse(js.Object.is(-0.0, +0.0))
  }

  @Test def entries_from_object(): Unit = {
    assumeTrue(
        "Assuming newer methods existence, which are not honored by Rhino",
        Platform.executingInNodeJS)
    val obj = new js.Object {
      val a = 42
      val b = "foo"
    }
    val entries = js.Object.entries(obj)
    assertEquals(2, entries.length)

    val js.Tuple2(key1, value1) = entries(0)
    assertEquals("a", key1)
    assertEquals(42, value1)

    val js.Tuple2(key2, value2) = entries(1)
    assertEquals("b", key2)
    assertEquals("foo", value2.asInstanceOf[String])
  }

  @Test def entries_from_dictionary(): Unit = {
    assumeTrue(
        "Assuming newer methods existence, which are not honored by Rhino",
        Platform.executingInNodeJS)
    val dict = js.Dictionary[Int]("a" -> 42, "b" -> 0)
    val entries = js.Object.entries(dict)
    assertEquals(2, entries.length)

    val js.Tuple2(key1, value1) = entries(0)
    assertEquals("a", key1)
    val value1IsInt: Int = value1
    assertEquals(42, value1IsInt)

    val js.Tuple2(key2, value2) = entries(1)
    assertEquals("b", key2)
    val value2IsInt: Int = value2
    assertEquals(0, value2IsInt)
  }

  @Test def fromEntries_array(): Unit = {
    assumeTrue(
        "Assuming newer methods existence, which are not honored by Rhino",
        Platform.executingInNodeJS)
    // from Array
    val array = js.Array(js.Tuple2("a", 42), js.Tuple2("b", "foo"))
    val obj1 = js.Object.fromEntries(array)
    assertEquals(obj1("a"), 42)
    assertEquals(obj1("b"), "foo")
  }

  // TODO: Add test for fromEntries(js.Map)
}

object ObjectTest {
  @js.native
  private trait ObjectCreator extends js.Object {
    @JSBracketAccess
    def update(s: js.Symbol, v: js.Any): Unit = js.native
  }
}
