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
import org.junit.Assume.assumeTrue
import org.junit.{BeforeClass, Test}

import scala.collection.mutable

import scala.scalajs.js

import org.scalajs.testsuite.utils.Platform._

class WrappedMapTest {

  // Methods we actually implement

  @Test def testGet(): Unit = {
    val map: mutable.Map[String, Any] =
      js.Map("a" -> "a", "b" -> 6, "e" -> js.undefined)
    assertTrue(map.get("a") == Some("a"))
    assertTrue(map.get("b") == Some(6))
    assertTrue(map.get("e") == Some(()))
    assertTrue(map.get("f") == None)
  }

  @Test def testPlusAndMinusOperator(): Unit = {
    val jsMap = js.Map[Int, String]()
    val map: mutable.Map[Int, String] = jsMap

    map += 1 -> "hello world"
    assertEquals("hello world", jsMap(1))
    map += 3 -> "foo bar"
    assertEquals("foo bar", jsMap(3))
    map -= 1
    assertFalse(jsMap.get(1).isDefined)
  }

  @Test def testIterator(): Unit = {
    val elems = ('a' to 'e').map(_.toString).zip(1 to 5)
    val jsMap = js.Map[String, Int]()
    val map: mutable.Map[String, Int] = jsMap

    jsMap ++= elems

    assertEquals(elems, map.iterator.toList)
  }

  // Some arbitrary methods to test the builders

  @Test def testWithFilter(): Unit = {
    val jsMap = js.Map[String, Int]()
    val filtered = jsMap.withFilter { case (k, v) => v > 5 || k == "a" }
    def size: Int = filtered.map(x => x).size

    assertEquals(0, size)
    jsMap += "a" -> 1
    assertEquals(1, size)
    jsMap += "b" -> 2
    assertEquals(1, size)
    jsMap += "c" -> 6
    assertEquals(2, size)
    jsMap += "b" -> 7
    assertEquals(3, size)
    jsMap -= "a"
    assertEquals(2, size)
  }

  @Test def testToList(): Unit = {
    val jsMap = js.Map[String, Any]("a" -> "a", "b" -> 6, "e" -> js.undefined)
    val list = jsMap.toList
    assertEquals(3, list.size)
  }

}
