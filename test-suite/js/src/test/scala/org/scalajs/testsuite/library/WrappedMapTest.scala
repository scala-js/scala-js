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
import org.junit.{ BeforeClass, Test }
import org.scalajs.testsuite.utils.Platform

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.scalajs.js

object WrappedMapTest {
  @BeforeClass
  def assumeRuntimeSupportsSet(): Unit = {
    assumeTrue("Assume Map exists in Global",
      Platform.hasInGlobal("Map"))
  }
}

class WrappedMapTest {

  // Methods we actually implement

  @Test def get(): Unit = {
    val map: mutable.Map[String, Any] =
      js.Map("a" -> "a", "b" -> 6, "e" -> js.undefined)
    assertTrue(map.get("a") == Some("a"))
    assertTrue(map.get("b") == Some(6))
    assertTrue(map.get("e") == Some(()))
    assertTrue(map.get("f") == None)
  }

  @Test def `+=_and_-=`(): Unit = {
    val jsMap = js.Map[Int, String]()
    val map: mutable.Map[Int, String] = jsMap

    map += 1 -> "hello world"
    assertEquals("hello world", jsMap(1))
    map += 3 -> "foo bar"
    assertEquals("foo bar", jsMap(3))
    map -= 1
    assertFalse(jsMap.get(1).isDefined)
  }

  @Test def iterator(): Unit = {
    val elems = ('a' to 'e').map(_.toString).zip(1 to 5)
    val jsMap = js.Map[String, Int]()
    val map: mutable.Map[String, Int] = jsMap

    jsMap ++= elems

    assertTrue(map.iterator.toList.sorted.sameElements(elems))
  }


  /* Methods that need to be overloaded in 2.13 collections to get the correct
   * result type.
   */

  @Test def map(): Unit = {
    def ct[A: ClassTag](x: A): ClassTag[A] = implicitly[ClassTag[A]]

    val jsMap = js.Map[String, Int]("one" -> 1, "two" -> 2, "three" -> 3)

    val mapChr = jsMap.map { case (k, v) => k(0)          -> v * 2 }
    val mapStr = jsMap.map { case (k, v) => k(0).toString -> v * 2 }

    assertNotSame(classOf[js.WrappedMap[_, _]], ct(mapChr).runtimeClass)
    assertSame(classOf[js.WrappedMap[_, _]], ct(mapStr).runtimeClass)

    assertEquals(2, mapChr.size)
    assertEquals(2, mapStr.size)
  }

  @Test def flatMap(): Unit = {
    def ct[A: ClassTag](x: A): ClassTag[A] = implicitly[ClassTag[A]]

    val jsMap = js.Map[String, Int]("one" -> 1, "two" -> 2, "three" -> 3)

    val flatMapChr = jsMap.flatMap {
      case (k, v) => List(k(0) -> v * 2, k(1) -> v * 3)
    }
    val flatMapStr = jsMap.flatMap {
      case (k, v) => List(k(0).toString -> v * 2, k(1).toString -> v * 3)
    }

    assertNotSame(classOf[js.WrappedMap[_, _]], ct(flatMapChr).runtimeClass)
    assertSame(classOf[js.WrappedMap[_, _]], ct(flatMapStr).runtimeClass)

    assertEquals(5, flatMapChr.size)
    assertEquals(5, flatMapStr.size)
  }


  @Test def collect(): Unit = {
    def ct[A: ClassTag](x: A): ClassTag[A] = implicitly[ClassTag[A]]

    val jsMap = js.Map[String, Int]("one" -> 1, "two" -> 2, "three" -> 3)

    val collectChr = jsMap.collect {
      case (k, v) if v > 1 => k(0) -> v * 2
    }
    val collectStr = jsMap.collect {
      case (k, v) if v > 1 => k(0).toString -> v * 2
    }

    assertNotSame(classOf[js.WrappedMap[_, _]], ct(collectChr).runtimeClass)
    assertSame(classOf[js.WrappedMap[_, _]], ct(collectStr).runtimeClass)

    assertEquals(1, collectChr.size)
    assertEquals(1, collectStr.size)
  }

  // Some arbitrary methods to test the builders

  @Test def withFilter(): Unit = {
    val jsMap = js.Map[String, Int]()
    val flt = jsMap.withFilter { case (k, v) => v > 5 || k == "a" }
    def size: Int = flt.map(x => x).size

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

  @Test def toList(): Unit = {
    val jsMap = js.Map[String, Any]("a" -> "a", "b" -> 6, "e" -> js.undefined)
    val list = jsMap.toList
    assertEquals(3, list.size)
  }

}
