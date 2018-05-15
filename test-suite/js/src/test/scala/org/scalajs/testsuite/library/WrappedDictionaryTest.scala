/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.library

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

import scala.collection.mutable

import scala.reflect.ClassTag

class WrappedDictionaryTest {

  // Methods we actually implement

  @Test def get(): Unit = {
    val map: mutable.Map[String, Any] =
      js.Dictionary("a" -> "a", "b" -> 6, "e" -> js.undefined)
    assertTrue(map.get("a") == Some("a"))
    assertTrue(map.get("b") == Some(6))
    assertTrue(map.get("e") == Some(()))
    assertTrue(map.get("f") == None)
  }

  @Test def `+=_and_-=`(): Unit = {
    val dict = js.Dictionary[String]()
    val map: mutable.Map[String, String] = dict

    assertArrayEquals(Array[AnyRef](), js.Object.properties(dict).toArray[AnyRef])

    map += "hello" -> "world"
    assertEquals("world", dict("hello"))
    map += "foo" -> "bar"
    assertEquals("bar", dict("foo"))
    map -= "hello"
    assertFalse(dict.get("hello").isDefined)
    assertArrayEquals(Array[AnyRef]("foo"), js.Object.properties(dict).toArray[AnyRef])
  }

  @Test def iterator(): Unit = {
    val elems = ('a' to 'e').map(_.toString).zip(1 to 5)
    val dict = js.Dictionary[Int]()
    val map: mutable.Map[String, Int] = dict

    dict ++= elems

    assertTrue(map.iterator.toList.sorted.sameElements(elems))
  }

  /* Methods that need to be overloaded in 2.13 collections to get the correct
   * result type.
   */

  @Test def map(): Unit = {
    def ct[A: ClassTag](x: A): ClassTag[A] = implicitly[ClassTag[A]]

    val dict = js.Dictionary[Int]("one" -> 1, "two" -> 2, "three" -> 3)

    val mapChr = dict.map { case (k, v) => k(0)          -> v * 2 }
    val mapStr = dict.map { case (k, v) => k(0).toString -> v * 2 }

    assertNotSame(classOf[js.WrappedDictionary[_]], ct(mapChr).runtimeClass)
    assertSame(classOf[js.WrappedDictionary[_]], ct(mapStr).runtimeClass)

    assertEquals(2, mapChr.size)
    assertEquals(2, mapStr.size)
  }

  @Test def flatMap(): Unit = {
    def ct[A: ClassTag](x: A): ClassTag[A] = implicitly[ClassTag[A]]

    val dict = js.Dictionary[Int]("one" -> 1, "two" -> 2, "three" -> 3)

    val flatMapChr = dict.flatMap {
      case (k, v) => List(k(0) -> v * 2, k(1) -> v * 3)
    }
    val flatMapStr = dict.flatMap {
      case (k, v) => List(k(0).toString -> v * 2, k(1).toString -> v * 3)
    }

    assertNotSame(classOf[js.WrappedDictionary[_]], ct(flatMapChr).runtimeClass)
    assertSame(classOf[js.WrappedDictionary[_]], ct(flatMapStr).runtimeClass)

    assertEquals(5, flatMapChr.size)
    assertEquals(5, flatMapStr.size)
  }

  @Test def collect(): Unit = {
    def ct[A: ClassTag](x: A): ClassTag[A] = implicitly[ClassTag[A]]

    val dict = js.Dictionary[Int]("one" -> 1, "two" -> 2, "three" -> 3)

    val collectChr = dict.collect {
      case (k, v) if v > 1 => k(0) -> v * 2
    }
    val collectStr = dict.collect {
      case (k, v) if v > 1 => k(0).toString -> v * 2
    }

    assertNotSame(classOf[js.WrappedDictionary[_]], ct(collectChr).runtimeClass)
    assertSame(classOf[js.WrappedDictionary[_]], ct(collectStr).runtimeClass)

    assertEquals(1, collectChr.size)
    assertEquals(1, collectStr.size)
  }

  // Some arbitrary methods to test the builders

  @Test def withFilter(): Unit = {
    val dict = js.Dictionary[Int]()
    val flt = dict.withFilter { case (k, v) => v > 5 || k == "a" }
    def size: Int = flt.map(x => x).size

    assertEquals(0, size)
    dict += "a" -> 1
    assertEquals(1, size)
    dict += "b" -> 2
    assertEquals(1, size)
    dict += "c" -> 6
    assertEquals(2, size)
    dict += "b" -> 7
    assertEquals(3, size)
    dict -= "a"
    assertEquals(2, size)
  }

  @Test def toList(): Unit = {
    val dict = js.Dictionary("a" -> "a", "b" -> 6, "e" -> js.undefined)
    val list = dict.toList
    assertEquals(3, list.size)
  }

}
