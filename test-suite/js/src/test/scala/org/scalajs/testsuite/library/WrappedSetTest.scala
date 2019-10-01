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
import scala.scalajs.js

object WrappedSetTest {
  @BeforeClass
  def assumeRuntimeSupportsSet(): Unit = {
    assumeTrue("Assume Map exists in Global",
      Platform.hasInGlobal("Set"))
  }
}

class WrappedSetTest {

  // Methods we actually implement

  @Test def get(): Unit = {
    val jsSet: mutable.Set[Any] = js.Set("a", 1, js.undefined)
    assertTrue(jsSet("a"))
    assertTrue(jsSet(1) )
    assertTrue(jsSet(()))
    assertTrue(!jsSet("f"))
  }

  @Test def add_and_remove(): Unit = {
    val jsSet = js.Set[String]()
    val set: mutable.Set[String] = jsSet

    assertFalse(jsSet("hello"))
    assertTrue(set.add("hello"))
    assertFalse(set.add("hello"))
    assertTrue(jsSet("hello"))

    assertTrue(set.remove("hello"))
    assertFalse(jsSet("hello"))
  }

  @Test def `+=_and_-=`(): Unit = {
    val jsSet = js.Set[String]()
    val set: mutable.Set[String] = jsSet

    set += "hello"
    assertTrue(jsSet("hello"))
    set += "foo"
    assertTrue(jsSet("foo"))
    set -= "hello"
    assertFalse(jsSet("hello"))
  }

  @Test def iterator(): Unit = {
    val elems = 1 to 5
    val jsSet = js.Set[Int]()
    val set: mutable.Set[Int] = jsSet

    jsSet ++= elems

    assertTrue(set.iterator.toList.sorted.sameElements(elems))
  }

  // Some arbitrary methods to test the builders

  @Test def withFilter(): Unit = {
    val set = js.Set[Int]()
    val flt = set.withFilter { case v => v > 5  }
    def size: Int = flt.map(x => x).size

    assertEquals(0, size)
    set += 6
    assertEquals(1, size)
    set += 2
    assertEquals(1, size)
    set += 7
    assertEquals(2, size)
    set += 8
    assertEquals(3, size)
    set -= 7
    assertEquals(2, size)
  }

  @Test def toList(): Unit = {
    val set = js.Set("a", "b", "e")
    val list = set.toList
    assertEquals(3, list.size)
  }

}
