/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import org.junit.Test
import org.junit.Assert._

import scala.reflect.ClassTag

trait SortedMapTest extends MapTest {

  def factory: SortedMapFactory

  def testSortedMapApi(): Unit = {
    testMapApi()
    should_always_be_sorted()
    should_return_the_firstKey()
    should_return_the_lastKey()
    should_return_a_proper_headMap()
    should_return_a_proper_tailMap()
    should_return_a_proper_subMap()
  }

  // TODO: implement tests (when we port the first SortedMap)

  @Test def should_always_be_sorted(): Unit = {

  }

  @Test def should_return_the_firstKey(): Unit = {

  }

  @Test def should_return_the_lastKey(): Unit = {

  }

  @Test def should_return_a_proper_headMap(): Unit = {

  }

  @Test def should_return_a_proper_tailMap(): Unit = {

  }

  @Test def should_return_a_proper_subMap(): Unit = {

  }
}

object SortedMapFactory {
  def allFactories: Iterator[SortedMapFactory] = Iterator.empty
}

trait SortedMapFactory extends MapFactory {
  def empty[K: ClassTag, V: ClassTag]: ju.SortedMap[K, V]
}
