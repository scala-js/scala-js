/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju}

trait SortedMapTest extends MapTest {

  def testSortedMapApi(sortedMapFactory: SortedMapFactory): Unit = {
    testMapApi(sortedMapFactory)

    // TODO: implement tests (when we port the first SortedMap)
    it("should always be sorted") {

    }

    it("should return the firstKey") {

    }

    it("should return the lastKey") {

    }

    it("should return a proper headMap") {

    }

    it("should return a proper tailMap") {

    }

    it("should return a proper subMap") {

    }
  }
}

object SortedMapFactory {
  def allFactories: Iterator[SortedMapFactory] = Iterator.empty
}

trait SortedMapFactory extends MapFactory {
  def empty[K, V]: ju.SortedMap[K, V]
}
