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

package org.scalajs.testsuite.javalib.util

import org.junit.Test
import org.junit.Assert._

import java.{util => ju}

class SequencedMapTest {

  @Test def knownSequencedMaps(): Unit = {
    def test(expected: Boolean, testClass: Class[_]): Unit = {
      assertEquals(expected, classOf[ju.SequencedMap[_, _]].isAssignableFrom(testClass))
    }

    test(true, classOf[ju.SortedMap[String, String]])
    test(true, classOf[ju.LinkedHashMap[String, String]])
    test(true, classOf[ju.TreeMap[String, String]])
    test(true, classOf[ju.NavigableMap[String, String]])

    test(false, classOf[ju.HashMap[String, String]])
    test(false, classOf[ju.LinkedHashSet[String]])
    test(false, classOf[ju.LinkedHashSet[String]])
    test(false, classOf[ju.TreeSet[String]])
    test(false, classOf[ju.NavigableSet[String]])
    test(false, classOf[ju.List[String]])
    test(false, classOf[ju.LinkedList[String]])
    test(false, classOf[ju.ArrayList[String]])
    test(false, classOf[ju.Deque[String]])
    test(false, classOf[ju.ArrayDeque[String]])
    test(false, classOf[ju.HashSet[String]])
    test(false, classOf[ju.SequencedCollection[String]])
    test(false, classOf[ju.SequencedSet[String]])
  }

}
