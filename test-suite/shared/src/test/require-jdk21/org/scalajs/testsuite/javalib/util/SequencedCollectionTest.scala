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

class SequencedCollectionTest {

  @Test def knownSequencedCollections(): Unit = {
    def test(expected: Boolean, testClass: Class[_]): Unit =
      assertEquals(expected, classOf[ju.SequencedCollection[_]].isAssignableFrom(testClass))

    test(true, classOf[ju.List[String]])
    test(true, classOf[ju.LinkedList[String]])
    test(true, classOf[ju.ArrayList[String]])
    test(true, classOf[ju.LinkedHashSet[String]])
    test(true, classOf[ju.TreeSet[String]])
    test(true, classOf[ju.NavigableSet[String]])
    test(true, classOf[ju.Deque[String]])
    test(true, classOf[ju.ArrayDeque[String]])
    test(true, classOf[ju.SequencedSet[String]])

    test(false, classOf[ju.SequencedMap[String, String]])
    test(false, classOf[ju.HashSet[String]])
    test(false, classOf[ju.HashMap[String, String]])
    test(false, classOf[ju.LinkedHashMap[String, String]])
    test(false, classOf[ju.TreeMap[String, String]])
    test(false, classOf[ju.NavigableMap[String, String]])
    test(false, classOf[ju.Queue[String]])
  }

}
