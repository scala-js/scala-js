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

import org.scalajs.testsuite.utils.AssertThrows._

class IteratorTest {
  @Test def testRemove(): Unit = {
    val iter = new ju.Iterator[String] {
      def hasNext(): Boolean = true
      def next(): String = "foo"
    }

    assertThrows(classOf[UnsupportedOperationException], iter.remove())
    iter.next()
    assertThrows(classOf[UnsupportedOperationException], iter.remove())
  }
}
