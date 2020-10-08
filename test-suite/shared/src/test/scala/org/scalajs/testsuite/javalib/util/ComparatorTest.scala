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

class ComparatorTest {

  @Test def reversed(): Unit = {
    class IntComparator extends ju.Comparator[Int] {
      def compare(a: Int, b: Int): Int = {
        /* Using Int.MinValue makes sure that Comparator.reversed() does not
         * use the naive implementation of negating the original comparator's
         * result.
         */
        if (a == b) 0
        else if (a < b) Int.MinValue
        else Int.MaxValue
      }
    }

    val comparator = new IntComparator
    val reversed = comparator.reversed()

    assertEquals(0, reversed.compare(5, 5))
    assertTrue(reversed.compare(3, 1) < 0)
    assertTrue(reversed.compare(6, 8) > 0)
  }

}
