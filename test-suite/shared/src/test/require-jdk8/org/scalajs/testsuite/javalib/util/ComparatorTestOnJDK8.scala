/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import org.junit.Test
import org.junit.Assert._

import java.{util => ju}

class ComparatorTestOnJDK8 {

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
