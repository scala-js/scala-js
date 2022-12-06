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

package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import java.{util => ju}

import org.scalajs.testsuite.utils.Platform._

class DefaultMethodsTest {

  @Test def canOverrideDefaultMethod(): Unit = {
    var counter = 0

    class SpecialIntComparator extends ju.Comparator[Int] {
      def compare(o1: Int, o2: Int): Int =
        o1.compareTo(o2)

      override def reversed(): ju.Comparator[Int] = {
        counter += 1
        super.reversed()
      }
    }

    val c = new SpecialIntComparator
    assertTrue(c.compare(5, 7) < 0)
    assertEquals(0, counter)

    val reversed = c.reversed()
    assertEquals(1, counter)
    assertTrue(reversed.compare(5, 7) > 0)
  }

  @Test def reflectiveCallDefaultMethod(): Unit = {
    import scala.language.reflectiveCalls

    class ReflectiveCallIntComparator extends ju.Comparator[Int] {
      def compare(o1: Int, o2: Int): Int =
        o1.compareTo(o2)
    }

    val c = new ReflectiveCallIntComparator
    val c2: { def reversed(): ju.Comparator[Int] } = c
    val reversed = c2.reversed()

    assertTrue(reversed.compare(5, 7) > 0)
  }
}
