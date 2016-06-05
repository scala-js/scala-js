/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._

import java.{util => ju}

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
