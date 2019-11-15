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

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform.hasCompliantArrayIndexOutOfBounds

class ArrayTest {

  @Test
  def getArrayIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant ArrayIndexOutOfBounds",
        hasCompliantArrayIndexOutOfBounds)

    val a = new Array[Int](5)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(-1))
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(5))
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(Int.MinValue))
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(Int.MaxValue))
  }

  @Test
  def setArrayIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant ArrayIndexOutOfBounds",
        hasCompliantArrayIndexOutOfBounds)

    val a = new Array[Int](5)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(-1) = 1)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(5) = 1)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(Int.MinValue) = 1)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(Int.MaxValue) = 1)
  }

  @Test
  def arraySelectSideEffecting_issue_3848(): Unit = {
    assumeTrue("Assuming compliant ArrayIndexOutOfBounds",
        hasCompliantArrayIndexOutOfBounds)

    // Force unit return type so the Emitter tries to get rid of the expression.
    @noinline
    def testAccess(a: Array[Int]): Unit = a(1)

    assertThrows(classOf[ArrayIndexOutOfBoundsException], testAccess(Array()))
  }
}
