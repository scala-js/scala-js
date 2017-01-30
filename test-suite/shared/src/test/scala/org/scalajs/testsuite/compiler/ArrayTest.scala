/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
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

}
