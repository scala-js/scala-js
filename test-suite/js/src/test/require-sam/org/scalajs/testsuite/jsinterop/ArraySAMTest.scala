/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.JSAssert._

class ArraySAMTest {

  import js.JSArrayOps._

  @Test def should_provide_jsMap(): Unit = {
    assertJSArrayEquals(js.Array(2, 3, 1, 2),
        js.Array("Sc", "ala", ".", "js").jsMap(_.length))
  }

  @Test def should_provide_jsFilter(): Unit = {
    assertJSArrayEquals(js.Array(56, -20, 86),
        js.Array(56, 30, -20, 33, 54, 86).jsFilter(_ % 3 != 0))
  }

}
