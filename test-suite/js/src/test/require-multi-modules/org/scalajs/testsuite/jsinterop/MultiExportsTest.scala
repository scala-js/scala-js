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

package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.JSAssert._
import org.scalajs.testsuite.utils.Platform._

import scala.annotation.meta

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.{JSUtils, Platform}
import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class MultiExportsTest {
  import ExportsTest.exportsNameSpace

  @Test
  def exportsToDifferentModules(): Unit = {
    assertEquals("Hello World from mod1",
        exportsNameSpace("mod1").MultiTopLevelExport("World"))

    assertEquals("Hello World from mod2",
        exportsNameSpace("mod2").MultiTopLevelExport("World"))
  }

  @Test
  def overloadsInASingleModule(): Unit = {
    assertEquals(5, exportsNameSpace("mod1").MultiTopLevelExport(2, 3))
  }
}

object MultiTopLevelExports {
  @JSExportTopLevel("MultiTopLevelExport", "mod1")
  def f1(x: Int, y: Int): Int = x + y

  @JSExportTopLevel("MultiTopLevelExport", "mod1")
  def f2(x: String): String =
    MultiTopLevelExportsGreeter.greet(x, "mod1")

  @JSExportTopLevel("MultiTopLevelExport", "mod2")
  def f3(x: String): String =
    MultiTopLevelExportsGreeter.greet(x, "mod2")
}

// Separate object so mod1 / mod2 share code.
object MultiTopLevelExportsGreeter {
  @noinline
  def greet(whom: String, greeter: String): String =
    s"Hello $whom from $greeter"
}
