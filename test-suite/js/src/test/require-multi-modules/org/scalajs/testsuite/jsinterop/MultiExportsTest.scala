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

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import org.junit.Assert._
import org.junit.Test

import org.scalajs.junit.async._

class MultiExportsTest {
  import MultiExportsTest._

  @Test
  def exportsToDifferentModulesMod1(): AsyncResult = await {
    for (res <- js.dynamicImport(mod1Fun("World")).toFuture)
      yield assertEquals("Hello World from mod1", res)
  }

  @Test
  def exportsToDifferentModulesMod2(): AsyncResult = await {
    for (res <- js.dynamicImport(mod2Fun("World")).toFuture)
      yield assertEquals("Hello World from mod2", res)
  }

  @Test
  def overloadsInASingleModule(): AsyncResult = await {
    for (res <- js.dynamicImport(mod1Fun(2, 3)).toFuture)
      yield assertEquals(5, res)
  }
}

object MultiExportsTest {
  @js.native
  @JSImport("./mod1.js", "MultiTopLevelExport")
  def mod1Fun(x: Int, y: Int): Int = js.native

  @js.native
  @JSImport("./mod1.js", "MultiTopLevelExport")
  def mod1Fun(x: String): String = js.native

  @js.native
  @JSImport("./mod2.js", "MultiTopLevelExport")
  def mod2Fun(x: String): String = js.native
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
