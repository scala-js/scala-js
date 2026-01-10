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

package org.scalajs.linker

import org.junit.Test

import org.scalajs.junit.async._

import org.scalajs.logging._

import org.scalajs.linker.interface.StandardConfig

import org.scalajs.linker.testutils.{MemClassDefIRFile, TestIRRepo}
import org.scalajs.linker.testutils.LinkingUtils._
import org.scalajs.linker.testutils.TestIRBuilder._

class GCCLinkerTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  @deprecated("tests deprecated APIs", since = "1.21.0")
  @Test
  def linkEmpty(): AsyncResult = await {
    /* Check a degenerate case where there are not public modules at all.
     * See the special check on ModuleSplitter for details.
     */
    testLink(Nil, Nil, config = StandardConfig().withClosureCompiler(true))
  }

  @deprecated("tests deprecated APIs", since = "1.21.0")
  @Test
  def linkIncrementalSmoke(): AsyncResult = await {
    /* Check that linking twice works. GCC trees are highly mutable, so if we
     * (re-)use them wrongly over multiple runs, things can fail unexpectedly.
     *
     * We change something about the code in the second run to force the linker
     * to actually re-run.
     */
    def classDef(text: String) =
      MemClassDefIRFile(mainTestClassDef(consoleLog(str(text))))

    val moduleInitializers = MainTestModuleInitializers

    val config = StandardConfig().withCheckIR(true).withClosureCompiler(true)
    val linker = StandardImpl.linker(config)

    val output = MemOutputDirectory()
    val logger = new ScalaConsoleLogger(Level.Error)

    for {
      lib <- TestIRRepo.minilib
      _ <- linker.link(lib :+ classDef("test 1"), moduleInitializers, output, logger)
      _ <- linker.link(lib :+ classDef("test 2"), moduleInitializers, output, logger)
    } yield ()
  }
}
