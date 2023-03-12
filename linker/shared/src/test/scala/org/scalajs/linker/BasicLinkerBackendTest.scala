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

import java.nio.charset.StandardCharsets

import scala.concurrent.{ExecutionContext, Future}

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.Trees._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._
import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

import org.scalajs.logging._

class BasicLinkerBackendTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  private val BackendInvalidatedTopLevelTreesStatsMessage =
    raw"""BasicBackend: total top-level trees: (\d+); re-computed: (\d+)""".r

  private val BackendInvalidatedModulesStatsMessage =
    raw"""BasicBackend: total modules: (\d+); re-written: (\d+)""".r

  /** Makes sure that linking a "substantial" program (using `println`) twice
   *  does not invalidate any top-level tree nor module in the second run.
   */
  @Test
  def noInvalidatedTopLevelTreeOrModuleInSecondRun(): AsyncResult = await {
    import ModuleSplitStyle._

    val classDefs = List(
      mainTestClassDef(systemOutPrintln(str("Hello world!")))
    )

    val results = for (splitStyle <- List(FewestModules, SmallestModules)) yield {
      val logger1 = new CapturingLogger
      val logger2 = new CapturingLogger

      val config = StandardConfig()
        .withCheckIR(true)
        .withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(splitStyle)

      val linker = StandardImpl.linker(config)
      val classDefsFiles = classDefs.map(MemClassDefIRFile(_))

      val initializers = MainTestModuleInitializers
      val outputDir = MemOutputDirectory()

      for {
        javalib <- TestIRRepo.javalib
        allIRFiles = javalib ++ classDefsFiles
        _ <- linker.link(allIRFiles, initializers, outputDir, logger1)
        _ <- linker.link(allIRFiles, initializers, outputDir, logger2)
      } yield {
        val lines1 = logger1.allLogLines
        val lines2 = logger2.allLogLines

        // Top-level trees

        val Seq(totalTrees1, recomputedTrees1) =
          lines1.assertContainsMatch(BackendInvalidatedTopLevelTreesStatsMessage).map(_.toInt)

        val Seq(totalTrees2, recomputedTrees2) =
          lines2.assertContainsMatch(BackendInvalidatedTopLevelTreesStatsMessage).map(_.toInt)

        // At the time of writing this test, totalTrees1 reports 382 trees
        assertTrue(
            s"Not enough total top-level trees (got $totalTrees1); extraction must have gone wrong",
            totalTrees1 > 300)

        assertEquals("First run must invalidate every top-level tree", totalTrees1, recomputedTrees1)
        assertEquals("Second run must have the same total top-level trees as first run", totalTrees1, totalTrees2)
        assertEquals("Second run must not invalidate any top-level tree", 0, recomputedTrees2)

        // Modules

        val Seq(totalModules1, rewrittenModules1) =
          lines1.assertContainsMatch(BackendInvalidatedModulesStatsMessage).map(_.toInt)

        val Seq(totalModules2, rewrittenModules2) =
          lines2.assertContainsMatch(BackendInvalidatedModulesStatsMessage).map(_.toInt)

        if (splitStyle == FewestModules) {
          assertEquals("Expected exactly one module with FewestModules", 1, totalModules1)
        } else {
          // At the time of writing this test, totalModules1 reports 9 modules
          assertTrue(
              s"Not enough total modules (got $totalModules1); extraction must have gone wrong",
              totalModules1 > 5)
        }

        assertEquals("First run must invalidate every module", totalModules1, rewrittenModules1)
        assertEquals("Second run must have the same total modules as first run", totalModules1, totalModules2)
        assertEquals("Second run must not invalidate any module", 0, rewrittenModules2)
      }
    }

    Future.sequence(results)
  }
}
