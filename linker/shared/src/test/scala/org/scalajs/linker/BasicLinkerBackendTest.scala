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

  /** Makes sure that linking a "substantial" program (using `println`) twice
   *  does not invalidate any top-level tree in the second run.
   */
  @Test
  def linkNoSecondAttemptInEmitter(): AsyncResult = await {
    val classDefs = List(
      mainTestClassDef(systemOutPrintln(str("Hello world!")))
    )

    val logger1 = new CapturingLogger
    val logger2 = new CapturingLogger

    val config = StandardConfig().withCheckIR(true)
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
      val Seq(total1, recomputed1) =
        lines1.assertContainsMatch(BackendInvalidatedTopLevelTreesStatsMessage).map(_.toInt)

      val lines2 = logger2.allLogLines
      val Seq(total2, recomputed2) =
        lines2.assertContainsMatch(BackendInvalidatedTopLevelTreesStatsMessage).map(_.toInt)

      // At the time of writing this test, total1 reports 382 trees
      assertTrue(
          s"Not enough total top-level trees (got $total1); extraction must have gone wrong",
          total1 > 300)

      assertEquals("First run must invalidate every top-level tree", total1, recomputed1)
      assertEquals("Second run must have the same total as first run", total1, total2)

      assertEquals(
          "Second run must not invalidate any top-level tree beside the module initializer",
          1, recomputed2)
    }
  }
}
