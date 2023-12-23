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
import org.scalajs.ir.Version

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._
import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

import org.scalajs.logging._

class EmitterTest {
  import EmitterTest._
  import scala.concurrent.ExecutionContext.Implicits.global

  @Test
  def jsHeader(): AsyncResult = await {
    val classDefs = List(
      mainTestClassDef(consoleLog(str("Hello world!")))
    )

    val t = "\t"
    val gClef = "\uD834\uDD1E"
    val header = s"""
      |#!/usr/bin/env node
      |// foo
      |  $t
      |  /* bar
      |   * A latin1 character: é
      |   * A BMP character: U+03B1 α Greek Small Letter Alpha
      |   * A supplementary character: U+1D11E $gClef Musical Symbol G Clef
      |baz
      |  */ $t // foo
    """.stripMargin.trim() + "\n"

    val config = StandardConfig().withJSHeader(header)

    for {
      fastContent <- linkToContent(classDefs,
          moduleInitializers = MainTestModuleInitializers,
          config = config)
      fullContent <- linkToContent(classDefs,
          moduleInitializers = MainTestModuleInitializers,
          config = config.withClosureCompilerIfAvailable(true))
    } yield {
      def testContent(content: String): Unit = {
        if (!content.startsWith(header)) {
          assertEquals(header, content.substring(0, header.length()))
          fail("unreachable")
        }
      }

      testContent(fastContent)
      testContent(fullContent)
    }

  }

  private val EmitterSetOfDangerousGlobalRefsChangedMessage =
    "Emitter: The set of dangerous global refs has changed."

  /** Makes sure that linking a "substantial" program (using `println`) does
   *  not trigger the second attempt in the Emitter due to dangerous global
   *  refs.
   */
  @Test
  def linkNoSecondAttemptInEmitter(): AsyncResult = await {
    val classDefs = List(
      mainTestClassDef(systemOutPrintln(str("Hello world!")))
    )

    val logger = new CapturingLogger

    val config = StandardConfig().withCheckIR(true)
    val linker = StandardImpl.linker(config)
    val classDefsFiles = classDefs.map(MemClassDefIRFile(_))

    for {
      javalib <- TestIRRepo.javalib
      report <- linker.link(javalib ++ classDefsFiles,
          MainTestModuleInitializers, MemOutputDirectory(), logger)
    } yield {
      logger.allLogLines.assertNotContains(EmitterSetOfDangerousGlobalRefsChangedMessage)
    }
  }

  /** Makes sure the test above tests the correct log message, by making sure
   *  that it is indeed emitted if there is a dangerous global ref.
   */
  @Test
  def linkYesSecondAttemptInEmitter(): AsyncResult = await {
    val classDefs = List(
      mainTestClassDef(systemOutPrintln(JSGlobalRef("$dangerousGlobalRef")))
    )

    val logger = new CapturingLogger

    val config = StandardConfig().withCheckIR(true)
    val linker = StandardImpl.linker(config)
    val classDefsFiles = classDefs.map(MemClassDefIRFile(_))

    for {
      javalib <- TestIRRepo.javalib
      report <- linker.link(javalib ++ classDefsFiles,
          MainTestModuleInitializers, MemOutputDirectory(), logger)
    } yield {
      logger.allLogLines.assertContains(EmitterSetOfDangerousGlobalRefsChangedMessage)
    }
  }

  private val EmitterClassTreeCacheStatsMessage =
    raw"""Emitter: Class tree cache stats: reused: (\d+) -- invalidated: (\d+)""".r

  private val EmitterMethodTreeCacheStatsMessage =
    raw"""Emitter: Method tree cache stats: reused: (\d+) -- invalidated: (\d+)""".r

  private val EmitterPostTransformStatsMessage =
    raw"""Emitter: Post transforms: total: (\d+) -- nested: (\d+) -- nested avoided: (\d+)""".r

  /** Makes sure that linking a "substantial" program (using `println`) twice
   *  does not invalidate any cache or top-level tree in the second run.
   */
  @Test
  def noInvalidatedCacheOrTopLevelTreeInSecondRun(): AsyncResult = await {
    val classDefs = List(
      mainTestClassDef(systemOutPrintln(str("Hello world!")))
    )

    val logger1 = new CapturingLogger
    val logger2 = new CapturingLogger

    val config = StandardConfig()
      .withCheckIR(true)
      .withModuleKind(ModuleKind.ESModule)

    val linker = StandardImpl.linker(config)
    val classDefsFiles = classDefs.map(MemClassDefIRFile(_, Version.fromInt(0)))

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

      // Class tree caches

      val Seq(classCacheReused1, classCacheInvalidated1) =
        lines1.assertContainsMatch(EmitterClassTreeCacheStatsMessage).map(_.toInt)

      val Seq(classCacheReused2, classCacheInvalidated2) =
        lines2.assertContainsMatch(EmitterClassTreeCacheStatsMessage).map(_.toInt)

      // At the time of writing this test, classCacheInvalidated1 reports 47
      assertTrue(
          s"Not enough invalidated class caches (got $classCacheInvalidated1); extraction must have gone wrong",
          classCacheInvalidated1 > 40)

      assertEquals("First run must not reuse any class cache", 0, classCacheReused1)

      assertEquals("Second run must reuse all class caches", classCacheReused2, classCacheInvalidated1)
      assertEquals("Second run must not invalidate any class cache", 0, classCacheInvalidated2)

      // Method tree caches

      val Seq(methodCacheReused1, methodCacheInvalidated1) =
        lines1.assertContainsMatch(EmitterMethodTreeCacheStatsMessage).map(_.toInt)

      val Seq(methodCacheReused2, methodCacheInvalidated2) =
        lines2.assertContainsMatch(EmitterMethodTreeCacheStatsMessage).map(_.toInt)

      // At the time of writing this test, methodCacheInvalidated1 reports 107
      assertTrue(
          s"Not enough invalidated method caches (got $methodCacheInvalidated1); extraction must have gone wrong",
          methodCacheInvalidated1 > 100)

      assertEquals("First run must not reuse any method cache", 0, methodCacheReused1)

      assertEquals("Second run must reuse all method caches", methodCacheReused2, methodCacheInvalidated1)
      assertEquals("Second run must not invalidate any method cache", 0, methodCacheInvalidated2)

      // Post transforms

      val Seq(postTransforms1, nestedPostTransforms1, _) =
        lines1.assertContainsMatch(EmitterPostTransformStatsMessage).map(_.toInt)

      val Seq(postTransforms2, nestedPostTransforms2, _) =
        lines2.assertContainsMatch(EmitterPostTransformStatsMessage).map(_.toInt)

      // At the time of writing this test, postTransforms1 reports 216
      assertTrue(
          s"Not enough post transforms (got $postTransforms1); extraction must have gone wrong",
          postTransforms1 > 200)

      assertEquals("Second run must only have nested post transforms",
          nestedPostTransforms2, postTransforms2)
      assertEquals("Both runs must have the same number of nested post transforms",
          nestedPostTransforms1, nestedPostTransforms2)
    }
  }
}

object EmitterTest {
  private def linkToContent(classDefs: Seq[ClassDef],
      moduleInitializers: Seq[ModuleInitializer] = Nil,
      config: StandardConfig)(
      implicit ec: ExecutionContext): Future[String] = {

    val logger = new ScalaConsoleLogger(Level.Error)
    val linker = StandardImpl.linker(config.withCheckIR(true))
    val classDefsFiles = classDefs.map(MemClassDefIRFile(_))
    val output = MemOutputDirectory()

    for {
      minilib <- TestIRRepo.minilib
      irFiles = minilib ++ classDefsFiles
      report <- linker.link(irFiles, moduleInitializers, output, logger)
    } yield {
      new String(output.content("main.js").get, StandardCharsets.UTF_8)
    }
  }

}
