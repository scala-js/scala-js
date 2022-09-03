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
