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

import org.scalajs.ir.Trees._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

class EmitterTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  private val EmitterSetOfDangerousGlobalRefsChangedMessage =
    "Emitter: The set of dangerous global refs has changed."

  /** Makes sure that linking a "substantial" program (using `println`) does
   *  not trigger the second attempt in the Emitter due to dangerous global
   *  refs.
   */
  @Test
  def linkNoSecondAttemptInEmitter(): AsyncResult = await {
    val classDefs = List(
      mainTestClassDef(predefPrintln(str("Hello world!")))
    )

    val logger = new CapturingLogger

    val linker = StandardImpl.linker(StandardConfig())
    val classDefsFiles = classDefs.map(MemClassDefIRFile(_))

    for {
      fulllib <- TestIRRepo.fulllib
      report <- linker.link(fulllib ++ classDefsFiles,
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
      mainTestClassDef(predefPrintln(JSGlobalRef("$dangerousGlobalRef")))
    )

    val logger = new CapturingLogger

    val linker = StandardImpl.linker(StandardConfig())
    val classDefsFiles = classDefs.map(MemClassDefIRFile(_))

    for {
      fulllib <- TestIRRepo.fulllib
      report <- linker.link(fulllib ++ classDefsFiles,
          MainTestModuleInitializers, MemOutputDirectory(), logger)
    } yield {
      logger.allLogLines.assertContains(EmitterSetOfDangerousGlobalRefsChangedMessage)
    }
  }
}
