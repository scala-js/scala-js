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

import scala.concurrent._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.testutils.LinkingUtils._
import org.scalajs.linker.testutils.TestIRBuilder._

class MaxModuleSplittingTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  /** Smoke test to ensure modules do not get merged too much. */
  @Test
  def avoidsCollisions(): AsyncResult = await {
    val classDefs = Seq(
      mainTestClassDef({
        consoleLog(str("Hello World!"))
      })
    )

    val expectedFiles = Set(
      "internal-0.js", // public module
      "internal-1.js", // public module
      "internal-2.js"  // internal module, avoiding internal-0 and internal-1.
    )

    val linkerConfig = StandardConfig()
      .withModuleKind(ModuleKind.ESModule)
      .withSourceMap(false)

    val mainInitializer =
      ModuleInitializer.mainMethodWithArgs("Test", "main")

    val moduleInitializers = List(
      mainInitializer.withModuleID("internal-0"),
      mainInitializer.withModuleID("internal-1")
    )

    val outputDirectory = MemOutputDirectory()

    for {
      _ <- testLink(classDefs, moduleInitializers,
          config = linkerConfig, output = outputDirectory)
    } yield {
      assertEquals(expectedFiles, outputDirectory.fileNames().toSet)
    }
  }
}
