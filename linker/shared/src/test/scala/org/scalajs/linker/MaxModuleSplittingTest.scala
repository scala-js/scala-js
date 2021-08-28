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

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.testutils.LinkingUtils._
import org.scalajs.linker.testutils.TestIRBuilder._

class MaxModuleSplittingTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  @Test
  def avoidsCollisions(): AsyncResult = await {
    val classDefs = Seq(
      mainTestClassDef({
        consoleLog(str("Hello World!"))
      })
    )

    val expectedFiles = Set(
      // public modules
      "internal-.js",
      "internal--.js",

      /* internal module, avoiding prefixes internal$ and internal$$
       *
       * Note that the hash can be asserted, because it must be the hash of the
       * above two IDs.
       */
      "internal---c993c7436ca6679362fa9e5390d2472d2a355314.js"
    )

    val linkerConfig = StandardConfig()
      .withModuleKind(ModuleKind.ESModule)
      .withSourceMap(false)

    val mainInitializer =
      ModuleInitializer.mainMethodWithArgs("Test", "main")

    val moduleInitializers = List(
      mainInitializer.withModuleID("internal-"),
      mainInitializer.withModuleID("internal--")
    )

    val outputDirectory = MemOutputDirectory()

    for {
      _ <- testLink(classDefs, moduleInitializers,
          config = linkerConfig, output = outputDirectory)
    } yield {
      assertEquals(expectedFiles, outputDirectory.fileNames().toSet)
    }
  }

  @Test
  def noMemExplosionOnImportChain_Issue4542(): AsyncResult = await {
    /* Create a long import chain. Previous versions of the linker would attempt
     * to construct the power set of import hops.
     *
     * In this test case, we construct 100 dynamic imports in a chain.
     * Therefore, if the power set were calculated, it would occupy at least
     * (assuming 1 byte per import hop) 2^100 Bytes = 2^50 PB.
     *
     * So it is safe to say that if this test case passes, the linker does not
     * suffer from this exponential memory explosion anymore.
     */

    val dynTargetName = m("dynTarget", Nil, ClassRef(ObjectClass))

    def callDynTarget(i: Int) =
      ApplyDynamicImport(EAF, "Dyn" + i, dynTargetName, Nil)

    def dynClass(i: Int, body: Tree): ClassDef = {
      val dynMethod = MethodDef(
          MemberFlags.empty.withNamespace(MemberNamespace.PublicStatic),
          dynTargetName, NON, Nil, AnyType, Some(body))(EOH, None)

      classDef(
          className = "Dyn" + i,
          kind = ClassKind.Interface,
          memberDefs = List(dynMethod)
      )
    }

    val classDefs = Seq(
      mainTestClassDef(callDynTarget(99))
    ) ++ (1 until 100).map(i =>
      dynClass(i, callDynTarget(i - 1))
    ) ++ Seq(
      dynClass(0, consoleLog(str("Hello World!")))
    )

    val linkerConfig = StandardConfig()
      .withModuleKind(ModuleKind.ESModule)

    testLink(classDefs, MainTestModuleInitializers, config = linkerConfig)
  }
}
