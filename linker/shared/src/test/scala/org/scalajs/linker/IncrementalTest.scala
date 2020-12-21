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

import java.nio.charset.StandardCharsets.UTF_8

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.logging._

import org.scalajs.junit.async._

import org.scalajs.linker._
import org.scalajs.linker.interface._

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

class IncrementalTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  import IncrementalTest._

  @Test // #4335
  def testChangeMethodReachedFromExport(): AsyncResult = await {

    val FooClass = ClassName("Foo")

    val jsMethodName = StringLiteral("foo")
    val staticMethodName = m("value", Nil, IntRef)

    def classDefs(pre: Boolean) = Seq(
        mainTestClassDef(
            consoleLog(JSMethodApply(LoadModule(FooClass), jsMethodName, Nil))
        ),
        classDef(
            FooClass,
            kind = ClassKind.ModuleClass,
            superClass = Some(ObjectClass),
            memberDefs = List(
                trivialCtor(FooClass),
                JSMethodDef(
                    EMF, jsMethodName, Nil,
                    if (pre) int(5)
                    else ApplyStatic(EAF, FooClass, staticMethodName, Nil)(IntType))(
                    EOH, None),
                MethodDef(EMF.withNamespace(MemberNamespace.PublicStatic),
                    staticMethodName, NON, Nil, IntType, Some(int(6)))(EOH, None)
            )
        )
    )

    testIncremental(classDefs(_), _ => MainTestModuleInitializers)
  }

}

object IncrementalTest {

  def testIncremental(
      classDefs: Boolean => Seq[ClassDef],
      moduleInitializers: Boolean => List[ModuleInitializer],
      config: StandardConfig = StandardConfig())(
      implicit ec: ExecutionContext): Future[Unit] = {
    for {
      _ <- testIncrementalStep(backward = false, classDefs, moduleInitializers, config)
      _ <- testIncrementalStep(backward = true, classDefs, moduleInitializers, config)
    } yield ()
  }

  private def testIncrementalStep(
      backward: Boolean,
      classDefs: Boolean => Seq[ClassDef],
      moduleInitializers: Boolean => List[ModuleInitializer],
      config: StandardConfig = StandardConfig())(
      implicit ec: ExecutionContext): Future[Unit] = {

    val outputInc = MemOutputDirectory()
    val outputBatch = MemOutputDirectory()

    val linkerInc = StandardImpl.linker(config)
    val linkerBatch = StandardImpl.linker(config)

    val logger = new ScalaConsoleLogger(Level.Error)

    for {
      minilib <- TestIRRepo.minilib
      classDefs0 = minilib ++ classDefs(!backward).map(MemClassDefIRFile(_))
      classDefs1 = minilib ++ classDefs(backward).map(MemClassDefIRFile(_))
      inits0 = moduleInitializers(backward)
      inits1 = moduleInitializers(!backward)
      _ <- linkerInc.link(classDefs0, inits0, outputInc, logger)
      reportInc <- linkerInc.link(classDefs1, inits1, outputInc, logger)
      reportBatch <- linkerBatch.link(classDefs1, inits1, outputBatch, logger)
    } yield {
      assertModulesEqual(s"Public modules in report equal (backward = $backward)",
          reportInc.publicModules, reportBatch.publicModules)

      assertOutputEquals(s"Outputs equal (backward = $backward)",
          outputInc, outputBatch)
    }
  }

  private def assertModulesEqual(msg: String, inc: Iterable[Report.Module],
      batch: Iterable[Report.Module]): Unit = {
    // Poor man's equality based on toString()

    def strs(ms: Iterable[Report.Module]) =
      ms.map(m => m.moduleID -> m.toString()).toMap

    assertEquals(msg, strs(inc), strs(batch))
  }

  private def assertOutputEquals(msg: String, inc: MemOutputDirectory,
      batch: MemOutputDirectory): Unit = {
    val filesInc = inc.fileNames()
    val filesBatch = batch.fileNames()

    assertEquals(s"$msg: set of files", filesInc.toSet, filesBatch.toSet)

    for (f <- filesInc.sorted) {
      assertEquals(
          s"$msg: content of $f",
          new String(inc.content(f).get, UTF_8),
          new String(batch.content(f).get, UTF_8)
      )
    }
  }
}
