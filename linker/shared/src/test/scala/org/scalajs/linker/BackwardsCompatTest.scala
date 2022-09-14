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

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.logging._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

/** Basic backwards compatibility test.
 *
 *  This does not replace the usual two-commit tests we do when introducing
 *  backwards compatibility hacks. But rather, it serves as addititional defense
 *  in depth.
 */
class BackwardsCompatTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  @Test
  def testHelloWorld(): AsyncResult = await {
    val classDefs = Seq(
      mainTestClassDef(systemOutPrintln(str("Hello world!")))
    )

    test(classDefs, MainTestModuleInitializers)
  }

  @Test // #3976
  def testSystemIdentityHashCode(): AsyncResult = await {
    val classDefs = Seq(
      mainTestClassDef(
          systemOutPrintln(ApplyStatic(EAF,
              "java.lang.System",
              m("identityHashCode", List(O), I),
              List(JSObjectConstr(Nil)))(IntType)))
    )

    test(classDefs, MainTestModuleInitializers)
  }

  @Test // #4391
  def testClone(): AsyncResult = await {
    val classDefs = Seq(
      classDef("A",
          superClass = Some(ObjectClass),
          interfaces = List(CloneableClass),
          memberDefs = List(trivialCtor("A"))),
      mainTestClassDef(
          systemOutPrintln(Apply(EAF,
              New("A", NoArgConstructorName, Nil),
              m("clone", Nil, O), Nil)(AnyType)))
    )

    test(classDefs, MainTestModuleInitializers)
  }

  private def test(classDefs: Seq[ClassDef],
      moduleInitializers: Seq[ModuleInitializer]): Future[_] = {
    val classDefFiles = classDefs.map(MemClassDefIRFile(_))
    val logger = new ScalaConsoleLogger(Level.Error)

    Future.traverse(TestIRRepo.previousLibs.toSeq) { case (version, libFuture) =>
      libFuture.flatMap { lib =>
        val config = StandardConfig().withCheckIR(true)
        val linker = StandardImpl.linker(config)
        val out = MemOutputDirectory()

        linker.link(lib ++ classDefFiles, moduleInitializers, out, logger)
      }.recover {
        case e: Throwable =>
          throw new AssertionError(
              s"linking stdlib $version failed: ${e.getMessage()}", e)
      }
    }
  }
}
