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
import scala.concurrent.duration._

import java.nio.file.Path

import org.junit.{Rule, Test}
import org.junit.Assert._
import org.junit.rules.TemporaryFolder

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.junit.async._

import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.jsenv.test.kit.TestKit

import org.scalajs.linker.interface._
import org.scalajs.linker.testutils.LinkingUtils._
import org.scalajs.linker.testutils.TestIRBuilder._

class RunTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  @(Rule @scala.annotation.meta.getter)
  val tempFolder = new TemporaryFolder()

  @Test
  def jlClassIsCore_Issue4616(): AsyncResult = await {
    // Check that if jl.Class is instantiated, it must be depended upon by jl.Object.

    val classDefs = Seq(
      mainTestClassDef({
        consoleLog(ClassOf(T))
      })
    )

    val linkerConfig = StandardConfig()
      .withModuleKind(ModuleKind.ESModule)
      .withModuleSplitStyle(ModuleSplitStyle.SmallestModules)
      .withSourceMap(false)
      .withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))

    testLinkAndRun(classDefs, MainTestModuleInitializers, linkerConfig,
        TestKit.InputKind.ESModule)
  }

  @Test
  def wrapAsThrowable(): AsyncResult = await {
    // Check that WrapAsThrowable can link without js.JavaScriptException on the classpath

    val getMessage = MethodName("getMessage", Nil, T)

    val e = VarRef("e")(ClassType(ThrowableClass, nullable = true))

    val classDefs = Seq(
      mainTestClassDef(Block(
        VarDef("e", NON, ClassType(ThrowableClass, nullable = true), mutable = false,
            WrapAsThrowable(JSNew(JSGlobalRef("RangeError"), List(str("boom"))))),
        genAssert(IsInstanceOf(e, ClassType("java.lang.Exception", nullable = false))),
        genAssertEquals(str("RangeError: boom"),
            Apply(EAF, e, getMessage, Nil)(ClassType(BoxedStringClass, nullable = true)))
      ))
    )

    testLinkAndRun(classDefs, MainTestModuleInitializers, StandardConfig(),
        TestKit.InputKind.Script)
  }

  private def genAssertEquals(expected: Tree, actual: Tree): Tree =
    genAssert(BinaryOp(BinaryOp.===, expected, actual))

  private def genAssert(test: Tree): Tree = {
    If(UnaryOp(UnaryOp.Boolean_!, test),
        Throw(JSNew(JSGlobalRef("Error"), List(str("Assertion failed")))),
        Skip())(
        NoType)
  }

  private def testLinkAndRun(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer],
      linkerConfig: StandardConfig, inputKind: TestKit.InputKind): Future[Unit] = {
    val output = tempFolder.newFolder().toPath

    val kit = new TestKit(new NodeJSEnv, timeout = 2.seconds, inputKind)

    for {
      report <- testLink(classDefs, moduleInitializers, linkerConfig, PathOutputDirectory(output))
    } yield {
      assertEquals(report.publicModules.size, 1)

      val path = output.resolve(report.publicModules.head.jsFileName)

      kit.withRun(Seq(kit.pathToInput(path))) {
        _.succeeds()
      }
    }
  }
}
