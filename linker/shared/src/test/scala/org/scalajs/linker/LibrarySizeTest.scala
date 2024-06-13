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

import org.scalajs.linker.analyzer._
import org.scalajs.linker.frontend.IRLoader
import org.scalajs.linker.interface._
import org.scalajs.linker.standard._

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

import org.scalajs.logging._

/** Tests for the effective size of the .js files produced when using certain
 *  parts of the standard library.
 *
 *  These tests are only executed for the default Scala version of the build.
 *  When we use any other version, the build filters out this test class.
 */
class LibrarySizeTest {
  import scala.concurrent.ExecutionContext.Implicits.global
  import LibrarySizeTest._

  @Test
  def juRegexSize(): AsyncResult = await {
    val PatternClass = ClassName("java.util.regex.Pattern")
    val MatcherClass = ClassName("java.util.regex.Matcher")

    def line(pattern: String, flags: Int, input: String): Tree = {
      val compiledPattern = ApplyStatic(EAF, PatternClass,
          m("compile", List(T, I), ClassRef(PatternClass)),
          List(str(pattern), int(flags)))(
          ClassType(PatternClass))

      val matcher = Apply(EAF, compiledPattern,
          m("matcher", List(ClassRef("java.lang.CharSequence")), ClassRef(MatcherClass)),
          List(str(input)))(
          ClassType(MatcherClass))

      consoleLog(Apply(EAF, matcher, m("matches", Nil, Z), Nil)(BooleanType))
    }

    val classDefs = Seq(
        mainTestClassDef(Block(
          line("[c-f]", 0, "d"),
          line("[c-f]", java.util.regex.Pattern.CASE_INSENSITIVE, "D")
        ))
    )

    testLinkedSizes(
      expectedFastLinkSize = 148205,
      expectedFullLinkSizeWithoutClosure = 86065,
      expectedFullLinkSizeWithClosure = 21588,
      classDefs,
      moduleInitializers = MainTestModuleInitializers
    )
  }
}

object LibrarySizeTest {
  private val reqsFactory = SymbolRequirement.factory("unit test")

  def testLinkedSizes(expectedFastLinkSize: Int,
      expectedFullLinkSizeWithoutClosure: Int,
      expectedFullLinkSizeWithClosure: Int,
      classDefs: Seq[ClassDef],
      symbolRequirements: SymbolRequirement = reqsFactory.none(),
      moduleInitializers: Seq[ModuleInitializer] = Nil)(
      implicit ec: ExecutionContext): Future[Unit] = {

    val logger = new ScalaConsoleLogger(Level.Error)

    val config = StandardConfig()
      .withCheckIR(true)

    val fullLinkConfig = config
      .withSemantics(_.optimized)
      .withClosureCompilerIfAvailable(true)
      .withMinify(true)

    val fastLinker = StandardImpl.linker(config)
    val fullLinker = StandardImpl.linker(fullLinkConfig)

    val classDefsFiles = classDefs.map(MemClassDefIRFile(_))

    val fastOutput = MemOutputDirectory()
    val fullOutput = MemOutputDirectory()

    for {
      javalib <- TestIRRepo.javalib
      irFiles = javalib ++ classDefsFiles
      fastLinkReport <- fastLinker.link(irFiles, moduleInitializers, fastOutput, logger)
      fullLinkReport <- fullLinker.link(irFiles, moduleInitializers, fullOutput, logger)
    } yield {
      val fastSize = fastOutput.content("main.js").get.length
      val fullSize = fullOutput.content("main.js").get.length

      val (expectedFullLinkSize, fullLinkTolerance) =
        if (fullLinkConfig.closureCompiler) (expectedFullLinkSizeWithClosure, 100)
        else (expectedFullLinkSizeWithoutClosure, 500)

      def roughlyEquals(expected: Int, actual: Int, tolerance: Int): Boolean =
        actual >= expected - tolerance && actual <= expected + tolerance

      if (!roughlyEquals(expectedFastLinkSize, fastSize, 500) ||
          !roughlyEquals(expectedFullLinkSize, fullSize, fullLinkTolerance)) {
        fail(
            s"\nFastLink expected $expectedFastLinkSize but got $fastSize" +
            s"\nFullLink expected $expectedFullLinkSize but got $fullSize")
      }
    }
  }
}
