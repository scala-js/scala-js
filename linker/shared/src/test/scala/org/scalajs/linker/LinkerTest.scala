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

import java.net.URI
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.Trees._

import org.scalajs.logging._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable.OutputDirectoryImpl
import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.LinkingUtils._
import org.scalajs.linker.testutils.TestIRBuilder._

class LinkerTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  val helloWorldClassDefs = Seq(
    mainTestClassDef {
      consoleLog(str("Hello world!"))
    }
  )

  /** Makes sure that the minilib is sufficient to completely link a hello
   *  world.
   */
  @Test
  def linkHelloWorld(): AsyncResult = await {
    testLink(helloWorldClassDefs, MainTestModuleInitializers)
  }

  @Test
  def linkEmpty(): AsyncResult = await {
    /* Check a degenerate case where there are not public modules at all.
     * See the special check on ModuleSplitter for details.
     */
    testLink(Nil, Nil)
  }

  @Test
  def cleanOutputDir(): AsyncResult = await {
    val staleFileName = "stale-code.js"

    val outputDirectory = MemOutputDirectory()

    for {
      // Simulate a stale output in the output directory.
      _ <- OutputDirectoryImpl.fromOutputDirectory(outputDirectory)
        .writeFull(staleFileName, ByteBuffer.wrap(Array()))
      report <- testLink(helloWorldClassDefs, MainTestModuleInitializers,
          output = outputDirectory)
    } yield {
      assertFalse(outputDirectory.content(staleFileName).isDefined)
      assertTrue(outputDirectory.content(report.publicModules.head.jsFileName).isDefined)
    }
  }

  /** This test exposes a problem where a linker in error state is called
   *  multiple times and ends up thinking it is being used concurrently.
   */
  @Test def cleanLinkingState(): AsyncResult = await {
    class DummyException extends Exception

    val badSeq = new IndexedSeq[IRFile] {
      def apply(x: Int): IRFile = throw new DummyException()
      def length: Int = throw new DummyException()
    }

    val linker = StandardImpl.linker(StandardConfig())

    def callLink(): Future[Report] = {
      val out = MemOutputDirectory()
      linker.link(badSeq, Nil, out, NullLogger)
    }

    // Call first time. Get exception from badSeq.
    // Note that the call must not throw immediately.
    val firstRun = callLink().failed.map {
      case e: DummyException => // ok.
      case _                 => fail("Expected DummyException")
    }

    def callInFailedState(prev: Future[Unit]): Future[Unit] = {
      prev.flatMap(_ => callLink()).failed.map {
        case e: IllegalStateException =>
          if (e.getMessage.contains("concurrent")) {
            fail("Found bad message in exception: " + e.getMessage)
          }

        case _ => fail("Expected IllegalStateException")
      }
    }

    (1 to 4).foldLeft(firstRun)((p, _) => callInFailedState(p))
  }

  @Test
  @deprecated("Mark deprecated to silence warnings", "never/always")
  def testLegacyAPISingleModule(): AsyncResult = await {
    val linker = StandardImpl.linker(StandardConfig())
    val classDefsFiles = helloWorldClassDefs.map(MemClassDefIRFile(_))

    val jsOutput = MemOutputFile()
    val smOutput = MemOutputFile()

    val output = LinkerOutput(jsOutput)
      .withSourceMap(smOutput)
      .withSourceMapURI(new URI("http://example.org/my-source-map-uri"))
      .withJSFileURI(new URI("http://example.org/my-js-file-uri"))

    for {
      minilib <- TestIRRepo.minilib
      _ <- linker.link(minilib ++ classDefsFiles, MainTestModuleInitializers,
          output, new ScalaConsoleLogger(Level.Error))
    } yield {
      val jsContent = new String(jsOutput.content, StandardCharsets.UTF_8)

      // Check we replaced the source map reference.
      assertTrue(jsContent.contains("\n//# sourceMappingURL=http://example.org/my-source-map-uri\n"))
      assertFalse(jsContent.contains("//# sourceMappingURL=main.js.map"))

      val smContent = new String(smOutput.content, StandardCharsets.UTF_8)

      // Check we replaced the js file reference.
      assertTrue(smContent.contains(""""file": "http://example.org/my-js-file-uri""""))
      assertFalse(smContent.contains("main.js"))
    }
  }

  @Test // #4271
  @deprecated("Mark deprecated to silence warnings", "never/always")
  def testLegacyAPIEmpty(): AsyncResult = await {
    val linker = StandardImpl.linker(StandardConfig())

    val jsOutput = MemOutputFile()
    val smOutput = MemOutputFile()

    val output = LinkerOutput(jsOutput)
      .withSourceMap(smOutput)
      .withSourceMapURI(new URI("http://example.org/my-source-map-uri"))
      .withJSFileURI(new URI("http://example.org/my-js-file-uri"))

    // Check it doesn't fail. Content is tested in ReportToLinkerOutputAdapterTest.
    TestIRRepo.minilib.flatMap { minilib =>
      linker.link(minilib, Nil, output, new ScalaConsoleLogger(Level.Error))
    }
  }

  /** Tests that enabling contentHash produces file names with a hash suffix
   *  and that the report reflects the hash-based file names.
   */
  @Test
  def contentHashInFileName(): AsyncResult = await {
    val outputDirectory = MemOutputDirectory()

    val config = StandardConfig()
      .withContentHash(true)
      .withSourceMap(false)

    for {
      report <- testLink(helloWorldClassDefs, MainTestModuleInitializers,
          config = config, output = outputDirectory)
    } yield {
      val jsFileName = report.publicModules.head.jsFileName
      // The file name should contain a hyphen followed by a hex hash.
      assertTrue(
          s"Expected content-hashed file name but got: $jsFileName",
          jsFileName.matches(".*-[0-9a-f]{16}\\.js"))
      // The file should actually be present in the output directory.
      assertTrue(
          s"File $jsFileName not found in output directory",
          outputDirectory.content(jsFileName).isDefined)
    }
  }

  /** Tests that with contentHash enabled, the source map file name also
   *  contains the hash and that sourceMappingURL in the JS file is updated.
   */
  @Test
  def contentHashInFileNameWithSourceMap(): AsyncResult = await {
    val outputDirectory = MemOutputDirectory()

    val config = StandardConfig()
      .withContentHash(true)
      .withSourceMap(true)

    for {
      report <- testLink(helloWorldClassDefs, MainTestModuleInitializers,
          config = config, output = outputDirectory)
    } yield {
      val module = report.publicModules.head
      val jsFileName = module.jsFileName
      val smFileName = module.sourceMapName.getOrElse(
          fail("Expected source map name in report"))

      // Both names should contain a hash.
      assertTrue(
          s"Expected content-hashed JS file name but got: $jsFileName",
          jsFileName.matches(".*-[0-9a-f]{16}\\.js"))
      assertTrue(
          s"Expected content-hashed source map file name but got: $smFileName",
          smFileName.matches(".*-[0-9a-f]{16}\\.js\\.map"))

      // The source map suffix of the JS file name and the source map file name
      // should be consistent.
      assertEquals(jsFileName + ".map", smFileName)

      // The JS file should contain a sourceMappingURL pointing to the
      // hash-based source map file name.
      val jsContent = new String(
          outputDirectory.content(jsFileName).get, StandardCharsets.UTF_8)
      assertTrue(
          s"Expected sourceMappingURL with hash-based name in JS content",
          jsContent.contains(s"//# sourceMappingURL=./$smFileName\n"))
    }
  }

  /** Tests that content hash changes when the content changes. */
  @Test
  def contentHashChangesWithContent(): AsyncResult = await {
    val classDefs1 = Seq(mainTestClassDef {
      consoleLog(str("Hello world!"))
    })

    val classDefs2 = Seq(mainTestClassDef {
      consoleLog(str("Different content!"))
    })

    val config = StandardConfig()
      .withContentHash(true)
      .withSourceMap(false)

    for {
      report1 <- testLink(classDefs1, MainTestModuleInitializers, config = config)
      report2 <- testLink(classDefs2, MainTestModuleInitializers, config = config)
    } yield {
      val fileName1 = report1.publicModules.head.jsFileName
      val fileName2 = report2.publicModules.head.jsFileName
      assertNotEquals(
          "File names should differ when content differs",
          fileName1, fileName2)
    }
  }

  /** Tests that without contentHash, file names are unchanged (regression). */
  @Test
  def noContentHashByDefault(): AsyncResult = await {
    val config = StandardConfig()
      .withSourceMap(false)

    for {
      report <- testLink(helloWorldClassDefs, MainTestModuleInitializers, config = config)
    } yield {
      val jsFileName = report.publicModules.head.jsFileName
      assertEquals("main.js", jsFileName)
    }
  }
}
