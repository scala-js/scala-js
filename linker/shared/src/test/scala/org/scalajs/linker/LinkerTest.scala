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
import java.nio.charset.StandardCharsets

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.Trees._

import org.scalajs.logging._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.LinkingUtils._
import org.scalajs.linker.testutils.TestIRBuilder._

class LinkerTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  /** Makes sure that the minilib is sufficient to completely link a hello
   *  world.
   */
  @Test
  def linkHelloWorld(): AsyncResult = await {
    val classDefs = Seq(
        mainTestClassDef({
          consoleLog(StringLiteral("Hello world!"))
        })
    )
    testLink(classDefs, MainTestModuleInitializers)
  }

  /** This test exposes a problem where a linker in error state is called
   *  multiple times and ends up thinking it is being used concurrently.
   */
  @Test
  def clean_linking_state(): AsyncResult = await {
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
  def testLegacyAPI(): AsyncResult = await {
    val classDefs = Seq(
        mainTestClassDef({
          consoleLog(StringLiteral("Hello world!"))
        })
    )

    val linker = StandardImpl.linker(StandardConfig())
    val classDefsFiles = classDefs.map(MemClassDefIRFile(_))

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
}
