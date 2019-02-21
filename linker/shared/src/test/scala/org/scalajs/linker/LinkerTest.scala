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
import scala.concurrent.ExecutionContext.Implicits.global

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Definitions._
import org.scalajs.ir.Trees._

import org.scalajs.logging._
import org.scalajs.io._

import org.scalajs.junit.async._

import org.scalajs.linker._
import org.scalajs.linker.irio._

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

class LinkerTest {
  import LinkerTest._

  /** Makes sure that the minilib is sufficient to completely link a hello
   *  world.
   */
  @Test
  def linkHelloWorld(): AsyncResult = {
    val name = "LHelloWorld$"
    val mainMethodBody = {
      JSBracketMethodApply(JSGlobalRef(Ident("console")), StringLiteral("log"),
          List(StringLiteral("Hello world!")))
    }
    val classDefs = Seq(
        classDef(name, kind = ClassKind.ModuleClass,
            superClass = Some(ObjectClass),
            memberDefs = List(
                trivialCtor(name),
                mainMethodDef(mainMethodBody)
            )
        )
    )
    val moduleInitializers = List(
        ModuleInitializer.mainMethodWithArgs("HelloWorld", "main")
    )
    await(testLink(classDefs, moduleInitializers))
  }

  /** This test exposes a problem where a linker in error state is called
   *  multiple times and ends up thinking it is being used concurrently.
   */
  @Test
  def clean_linking_state(): AsyncResult = {
    class DummyException extends Exception

    val badSeq = new IndexedSeq[VirtualScalaJSIRFile] {
      def apply(x: Int): VirtualScalaJSIRFile = throw new DummyException()
      def length: Int = throw new DummyException()
    }

    val linker = StandardLinker(StandardLinker.Config())

    def callLink(): Future[Unit] = {
      val out = LinkerOutput(new WritableMemVirtualBinaryFile)
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

    await((1 to 4).foldLeft(firstRun)((p, _) => callInFailedState(p)))
  }

}

object LinkerTest {
  def testLink(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer]): Future[Unit] = {

    val linker = StandardLinker(StandardLinker.Config())

    val classDefsFiles = classDefs.map { classDef =>
      new VirtualScalaJSIRFile {
        def exists: Boolean = true
        def path: String = "mem://" + classDef.name.name + ".sjsir"
        def tree: ClassDef = classDef
        def relativePath: String = classDef.name.name + ".sjsir"
      }
    }

    val allIRFiles = TestIRRepo.minilib.stdlibIRFiles ++ classDefsFiles

    val output = LinkerOutput(new WritableMemVirtualBinaryFile)

    linker.link(allIRFiles, moduleInitializers, output,
        new ScalaConsoleLogger(Level.Error))
  }
}
