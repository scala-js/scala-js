package org.scalajs.linker.test

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.typedarray._
import scala.scalajs.js.JSConverters._

import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.interface._

@JSExportTopLevel("TestSuiteLinker")
object QuickLinker {
  /** Link the Scala.js test suite on Node.js */
  @JSExport
  def linkTestSuiteNode(cp: js.Array[String], outputDir: String,
      reportPath: String): js.Promise[Unit] = {
    val config = StandardConfig()
      .withSemantics(build.TestSuiteLinkerOptions.semantics _)
      .withCheckIR(true)
      .withBatchMode(true)

    val linker = StandardImpl.linker(config)

    val moduleInitializers = {
      build.TestSuiteLinkerOptions.moduleInitializers :+
      // Copied from org.scalajs.testing.adapter.TestAdapaterInitializer.
      ModuleInitializer.mainMethod("org.scalajs.testing.bridge.Bridge", "start")
    }

    val out = NodeOutputDirectory(outputDir)

    val cache = StandardImpl.irFileCache().newCache

    NodeIRContainer.fromClasspath(cp.toSeq)
      .map(_._1)
      .flatMap(cache.cached _)
      .flatMap(linker.link(_, moduleInitializers, out, new ScalaConsoleLogger))
      .flatMap(writeReport(reportPath, _))
      .toJSPromise
  }

  private def writeReport(path: String, report: Report): Future[Unit] = {
    val int8arr = Report.serialize(report).toTypedArray
    val uint8arr = new Uint8Array(int8arr.buffer, int8arr.byteOffset, int8arr.byteLength)
    PromisesFS.writeFile(path, uint8arr).toFuture
  }

  @JSImport("fs", "promises")
  @js.native
  object PromisesFS extends js.Object {
    def writeFile(path: String, data: Uint8Array): js.Promise[Unit] = js.native
  }
}
