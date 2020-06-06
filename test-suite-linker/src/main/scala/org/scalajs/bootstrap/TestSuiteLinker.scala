package org.scalajs.linker.test

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSConverters._

import java.net.URI

import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.interface._

@JSExportTopLevel("TestSuiteLinker")
object QuickLinker {
  /** Link the Scala.js test suite on Node.js */
  @JSExport
  def linkTestSuiteNode(cp: js.Array[String], outputPath: String): js.Promise[Unit] = {
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

    val smPath = outputPath + ".map"

    def relURI(path: String) = new URI(null, null, basename(path), null)

    val out = LinkerOutput(NodeOutputFile(outputPath))
      .withSourceMap(NodeOutputFile(smPath))
      .withSourceMapURI(relURI(smPath))
      .withJSFileURI(relURI(outputPath))

    val cache = StandardImpl.irFileCache().newCache

    NodeIRContainer.fromClasspath(cp.toSeq)
      .map(_._1)
      .flatMap(cache.cached _)
      .flatMap(linker.link(_, moduleInitializers, out, new ScalaConsoleLogger))
      .toJSPromise
  }

  @JSImport("path", "basename")
  @js.native
  private def basename(str: String): String = js.native
}
