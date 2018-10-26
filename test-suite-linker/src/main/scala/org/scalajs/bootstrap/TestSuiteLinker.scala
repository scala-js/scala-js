package org.scalajs.linker.test

import scala.concurrent.ExecutionContext.Implicits.global

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSConverters._

import java.net.URI

import org.scalajs.io._

import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.irio._

@JSExportTopLevel("TestSuiteLinker")
object QuickLinker {
  /** Link the Scala.js test suite on Node.js */
  @JSExport
  def linkTestSuiteNode(irFilesAndJars: js.Array[String], outputPath: String): js.Promise[Unit] = {
    val config = StandardLinker.Config()
      .withSemantics(build.TestSuiteLinkerOptions.semantics _)
      .withCheckIR(true)
      .withBatchMode(true)

    val linker = StandardLinker(config)

    val moduleInitializers = {
      build.TestSuiteLinkerOptions.moduleInitializers :+
      // Copied from org.scalajs.testing.adapter.TestAdapaterInitializer.
      ModuleInitializer.mainMethod("org.scalajs.testing.interface.Bridge", "start")
    }

    val ir = extractIR(irFilesAndJars)

    val smPath = outputPath + ".map"

    def relURI(path: String) =
      new URI(null, null, NodePath.basename(path), null)

    val out = LinkerOutput(WritableNodeVirtualBinaryFile(outputPath))
      .withSourceMap(WritableNodeVirtualBinaryFile(smPath))
      .withSourceMapURI(relURI(smPath))
      .withJSFileURI(relURI(outputPath))

    linker.link(ir, moduleInitializers, out, new ScalaConsoleLogger).toJSPromise
  }

  private def extractIR(irFilesAndJars: Seq[String]): Seq[VirtualScalaJSIRFile] = {
    val cache = (new IRFileCache).newCache
    val irContainers = irFilesAndJars.map { file =>
      if (file.endsWith(".jar")) {
        new NodeVirtualJarScalaJSIRContainer(file)
      } else if (file.endsWith(".sjsir")) {
        // The compiler should not use this (only scalajsp does)
        val relativePath: String = s"<dummy relative path from $getClass>"
        new NodeVirtualScalaJSIRFile(file, relativePath)
      } else {
        throw new IllegalArgumentException("Illegal IR file / Jar: " + file)
      }
    }

    cache.cached(irContainers)
  }

  @JSImport("path", JSImport.Namespace)
  @js.native
  private object NodePath extends js.Object {
    def basename(str: String): String = js.native
  }
}
