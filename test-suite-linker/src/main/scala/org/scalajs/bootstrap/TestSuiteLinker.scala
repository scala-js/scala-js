package org.scalajs.linker.test

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.io._

import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.irio._

@JSExportTopLevel("TestSuiteLinker")
object QuickLinker {
  /** Link the Scala.js test suite on Node.js */
  @JSExport
  def linkTestSuiteNode(irFilesAndJars: js.Array[String], outputPath: String): Unit = {
    val config = StandardLinker.Config()
      .withSemantics(build.TestSuiteLinkerOptions.semantics _)
      .withCheckIR(true)
      .withBatchMode(true)

    val linker = StandardLinker(config)

    val moduleInitializers = {
      build.TestSuiteLinkerOptions.moduleInitializers :+
      // Copied from org.scalajs.testadapter.TestAdapaterInitializer.
      ModuleInitializer.mainMethod("org.scalajs.testinterface.Bridge", "start")
    }

    val ir = extractIR(irFilesAndJars)

    val out = WritableNodeVirtualJSFile(outputPath)
    linker.link(ir, moduleInitializers, out, new ScalaConsoleLogger)

    out.content
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
}
