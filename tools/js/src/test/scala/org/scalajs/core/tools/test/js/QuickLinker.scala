package org.scalajs.core.tools.test.js

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.linker.backend.{OutputMode, ModuleKind}
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.io.IRFileCache.IRContainer
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.linker.Linker

import scala.scalajs.js.annotation.JSExport

@JSExport("scalajs.QuickLinker")
object QuickLinker {

  /** Link a Scala.js application on Node.js */
  @JSExport
  def linkNode(irFilesAndJars: String*): String =
    linkNodeInternal(Semantics.Defaults, irFilesAndJars)

  /** Link the Scala.js test suite on Node.js */
  @JSExport
  def linkTestSuiteNode(irFilesAndJars: String*): String = {
    val semantics = Semantics.Defaults.withRuntimeClassName(_.fullName match {
      case "org.scalajs.testsuite.compiler.ReflectionTest$RenamedTestClass" =>
        "renamed.test.Class"
      case fullName =>
        fullName
    })
    linkNodeInternal(semantics, irFilesAndJars)
  }

  /** Link a Scala.js application on Node.js */
  def linkNodeInternal(semantics: Semantics,
      irFilesAndJars: Seq[String]): String = {
    val cache = (new IRFileCache).newCache

    val linker = Linker(semantics, OutputMode.ECMAScript51Isolated,
        ModuleKind.NoModule, Linker.Config())

    val irContainers = irFilesAndJars.map { file =>
      if (file.endsWith(".jar")) {
        val vf = new NodeVirtualBinaryFile(file) with VirtualJarFile
        IRContainer.Jar(vf)
      } else if (file.endsWith(".sjsir")) {
        val vf = new NodeVirtualScalaJSIRFile(file) with RelativeVirtualFile {
          // The compiler should not use this (only scalajsp does)
          def relativePath: String = s"<dummy relative path from $getClass>"
        }
        IRContainer.File(vf)
      } else {
        sys.error("Illegal IR file / Jar: " + file)
      }
    }

    val ir = cache.cached(irContainers)

    val out = WritableMemVirtualJSFile("out.js")
    linker.link(ir, out, new ScalaConsoleLogger)

    out.content
  }

}
