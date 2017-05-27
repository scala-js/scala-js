package org.scalajs.core.tools.test.js

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.io.IRFileCache.IRContainer
import org.scalajs.core.tools.linker.{ModuleInitializer, Linker}
import org.scalajs.core.tools.linker.frontend.LinkerFrontend
import org.scalajs.core.tools.linker.backend._
import org.scalajs.core.tools.logging._

import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSExportTopLevel("scalajs.QuickLinker")
object QuickLinker {

  /** Link a Scala.js application on Node.js */
  @JSExport
  def linkNode(irFilesAndJars: js.Array[String],
      mainMethods: js.Array[String]): String = {
    linkNodeInternal(Semantics.Defaults, irFilesAndJars, mainMethods)
  }

  /** Link the Scala.js test suite on Node.js */
  @JSExport
  def linkTestSuiteNode(irFilesAndJars: js.Array[String],
      mainMethods: js.Array[String]): String = {
    val semantics = Semantics.Defaults.withRuntimeClassName(_.fullName match {
      case "org.scalajs.testsuite.compiler.ReflectionTest$RenamedTestClass" =>
        "renamed.test.Class"
      case fullName =>
        fullName
    })
    linkNodeInternal(semantics, irFilesAndJars, mainMethods)
  }

  /** Link a Scala.js application on Node.js */
  def linkNodeInternal(semantics: Semantics,
      irFilesAndJars: Seq[String], mainMethods: Seq[String]): String = {
    val cache = (new IRFileCache).newCache

    val linker = Linker(semantics, OutputMode.ECMAScript51Isolated,
        ModuleKind.NoModule, LinkerFrontend.Config(), LinkerBackend.Config())

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
        throw new IllegalArgumentException("Illegal IR file / Jar: " + file)
      }
    }

    val ir = cache.cached(irContainers)

    val moduleInitializers = mainMethods.map { mainMethod =>
      val lastDot = mainMethod.lastIndexOf('.')
      if (lastDot < 0)
        throw new IllegalArgumentException(s"$mainMethod is not a valid main method")
      ModuleInitializer.mainMethod(mainMethod.substring(0, lastDot),
          mainMethod.substring(lastDot + 1))
    }

    val out = WritableMemVirtualJSFile("out.js")
    linker.link(ir, moduleInitializers, out, new ScalaConsoleLogger)

    out.content
  }

}
