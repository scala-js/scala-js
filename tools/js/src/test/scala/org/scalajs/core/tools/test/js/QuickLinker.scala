package org.scalajs.core.tools.test.js

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.io.IRFileCache.IRContainer
import org.scalajs.core.tools.linker._
import org.scalajs.core.tools.logging._

import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSExportTopLevel("scalajs.QuickLinker")
object QuickLinker {

  /** Link a Scala.js application on Node.js */
  @JSExport
  def linkNode(irFilesAndJars: js.Array[String],
      moduleInitializers: js.Array[String]): String = {
    linkNodeInternal(Semantics.Defaults, irFilesAndJars, moduleInitializers)
  }

  /** Link the Scala.js test suite on Node.js */
  @JSExport
  def linkTestSuiteNode(irFilesAndJars: js.Array[String],
      moduleInitializers: js.Array[String]): String = {
    import Semantics.RuntimeClassNameMapper

    val semantics = Semantics.Defaults.withRuntimeClassNameMapper(
        RuntimeClassNameMapper.custom(_.fullName match {
          case "org.scalajs.testsuite.compiler.ReflectionTest$RenamedTestClass" =>
            "renamed.test.Class"
          case fullName =>
            fullName
        }).andThen(
            RuntimeClassNameMapper.regexReplace(
                raw"""^org\.scalajs\.testsuite\.compiler\.ReflectionTest\$$Prefix""".r,
                "renamed.test.byprefix.")
        ).andThen(
            RuntimeClassNameMapper.regexReplace(
                raw"""^org\.scalajs\.testsuite\.compiler\.ReflectionTest\$$OtherPrefix""".r,
                "renamed.test.byotherprefix.")
        )
    )

    linkNodeInternal(semantics, irFilesAndJars, moduleInitializers)
  }

  /** Link a Scala.js application on Node.js */
  def linkNodeInternal(semantics: Semantics,
      irFilesAndJars: collection.Seq[String], moduleInitializers: collection.Seq[String]): String = {
    val cache = (new IRFileCache).newCache

    val config = StandardLinker.Config()
      .withSemantics(semantics)
      .withBatchMode(true)
    val linker = StandardLinker(config)

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

    val ir = cache.cached(irContainers.toSeq)

    val parsedModuleInitializers =
      moduleInitializers.map(parseModuleInitializer)

    val out = WritableMemVirtualJSFile("out.js")
    linker.link(ir, parsedModuleInitializers.toSeq, out, new ScalaConsoleLogger)

    out.content
  }

  private def parseModuleInitializer(spec: String): ModuleInitializer = {
    def fail(): Nothing = {
      throw new IllegalArgumentException(
          s"'$spec' is not a valid module initializer spec")
    }

    def parseObjectAndMain(str: String): (String, String) = {
      val lastDot = str.lastIndexOf('.')
      if (lastDot < 0)
        fail()
      (str.substring(0, lastDot), str.substring(lastDot + 1))
    }

    val parenPos = spec.indexOf('(')
    if (parenPos < 0) {
      val (objectName, mainMethodName) = parseObjectAndMain(spec)
      ModuleInitializer.mainMethod(objectName, mainMethodName)
    } else {
      if (spec.last != ')')
        fail()
      val (objectName, mainMethodName) =
        parseObjectAndMain(spec.substring(0, parenPos))
      val args =
        spec.substring(parenPos + 1, spec.length - 1).split(",", -1).toList
      ModuleInitializer.mainMethodWithArgs(objectName, mainMethodName, args)
    }
  }

}
