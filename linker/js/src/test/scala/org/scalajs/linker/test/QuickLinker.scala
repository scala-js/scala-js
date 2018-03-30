package org.scalajs.linker.test

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.io._

import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.irio._

import org.scalajs.linker.testutils.Platform

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
        RuntimeClassNameMapper.keepAll().andThen(
            RuntimeClassNameMapper.regexReplace(
                raw"""^org\.scalajs\.testsuite\.compiler\.ReflectionTest\$$RenamedTestClass$$""".r,
                "renamed.test.Class")
        ).andThen(
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
      irFilesAndJars: Seq[String], moduleInitializers: Seq[String]): String = {
    val cache = (new IRFileCache).newCache

    val config = StandardLinker.Config()
      .withSemantics(semantics)
      .withBatchMode(true)
    val linker = StandardLinker(config)

    val irContainers = irFilesAndJars.map { file =>
      if (file.endsWith(".jar")) {
        Platform.loadJar(file)
      } else if (file.endsWith(".sjsir")) {
        // The compiler should not use this (only scalajsp does)
        val relativePath: String = s"<dummy relative path from $getClass>"
        new NodeVirtualScalaJSIRFile(file, relativePath)
      } else {
        throw new IllegalArgumentException("Illegal IR file / Jar: " + file)
      }
    }

    val ir = cache.cached(irContainers)

    val parsedModuleInitializers =
      moduleInitializers.map(parseModuleInitializer)

    val out = WritableMemVirtualJSFile("out.js")
    linker.link(ir, parsedModuleInitializers, out, new ScalaConsoleLogger)

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
