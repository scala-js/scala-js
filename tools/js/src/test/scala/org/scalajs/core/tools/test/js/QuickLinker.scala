package org.scalajs.core.tools.test.js

import java.io.InputStream

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.linker._
import org.scalajs.core.tools.logging._

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.typedarray._

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
      irFilesAndJars: Seq[String], moduleInitializers: Seq[String]): String = {
    val cache = (new IRFileCache).newCache

    val config = StandardLinker.Config()
      .withSemantics(semantics)
      .withBatchMode(true)
    val linker = StandardLinker(config)

    val irContainers = irFilesAndJars.map { file =>
      if (file.endsWith(".jar")) {
        new NodeVirtualJarFile(file)
      } else if (file.endsWith(".sjsir")) {
        new NodeVirtualScalaJSIRFile(file) with VirtualRelativeScalaJSIRFile {
          // The compiler should not use this (only scalajsp does)
          def relativePath: String = s"<dummy relative path from $getClass>"
        }
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

  private class NodeVirtualJarFile(file: String)
      extends NodeVirtualBinaryFile(file) with VirtualFileContainer {

    def listEntries[T](p: String => Boolean)(
        makeResult: (String, InputStream) => T): List[T] = {
      import js.Dynamic.{global => g}

      val stream = inputStream
      try {
        /* Build a Uint8Array with the content of this jar file.
         * We know that in practice, NodeVirtualBinaryFile#inputStream returns
         * an ArrayBufferInputStream, so we just fetch its internal ArrayBuffer
         * rather than copying.
         *
         * Since we have NodeVirtualBinaryFile under our control, in the same
         * repository, we can make this assumption. Should we change
         * NodeVirtualBinaryFile, this test will immediately fail, and we can
         * adapt it.
         */
        val data = stream match {
          case stream: ArrayBufferInputStream =>
            // Simulate reading all the data
            while (stream.skip(stream.available()) > 0) {}
            new Uint8Array(stream.buffer, stream.offset, stream.length)
          case _ =>
            throw new AssertionError(
                s"Uh! '$file' was not read as an ArrayBufferInputStream")
        }

        val zip = new JSZip(data)

        for ((name, entry) <- zip.files.toList if p(name)) yield {
          val entryStream = new ArrayBufferInputStream(entry.asArrayBuffer())
          try {
            makeResult(name, entryStream)
          } finally {
            entryStream.close()
          }
        }
      } finally {
        stream.close()
      }
    }
  }

  @js.native
  @JSImport("jszip", JSImport.Default)
  private class JSZip(data: Uint8Array) extends js.Object {
    def files: js.Dictionary[JSZipEntry] = js.native
  }

  private trait JSZipEntry extends js.Object {
    def asArrayBuffer(): ArrayBuffer
  }

}
