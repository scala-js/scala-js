package org.scalajs.core.tools.test.js

import java.io.InputStream

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.linker.{ModuleInitializer, Linker}
import org.scalajs.core.tools.linker.backend.{OutputMode, ModuleKind}
import org.scalajs.core.tools.logging._

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.typedarray._

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
        ModuleKind.NoModule, Linker.Config())

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
  @JSGlobal("JSZip")
  private class JSZip(data: Uint8Array) extends js.Object {
    def files: js.Dictionary[JSZipEntry] = js.native
  }

  private trait JSZipEntry extends js.Object {
    def asArrayBuffer(): ArrayBuffer
  }

}
