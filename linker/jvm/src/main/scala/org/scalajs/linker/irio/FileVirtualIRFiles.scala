/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2018, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.linker.irio

import java.io._

import org.scalajs.io._

class FileVirtualScalaJSIRFile(f: File)
    extends FileVirtualBinaryFile(f) with VirtualSerializedScalaJSIRFile

object FileVirtualScalaJSIRFile extends (File => FileVirtualScalaJSIRFile) {
  import FileVirtualFile._

  def apply(f: File): FileVirtualScalaJSIRFile =
    new FileVirtualScalaJSIRFile(f)

  def relative(f: File, relPath: String): FileVirtualScalaJSIRFile
      with VirtualRelativeScalaJSIRFile = {
    new FileVirtualScalaJSIRFile(f) with VirtualRelativeScalaJSIRFile {
      def relativePath: String = relPath
    }
  }

  def isScalaJSIRFile(file: File): Boolean =
    hasExtension(file, ".sjsir")
}

object FileScalaJSIRContainer {
  def fromClasspath(
      classpath: Seq[File]): Seq[ScalaJSIRContainer with FileVirtualFile] = {
    classpath.flatMap { entry =>
      if (!entry.exists)
        Nil
      else if (entry.isDirectory)
        fromDirectory(entry)
      else if (entry.getName.endsWith(".jar"))
        List(new FileVirtualJarScalaJSIRContainer(entry))
      else
        throw new IllegalArgumentException("Illegal classpath entry " + entry)
    }
  }

  private def fromDirectory(
      dir: File): Seq[ScalaJSIRContainer with FileVirtualFile] = {
    require(dir.isDirectory)

    val baseDir = dir.getAbsoluteFile

    def walkForIR(dir: File): Seq[File] = {
      val (subdirs, files) = dir.listFiles().partition(_.isDirectory)
      subdirs.flatMap(walkForIR) ++ files.filter(_.getName.endsWith(".sjsir"))
    }

    for (ir <- walkForIR(baseDir)) yield {
      val relPath = ir.getPath
        .stripPrefix(baseDir.getPath)
        .replace(java.io.File.separatorChar, '/')
        .stripPrefix("/")
      FileVirtualScalaJSIRFile.relative(ir, relPath)
    }
  }
}

class FileVirtualJarScalaJSIRContainer(file: File)
    extends FileVirtualJarFile(file) with ScalaJSIRContainer {

  def sjsirFiles: List[VirtualRelativeScalaJSIRFile] =
    ScalaJSIRContainer.sjsirFilesIn(this)
}
