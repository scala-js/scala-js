/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2018, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.linker.irio

import java.io._
import java.util.zip.{ZipFile, ZipEntry}

import scala.collection.JavaConverters._

import org.scalajs.ir
import org.scalajs.io._

final class FileVirtualScalaJSIRFile(f: File, val relativePath: String)
    extends FileVirtualBinaryFile(f) with VirtualSerializedScalaJSIRFile

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
      new FileVirtualScalaJSIRFile(ir, relPath)
    }
  }
}

final class FileVirtualJarScalaJSIRContainer(file: File)
    extends FileVirtualFile(file) with ScalaJSIRContainer {
  def sjsirFiles: List[VirtualScalaJSIRFile] = {
    val pinnedVersion = version
    withZipFile { zipFile =>
      zipFile.entries().asScala
        .withFilter(_.getName.endsWith(".sjsir"))
        .map(entryFile(_, zipFile, pinnedVersion))
        .toList
    }
  }

  private def entryFile(entry: ZipEntry, zipFile: ZipFile, fileVersion: Option[String]) = {
    val filePath = s"$path:${entry.getName()}"

    @inline
    def withInputStream[A](zf: ZipFile)(f: InputStream => A) = {
      val in = zf.getInputStream(entry)
      try VirtualScalaJSIRFile.withPathExceptionContext(filePath)(f(in))
      finally in.close()
    }

    val info =
      withInputStream(zipFile)(ir.Serializers.deserializeEntryPointsInfo)

    new VirtualScalaJSIRFile {
      val path: String = filePath
      val relativePath: String = entry.getName()
      override val version: Option[String] = fileVersion
      override val entryPointsInfo: ir.EntryPointsInfo = info
      def tree: ir.Trees.ClassDef =
        withZipFile(withInputStream(_)(ir.Serializers.deserialize))
    }
  }

  @inline
  private def withZipFile[A](f: ZipFile => A): A = {
    val zf = new ZipFile(file)
    try f(zf)
    finally zf.close()
  }
}
