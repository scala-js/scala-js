/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2018, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.linker.irio

import java.io._
import java.util.zip.{ZipInputStream, ZipEntry}

import scala.annotation.tailrec

import org.scalajs.io._

class FileVirtualScalaJSIRFile(f: File, val relativePath: String)
    extends FileVirtualBinaryFile(f) with VirtualSerializedScalaJSIRFile

object FileVirtualScalaJSIRFile {
  import FileVirtualFile._

  def apply(f: File, relPath: String): FileVirtualScalaJSIRFile =
    new FileVirtualScalaJSIRFile(f, relPath)

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
      FileVirtualScalaJSIRFile(ir, relPath)
    }
  }
}

final class FileVirtualJarScalaJSIRContainer(file: File)
    extends FileVirtualFile(file) with ScalaJSIRContainer {
  def sjsirFiles: List[VirtualScalaJSIRFile] = {
    val stream = new ZipInputStream(new BufferedInputStream(new FileInputStream(file)))
    try {
      val buf = new Array[Byte](4096)

      @tailrec
      def readAll(out: OutputStream): Unit = {
        val read = stream.read(buf)
        if (read != -1) {
          out.write(buf, 0, read)
          readAll(out)
        }
      }

      def makeVF(e: ZipEntry) = {
        val size = e.getSize
        val out =
          if (0 <= size && size <= Int.MaxValue) new ByteArrayOutputStream(size.toInt)
          else new ByteArrayOutputStream()

        try {
          readAll(out)
          val path = s"${this.path}:${e.getName}"
          new MemVirtualSerializedScalaJSIRFile(path, e.getName)
            .withContent(out.toByteArray)
            .withVersion(this.version)
        } finally {
          out.close()
        }
      }

      Iterator.continually(stream.getNextEntry())
        .takeWhile(_ != null)
        .filter(_.getName.endsWith(".sjsir"))
        .map(makeVF)
        .toList
    } finally {
      stream.close()
    }
  }
}
