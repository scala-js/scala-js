/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.irio

import java.io._
import java.util.zip.{ZipInputStream, ZipEntry}

import scala.annotation.tailrec

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
          new MemVirtualSerializedScalaJSIRFile(
              path = s"${this.path}:${e.getName}",
              relativePath = e.getName,
              content = out.toByteArray,
              version = this.version
          )
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
