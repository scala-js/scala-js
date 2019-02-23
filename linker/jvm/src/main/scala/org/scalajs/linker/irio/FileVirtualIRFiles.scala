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

import scala.annotation.tailrec
import scala.concurrent._

import java.io._
import java.util.zip.{ZipInputStream, ZipEntry}

import org.scalajs.ir

trait FileScalaJSIRContainer extends ScalaJSIRContainer {
  val file: File
}

object FileScalaJSIRContainer {
  def fromClasspath(classpath: Seq[File]): Seq[FileScalaJSIRContainer] = {
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

  private def fromDirectory(dir: File): Seq[FileScalaJSIRContainer] = {
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

final class FileVirtualScalaJSIRFile(
    val file: File, val relativePath: String)
    extends VirtualScalaJSIRFile with FileScalaJSIRContainer {
  val path: String = file.getPath

  val version: Option[String] = {
    if (!file.isFile) None
    else Some(file.lastModified.toString)
  }

  def entryPointsInfo(implicit ec: ExecutionContext): Future[ir.EntryPointsInfo] =
    withInputStream(ir.Serializers.deserializeEntryPointsInfo)

  def tree(implicit ec: ExecutionContext): Future[ir.Trees.ClassDef] =
    withInputStream(ir.Serializers.deserialize)

  @inline
  private def withInputStream[A](f: InputStream => A)(
      implicit ec: ExecutionContext): Future[A] = {
    def read() = {
      val stream = new BufferedInputStream(new FileInputStream(file))
      try f(stream)
      finally stream.close()
    }

    VirtualScalaJSIRFile.withPathExceptionContext(path, Future(blocking(read())))
  }
}

final class FileVirtualJarScalaJSIRContainer(val file: File) extends FileScalaJSIRContainer {
  val path: String = file.getPath

  val version: Option[String] = {
    if (!file.isFile) None
    else Some(file.lastModified.toString)
  }

  def sjsirFiles(implicit ec: ExecutionContext): Future[List[VirtualScalaJSIRFile]] =
    Future(blocking(read()))

  private def read(): List[VirtualScalaJSIRFile] = {
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
