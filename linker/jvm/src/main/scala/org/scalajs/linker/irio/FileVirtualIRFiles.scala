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
import java.nio._
import java.nio.channels._
import java.util.zip.{ZipInputStream, ZipEntry}

import org.scalajs.ir

trait FileScalaJSIRContainer extends ScalaJSIRContainer {
  val file: File
}

object FileScalaJSIRContainer {
  def fromClasspath(classpath: Seq[File])(
      implicit ec: ExecutionContext): Future[Seq[FileScalaJSIRContainer]] = {
    Future.traverse(classpath) { entry =>
      if (!entry.exists)
        Future.successful(Nil)
      else if (entry.isDirectory)
        fromDirectory(entry)
      else if (entry.getName.endsWith(".jar"))
        fromJar(entry).map(List(_))
      else
        throw new IllegalArgumentException("Illegal classpath entry " + entry)
    }.map(_.flatten)
  }

  def fromJar(file: File)(implicit ec: ExecutionContext): Future[FileScalaJSIRContainer] =
    Future(new FileVirtualJarScalaJSIRContainer(file))

  def fromSingleFile(file: File, relativePath: String)(
      implicit ec: ExecutionContext): Future[FileScalaJSIRContainer] = {
    Future(new FileVirtualScalaJSIRFile(file, relativePath))
  }

  private def fromDirectory(dir: File)(
      implicit ec: ExecutionContext): Future[Seq[FileScalaJSIRContainer]] = {
    require(dir.isDirectory)

    val baseDir = dir.getAbsoluteFile

    def walkForIR(dir: File): Seq[File] = {
      val (subdirs, files) = dir.listFiles().partition(_.isDirectory)
      subdirs.flatMap(walkForIR) ++ files.filter(_.getName.endsWith(".sjsir"))
    }

    Future.traverse(walkForIR(baseDir)) { ir =>
      val relPath = ir.getPath
        .stripPrefix(baseDir.getPath)
        .replace(java.io.File.separatorChar, '/')
        .stripPrefix("/")
      fromSingleFile(ir, relPath)
    }
  }
}

private final class FileVirtualScalaJSIRFile(
    val file: File, val relativePath: String)
    extends VirtualScalaJSIRFile with FileScalaJSIRContainer {
  val path: String = file.getPath

  val version: Option[String] = {
    if (!file.isFile) None
    else Some(file.lastModified.toString)
  }

  def entryPointsInfo(implicit ec: ExecutionContext): Future[ir.EntryPointsInfo] = {
    val chan = AsynchronousFileChannel.open(file.toPath)

    def loop(buf: ByteBuffer): Future[ir.EntryPointsInfo] = {
      AsyncIO.read(chan, buf).map { _ =>
        buf.flip()
        ir.Serializers.deserializeEntryPointsInfo(buf)
      }.recoverWith {
        case _: BufferUnderflowException =>
          // Reset to write again.
          buf.position(buf.limit())
          buf.limit(buf.capacity())

          val newBuf = if (buf.remaining() <= 0) {
            val newBuf = ByteBuffer.allocate(buf.capacity() * 2)
            buf.flip()
            newBuf.put(buf)
            buf
          } else {
            buf
          }

          loop(newBuf)
      }
    }

    val result = loop(ByteBuffer.allocate(1024)).andThen { case _ => chan.close() }
    VirtualScalaJSIRFile.withPathExceptionContext(path, result)
  }

  def tree(implicit ec: ExecutionContext): Future[ir.Trees.ClassDef] = {
    val chan = AsynchronousFileChannel.open(file.toPath)
    val s = chan.size()
    if (s > Int.MaxValue) {
      Future.failed(new IOException("$file is too big ($s bytes)"))
    } else {
      val buf = ByteBuffer.allocate(s.toInt)
      def read(): Future[Unit] = AsyncIO.read(chan, buf).flatMap { _ =>
        if (buf.hasRemaining()) read()
        else Future.successful(())
      }

      val result = for {
        _ <- read()
      } yield {
        buf.flip()
        ir.Serializers.deserialize(buf)
      }

      VirtualScalaJSIRFile.withPathExceptionContext(path, result)
    }
  }
}

private final class FileVirtualJarScalaJSIRContainer(val file: File) extends FileScalaJSIRContainer {
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

private object AsyncIO {
  def read(chan: AsynchronousFileChannel, buf: ByteBuffer): Future[Unit] = {
    val promise = Promise[Unit]()
    chan.read(buf, buf.position(), promise, ReadCompletionHandler)
    promise.future
  }

  private object ReadCompletionHandler extends CompletionHandler[Integer, Promise[Unit]] {
    def completed(result: Integer, attachment: Promise[Unit]): Unit =
      attachment.success(())

    def failed(exc: Throwable, attachment: Promise[Unit]): Unit =
      attachment.failure(exc)
  }
}
