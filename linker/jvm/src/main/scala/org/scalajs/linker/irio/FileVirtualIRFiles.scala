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
import java.nio.file._
import java.nio.file.attribute._
import java.nio.channels._
import java.util.EnumSet
import java.util.zip.{ZipInputStream, ZipEntry}

import org.scalajs.ir

object FileScalaJSIRContainer {
  def fromClasspath(classpath: Seq[Path])(
      implicit ec: ExecutionContext): Future[Seq[FileScalaJSIRContainer]] = Future {
    val result = Seq.newBuilder[FileScalaJSIRContainer]

    val dirVisitor = new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        if (file.getFileName().toString().endsWith(".sjsir")) {
          result += new FileVirtualScalaJSIRFile(file, attrs.lastModifiedTime())
        }
        super.visitFile(file, attrs)
      }
    }

    blocking {
      for (entry <- classpath if Files.exists(entry)) {
        val attrs = Files.readAttributes(entry, classOf[BasicFileAttributes])

        if (attrs.isDirectory())
          Files.walkFileTree(entry, EnumSet.of(FileVisitOption.FOLLOW_LINKS), Int.MaxValue, dirVisitor)
        else if (entry.getFileName().toString().endsWith(".jar"))
          result += new FileVirtualJarScalaJSIRContainer(entry, attrs.lastModifiedTime())
        else
          throw new IllegalArgumentException("Illegal classpath entry " + entry)
      }
    }

    result.result()
  }

  def fromJar(file: Path)(implicit ec: ExecutionContext): Future[FileScalaJSIRContainer] =
    lastModified(file).map(new FileVirtualJarScalaJSIRContainer(file, _))

  def fromSingleFile(file: Path)(implicit ec: ExecutionContext): Future[FileScalaJSIRContainer] =
    lastModified(file).map(new FileVirtualScalaJSIRFile(file, _))

  private def lastModified(path: Path)(implicit ec: ExecutionContext): Future[FileTime] =
    Future(blocking(Files.getLastModifiedTime(path)))
}

abstract class FileScalaJSIRContainer private[irio] (val file: Path, lastModified: FileTime)
    extends ScalaJSIRContainer {
  final val path: String = file.toString

  final val version: Option[String] = Some(lastModified.toString)
}

private final class FileVirtualScalaJSIRFile(file: Path, lastModified: FileTime)
    extends FileScalaJSIRContainer(file, lastModified) with VirtualScalaJSIRFile {
  def entryPointsInfo(implicit ec: ExecutionContext): Future[ir.EntryPointsInfo] = {
    def loop(chan: AsynchronousFileChannel, buf: ByteBuffer): Future[ir.EntryPointsInfo] = {
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

          loop(chan, newBuf)
      }
    }

    withChannel(loop(_, ByteBuffer.allocate(1024)))
  }

  def tree(implicit ec: ExecutionContext): Future[ir.Trees.ClassDef] = {
    withChannel { chan =>
      val s = chan.size()
      if (s > Int.MaxValue) {
        throw new IOException("$file is too big ($s bytes)")
      } else {
        val buf = ByteBuffer.allocate(s.toInt)
        def read(): Future[Unit] = AsyncIO.read(chan, buf).flatMap { _ =>
          if (buf.hasRemaining()) read()
          else Future.successful(())
        }

        read().map { _ =>
          buf.flip()
          ir.Serializers.deserialize(buf)
        }
      }
    }
  }

  def sjsirFiles(implicit ec: ExecutionContext): Future[List[VirtualScalaJSIRFile]] =
    Future.successful(this :: Nil)

  private def withChannel[T](body: AsynchronousFileChannel => Future[T])(
      implicit ec: ExecutionContext): Future[T] = {
    val result = Future(AsynchronousFileChannel.open(file)).flatMap { chan =>
      body(chan).finallyWith(Future(blocking(chan.close())))
    }

    VirtualScalaJSIRFile.withPathExceptionContext(path, result)
  }
}

private final class FileVirtualJarScalaJSIRContainer(file: Path, lastModified: FileTime)
    extends FileScalaJSIRContainer(file, lastModified) {
  def sjsirFiles(implicit ec: ExecutionContext): Future[List[VirtualScalaJSIRFile]] =
    Future(blocking(read()))

  private def read(): List[VirtualScalaJSIRFile] = {
    val stream = new ZipInputStream(new BufferedInputStream(Files.newInputStream(file)))
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
    def completed(result: Integer, attachment: Promise[Unit]): Unit = {
      if (result <= 0)
        attachment.failure(new EOFException)
      else
        attachment.success(())
    }

    def failed(exc: Throwable, attachment: Promise[Unit]): Unit =
      attachment.failure(exc)
  }
}
