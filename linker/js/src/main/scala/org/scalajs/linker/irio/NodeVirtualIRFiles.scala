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

import scala.util.{Success, Failure}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray._
import scala.scalajs.js.typedarray.TypedArrayBufferOps._

import java.io.EOFException
import java.nio._

import org.scalajs.ir

object NodeScalaJSIRContainer {
  import NodeFS._

  def fromClasspath(cp: Seq[String])(
      implicit ec: ExecutionContext): Future[Seq[NodeScalaJSIRContainer]] = {
    Future.traverse(cp) { entry =>
      stat(entry).transformWith {
        case Success(stat) =>
          if (stat.isDirectory)
            fromDirectory(entry)
          else if (entry.endsWith(".jar"))
            Future.successful(Seq(new NodeVirtualJarScalaJSIRContainer(entry, version(stat))))
          else
            throw new IllegalArgumentException("Illegal classpath entry " + entry)

        case Failure(js.JavaScriptException(e: js.Error)) if isNotFound(e) =>
          Future.successful(Nil)

        case Failure(t) =>
          throw t
      }
    }.map(_.flatten)
  }

  def fromJar(path: String)(implicit ec: ExecutionContext): Future[NodeScalaJSIRContainer] =
    stat(path).map(s => new NodeVirtualJarScalaJSIRContainer(path, version(s)))

  def fromSingleFile(path: String)(
      implicit ec: ExecutionContext): Future[NodeScalaJSIRContainer] = {
    stat(path).map(s => new NodeVirtualScalaJSIRFile(path, version(s)))
  }

  private def fromDirectory(dir: String)(
      implicit ec: ExecutionContext): Future[Seq[NodeScalaJSIRContainer]] = {
    cbFuture[js.Array[Dirent]](FS.readdir(dir, ReadDirOpt, _)).flatMap { entries =>
      val (dirs, files) = entries.toSeq.partition(_.isDirectory)

      val subdirFiles = Future.traverse(dirs) { e =>
        val path = Path.join(dir, e.name)
        fromDirectory(path)
      }

      val irFileNames = files.map(_.name).filter(_.endsWith(".sjsir"))
      val directFiles =
        Future.traverse(irFileNames)(n => fromSingleFile(Path.join(dir, n)))

      for {
        sdf <- subdirFiles
        df <- directFiles
      } yield sdf.flatten ++ df
    }
  }

  private def stat(path: String)(implicit ec: ExecutionContext): Future[Stats] =
    cbFuture[Stats](FS.stat(path, _))

  private def version(stats: Stats): Option[String] =
    stats.mtime.map(_.getTime.toString).toOption

  private def isNotFound(e: js.Error): Boolean =
    (e.asInstanceOf[js.Dynamic].code: Any) == "ENOENT"
}

abstract class NodeScalaJSIRContainer private[irio] (
    val path: String, val version: Option[String]) extends ScalaJSIRContainer

private class NodeVirtualScalaJSIRFile(path: String, version: Option[String])
    extends NodeScalaJSIRContainer(path, version) with VirtualScalaJSIRFile {
  import NodeFS._

  def entryPointsInfo(implicit ec: ExecutionContext): Future[ir.EntryPointsInfo] = {
    def loop(fd: Int, buf: ByteBuffer): Future[ir.EntryPointsInfo] = {
      val len = buf.remaining()
      val off = buf.position()

      cbFuture[Int](FS.read(fd, buf.typedArray(), off, len, off, _)).map { bytesRead =>
        if (bytesRead <= 0)
          throw new EOFException

        buf.position(buf.position() + bytesRead)
        buf.flip()
        ir.Serializers.deserializeEntryPointsInfo(buf)
      }.recoverWith {
        case _: BufferUnderflowException =>
          // Reset to write again.
          buf.position(buf.limit())
          buf.limit(buf.capacity())

          val newBuf = if (buf.remaining() <= 0) {
            val newBuf = ByteBuffer.allocateDirect(buf.capacity() * 2)
            buf.flip()
            newBuf.put(buf)
            buf
          } else {
            buf
          }

          loop(fd, newBuf)
      }
    }

    val result = cbFuture[Int](FS.open(path, "r", _)).flatMap { fd =>
      loop(fd, ByteBuffer.allocateDirect(1024))
        .finallyWith(cbFuture[Unit](FS.close(fd, _)))
    }

    VirtualScalaJSIRFile.withPathExceptionContext(path, result)
  }

  def tree(implicit ec: ExecutionContext): Future[ir.Trees.ClassDef] = {
    val result = cbFuture[Uint8Array](FS.readFile(path, _)).map { arr =>
      ir.Serializers.deserialize(TypedArrayBuffer.wrap(arr.buffer))
    }

    VirtualScalaJSIRFile.withPathExceptionContext(path, result)
  }

  def sjsirFiles(implicit ec: ExecutionContext): Future[List[VirtualScalaJSIRFile]] =
    Future.successful(this :: Nil)
}

private class NodeVirtualJarScalaJSIRContainer(path: String, version: Option[String])
    extends NodeScalaJSIRContainer(path, version) {
  import NodeFS._

  def sjsirFiles(implicit ec: ExecutionContext): Future[List[VirtualScalaJSIRFile]] = {
    for {
      arr <- cbFuture[Uint8Array](FS.readFile(path, _))
      zip <- JSZip.loadAsync(arr).toFuture
      files <- loadFromZip(zip)
    } yield {
      files.toList
    }
  }

  private def loadFromZip(obj: JSZip.JSZip)(
      implicit ec: ExecutionContext): Future[Iterator[VirtualScalaJSIRFile]] = {
    val entries = obj.files.valuesIterator
      .filter(e => e.name.endsWith(".sjsir") && !e.dir)

    Future.traverse(entries) { entry =>
      entry.async(JSZipInterop.arrayBuffer).toFuture.map { buf =>
        new MemVirtualSerializedScalaJSIRFile(
            path = s"${this.path}:${entry.name}",
            content = new Int8Array(buf).toArray,
            version = version
        )
      }
    }
  }
}

private object JSZipInterop {
  val arrayBuffer: String = "arraybuffer"
}

@js.native
@JSImport("jszip", JSImport.Default)
private object JSZip extends js.Object {
  trait JSZip extends js.Object {
    val files: js.Dictionary[ZipObject]
  }

  trait ZipObject extends js.Object {
    val name: String
    val dir: Boolean
    def async(tpe: JSZipInterop.arrayBuffer.type): js.Promise[ArrayBuffer]
  }

  def loadAsync(data: Uint8Array): js.Promise[JSZip] = js.native
}

@JSImport("path", JSImport.Namespace)
@js.native
private object Path extends js.Object {
  def join(paths: String*): String = js.native
}
