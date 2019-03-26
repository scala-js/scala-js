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

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray._
import scala.scalajs.js.typedarray.TypedArrayBufferOps._

import java.nio._

import org.scalajs.ir

trait NodeScalaJSIRContainer extends ScalaJSIRContainer {
  val path: String
}

object NodeScalaJSIRContainer {
  import NodeInterop._

  def fromClasspath(cp: Seq[String])(
      implicit ec: ExecutionContext): Future[Seq[NodeScalaJSIRContainer]] = {
    Future.traverse(cp) { entry =>
      stat(entry).flatMap { stat =>
        if (stat.isDirectory)
          fromDirectory(entry)
        else if (entry.endsWith(".jar"))
          Future.successful(Seq(new NodeVirtualJarScalaJSIRContainer(entry, version(stat))))
        else
          throw new IllegalArgumentException("Illegal classpath entry " + entry)
      }.recover {
        case js.JavaScriptException(e: js.Error) if isNotFound(e) =>
          Nil
      }
    }.map(_.flatten)
  }

  def fromJar(path: String)(implicit ec: ExecutionContext): Future[NodeScalaJSIRContainer] =
    stat(path).map(s => new NodeVirtualJarScalaJSIRContainer(path, version(s)))

  def fromSingleFile(path: String, relativePath: String)(
      implicit ec: ExecutionContext): Future[NodeScalaJSIRContainer] = {
    stat(path).map(s => new NodeVirtualScalaJSIRFile(path, relativePath, version(s)))
  }

  private def fromDirectory(dir: String)(
      implicit ec: ExecutionContext): Future[Seq[NodeScalaJSIRContainer]] = {
    cbFuture[js.Array[FS.Dirent]](FS.readdir(dir, ReadDirOpt, _)).flatMap { entries =>
      val (dirs, files) = entries.toSeq.partition(_.isDirectory)

      val subdirFiles = Future.traverse(dirs) { e =>
        val path = Path.join(dir, e.name)
        fromDirectory(path)
      }

      // Since we will remove relativePath (#3580) we do not bother calculating it here.
      val irFileNames = files.map(_.name).filter(_.endsWith(".sjsir"))
      val directFiles = Future.traverse(irFileNames) { name =>
        val path = Path.join(dir, name)
        fromSingleFile(path, "dummy")
      }

      for {
        sdf <- subdirFiles
        df <- directFiles
      } yield sdf.flatten ++ df
    }
  }

  private def stat(path: String)(implicit ec: ExecutionContext): Future[FS.Stats] =
    cbFuture[FS.Stats](FS.stat(path, _))

  private def version(stats: FS.Stats): Option[String] =
    stats.mtime.map(_.getTime.toString).toOption

  private def isNotFound(e: js.Error): Boolean = {
    val code = e.asInstanceOf[js.Dynamic].code.asInstanceOf[js.UndefOr[String]]
    code.exists(_ == "ENOENT")
  }
}

private class NodeVirtualScalaJSIRFile(
    val path: String,
    val relativePath: String,
    val version: Option[String]
) extends VirtualScalaJSIRFile with NodeScalaJSIRContainer {
  import NodeInterop._

  def entryPointsInfo(implicit ec: ExecutionContext): Future[ir.EntryPointsInfo] = {
    def loop(fd: Int, buf: ByteBuffer): Future[ir.EntryPointsInfo] = {
      val len = buf.remaining()
      val off = buf.position()

      cbFuture[Int](FS.read(fd, buf.typedArray(), off, len, off, _)).map { bytesRead =>
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

    def futureFinally[T](v: Future[T], f: => Future[Unit]): Future[T] = {
      v.recoverWith { case vt =>
        f.transform(_ => throw vt, ft => { ft.addSuppressed(vt); ft })
      }.flatMap { res => f.map(_ => res) }
    }

    val result = cbFuture[Int](FS.open(path, "r", _)).flatMap { fd =>
      futureFinally(
          loop(fd, ByteBuffer.allocateDirect(1024)),
          cbFuture[Unit](FS.close(fd, _)))
    }

    VirtualScalaJSIRFile.withPathExceptionContext(path, result)
  }

  def tree(implicit ec: ExecutionContext): Future[ir.Trees.ClassDef] = {
    val result = cbFuture[Uint8Array](FS.readFile(path, _)).map { arr =>
      ir.Serializers.deserialize(TypedArrayBuffer.wrap(arr.buffer))
    }

    VirtualScalaJSIRFile.withPathExceptionContext(path, result)
  }
}

private class NodeVirtualJarScalaJSIRContainer(
    val path: String, val version: Option[String]) extends NodeScalaJSIRContainer {
  import NodeInterop._

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
            relativePath = entry.name,
            content = new Int8Array(buf).toArray,
            version = version
        )
      }
    }
  }
}

private object NodeInterop {
  type CB[T] = js.Function2[js.Error, T, Unit]

  def cbFuture[A](op: CB[A] => Unit): Future[A] = {
    val promise = Promise[A]()

    def cb(err: js.Error, v: A): Unit = {
      import js.DynamicImplicits.truthValue

      if (err.asInstanceOf[js.Dynamic])
        promise.failure(new js.JavaScriptException(err))
      else
        promise.success(v)
    }

    op(cb _)

    promise.future
  }

  object ReadDirOpt extends js.Object {
    val withFileTypes: Boolean = true
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

@JSImport("fs", JSImport.Namespace)
@js.native
private object FS extends js.Object {
  trait Stats extends js.Object {
    val mtime: js.UndefOr[js.Date]
    def isDirectory(): Boolean
  }

  trait Dirent extends js.Object {
    val name: String
    def isDirectory(): Boolean
  }

  def open(path: String, flags: String, callback: NodeInterop.CB[Int]): Unit = js.native
  def close(fd: Int, callback: NodeInterop.CB[Unit]): Unit = js.native

  def read(fd: Int, buffer: TypedArray[_, _], offset: Int, length: Int, position: Int,
    callback: NodeInterop.CB[Int]): Unit = js.native

  def readdir(path: String, opts: NodeInterop.ReadDirOpt.type,
      cb: NodeInterop.CB[js.Array[Dirent]]): Unit = js.native

  def readFile(path: String, cb: NodeInterop.CB[Uint8Array]): Unit = js.native

  def stat(path: String, cb: NodeInterop.CB[Stats]): Unit = js.native
}

@JSImport("path", JSImport.Namespace)
@js.native
private object Path extends js.Object {
  def join(paths: String*): String = js.native
}
