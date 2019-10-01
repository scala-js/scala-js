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

package org.scalajs.linker

import scala.concurrent._

import scala.util.{Success, Failure}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray._
import scala.scalajs.js.typedarray.TypedArrayBufferOps._

import java.nio._

import org.scalajs.linker.standard.{IRContainerImpl, MemIRFileImpl}

abstract class IRContainerPlatformExtensions private[linker] () {
  import NodeFS._
  import FS._
  import IRContainerPlatformExtensions._

  def fromNodeClasspath(classpath: Seq[String])(
      implicit ec: ExecutionContext): Future[(Seq[IRContainer], Seq[String])] = {
    Future.traverse(classpath) { entry =>
      cbFuture[Stats](FS.stat(entry, _)).transformWith {
        case Success(stat) if stat.isDirectory =>
          fromDirectory(entry)

        case Success(stat) if entry.endsWith(".jar") =>
          val c = new NodeJarIRContainer(entry, stat.mtime.toOption)
          Future.successful(Seq((c, entry)))

        case Success(_) =>
          throw new IllegalArgumentException("Illegal classpath entry: " + entry)

        case Failure(js.JavaScriptException(e: js.Error)) if isNotFound(e) =>
          Future.successful(Nil)

        case Failure(t) =>
          throw t
      }
    }.map(_.flatten.unzip)
  }

  private def fromDirectory(dir: String)(
      implicit ec: ExecutionContext): Future[Seq[(IRContainer, String)]] = {
    cbFuture[js.Array[Dirent]](FS.readdir(dir, ReadDirOpt, _)).flatMap { entries =>
      val (dirs, files) = entries.toSeq.partition(_.isDirectory)

      val subdirFiles = Future.traverse(dirs) { e =>
        val path = Path.join(dir, e.name)
        fromDirectory(path)
      }

      val irFileNames = files.map(_.name).filter(_.endsWith(".sjsir"))
      val directFiles = Future.traverse(irFileNames) { n =>
        val path = Path.join(dir, n)
        IRFile.fromNodePath(path).map(f => (IRContainer.fromIRFile(f), path))
      }

      for {
        sdf <- subdirFiles
        df <- directFiles
      } yield sdf.flatten ++ df
    }
  }

  private def isNotFound(e: js.Error): Boolean =
    (e.asInstanceOf[js.Dynamic].code: Any) == "ENOENT"
}

private object IRContainerPlatformExtensions {
  import NodeFS._

  private final class NodeJarIRContainer(path: String, version: Option[js.Date])
      extends IRContainerImpl(path, version.map(_.getTime.toString)) {
    import NodeFS._

    def sjsirFiles(implicit ec: ExecutionContext): Future[List[IRFile]] = {
      for {
        arr <- cbFuture[Uint8Array](FS.readFile(path, _))
        zip <- JSZip.loadAsync(arr).toFuture
        files <- loadFromZip(zip)
      } yield {
        files.toList
      }
    }

    private def loadFromZip(obj: JSZip.JSZip)(
        implicit ec: ExecutionContext): Future[Iterator[IRFile]] = {
      val entries = obj.files.valuesIterator
        .filter(e => e.name.endsWith(".sjsir") && !e.dir)

      Future.traverse(entries) { entry =>
        entry.async(JSZipInterop.arrayBuffer).toFuture.map { buf =>
          new MemIRFileImpl(s"${this.path}:${entry.name}", version, new Int8Array(buf).toArray)
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
}
