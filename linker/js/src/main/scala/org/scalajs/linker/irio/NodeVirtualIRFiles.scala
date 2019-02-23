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

import java.io._

import org.scalajs.ir

class NodeVirtualScalaJSIRFile(val path: String, val relativePath: String) extends VirtualScalaJSIRFile {
  val version: Option[String] =
    NodeFS.statSync(path).mtime.map(_.getTime.toString).toOption

  def entryPointsInfo(implicit ec: ExecutionContext): Future[ir.EntryPointsInfo] =
    withInputStream(ir.Serializers.deserializeEntryPointsInfo)

  def tree(implicit ec: ExecutionContext): Future[ir.Trees.ClassDef] =
    withInputStream(ir.Serializers.deserialize)

  @inline
  private def withInputStream[A](f: InputStream => A)(
      implicit ec: ExecutionContext): Future[A] = {
    def read() = {
      val buf = new Uint8Array(NodeFS.readFileSync(path)).buffer
      val stream = new ArrayBufferInputStream(buf)
      try f(stream)
      finally stream.close()
    }

    VirtualScalaJSIRFile.withPathExceptionContext(path, Future(blocking(read())))
  }
}

private[scalajs] class NodeVirtualJarScalaJSIRContainer(val path: String) extends ScalaJSIRContainer {
  import NodeVirtualJarScalaJSIRContainer.JSZip

  val version: Option[String] =
    NodeFS.statSync(path).mtime.map(_.getTime.toString).toOption

  def sjsirFiles(implicit ec: ExecutionContext): Future[List[VirtualScalaJSIRFile]] = Future {
    val zip = new JSZip(NodeFS.readFileSync(path))

    for {
      (name, entry) <- zip.files.toList
      if name.endsWith(".sjsir")
    } yield {
      new MemVirtualSerializedScalaJSIRFile(
          path = s"${this.path}:$name",
          relativePath = name,
          content = new Int8Array(entry.asArrayBuffer()).toArray,
          version = this.version
      )
    }
  }
}

private object NodeVirtualJarScalaJSIRContainer {
  @js.native
  @JSImport("jszip", JSImport.Default)
  private class JSZip(data: js.Array[Int]) extends js.Object {
    def files: js.Dictionary[JSZipEntry] = js.native
  }

  private trait JSZipEntry extends js.Object {
    def asArrayBuffer(): ArrayBuffer
  }
}

@JSImport("fs", JSImport.Namespace)
@js.native
private object NodeFS extends js.Object {
  trait Stat extends js.Object {
    val mtime: js.UndefOr[js.Date]
  }

  def readFileSync(path: String): js.Array[Int] = js.native
  def statSync(path: String): Stat = js.native
}
