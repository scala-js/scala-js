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

import org.scalajs.io._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray._

class NodeVirtualScalaJSIRFile(p: String, val relativePath: String)
    extends NodeVirtualBinaryFile(p) with VirtualSerializedScalaJSIRFile

private[scalajs] class NodeVirtualJarScalaJSIRContainer(path: String)
    extends NodeVirtualFile(path) with ScalaJSIRContainer {
  import NodeVirtualJarScalaJSIRContainer.JSZip

  def sjsirFiles: List[VirtualScalaJSIRFile] = {
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
