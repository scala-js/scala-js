/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2018, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

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
      val path = s"${this.path}:$name"
      new MemVirtualSerializedScalaJSIRFile(path, name)
        .withContent(new Int8Array(entry.asArrayBuffer()).toArray)
        .withVersion(this.version)
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
