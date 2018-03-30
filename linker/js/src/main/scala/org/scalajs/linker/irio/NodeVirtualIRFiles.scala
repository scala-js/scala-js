/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2018, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.linker.irio

import org.scalajs.io._

class NodeVirtualScalaJSIRFile(p: String)
    extends NodeVirtualBinaryFile(p) with VirtualSerializedScalaJSIRFile

class NodeVirtualJarScalaJSIRContainer(file: String)
    extends NodeVirtualJarFile(file) with ScalaJSIRContainer {

  def sjsirFiles: List[VirtualRelativeScalaJSIRFile] =
    ScalaJSIRContainer.sjsirFilesIn(this)
}
