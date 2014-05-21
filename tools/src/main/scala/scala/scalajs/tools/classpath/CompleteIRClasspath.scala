/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.classpath

import scala.collection.immutable.{Seq, Traversable}

import scala.scalajs.tools.io._
import scala.scalajs.tools.logging._
import scala.scalajs.tools.packager.ScalaJSPackager

/** A CompleteCIClasspath that contains only IR as cijsCode */
class CompleteIRClasspath(
    /** The JS libraries the IR code depends on */
    jsLibs: Seq[VirtualJSFile],
    /** The IR itself. Ancestor count is used for later ordering */
    val scalaJSIR: Traversable[VirtualScalaJSIRFile],
    version: Option[String]
) extends CompleteCIClasspath(jsLibs, version) {

  /** Orders and desugars the contained IR.
   *
   *  Consider using ScalaJSPackager for a canonical way to do so. It allows to
   *  persist the resulting file and create a source map.
   */
  override def cijsCode: Seq[VirtualJSFile] = {
    import ScalaJSPackager._

    val output = WritableMemVirtualJSFile("temporary-package.js")
    (new ScalaJSPackager).packageIR(scalaJSIR, OutputConfig(output),
        NullLogger, addCoreJSLib = true)

    output :: Nil
  }
}
