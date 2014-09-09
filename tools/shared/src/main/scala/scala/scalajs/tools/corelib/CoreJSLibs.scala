/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.corelib

import java.net.URI

import scala.scalajs.ir.ScalaJSVersions
import scala.scalajs.tools.io._

import scala.collection.immutable.Seq

object CoreJSLibs {

  private val gitHubBaseURI =
    new URI("https://raw.githubusercontent.com/scala-js/scala-js/")

  lazy val libs: Seq[VirtualJSFile] = Seq(ScalaJSEnvVirtualJSFile)

  private object ScalaJSEnvVirtualJSFile extends VirtualJSFile {
    override def path: String = "scalajsenv.js"
    override def version: Option[String] = Some("")
    override def exists: Boolean = true
    override def content: String = ScalaJSEnvHolder.scalajsenv

    override def toURI: URI = {
      if (!ScalaJSVersions.currentIsSnapshot)
        gitHubBaseURI.resolve(s"v${ScalaJSVersions.current}/tools/$path")
      else
        super.toURI
    }
  }

}
