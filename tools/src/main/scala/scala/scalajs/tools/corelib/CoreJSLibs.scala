/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.corelib

import java.io.File
import java.net.URI

import scala.scalajs.ir.ScalaJSVersions
import scala.scalajs.tools.io._

object CoreJSLibs {

  private val libNames = Seq(
      "scalajsenv.js",
      "javalangObject.js",
      "javalangString.js",
      "DummyParents.js")

  lazy val libs: Seq[VirtualJSFile] = loadLibs()

  private def loadLibs(): Seq[VirtualJSFile] = {
    for {
      libName <- libNames
    } yield {
      val url = getClass.getResource(libName)
      assert(url != null, s"Require $libName on the classpath")

      val uri = url.toURI
      val file = new FileVirtualJSFile(new File(uri))
      val publicURI =
        if (ScalaJSVersions.currentIsSnapshot) uri
        else githubCoreLibURI.resolve(file.name)

      new MemVirtualJSFile(publicURI.toString)
        .withContent(file.content)
        .withVersion(Some(())) // it won't change
    }
  }

  private def githubCoreLibURI: URI = {
    val version = ScalaJSVersions.current
    new URI(s"https://raw.githubusercontent.com/scala-js/scala-js/v$version/"+
        "tools/src/main/resources/scala/scalajs/tools/corelib/")
  }

}
