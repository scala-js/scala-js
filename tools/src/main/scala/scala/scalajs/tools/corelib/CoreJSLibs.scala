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

import scala.io.Source

import scala.scalajs.ir.ScalaJSVersions
import scala.scalajs.tools.io._

import scala.collection.immutable.Seq

object CoreJSLibs {

  private val libNames = Seq("scalajsenv.js")

  lazy val libs: Seq[VirtualJSFile] = loadLibs()

  private def loadLibs(): Seq[VirtualJSFile] = {
    for {
      libName <- libNames
    } yield {
      val stream = getClass.getResourceAsStream(libName)
      assert(stream != null, s"Require $libName on the classpath")
      val content =
        try Source.fromInputStream(stream, "UTF-8").mkString
        finally stream.close()

      val publicURI =
        if (ScalaJSVersions.currentIsSnapshot) getClass.getResource(libName).toURI
        else githubCoreLibURI.resolve(libName)

      new MemVirtualJSFile(publicURI.toString)
        .withContent(content)
        .withVersion(Some("")) // it won't change
    }
  }

  private def githubCoreLibURI: URI = {
    val version = ScalaJSVersions.current
    new URI(s"https://raw.githubusercontent.com/scala-js/scala-js/v$version/"+
        "tools/src/main/resources/scala/scalajs/tools/corelib/")
  }

}
