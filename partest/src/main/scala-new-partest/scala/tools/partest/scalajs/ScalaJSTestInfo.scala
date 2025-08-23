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

package scala.tools.partest.scalajs

import java.io.File

import scala.tools.partest.FileOps
import scala.tools.partest.nest.TestInfo

class ScalaJSTestInfo(testFile: File, scalaJSOverridePath: String, options: ScalaJSPartestOptions)
    extends TestInfo(testFile) {

  override val checkFile: File = {
    scalaJSConfigFile("check")
      .orElse(scalaJSConfigFile("check" + options.targetSpecificCheckFileSuffix))
      .getOrElse {
        // this is super.checkFile, but apparently we can't do that
        new FileOps(testFile).changeExtension("check")
      }
  }

  val compliantSems: List[String] = {
    scalaJSConfigFile("sem").fold(List.empty[String]) { file =>
      val source = scala.io.Source.fromFile(file)
      try {
        source.getLines().toList
      } finally {
        source.close()
      }
    }
  }

  private def scalaJSConfigFile(ext: String): Option[File] = {
    val overrideFile = s"$scalaJSOverridePath/$kind/$fileBase.$ext"
    val url = getClass.getResource(overrideFile)
    Option(url).map(url => new File(url.toURI))
  }
}
