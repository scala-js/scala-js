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

import scala.tools.partest.{FileOps, nest}
import scala.tools.partest.nest.{DirectCompiler, SuiteRunner}

class ScalaJSRunner(testFile: File, suiteRunner: SuiteRunner,
    scalaJSOverridePath: String, options: ScalaJSPartestOptions)
    extends nest.Runner(testFile, suiteRunner,
        new nest.NestUI(diffOnFail = options.showDiff, colorEnabled = true)) {

  private val compliantSems: List[String] = {
    scalaJSConfigFile("sem").fold(List.empty[String]) { file =>
      val source = scala.io.Source.fromFile(file)
      try {
        source.getLines.toList
      } finally {
        source.close()
      }
    }
  }

  override val checkFile: File = {
    scalaJSConfigFile("check")
      .orElse(scalaJSConfigFile("check" + options.targetSpecificCheckFileSuffix))
      .getOrElse {
        // this is super.checkFile, but apparently we can't do that
        new FileOps(testFile).changeExtension("check")
      }
  }

  private def scalaJSConfigFile(ext: String): Option[File] = {
    val overrideFile = s"$scalaJSOverridePath/$kind/$fileBase.$ext"
    val url = getClass.getResource(overrideFile)
    Option(url).map(url => new File(url.toURI))
  }

  override def newCompiler = new DirectCompiler(this) with ScalaJSDirectCompiler

  override def flagsForCompilation(sources: List[File]): List[String] = {
    // Never warn, so we do not need to update tons of checkfiles.
    "-P:scalajs:nowarnGlobalExecutionContext" ::
    super.flagsForCompilation(sources)
  }

  override def extraJavaOptions = {
    super.extraJavaOptions ++ Seq(
      s"-Dscalajs.partest.useWasm=${options.useWasm}",
      s"-Dscalajs.partest.optMode=${options.optMode.id}",
      s"-Dscalajs.partest.compliantSems=${compliantSems.mkString(",")}"
    )
  }
}
