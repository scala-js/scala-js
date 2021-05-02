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

import scala.tools.nsc.io.Directory

class ScalaJSTestFilter(scalaVersion: String, options: ScalaJSPartestOptions,
    pathSettingsSrcDir: Directory) {

  private val listDir =
    s"/scala/tools/partest/scalajs/$scalaVersion"

  private val blacklistedTests = {
    val source =
      scala.io.Source.fromURL(getClass.getResource(s"$listDir/BlacklistedTests.txt"))

    val files = for {
      line <- source.getLines()
      trimmed = line.trim
      if trimmed != "" && !trimmed.startsWith("#")
    } yield {
      extendShortTestName(trimmed)
    }

    files.toSet
  }

  private def extendShortTestName(testName: String): File = {
    val f = (pathSettingsSrcDir / testName).jfile
    require(f.exists(), s"$testName does not exist")
    f
  }

  val filter: File => Boolean = {
    import ScalaJSPartestOptions._
    options.testFilter match {
      case BlacklistedTests => blacklistedTests
      case WhitelistedTests => n => !blacklistedTests.contains(n)
      case SomeTests(names) => names.map(extendShortTestName).toSet
    }
  }

}
