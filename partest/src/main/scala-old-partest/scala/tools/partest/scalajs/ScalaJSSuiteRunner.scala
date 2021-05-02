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

import scala.tools.partest._
import scala.tools.partest.nest._
import scala.tools.partest.Path._

import java.io.File

trait ScalaJSSuiteRunner extends SuiteRunner {

  // Stuff to mix in

  val options: ScalaJSPartestOptions

  /** Full scala version name. Used to discover blacklist (etc.) files */
  val scalaVersion: String

  // Stuff we provide

  override def banner: String = super.banner.trim + options.banner

  override def runTest(testFile: File): TestState = {
    // Mostly copy-pasted from SuiteRunner.runTest(), unfortunately :-(
    val runner = new ScalaJSRunner(testFile, this, listDir, options)

    // when option "--failed" is provided execute test only if log
    // is present (which means it failed before)
    val state = if (failed && !runner.logFile.canRead) {
      runner.genPass()
    } else {
      val (state, elapsed) = try {
        timed(runner.run())
      } catch {
        case t: Throwable => throw new RuntimeException(s"Error running $testFile", t)
      }
      nestUI.reportTest(state, runner)
      runner.cleanup()
      state
    }
    onFinishTest(testFile, state)
  }

  override def runTestsForFiles(kindFiles: Array[File], kind: String): Array[TestState] =
    super.runTestsForFiles(kindFiles.filter(testFilter.filter), kind)

  private lazy val testFilter =
    new ScalaJSTestFilter(scalaVersion, options, PathSettings.srcDir)

  private lazy val listDir =
    s"/scala/tools/partest/scalajs/$scalaVersion"
}
