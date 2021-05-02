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
import java.net.URLClassLoader
import java.util.concurrent.TimeUnit

import scala.tools.partest.TestState
import scala.tools.partest.nest._
import scala.tools.partest.sbt.SBTRunner

import _root_.sbt.testing._

class ScalaJSSBTRunner(
  partestFingerprint: Fingerprint,
  eventHandler: EventHandler,
  loggers: Array[Logger],
  testClassLoader: URLClassLoader,
  javaCmd: File,
  javacCmd: File,
  scalacArgs: Array[String],
  args: Array[String],
  val options: ScalaJSPartestOptions,
  val scalaVersion: String,
) extends SBTRunner(
  RunnerSpec.forArgs(args),
  partestFingerprint,
  eventHandler,
  loggers,
  "test/files",
  testClassLoader,
  javaCmd,
  javacCmd,
  scalacArgs,
  args,
) {

  override def banner: String = super.banner.trim + options.banner

  override def runTest(testFile: File): TestState = {
    /* Mostly copy-pasted from AbstractRunner.runTest(), unfortunately :-(
     * The only changes are the instantiations of `info` and `runner`, as well
     * as the hard-coding of `onlyIndividualTests` to false.
     */

    val onlyIndividualTests = false

    val start = System.nanoTime()
    val info = new ScalaJSTestInfo(testFile, listDir)
    val runner = new ScalaJSRunner(info, this, options)
    var stopwatchDuration: Option[Long] = None

    // when option "--failed" is provided execute test only if log
    // is present (which means it failed before)
    val state = if (config.optFailed && !info.logFile.canRead) {
      runner.genPass()
    } else {
      val (state, durationMs) = try {
        runner.run()
      } catch {
        case t: Throwable => throw new RuntimeException(s"Error running $testFile", t)
      }
      stopwatchDuration = Some(durationMs)
      val verboseSummation = onlyIndividualTests && !terse
      val more = reportTest(state, info, durationMs,
          diffOnFail = config.optShowDiff || verboseSummation,
          logOnFail = config.optShowLog || verboseSummation)
      runner.cleanup(state)
      if (more.isEmpty) {
        state
      } else {
        state match {
          case f: TestState.Fail => f.copy(transcript = more.toArray)
          case _                 => state
        }
      }
    }
    val end = System.nanoTime()
    val durationMs = stopwatchDuration.getOrElse(TimeUnit.NANOSECONDS.toMillis(end - start))
    onFinishTest(testFile, state, durationMs)
  }

  override def runTestsForFiles(kindFiles: Array[File], kind: String): Array[TestState] =
    super.runTestsForFiles(kindFiles.filter(testFilter.filter), kind)

  private lazy val testFilter =
    new ScalaJSTestFilter(scalaVersion, options, pathSettings.srcDir)

  private lazy val listDir =
    s"/scala/tools/partest/scalajs/$scalaVersion"
}
