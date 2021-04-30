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

import scala.tools.partest._
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
  val scalaVersion: String
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
  args
) {

  override val suiteRunner = new SuiteRunner(
      testSourcePath = config.optSourcePath orElse Option("test/files") getOrElse PartestDefaults.sourcePath,
      fileManager = new FileManager(testClassLoader = testClassLoader),
      updateCheck = config.optUpdateCheck,
      failed = config.optFailed,
      nestUI = nestUI,
      javaCmdPath = Option(javaCmd).map(_.getAbsolutePath) getOrElse PartestDefaults.javaCmd,
      javacCmdPath = Option(javacCmd).map(_.getAbsolutePath) getOrElse PartestDefaults.javacCmd,
      scalacExtraArgs = scalacArgs,
      javaOpts = javaOpts) with ScalaJSSuiteRunner {

    val options: ScalaJSPartestOptions = ScalaJSSBTRunner.this.options
    val scalaVersion: String = ScalaJSSBTRunner.this.scalaVersion

    override def onFinishTest(testFile: File, result: TestState): TestState = {
      eventHandler.handle(new Event {
        def fullyQualifiedName: String = testFile.testIdent
        def fingerprint: Fingerprint = partestFingerprint
        def selector: Selector = new TestSelector(testFile.testIdent)
        val (status, throwable) = makeStatus(result)
        def duration: Long = -1
      })
      result
    }
  }
}
