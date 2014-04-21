/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.tools.partest
package scalajs

import nest._
import Path._

import scala.tools.nsc.{ Global, Settings }
import scala.tools.nsc.reporters.{ Reporter }
import scala.tools.nsc.plugins.Plugin

import scala.scalajs.compiler.ScalaJSPlugin

import sbt.testing.{ EventHandler, Logger, Fingerprint }
import java.io.File
import java.net.URLClassLoader

trait ScalaJSDirectCompiler extends DirectCompiler {
  override def newGlobal(settings: Settings, reporter: Reporter): PartestGlobal = {
    new PartestGlobal(settings, reporter) {
      override protected def loadRoughPluginsList(): List[Plugin] = {
        (super.loadRoughPluginsList() :+
            Plugin.instantiate(classOf[ScalaJSPlugin], this))
      }
    }
  }
}

trait ScalaJSRunner extends nest.Runner {
  val options: ScalaJSPartestOptions
  override def newCompiler = new DirectCompiler(this) with ScalaJSDirectCompiler
  override def extraJavaOptions = {
    if (options.optimize)
      super.extraJavaOptions :+ "-Dscalajs.partest.optimize=true"
    else
      super.extraJavaOptions
  }
}

trait ScalaJSSuiteRunner extends SuiteRunner {

  // Stuff to mix in

  val options: ScalaJSPartestOptions

  /** Full scala version name. Used to discover blacklist (etc.) files */
  val scalaVersion: String

  // Stuf we provide

  override def runTest(testFile: File): TestState = {
    // Mostly copy-pasted from SuiteRunner.runTest(), unfortunately :-(
    val runner = new nest.Runner(testFile, this) with ScalaJSRunner {
      val options = ScalaJSSuiteRunner.this.options
    }

    // when option "--failed" is provided execute test only if log
    // is present (which means it failed before)
    val state =
      if (failed && !runner.logFile.canRead)
        runner.genPass()
      else {
        val (state, elapsed) =
          try timed(runner.run())
          catch {
            case t: Throwable => throw new RuntimeException(s"Error running $testFile", t)
          }
        NestUI.reportTest(state)
        runner.cleanup()
        state
      }
    onFinishTest(testFile, state)
  }

  override def runTestsForFiles(kindFiles: Array[File],
      kind: String): Array[TestState] = {
    super.runTestsForFiles(kindFiles.filter(shouldUseTest), kind)
  }

  private lazy val listDir =
    s"/scala/tools/partest/scalajs/$scalaVersion"

  private lazy val buglistedTestFileNames =
    readTestList(s"$listDir/BuglistedTests.txt")

  private lazy val blacklistedTestFileNames =
    readTestList(s"$listDir/BlacklistedTests.txt")

  private lazy val whitelistedTestFileNames =
    readTestList(s"$listDir/WhitelistedTests.txt")

  private def readTestList(resourceName: String): Set[String] = {
    val source = scala.io.Source.fromURL(getClass.getResource(resourceName))

    val fileNames = for {
      line <- source.getLines
      trimmed = line.trim
      if trimmed != "" && !trimmed.startsWith("#")
    } yield extendShortTestName(trimmed)

    fileNames.toSet
  }

  private def extendShortTestName(testName: String) = {
    val srcDir = PathSettings.srcDir
    (srcDir / testName).toCanonical.getAbsolutePath
  }

  private lazy val testFilter: String => Boolean = {
    import ScalaJSPartestOptions._
    options.testFilter match {
      case UnknownTests => { absPath =>
        !blacklistedTestFileNames.contains(absPath) &&
        !whitelistedTestFileNames.contains(absPath) &&
        !buglistedTestFileNames.contains(absPath)
      }
      case BlacklistedTests => blacklistedTestFileNames
      case BuglistedTests   => buglistedTestFileNames
      case WhitelistedTests => whitelistedTestFileNames
      case SomeTests(names) => names.map(extendShortTestName _).toSet
    }
  }

  private def shouldUseTest(testFile: File): Boolean = {
    val absPath = testFile.toCanonical.getAbsolutePath
    testFilter(absPath)
  }
}

/* Pre-mixin ScalaJSSuiteRunner in SBTRunner, because this is looked up
 * via reflection from the sbt partest interface of Scala.js
 */
class ScalaJSSBTRunner(
    partestFingerprint: Fingerprint,
    eventHandler: EventHandler,
    loggers: Array[Logger],
    srcDir: String,
    testClassLoader: URLClassLoader,
    javaCmd: File,
    javacCmd: File,
    scalacArgs: Array[String],
    val options: ScalaJSPartestOptions,
    val scalaVersion: String
) extends SBTRunner(
    partestFingerprint, eventHandler, loggers, srcDir, testClassLoader,
    javaCmd, javacCmd, scalacArgs
) with ScalaJSSuiteRunner
