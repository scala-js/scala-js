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
  override def newCompiler = new DirectCompiler(this) with ScalaJSDirectCompiler
}

trait ScalaJSSuiteRunner extends SuiteRunner {
  override def runTest(testFile: File): TestState = {
    // Mostly copy-pasted from SuiteRunner.runTest(), unfortunately :-(
    val runner = new nest.Runner(testFile, this) with ScalaJSRunner

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

  private lazy val useBlacklist =
    scala.util.Properties.propOrFalse("scala.tools.partest.scalajs.useblacklist")

  private lazy val testUnknownOnly =
    scala.util.Properties.propOrFalse("scala.tools.partest.scalajs.testunknownonly")

  private lazy val buglistedTestFileNames =
    readTestList("/scala/tools/partest/scalajs/BuglistedTests.txt")

  private lazy val blacklistedTestFileNames =
    readTestList("/scala/tools/partest/scalajs/BlacklistedTests.txt")

  private lazy val whitelistedTestFileNames =
    readTestList("/scala/tools/partest/scalajs/WhitelistedTests.txt")

  private def readTestList(resourceName: String): Set[String] = {
    val source = scala.io.Source.fromURL(getClass.getResource(resourceName))

    val srcDir = PathSettings.srcDir

    val fileNames = for {
      line <- source.getLines
      trimmed = line.trim
      if trimmed != "" && !trimmed.startsWith("#")
    } yield {
      (srcDir / trimmed).toCanonical.getAbsolutePath
    }

    fileNames.toSet
  }

  def shouldUseTest(testFile: File): Boolean = {
    val absPath = testFile.toCanonical.getAbsolutePath
    if (testUnknownOnly)
      (!blacklistedTestFileNames.contains(absPath) &&
       !whitelistedTestFileNames.contains(absPath) &&
       !buglistedTestFileNames.contains(absPath))
    else if (useBlacklist) !blacklistedTestFileNames.contains(absPath)
    else whitelistedTestFileNames.contains(absPath)
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
    scalacArgs: Array[String]
) extends SBTRunner(
    partestFingerprint, eventHandler, loggers, srcDir, testClassLoader,
    javaCmd, javacCmd, scalacArgs
) with ScalaJSSuiteRunner
