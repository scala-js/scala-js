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

import scala.tools.partest.sbt.SBTRunner

import org.scalajs.core.compiler.ScalaJSPlugin

import scala.io.Source

import _root_.sbt.testing._
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

class ScalaJSRunner(testFile: File, suiteRunner: SuiteRunner,
    scalaJSOverridePath: String,
    options: ScalaJSPartestOptions) extends nest.Runner(testFile, suiteRunner,
        new nest.NestUI(diffOnFail = options.showDiff, colorEnabled = true)) {
  private val compliantSems: List[String] = {
    scalaJSConfigFile("sem").fold(List.empty[String]) { file =>
      Source.fromFile(file).getLines.toList
    }
  }

  override val checkFile: File = {
    scalaJSConfigFile("check") getOrElse {
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
  override def extraJavaOptions = {
    super.extraJavaOptions ++ Seq(
        s"-Dscalajs.partest.optMode=${options.optMode.id}",
        s"-Dscalajs.partest.compliantSems=${compliantSems.mkString(",")}"
    )
  }
}

trait ScalaJSSuiteRunner extends SuiteRunner {

  // Stuff to mix in

  val options: ScalaJSPartestOptions

  /** Full scala version name. Used to discover blacklist (etc.) files */
  val scalaVersion: String

  // Stuff we provide

  override def banner: String = {
    import org.scalajs.core.ir.ScalaJSVersions.{ current => currentVersion }

    super.banner.trim + s"""
    |Scala.js version is: $currentVersion
    |Scala.js options are:
    |optimizer:           ${options.optMode.shortStr}
    |testFilter:          ${options.testFilter.descr}
    """.stripMargin
  }

  override def runTest(testFile: File): TestState = {
    // Mostly copy-pasted from SuiteRunner.runTest(), unfortunately :-(
    val runner = new ScalaJSRunner(testFile, this, listDir, options)

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
        nestUI.reportTest(state, runner)
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
    testRoot: File,
    testClassLoader: URLClassLoader,
    javaCmd: File,
    javacCmd: File,
    scalacArgs: Array[String],
    args: Array[String],
    val options: ScalaJSPartestOptions,
    val scalaVersion: String
) extends SBTRunner(
    RunnerSpec.forArgs(args), partestFingerprint, eventHandler, loggers,
    "test/files", testClassLoader, javaCmd, javacCmd, scalacArgs, args
) {

  // The test root for partest is read out through the system properties,
  // not passed as an argument
  sys.props("partest.root") = testRoot.getAbsolutePath()

  // Partests take at least 5h. We double, just to be sure. (default is 4 hours)
  sys.props("partest.timeout") = "10 hours"

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
