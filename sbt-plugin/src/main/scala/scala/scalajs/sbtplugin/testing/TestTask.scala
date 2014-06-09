/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.testing

import sbt.testing._

import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.env._

import scala.scalajs.sbtplugin.JSUtils._

import scala.annotation.tailrec
import scala.util.control.NonFatal

class TestTask(
    env: JSEnv,
    classpath: CompleteClasspath,
    jsConsole: JSConsole,
    testFramework: String,
    args: Array[String],
    val taskDef: TaskDef) extends Task {

  import TestTask._

  val tags = Array.empty[String]
  val options = readArgs(args.toList)

  def execute(eventHandler: EventHandler,
      loggers: Array[Logger]): Array[Task] = {

    val runnerFile = testRunnerFile(options.frameworkArgs)
    val testConsole = new TestOutputConsole(jsConsole, eventHandler,
      loggers, new Events(taskDef), classpath, options.noSourceMap)
    val logger = new SbtTestLoggerAccWrapper(loggers)

    // Actually execute test
    env.runJS(classpath, runnerFile, logger, testConsole)

    Array.empty
  }

  private def testRunnerFile(args: List[String]) = {
    val testKey = taskDef.fullyQualifiedName

    // Note that taskDef does also have the selector, fingerprint and
    // explicitlySpecified value we could pass to the framework. However, we
    // believe that these are only moderately useful. Therefore, we'll silently
    // ignore them.

    val jsArgArray = listToJS(args)

    new MemVirtualJSFile("Generated test launcher file").
      withContent(s"""$testFramework().safeRunTest(
                     |  scala.scalajs.test.internal.ConsoleTestOutput(),
                     |  $jsArgArray,
                     |  $testKey);""".stripMargin)
  }


}

object TestTask {

  def apply(environment: JSEnv, classpath: CompleteClasspath,
    jsConsole: JSConsole, testFramework: String, args: Array[String]
  )(taskDef: TaskDef) =
    new TestTask(environment, classpath, jsConsole,
      testFramework, args, taskDef)

  case class ArgOptions(
    noSourceMap: Boolean,
    frameworkArgs: List[String]
  )

  private def readArgs(args0: List[String]) = {
    // State for each option
    var noSourceMap = false

    def mkOptions(frameworkArgs: List[String]) =
      ArgOptions(noSourceMap, frameworkArgs)

    @tailrec
    def read0(args: List[String]): ArgOptions = args match {
      case "-no-source-map" :: xs =>
        noSourceMap = true
        read0(xs)

      // Explicitly end our argument list
      case "--" :: xs =>
        mkOptions(xs)

      // Unknown argument
      case xs =>
        mkOptions(xs)

    }

    read0(args0)
  }

}
