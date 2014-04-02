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

import scala.annotation.tailrec
import scala.util.control.NonFatal

class TestTask(
    env: JSEnv,
    jsClasspath: JSClasspath,
    testFramework: String,
    args: Array[String],
    val taskDef: TaskDef) extends Task {

  import TestTask._

  val tags = Array.empty[String]
  val options = readArgs(args.toList)

  def execute(eventHandler: EventHandler,
      loggers: Array[Logger]): Array[Task] = {

    val runnerFile = testRunnerFile(options.frameworkArgs)
    val testConsole = new TestOutputConsole(eventHandler, loggers,
        new Events(taskDef), jsClasspath, options.noSourceMap)
    val logger = new SbtTestLoggerAccWrapper(loggers)

    try {
      // Actually execute test
      env.runJS(jsClasspath, runnerFile, logger, testConsole)
    } catch {
      case NonFatal(e) => logger.trace(e)
    }

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
      withContent(s"""$testFramework().runTests(
                     |  scala.scalajs.test.internal.ConsoleTestOutput(),
                     |  $jsArgArray,
                     |  $testKey);""".stripMargin)
  }


}

object TestTask {

  def apply(environment: JSEnv, jsClasspath: JSClasspath,
    testFramework: String, args: Array[String])(taskDef: TaskDef) =
      new TestTask(environment, jsClasspath, testFramework, args, taskDef)

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

  private def listToJS(xs: List[String]) =
    xs.map(x => '"' + escapeJS(x) + '"').mkString("[",",","]")

  /** Stolen from scala.scalajs.compiler.JSPrinters */
  private def escapeJS(str: String): String = {
    /* Note that Java and JavaScript happen to use the same encoding for
     * Unicode, namely UTF-16, which means that 1 char from Java always equals
     * 1 char in JavaScript. */
    val builder = new StringBuilder
    str foreach {
      case '\\' => builder.append("\\\\")
      case '"' => builder.append("\\\"")
      case '\u0007' => builder.append("\\a")
      case '\u0008' => builder.append("\\b")
      case '\u0009' => builder.append("\\t")
      case '\u000A' => builder.append("\\n")
      case '\u000B' => builder.append("\\v")
      case '\u000C' => builder.append("\\f")
      case '\u000D' => builder.append("\\r")
      case c =>
        if (c >= 32 && c <= 126) builder.append(c.toChar) // ASCII printable characters
        else builder.append(f"\\u$c%04x")
    }
    builder.result()
  }

}
