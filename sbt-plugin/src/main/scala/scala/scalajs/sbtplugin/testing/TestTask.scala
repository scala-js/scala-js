/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.testing

import scala.annotation.tailrec

import sbt.testing.TaskDef
import sbt.testing.EventHandler
import sbt.testing.Task
import sbt.testing.Logger

import org.mozilla.javascript
import org.mozilla.javascript.{Context, Scriptable, Function}
import org.mozilla.javascript.Scriptable.NOT_FOUND
import org.mozilla.javascript.RhinoException

import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.environment._

class TestTask(
    environment: ScalaJSEnvironment,
    jsClasspath: JSClasspath,
    testFramework: String,
    val taskDef: TaskDef) extends Task {

  val tags = Array.empty[String]

  def execute(eventHandler: EventHandler,
      loggers: Array[Logger]): Array[Task] = {

    val runnerFile = testRunnerFile
    val testConsole = new TestOutputConsole(eventHandler, loggers,
        new Events(taskDef), jsClasspath)

    // Actually execute test
    environment.runJS(jsClasspath, runnerFile, testConsole)

    Array.empty
  }

  private def testRunnerFile = {
    val testKey = taskDef.fullyQualifiedName

    // Note that taskDef does also have the selector, fingerprint and
    // explicitlySpecified value we could pass to the framework. However, we
    // believe that these are only moderately useful. Therefore, we'll silently
    // ignore them.

    new MemVirtualJSFile("Generated test launcher file").
      withContent(s"""$testFramework().runTests(
                     |  scala.scalajs.test.internal.ConsoleTestOutput(),
                     |  $testKey);""".stripMargin)
  }
}

object TestTask {

  def apply(environment: ScalaJSEnvironment, jsClasspath: JSClasspath,
    testFramework: String)(taskDef: TaskDef) =
      new TestTask(environment, jsClasspath, testFramework, taskDef)

}
