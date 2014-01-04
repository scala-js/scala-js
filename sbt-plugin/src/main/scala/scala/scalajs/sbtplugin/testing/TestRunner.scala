/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.testing

import sbt.testing.TaskDef
import sbt.testing.Task
import sbt.testing.Runner
import scala.scalajs.sbtplugin.ScalaJSEnvironment

case class TestRunner(
    args: Array[String], remoteArgs: Array[String],
    environment: ScalaJSEnvironment,
    testRunnerClass: String, testFramework: String) extends Runner {

  def tasks(taskDefs: Array[TaskDef]): Array[Task] =
    if (_done) throw new IllegalStateException("Done has already been called")
    else taskDefs.map(TestTask(environment, testRunnerClass, testFramework))

  def done(): String = {
    _done = true
    ""
  }

  private var _done = false
}
