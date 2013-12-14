package scala.scalajs.test

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

  def done: String = {
    _done = true
    ""
  }

  private var _done = false
}
