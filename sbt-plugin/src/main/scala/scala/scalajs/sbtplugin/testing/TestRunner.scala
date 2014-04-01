/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.testing

import sbt.testing._

import scala.scalajs.tools.environment._
import scala.scalajs.tools.classpath._

class TestRunner(
    environment: ScalaJSEnvironment,
    jsClasspath: JSClasspath,
    testFramework: String,
    // TODO add arguments to interface for framework
    val args: Array[String],
    val remoteArgs: Array[String]) extends Runner {

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = if (_done) {
    throw new IllegalStateException("Done has already been called")
  } else {
    taskDefs.map(TestTask(environment, jsClasspath, testFramework))
  }

  def done(): String = {
    _done = true
    ""
  }

  private var _done = false
}
