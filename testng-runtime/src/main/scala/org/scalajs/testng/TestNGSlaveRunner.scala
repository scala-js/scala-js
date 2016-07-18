package org.scalajs.testng

import sbt.testing._

final class TestNGSlaveRunner(
    args: Array[String],
    remoteArgs: Array[String],
    testClassLoader: ClassLoader,
    send: String => Unit,
    runSettings: RunSettings)
    extends TestNGBaseRunner(args, remoteArgs, testClassLoader, runSettings) {

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = {
    taskDefs.map(newTask)
  }

  def done(): String = {
    send("d" + TestNGBaseRunner.Done(taskDoneCount, passedCount, failedCount,
        ignoredCount, skippedCount, totalCount).serialize)
    ""
  }

  def receiveMessage(msg: String): Option[String] = {
    None // <- ignored
  }
}
