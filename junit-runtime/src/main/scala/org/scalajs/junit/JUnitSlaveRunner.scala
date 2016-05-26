package org.scalajs.junit

import com.novocode.junit.RunSettings
import sbt.testing._

final class JUnitSlaveRunner(
    args: Array[String],
    remoteArgs: Array[String],
    testClassLoader: ClassLoader,
    send: String => Unit,
    runSettings: RunSettings)
    extends JUnitBaseRunner(args, remoteArgs, testClassLoader, runSettings) {

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = {
    taskDefs.map(newTask)
  }

  def done(): String = {
    send("d" + JUnitBaseRunner.Done(taskDoneCount, passedCount, failedCount,
        ignoredCount, skippedCount, totalCount).serialize)
    ""
  }

  def receiveMessage(msg: String): Option[String] = {
    None // <- ignored
  }
}
