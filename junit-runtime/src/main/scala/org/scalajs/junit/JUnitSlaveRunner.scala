/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
