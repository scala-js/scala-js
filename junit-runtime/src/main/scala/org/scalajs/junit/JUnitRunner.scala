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

import sbt.testing._

private[junit] final class JUnitRunner(
    val args: Array[String],
    val remoteArgs: Array[String],
    runSettings: RunSettings) extends Runner {

  def tasks(taskDefs: Array[TaskDef]): Array[Task] =
    taskDefs.map(new JUnitTask(_, runSettings))

  def done(): String = ""

  def serializeTask(task: Task, serializer: TaskDef => String): String =
    serializer(task.taskDef)

  def deserializeTask(task: String, deserializer: String => TaskDef): Task =
    new JUnitTask(deserializer(task), runSettings)

  def receiveMessage(msg: String): Option[String] = None
}
