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

package org.scalajs.testing.bridge

import sbt.testing._

import org.scalajs.testing.common.{TaskInfo, Serializer}

private[bridge] object TaskInfoBuilder {
  def detachTask(task: Task, runner: Runner): TaskInfo = {
    def optSerializer(t: TaskDef) =
      if (t == task.taskDef) ""
      else Serializer.serialize(t)

    new TaskInfo(runner.serializeTask(task, optSerializer),
        task.taskDef, task.tags.toList)
  }

  def attachTask(info: TaskInfo, runner: Runner): Task = {
    def optDeserializer(s: String) =
      if (s == "") info.taskDef
      else Serializer.deserialize[TaskDef](s)

    runner.deserializeTask(info.serializedTask, optDeserializer)
  }
}
