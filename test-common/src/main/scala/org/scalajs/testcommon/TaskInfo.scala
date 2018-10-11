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

package org.scalajs.testcommon

import sbt.testing._

private[scalajs] final class TaskInfo(
    val serializedTask: String,
    val taskDef: TaskDef,
    val tags: List[String])

private[scalajs] object TaskInfo {
  implicit object TaskInfoSerializer extends Serializer[TaskInfo] {
    def serialize(x: TaskInfo, out: Serializer.SerializeState): Unit = {
      out.write(x.serializedTask)
      out.write(x.taskDef)
      out.write(x.tags)
    }

    def deserialize(in: Serializer.DeserializeState): TaskInfo =
      new TaskInfo(in.read[String](), in.read[TaskDef](), in.read[List[String]]())
  }
}
