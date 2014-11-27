/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import sbt.testing._

import org.scalajs.core.tools.json._

import TaskDefSerializers._

private[testadapter] final class TaskInfo private (
    val serializedTask: String,
    val taskDef: TaskDef,
    val tags: Array[String])

private[testadapter] object TaskInfo {
  implicit object Deserializer extends JSONDeserializer[TaskInfo] {
    def deserialize(x: JSON): TaskInfo = {
      val obj = new JSONObjExtractor(x)
      new TaskInfo(
          obj.fld[String]      ("serializedTask"),
          obj.fld[TaskDef]     ("taskDef"),
          obj.fld[List[String]]("tags").toArray)
    }
  }
}
