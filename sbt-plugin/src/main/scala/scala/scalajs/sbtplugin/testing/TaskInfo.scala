/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.testing

import sbt.testing._

import scala.scalajs.tools.json._

import TaskDefSerializers._

private[testing] final class TaskInfo private (
    val serializedTask: String,
    val taskDef: TaskDef,
    val tags: Array[String])

private[testing] object TaskInfo {
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
