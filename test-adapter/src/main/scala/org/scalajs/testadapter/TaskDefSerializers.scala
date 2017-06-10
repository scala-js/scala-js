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

import FingerprintSerializers._
import SelectorSerializers._

private[scalajs] object TaskDefSerializers {

  implicit object TaskDefSerializer extends JSONSerializer[TaskDef] {
    def serialize(td: TaskDef): JSON = {
      new JSONObjBuilder()
        .fld("fullyQualifiedName", td.fullyQualifiedName)
        .fld("fingerprint", td.fingerprint)
        .fld("explicitlySpecified", td.explicitlySpecified)
        .fld("selectors", td.selectors.toList)
        .toJSON
    }
  }

  implicit object TaskDefDeserializer extends JSONDeserializer[TaskDef] {
    def deserialize(x: JSON): TaskDef = {
      val obj = new JSONObjExtractor(x)
      new TaskDef(
          obj.fld[String]("fullyQualifiedName"),
          obj.fld[Fingerprint]("fingerprint"),
          obj.fld[Boolean]("explicitlySpecified"),
          obj.fld[List[Selector]]("selectors").toArray)
    }
  }

}
