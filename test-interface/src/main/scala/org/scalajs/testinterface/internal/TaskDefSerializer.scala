package org.scalajs.testinterface.internal

import scala.scalajs.js
import js.Dynamic.{literal => lit}
import js.JSConverters._

import sbt.testing._

private[testinterface] object TaskDefSerializer {

  def serialize(td: TaskDef): js.Dynamic = {
    lit(fullyQualifiedName = td.fullyQualifiedName,
        fingerprint = FingerprintSerializer.serialize(td.fingerprint),
        explicitlySpecified = td.explicitlySpecified,
        selectors = td.selectors.map(SelectorSerializer.serialize _).toJSArray)
  }

  def deserialize(obj: js.Dynamic): TaskDef = {
    val selectors = obj.selectors.asInstanceOf[js.Array[js.Dynamic]]
      .map(SelectorSerializer.deserialize _).toArray

    new TaskDef(
        obj.fullyQualifiedName.asInstanceOf[String],
        FingerprintSerializer.deserialize(obj.fingerprint),
        obj.explicitlySpecified.asInstanceOf[Boolean],
        selectors)
  }

}
