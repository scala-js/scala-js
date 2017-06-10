package org.scalajs.testadapter.json

import scala.collection.mutable

private[testadapter] class JSONObjBuilder {

  private val flds = mutable.Map.empty[String, JSON]

  def fld[T: JSONSerializer](name: String, v: T): this.type = {
    flds.put(name, v.toJSON)
    this
  }

  def opt[T: JSONSerializer](name: String, v: Option[T]): this.type = {
    v.foreach(v => flds.put(name, v.toJSON))
    this
  }

  def toJSON: JSON = Impl.fromMap(flds.toMap)
}
