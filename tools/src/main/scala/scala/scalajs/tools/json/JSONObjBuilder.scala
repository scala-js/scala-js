package scala.scalajs.tools.json

import scala.collection.mutable

class JSONObjBuilder {
  import scala.collection.JavaConverters._

  private val flds = mutable.Map.empty[String, Object]

  def fld[T : JSONSerializer](name: String, v: T): this.type = {
    flds.put(name, v.toJSON)
    this
  }

  def opt[T : JSONSerializer](name: String, v: Option[T]): this.type = {
    v.foreach(v => flds.put(name, v.toJSON))
    this
  }

  def toJSON = flds.asJava
}
