package scala.scalajs.tools.jsdep

import scala.scalajs.tools.json._

/** The place a JSDependency originated from */
final case class Origin(moduleName: String, configuration: String) {
  override def toString(): String = s"$moduleName:$configuration"
}

object Origin {
  implicit object OriginJSONSerializer extends JSONSerializer[Origin] {
    def serialize(x: Origin): Object = {
      new JSONObjBuilder()
        .fld("moduleName",    x.moduleName)
        .fld("configuration", x.configuration)
        .toJSON
    }
  }

  implicit object OriginDeserializer extends JSONDeserializer[Origin] {
    def deserialize(x: Object): Origin = {
      val obj = new JSONObjExtractor(x)
      Origin(
          obj.fld[String]("moduleName"),
          obj.fld[String]("configuration"))
    }
  }
}
