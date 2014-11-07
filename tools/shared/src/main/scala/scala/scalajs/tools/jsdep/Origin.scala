package scala.scalajs.tools.jsdep

import scala.scalajs.tools.json._

/** The place a JSDependency originated from */
final class Origin(val moduleName: String, val configuration: String) {
  override def toString(): String = s"$moduleName:$configuration"
}

object Origin {
  implicit object OriginJSONSerializer extends JSONSerializer[Origin] {
    def serialize(x: Origin): JSON = {
      new JSONObjBuilder()
        .fld("moduleName",    x.moduleName)
        .fld("configuration", x.configuration)
        .toJSON
    }
  }

  implicit object OriginDeserializer extends JSONDeserializer[Origin] {
    def deserialize(x: JSON): Origin = {
      val obj = new JSONObjExtractor(x)
      new Origin(
          obj.fld[String]("moduleName"),
          obj.fld[String]("configuration"))
    }
  }
}
