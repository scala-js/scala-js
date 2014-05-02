package scala.scalajs.tools.jsdep

import scala.scalajs.tools.json._

final case class JSDependency(
    resourceName: String,
    dependencies: List[String] = Nil) {

  def dependsOn(names: String*) = copy(dependencies = dependencies ++ names)
}

object JSDependency {

  implicit object JSDepJSONSerializer extends JSONSerializer[JSDependency] {
    def serialize(x: JSDependency): Object = {
      new JSONObjBuilder()
        .fld("resourceName", x.resourceName)
        .fld("dependencies", x.dependencies)
        .toJSON
    }
  }

  implicit object JSDepJSONDeserializer extends JSONDeserializer[JSDependency] {
    def deserialize(x: Object): JSDependency = {
      val obj = new JSONObjExtractor(x: Object)
      JSDependency(
          obj.fld[String]      ("resourceName"),
          obj.fld[List[String]]("dependencies"))
    }
  }
}
