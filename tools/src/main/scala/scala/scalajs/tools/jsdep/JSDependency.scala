package scala.scalajs.tools.jsdep

import scala.scalajs.tools.json._

/** Expresses a dependency on a raw JS library and the JS libraries this library
 *  itself depends on.
 *
 *  Both the [[resourceName]] and each element of [[dependencies]] is the
 *  unqualified filename of the library (e.g. "jquery.js")
 */
final case class JSDependency(
    resourceName: String,
    dependencies: List[String] = Nil) {

  def dependsOn(names: String*): JSDependency =
    copy(dependencies = dependencies ++ names)
  def withOrigin(origin: Origin): FlatJSDependency =
    FlatJSDependency(origin, resourceName, dependencies)
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
