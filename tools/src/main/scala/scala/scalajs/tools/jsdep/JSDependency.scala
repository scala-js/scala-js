package scala.scalajs.tools.jsdep

import scala.scalajs.tools.json._

import scala.scalajs.ir.Trees.isValidIdentifier

/** Expresses a dependency on a raw JS library and the JS libraries this library
 *  itself depends on.
 *
 *  Both the [[resourceName]] and each element of [[dependencies]] is the
 *  unqualified filename of the library (e.g. "jquery.js").
 *
 *  @param resourceName Filename of the JavaScript file to include
 *      (e.g. "jquery.js")
 *  @param dependencies Filenames of JavaScript files that must be included
 *      before this JavaScript file.
 *  @param commonJSName A JavaScript variable name this dependency should be
 *      required in a commonJS environment (n.b. Node.js). Should only be set if
 *      the JavaScript library will register its exports.
 */
final case class JSDependency(
    resourceName: String,
    dependencies: List[String] = Nil,
    commonJSName: Option[String] = None) {

  require(commonJSName.forall(isValidIdentifier),
    "commonJSName must be a valid JavaScript identifier")

  def dependsOn(names: String*): JSDependency =
    copy(dependencies = dependencies ++ names)
  def commonJSName(name: String): JSDependency =
    copy(commonJSName = Some(name))
  def withOrigin(origin: Origin): FlatJSDependency =
    FlatJSDependency(origin, resourceName, dependencies, commonJSName)
}

object JSDependency {

  implicit object JSDepJSONSerializer extends JSONSerializer[JSDependency] {
    def serialize(x: JSDependency): Object = {
      new JSONObjBuilder()
        .fld("resourceName", x.resourceName)
        .fld("dependencies", x.dependencies)
        .opt("commonJSName", x.commonJSName)
        .toJSON
    }
  }

  implicit object JSDepJSONDeserializer extends JSONDeserializer[JSDependency] {
    def deserialize(x: Object): JSDependency = {
      val obj = new JSONObjExtractor(x: Object)
      JSDependency(
          obj.fld[String]      ("resourceName"),
          obj.fld[List[String]]("dependencies"),
          obj.opt[String]      ("commonJSName"))
    }
  }
}
