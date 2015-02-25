package org.scalajs.core.tools.jsdep

import org.scalajs.core.tools.json._

import org.scalajs.core.ir.Trees.isValidIdentifier

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
final class JSDependency(
    val resourceName: String,
    val dependencies: List[String] = Nil,
    val commonJSName: Option[String] = None) {

  import JSDependency._

  require(commonJSName.forall(isValidIdentifier),
    "commonJSName must be a valid JavaScript identifier")

  def dependsOn(names: String*): JSDependency =
    copy(dependencies = dependencies ++ names)
  def commonJSName(name: String): JSDependency =
    copy(commonJSName = Some(name))
  def withOrigin(origin: Origin): FlatJSDependency =
    new FlatJSDependency(origin, resourceName, dependencies, commonJSName)

  private def copy(
      resourceName: String = this.resourceName,
      dependencies: List[String] = this.dependencies,
      commonJSName: Option[String] = this.commonJSName) = {
    new JSDependency(resourceName, dependencies, commonJSName)
  }

  override def equals(that: Any): Boolean = that match {
    case that: JSDependency =>
      this.resourceName == that.resourceName &&
      this.dependencies == that.dependencies &&
      this.commonJSName == that.commonJSName
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, resourceName.##)
    acc = mix(acc, dependencies.##)
    acc = mixLast(acc, commonJSName.##)
    finalizeHash(acc, 3)
  }
}

object JSDependency {
  // "org.scalajs.core.tools.jsdep.JSDependency".##
  private final val HashSeed = 2103455349

  implicit object JSDepJSONSerializer extends JSONSerializer[JSDependency] {
    def serialize(x: JSDependency): JSON = {
      new JSONObjBuilder()
        .fld("resourceName", x.resourceName)
        .opt("dependencies",
            if (x.dependencies.nonEmpty) Some(x.dependencies) else None)
        .opt("commonJSName", x.commonJSName)
        .toJSON
    }
  }

  implicit object JSDepJSONDeserializer extends JSONDeserializer[JSDependency] {
    def deserialize(x: JSON): JSDependency = {
      val obj = new JSONObjExtractor(x)
      new JSDependency(
          obj.fld[String]      ("resourceName"),
          obj.opt[List[String]]("dependencies").getOrElse(Nil),
          obj.opt[String]      ("commonJSName"))
    }
  }
}
