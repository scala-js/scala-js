package org.scalajs.core.tools.jsdep

import org.scalajs.core.tools.json._

import org.scalajs.core.ir.Trees.isValidIdentifier

/** Expresses a dependency on a raw JS library and the JS libraries this library
 *  itself depends on.
 *
 *  Both the [[resourceName]] and each element of [[dependencies]] are
 *  potentially partial relative paths from the root of the classpath entry to
 *  the library. Examples are "jquery.js" or "compressed/history.js".
 *
 *  @param resourceName Resource name, i.e., potentially partial relative path
 *      to the .js file. Examples: "jquery.js", "compressed/history.js".
 *  @param dependencies Potentially relative paths of the dependencies of this
 *      resource, i.e., JavaScript files that must be included before this
 *      JavaScript file.
 *  @param commonJSName A JavaScript variable name this dependency should be
 *      required in a commonJS environment (n.b. Node.js). Should only be set if
 *      the JavaScript library will register its exports.
 *  @param minifiedResourceName Resource name for the minified version
 */
final class JSDependency(
    val resourceName: String,
    val dependencies: List[String] = Nil,
    val commonJSName: Option[String] = None,
    val minifiedResourceName: Option[String] = None) {

  // Binary compatibility
  def this(resourceName: String, dependencies: List[String],
      commonJSName: Option[String]) = {
    this(resourceName, dependencies, commonJSName, None)
  }

  import JSDependency._

  require(commonJSName.forall(isValidIdentifier),
    "commonJSName must be a valid JavaScript identifier")

  def dependsOn(names: String*): JSDependency =
    copy(dependencies = dependencies ++ names)
  def commonJSName(name: String): JSDependency =
    copy(commonJSName = Some(name))
  def minified(name: String): JSDependency =
    copy(minifiedResourceName = Some(name))

  @deprecated("withOrigin doesn't resolve partial paths. Use your own code instead.", "0.6.1")
  def withOrigin(origin: Origin): FlatJSDependency = {
    new FlatJSDependency(origin, resourceName, dependencies,
        commonJSName, minifiedResourceName)
  }

  private def copy(
      resourceName: String = this.resourceName,
      dependencies: List[String] = this.dependencies,
      commonJSName: Option[String] = this.commonJSName,
      minifiedResourceName: Option[String] = this.minifiedResourceName) = {
    new JSDependency(resourceName, dependencies,
        commonJSName, minifiedResourceName)
  }

  override def equals(that: Any): Boolean = that match {
    case that: JSDependency =>
      this.resourceName         == that.resourceName &&
      this.dependencies         == that.dependencies &&
      this.commonJSName         == that.commonJSName &&
      this.minifiedResourceName == that.minifiedResourceName
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, resourceName.##)
    acc = mix(acc, dependencies.##)
    acc = mix(acc, commonJSName.##)
    acc = mixLast(acc, minifiedResourceName.##)
    finalizeHash(acc, 4)
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
        .opt("minifiedResourceName", x.minifiedResourceName)
        .toJSON
    }
  }

  implicit object JSDepJSONDeserializer extends JSONDeserializer[JSDependency] {
    def deserialize(x: JSON): JSDependency = {
      val obj = new JSONObjExtractor(x)
      new JSDependency(
          obj.fld[String]      ("resourceName"),
          obj.opt[List[String]]("dependencies").getOrElse(Nil),
          obj.opt[String]      ("commonJSName"),
          obj.opt[String]      ("minifiedResourceName"))
    }
  }
}
