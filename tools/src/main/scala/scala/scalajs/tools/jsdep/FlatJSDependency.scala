package scala.scalajs.tools.jsdep

import scala.scalajs.ir.Trees.isValidIdentifier

/** The same as a [[JSDependency]] but containing the origin from the containing
 *  JSDependencyManifest. This class is used for filtering of dependencies.
 */
case class FlatJSDependency(
    origin: Origin,
    resourceName: String,
    dependencies: List[String] = Nil,
    commonJSName: Option[String] = None) {

  require(commonJSName.forall(isValidIdentifier),
    "commonJSName must be a valid JavaScript identifier")

}
