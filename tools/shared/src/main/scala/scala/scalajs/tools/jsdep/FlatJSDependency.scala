package scala.scalajs.tools.jsdep

import scala.scalajs.ir.Trees.isValidIdentifier

/** The same as a [[JSDependency]] but containing the origin from the containing
 *  JSDependencyManifest. This class is used for filtering of dependencies.
 */
final class FlatJSDependency(
    val origin: Origin,
    val resourceName: String,
    val dependencies: List[String] = Nil,
    val commonJSName: Option[String] = None) {

  require(commonJSName.forall(isValidIdentifier),
    "commonJSName must be a valid JavaScript identifier")

}
