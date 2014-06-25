package scala.scalajs.tools.jsdep

/** The same as a [[JSDependency]] but containing the origin from the containing
 *  JSDependencyManifest. This class is used for filtering of dependencies.
 */
case class FlatJSDependency(
    origin: Origin,
    resourceName: String,
    dependencies: List[String] = Nil
)
