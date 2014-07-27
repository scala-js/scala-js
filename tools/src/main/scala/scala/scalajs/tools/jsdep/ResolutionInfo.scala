package scala.scalajs.tools.jsdep

/** Information about a resolved JSDependency
 *  
 *  @param resourceName Filename of the JavaScript file
 *  @param dependencies Filenames this dependency depends on
 *  @param origins Who declared this dependency
 */
final case class ResolutionInfo(
  resourceName: String,
  dependencies: Set[String],
  origins: List[Origin])
