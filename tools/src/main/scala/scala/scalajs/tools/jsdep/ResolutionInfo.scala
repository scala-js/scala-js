package scala.scalajs.tools.jsdep

import scala.scalajs.ir.Trees.isValidIdentifier

/** Information about a resolved JSDependency
 *  
 *  @param resourceName Filename of the JavaScript file
 *  @param dependencies Filenames this dependency depends on
 *  @param origins Who declared this dependency
 *  @param commonJSName Variable name in commonJS environments
 */
final case class ResolutionInfo(
  resourceName: String,
  dependencies: Set[String],
  origins: List[Origin],
  commonJSName: Option[String]) {

  require(commonJSName.forall(isValidIdentifier),
    "commonJSName must be a valid JavaScript identifier")

}
