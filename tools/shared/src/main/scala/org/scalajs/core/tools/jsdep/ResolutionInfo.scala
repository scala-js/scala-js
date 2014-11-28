package org.scalajs.core.tools.jsdep

import org.scalajs.core.ir.Trees.isValidIdentifier

/** Information about a resolved JSDependency
 *
 *  @param resourceName Filename of the JavaScript file
 *  @param dependencies Filenames this dependency depends on
 *  @param origins Who declared this dependency
 *  @param commonJSName Variable name in commonJS environments
 */
final class ResolutionInfo(
  val resourceName: String,
  val dependencies: Set[String],
  val origins: List[Origin],
  val commonJSName: Option[String]) {

  require(commonJSName.forall(isValidIdentifier),
    "commonJSName must be a valid JavaScript identifier")

}
