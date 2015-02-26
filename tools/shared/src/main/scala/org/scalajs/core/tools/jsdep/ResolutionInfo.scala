package org.scalajs.core.tools.jsdep

import org.scalajs.core.ir.Trees.isValidIdentifier

/** Information about a resolved JSDependency
 *
 *  @param relPath Path of the JavaScript file, relative to the classpath entry
 *  @param dependencies Relative paths of files this dependency depends on
 *  @param origins Who declared this dependency
 *  @param commonJSName Variable name in commonJS environments
 */
final class ResolutionInfo(
    @deprecatedName('resourceName)
    val relPath: String,
    val dependencies: Set[String],
    val origins: List[Origin],
    val commonJSName: Option[String]) {

  require(commonJSName.forall(isValidIdentifier),
    "commonJSName must be a valid JavaScript identifier")

  @deprecated("Use relPath instead.", "0.6.1")
  val resourceName = relPath

}
