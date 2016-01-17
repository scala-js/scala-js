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
    val relPath: String,
    val dependencies: Set[String],
    val origins: List[Origin],
    val commonJSName: Option[String],
    val relPathMinified: Option[String]) {

  require(commonJSName.forall(isValidIdentifier),
    "commonJSName must be a valid JavaScript identifier")

  override def toString(): String = {
    val b = new StringBuilder
    b ++= s"ResolutionInfo(relPath=$relPath"
    if (dependencies.nonEmpty)
      b ++= s", dependencies=$dependencies"
    if (origins.nonEmpty)
      b ++= s", origins=$origins"
    if (commonJSName.nonEmpty)
      b ++= s", commonJSName=$commonJSName"
    if (relPathMinified.nonEmpty)
      b ++= s", relPathMinified=$relPathMinified"
    b ++= ")"
    b.result()
  }
}
