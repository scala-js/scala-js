package org.scalajs.core.tools.jsdep

import org.scalajs.core.ir.Trees.isValidIdentifier

/** The same as a [[JSDependency]] but containing the origin from the containing
 *  JSDependencyManifest, and resolved relative paths.
 *
 *  This class is used for filtering of dependencies.
 *
 *  @param origin What module declared this dependency
 *  @param relPath Path of the JavaScript file, relative to the classpath entry
 *  @param dependencies Relative paths of files this dependency depends on
 *  @param commonJSName Variable name in commonJS environments
 *  @param relPathMinified Path of the minified JavaScript file, relative to
 *      the classpath entry
 */
final class FlatJSDependency(
    val origin: Origin,
    @deprecatedName('resourceName)
    val relPath: String,
    val dependencies: List[String] = Nil,
    val commonJSName: Option[String] = None,
    val relPathMinified: Option[String] = None) {

  // Binary compatibility
  def this(origin: Origin, @deprecatedName('resourceName) relPath: String,
      dependencies: List[String], commonJSName: Option[String]) = {
    this(origin, relPath, dependencies, commonJSName, None)
  }

  require(commonJSName.forall(isValidIdentifier),
    "commonJSName must be a valid JavaScript identifier")

  @deprecated("Use relPath instead.", "0.6.1")
  val resourceName: String = relPath

  override def toString(): String = {
    val b = new StringBuilder
    b ++= s"FlatJSDependency(origin=$origin, relPath=$relPath"
    if (dependencies.nonEmpty)
      b ++= s", dependencies=$dependencies"
    if (commonJSName.nonEmpty)
      b ++= s", commonJSName=$commonJSName"
    if (relPathMinified.nonEmpty)
      b ++= s", relPathMinified=$relPathMinified"
    b ++= ")"
    b.result()
  }
}
