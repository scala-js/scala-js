/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.sbtplugin.internal

import sbt._

import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.isScalaJSProject

/* Additional note about the deprecation below. Even though we are in the
 * `internal` package, ScalaJSGlobalPlugin will be imported by default in the
 * scope of `.sbt` files because it is an `AutoPlugin`. We do not want users to
 * refer to it by accident, without noticing they are using something from
 * `internal`.
 *
 * Since it will be loaded by sbt using reflection anyway, the deprecation will
 * not creep into legitimate builds.
 */

/** An `AutoPlugin` setting up some global settings that must be present in the
 *  build even if no project enables [[ScalaJSPlugin]].
 *
 *  This plugin should never be directly referenced in source code, and is
 *  therefore "deprecated forever".
 */
@deprecated("Do not explicitly mention ScalaJSGlobalPlugin in source code",
    "forever")
object ScalaJSGlobalPlugin extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements

  override def globalSettings: Seq[Setting[_]] = {
    Seq(
        isScalaJSProject := false
    )
  }
}
