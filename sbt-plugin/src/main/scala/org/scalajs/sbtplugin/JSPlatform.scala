/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.sbtplugin

import sbt._

import sbtcrossproject._

case object JSPlatform extends Platform {
  val crossBinary: CrossVersion = ScalaJSCrossVersion.binary
  val crossFull: CrossVersion = ScalaJSCrossVersion.full

  def identifier: String = "js"
  def sbtSuffix: String = "JS"

  def enable(project: Project): Project =
    project.enablePlugins(ScalaJSPlugin)
}
