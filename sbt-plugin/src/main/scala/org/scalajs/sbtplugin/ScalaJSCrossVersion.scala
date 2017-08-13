/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.sbtplugin

import sbt._

import org.scalajs.core.ir.ScalaJSVersions

import SBTCompat._

object ScalaJSCrossVersion {
  private final val ReleaseVersion =
    raw"""(\d+)\.(\d+)\.(\d+)""".r
  private final val MinorSnapshotVersion =
    raw"""(\d+)\.(\d+)\.([1-9]\d*)-SNAPSHOT""".r

  val currentBinaryVersion = binaryScalaJSVersion(ScalaJSVersions.binaryEmitted)

  def binaryScalaJSVersion(full: String): String = full match {
    case ReleaseVersion(major, _, _)       => major
    case MinorSnapshotVersion(major, _, _) => major
    case _                                 => full
  }

  def scalaJSMapped(cross: CrossVersion): CrossVersion =
    crossVersionAddScalaJSPart(cross, "sjs" + currentBinaryVersion)

  val binary: CrossVersion = scalaJSMapped(CrossVersion.binary)

  val full: CrossVersion = scalaJSMapped(CrossVersion.full)
}
