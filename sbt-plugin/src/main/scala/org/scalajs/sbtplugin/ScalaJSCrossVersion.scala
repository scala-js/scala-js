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

object ScalaJSCrossVersion {
  private val scalaJSVersionUnmapped: String => String =
    _ => s"sjs$currentBinaryVersion"

  private val scalaJSVersionMap: String => String =
    version => s"sjs${currentBinaryVersion}_$version"

  private final val ReleaseVersion =
    raw"""(\d+)\.(\d+)\.(\d+)""".r
  private final val MinorSnapshotVersion =
    raw"""(\d+)\.(\d+)\.([1-9]\d*)-SNAPSHOT""".r

  val currentBinaryVersion = binaryScalaJSVersion(ScalaJSVersions.binaryEmitted)

  def binaryScalaJSVersion(full: String): String = full match {
    case ReleaseVersion(major, minor, _)       => s"$major.$minor"
    case MinorSnapshotVersion(major, minor, _) => s"$major.$minor"
    case _                                     => full
  }

  def scalaJSMapped(cross: CrossVersion): CrossVersion = cross match {
    case CrossVersion.Disabled =>
      CrossVersion.binaryMapped(scalaJSVersionUnmapped)
    case cross: CrossVersion.Binary =>
      CrossVersion.binaryMapped(cross.remapVersion andThen scalaJSVersionMap)
    case cross: CrossVersion.Full =>
      CrossVersion.fullMapped(cross.remapVersion andThen scalaJSVersionMap)
  }

  val binary: CrossVersion = scalaJSMapped(CrossVersion.binary)

  val full: CrossVersion = scalaJSMapped(CrossVersion.full)
}
