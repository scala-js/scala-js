/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin

import sbt._

import ScalaJSPlugin.scalaJSBinaryVersion

object ScalaJSCrossVersion {
  private val scalaJSVersionUnmapped: String => String =
    _ => s"sjs$scalaJSBinaryVersion"

  private val scalaJSVersionMap: String => String =
    version => s"sjs${scalaJSBinaryVersion}_$version"

  private final val ReleaseVersion = raw"""(\d+)\.(\d+)\.(\d+)""".r

  def binaryScalaJSVersion(full: String): String = full match {
    case ReleaseVersion(major, minor, release) => s"$major.$minor"
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

  def binary: CrossVersion = scalaJSMapped(CrossVersion.binary)

  def full: CrossVersion = scalaJSMapped(CrossVersion.full)
}
