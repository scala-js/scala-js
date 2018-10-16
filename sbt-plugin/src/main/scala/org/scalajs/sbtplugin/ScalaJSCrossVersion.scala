/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.sbtplugin

import sbt._

import org.scalajs.ir.ScalaJSVersions

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
