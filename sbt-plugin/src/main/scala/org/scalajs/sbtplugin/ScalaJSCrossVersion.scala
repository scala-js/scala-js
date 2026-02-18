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

object ScalaJSCrossVersion {
  private val sjsPrefix = "sjs" + ScalaJSVersions.binaryCross + "_"

  // Instead of using hard-coded one in sbt https://github.com/sbt/sbt/blob/2f27b5cecde3790ecb1b9d7205baa48d97300939/lm-core/src/main/scala/sbt/librarymanagement/Platform.scala#L4
  val binaryPlatform: String = "sjs" + ScalaJSVersions.binaryCross

  val binary: CrossVersion =
    CrossVersion.binaryWith(prefix = sjsPrefix, suffix = "")

  val full: CrossVersion =
    CrossVersion.fullWith(prefix = sjsPrefix, suffix = "")
}
