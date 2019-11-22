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

  val binary: CrossVersion =
    CrossVersion.binaryWith(prefix = sjsPrefix, suffix = "")

  val full: CrossVersion =
    CrossVersion.fullWith(prefix = sjsPrefix, suffix = "")
}
