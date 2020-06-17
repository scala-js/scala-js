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

object ScalaJSCrossVersion {
  private val sjsPrefix = "sjs1_"

  val binary: CrossVersion =
    CrossVersion.binaryWith(prefix = sjsPrefix, suffix = "")

  val full: CrossVersion =
    CrossVersion.fullWith(prefix = sjsPrefix, suffix = "")
}
