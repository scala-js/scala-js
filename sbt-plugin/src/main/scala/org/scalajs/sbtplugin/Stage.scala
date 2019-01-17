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

sealed trait Stage {
  def tagName: String
  def fileSuffix: String
  def name: String
}

object Stage {
  case object FullOpt extends Stage {
    val tagName: String = "fullopt"
    val fileSuffix: String = "opt"
    val name: String = "Full"
  }
  case object FastOpt extends Stage {
    val tagName: String = "fastopt"
    val fileSuffix: String = "fastopt"
    val name: String = "Fast"
  }
}
