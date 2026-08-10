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

package org.scalajs.nscplugin.test.util

object VersionDependentUtils {
  val scalaVersion = scala.util.Properties.versionNumberString

  private val isScala212 = scalaVersion.startsWith("2.12.")

  def methodSig(params: String, resultType: String): String =
    if (!isScala212) params + ": " + resultType
    else params + resultType
}
