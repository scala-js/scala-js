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

  /** Does the current Scala version support the `@nowarn` annotation? */
  val scalaSupportsNoWarn = {
    !scalaVersion.startsWith("2.11.") &&
    !scalaVersion.startsWith("2.12.") &&
    scalaVersion != "2.13.0" &&
    scalaVersion != "2.13.1"
  }

  private val usesColonInMethodSig = {
    /* Yes, this is the same test as in scalaSupportsNoWarn, but that's
     * completely coincidental, so we have a copy.
     */
    !scalaVersion.startsWith("2.11.") &&
    !scalaVersion.startsWith("2.12.") &&
    scalaVersion != "2.13.0" &&
    scalaVersion != "2.13.1"
  }

  def methodSig(params: String, resultType: String): String =
    if (usesColonInMethodSig) params + ": " + resultType
    else params + resultType
}
