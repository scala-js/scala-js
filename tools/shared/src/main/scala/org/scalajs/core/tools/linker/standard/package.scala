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

package org.scalajs.core.tools.linker

package object standard {
  type OutputMode = org.scalajs.core.tools.linker.backend.OutputMode

  val OutputMode: org.scalajs.core.tools.linker.backend.OutputMode.type =
    org.scalajs.core.tools.linker.backend.OutputMode

  implicit class StandardLinkerConfigStandardOps(
      val __self: StandardLinker.Config) extends AnyVal {

    import StandardLinker.Config

    /** Standard output mode. */
    @deprecated("Use esFeatures instead.", "0.6.23")
    def outputMode: OutputMode = __self.esFeatures

    @deprecated("Use withESFeatures instead.", "0.6.23")
    def withOutputMode(outputMode: OutputMode): Config =
      __self.withESFeatures(outputMode)
  }
}
