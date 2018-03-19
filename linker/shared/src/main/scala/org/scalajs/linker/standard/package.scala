/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.linker

package object standard {
  implicit class StandardLinkerConfigStandardOps private[standard] (
      val __private_self: StandardLinker.Config) extends AnyVal {

    import StandardLinker.Config

    @inline private def self: Config = __private_self

    /** Standard output mode. */
    @deprecated("Use esFeatures instead.", "0.6.23")
    def outputMode: OutputMode = self.esFeatures

    @deprecated("Use withESFeatures instead.", "0.6.23")
    def withOutputMode(outputMode: OutputMode): Config =
      self.withESFeatures(outputMode)
  }
}
