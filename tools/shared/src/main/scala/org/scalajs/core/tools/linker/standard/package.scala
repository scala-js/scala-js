/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.core.tools.linker

package object standard {
  implicit class StandardLinkerConfigStandardOps(
      val __self: StandardLinker.Config) extends AnyVal {

    import StandardLinker.Config

    /** Standard output mode. */
    def outputMode: OutputMode = __self.outputMode

    def withOutputMode(outputMode: OutputMode): Config =
      __self.withOutputMode(outputMode)
  }
}
