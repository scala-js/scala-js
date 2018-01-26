/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.linker.backend

import org.scalajs.linker.backend.closure.ClosureLinkerBackend

object LinkerBackendImplPlatformExtensions {
  import LinkerBackendImpl.Config

  final class ConfigExt private[backend] (val __private_self: Config)
      extends AnyVal {

    @inline private def self: Config = __private_self

    /** Whether to actually use the Google Closure Compiler pass. */
    def closureCompiler: Boolean = self.closureCompilerIfAvailable

    def withClosureCompiler(closureCompiler: Boolean): Config =
      self.withClosureCompilerIfAvailable(closureCompiler)
  }
}
