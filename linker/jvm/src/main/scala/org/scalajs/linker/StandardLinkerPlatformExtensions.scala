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

package org.scalajs.linker

object StandardLinkerPlatformExtensions {
  import StandardLinker.Config

  final class ConfigExt private[linker] (val __private_self: Config)
      extends AnyVal {

    @inline private def self: Config = __private_self

    /** Whether to actually use the Google Closure Compiler pass. */
    def closureCompiler: Boolean = self.closureCompilerIfAvailable

    def withClosureCompiler(closureCompiler: Boolean): Config =
      self.withClosureCompilerIfAvailable(closureCompiler)
  }
}
