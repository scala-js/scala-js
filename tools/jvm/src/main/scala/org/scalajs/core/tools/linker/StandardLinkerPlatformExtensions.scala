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

object StandardLinkerPlatformExtensions {
  import StandardLinker.Config

  final class ConfigExt(val config: Config) extends AnyVal {
    /** Whether to actually use the Google Closure Compiler pass. */
    def closureCompiler: Boolean = config.closureCompilerIfAvailable

    def withClosureCompiler(closureCompiler: Boolean): Config =
      config.withClosureCompilerIfAvailable(closureCompiler)
  }
}
