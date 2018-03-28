/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.linker.backend

object LinkerBackendImplPlatformExtensions {
  import LinkerBackendImpl.Config

  final class ConfigExt private[backend] (private val self: Config)
      extends AnyVal {

    /** Whether to actually use the Google Closure Compiler pass.
     *
     *  On the JavaScript platform, this always returns `false`, as GCC is not
     *  available.
     */
    def closureCompiler: Boolean = false
  }
}
