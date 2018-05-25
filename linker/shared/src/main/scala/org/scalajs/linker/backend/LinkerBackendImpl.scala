/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.linker.backend

import scala.language.implicitConversions

import java.net.URI

import org.scalajs.logging.Logger

import org.scalajs.linker._
import org.scalajs.linker.standard._

/** A backend of the Scala.js linker.
 *
 *  Produces a JavaScript file with an optional source map.
 *
 *  You probably want to use an instance of [[linker.Linker]], rather than this
 *  low-level class.
 */
abstract class LinkerBackendImpl(
    protected val config: LinkerBackendImpl.Config)
    extends LinkerBackend {

  /** Core specification that this linker backend implements. */
  val coreSpec = config.commonConfig.coreSpec
}

object LinkerBackendImpl {
  def apply(config: Config): LinkerBackendImpl =
    LinkerBackendImplPlatform.createLinkerBackend(config)

  /** Configurations relevant to the backend */
  final class Config private (
      /** Common phase config. */
      val commonConfig: CommonPhaseConfig,
      /** Whether to emit a source map. */
      val sourceMap: Boolean,
      /** Base path to relativize paths in the source map. */
      val relativizeSourceMapBase: Option[URI],
      /** Whether to use the Google Closure Compiler pass, if it is available.
       *  On the JavaScript platform, this does not have any effect.
       */
      val closureCompilerIfAvailable: Boolean,
      /** Pretty-print the output. */
      val prettyPrint: Boolean
  ) {
    private def this() = {
      this(
          commonConfig = CommonPhaseConfig(),
          sourceMap = true,
          relativizeSourceMapBase = None,
          closureCompilerIfAvailable = false,
          prettyPrint = false)
    }

    def withCommonConfig(commonConfig: CommonPhaseConfig): Config =
      copy(commonConfig = commonConfig)

    def withSourceMap(sourceMap: Boolean): Config =
      copy(sourceMap = sourceMap)

    def withRelativizeSourceMapBase(relativizeSourceMapBase: Option[URI]): Config =
      copy(relativizeSourceMapBase = relativizeSourceMapBase)

    def withClosureCompilerIfAvailable(closureCompilerIfAvailable: Boolean): Config =
      copy(closureCompilerIfAvailable = closureCompilerIfAvailable)

    def withPrettyPrint(prettyPrint: Boolean): Config =
      copy(prettyPrint = prettyPrint)

    private def copy(
        commonConfig: CommonPhaseConfig = commonConfig,
        sourceMap: Boolean = sourceMap,
        relativizeSourceMapBase: Option[URI] = relativizeSourceMapBase,
        closureCompilerIfAvailable: Boolean = closureCompilerIfAvailable,
        prettyPrint: Boolean = prettyPrint): Config = {
      new Config(commonConfig, sourceMap, relativizeSourceMapBase,
          closureCompilerIfAvailable, prettyPrint)
    }
  }

  object Config {
    import LinkerBackendImplPlatformExtensions._

    implicit def toPlatformExtensions(config: Config): ConfigExt =
      new ConfigExt(config)

    def apply(): Config = new Config()
  }
}
