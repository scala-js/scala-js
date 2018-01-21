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

import org.scalajs.io.WritableVirtualJSFile
import org.scalajs.logging.Logger

import org.scalajs.linker._
import org.scalajs.linker.standard._
import org.scalajs.linker.analyzer.SymbolRequirement

/** A backend of the Scala.js linker. Produces a
 *  [[org.scalajs.io.VirtualJSFile VirtualJSFile]].
 *
 *  You probably want to use an instance of [[linker.Linker]], rather than this
 *  low-level class.
 */
abstract class LinkerBackend(protected val config: LinkerBackend.Config) {
  /** Core specification that this linker backend implements. */
  val coreSpec = config.commonConfig.coreSpec

  /** Symbols this backend needs to be present in the linking unit. */
  val symbolRequirements: SymbolRequirement

  /** Emit the given [[standard.LinkingUnit LinkingUnit]] to the target output.
   *
   *  The linking unit given to `emit` must:
   *
   *  - have the same `coreSpec` as this linker backend, and
   *  - contain the symbols listed in [[symbolRequirements]].
   *
   *  @param unit [[standard.LinkingUnit LinkingUnit]] to emit
   *  @param output File to write to
   *  @param logger Logger to use
   */
  def emit(unit: LinkingUnit, output: WritableVirtualJSFile,
      logger: Logger): Unit

  /** Verify that a [[standard.LinkingUnit LinkingUnit]] can be processed by
   *  this [[LinkerBackend]].
   *
   *  Currently, this only tests that the linking unit core specification
   *  matches [[coreSpec]].
   *
   *  In the future, this test could be extended to test [[symbolRequirements]]
   *  too.
   *
   *  @throws java.lang.IllegalArgumentException if there is a mismatch
   */
  protected def verifyUnit(unit: LinkingUnit): Unit = {
    require(unit.coreSpec == coreSpec,
        "LinkingUnit and LinkerBackend must agree on their core specification")
  }
}

object LinkerBackend {
  def apply(config: Config): LinkerBackend =
    LinkerBackendPlatform.createLinkerBackend(config)

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
    import LinkerBackendPlatformExtensions._

    implicit def toPlatformExtensions(config: Config): ConfigExt =
      new ConfigExt(config)

    def apply(): Config = new Config()
  }
}
