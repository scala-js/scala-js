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

package org.scalajs.linker.backend

import scala.language.implicitConversions

import java.net.URI

import org.scalajs.logging.Logger

import org.scalajs.linker._
import org.scalajs.linker.interface.{OutputPatterns, StandardConfig}
import org.scalajs.linker.standard._

/** A backend of the Scala.js linker.
 *
 *  Produces a JavaScript file with an optional source map.
 *
 *  You probably want to use an instance of [[interface.Linker]], rather than
 *  this low-level class.
 */
abstract class LinkerBackendImpl(
    protected val config: LinkerBackendImpl.Config)
    extends LinkerBackend {

  /** Core specification that this linker backend implements. */
  val coreSpec = config.commonConfig.coreSpec
}

object LinkerBackendImpl {
  def apply(config: Config): LinkerBackendImpl = {
    if (config.experimentalUseWebAssembly)
      new WebAssemblyLinkerBackend(config)
    else
      LinkerBackendImplPlatform.createJSLinkerBackend(config)
  }

  /** Configurations relevant to the backend */
  final class Config private (
      /** Common phase config. */
      val commonConfig: CommonPhaseConfig,
      /** A header that will be added at the top of generated .js files. */
      val jsHeader: String,
      /** Whether to emit a source map. */
      val sourceMap: Boolean,
      /** Name patterns for output. */
      val outputPatterns: OutputPatterns,
      /** Base path to relativize paths in the source map. */
      val relativizeSourceMapBase: Option[URI],
      /** Whether to use Scala.js' minifier for property names. */
      val minify: Boolean,
      /** Whether to use the Google Closure Compiler pass, if it is available.
       *  On the JavaScript platform, this does not have any effect.
       */
      val closureCompilerIfAvailable: Boolean,
      /** Pretty-print the output. */
      val prettyPrint: Boolean,
      /** The maximum number of (file) writes executed concurrently. */
      val maxConcurrentWrites: Int,
      /** If true, use the experimental WebAssembly backend. */
      val experimentalUseWebAssembly: Boolean
  ) {
    private def this() = {
      this(
          commonConfig = CommonPhaseConfig(),
          jsHeader = "",
          sourceMap = true,
          outputPatterns = OutputPatterns.Defaults,
          relativizeSourceMapBase = None,
          minify = false,
          closureCompilerIfAvailable = false,
          prettyPrint = false,
          maxConcurrentWrites = 50,
          experimentalUseWebAssembly = false
      )
    }

    def withCommonConfig(commonConfig: CommonPhaseConfig): Config =
      copy(commonConfig = commonConfig)

    def withJSHeader(jsHeader: String): Config = {
      require(StandardConfig.isValidJSHeader(jsHeader), jsHeader)
      copy(jsHeader = jsHeader)
    }

    def withSourceMap(sourceMap: Boolean): Config =
      copy(sourceMap = sourceMap)

    def withOutputPatterns(outputPatterns: OutputPatterns): Config =
      copy(outputPatterns = outputPatterns)

    def withRelativizeSourceMapBase(relativizeSourceMapBase: Option[URI]): Config =
      copy(relativizeSourceMapBase = relativizeSourceMapBase)

    def withMinify(minify: Boolean): Config =
      copy(minify = minify)

    def withClosureCompilerIfAvailable(closureCompilerIfAvailable: Boolean): Config =
      copy(closureCompilerIfAvailable = closureCompilerIfAvailable)

    def withPrettyPrint(prettyPrint: Boolean): Config =
      copy(prettyPrint = prettyPrint)

    def withMaxConcurrentWrites(maxConcurrentWrites: Int): Config =
      copy(maxConcurrentWrites = maxConcurrentWrites)

    def withExperimentalUseWebAssembly(experimentalUseWebAssembly: Boolean): Config =
      copy(experimentalUseWebAssembly = experimentalUseWebAssembly)

    private def copy(
        commonConfig: CommonPhaseConfig = commonConfig,
        jsHeader: String = jsHeader,
        sourceMap: Boolean = sourceMap,
        outputPatterns: OutputPatterns = outputPatterns,
        relativizeSourceMapBase: Option[URI] = relativizeSourceMapBase,
        minify: Boolean = minify,
        closureCompilerIfAvailable: Boolean = closureCompilerIfAvailable,
        prettyPrint: Boolean = prettyPrint,
        maxConcurrentWrites: Int = maxConcurrentWrites,
        experimentalUseWebAssembly: Boolean = experimentalUseWebAssembly
    ): Config = {
      new Config(
        commonConfig,
        jsHeader,
        sourceMap,
        outputPatterns,
        relativizeSourceMapBase,
        minify,
        closureCompilerIfAvailable,
        prettyPrint,
        maxConcurrentWrites,
        experimentalUseWebAssembly
      )
    }
  }

  object Config {
    import LinkerBackendImplPlatformExtensions._

    implicit def toPlatformExtensions(config: Config): ConfigExt =
      new ConfigExt(config)

    def apply(): Config = new Config()
  }
}
