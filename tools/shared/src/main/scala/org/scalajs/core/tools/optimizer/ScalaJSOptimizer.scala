/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.optimizer

import scala.annotation.{switch, tailrec}

import scala.collection.mutable
import scala.collection.immutable.{Seq, Traversable}

import java.net.URI

import org.scalajs.core.ir
import ir.Infos
import ir.ClassKind

import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.sourcemap._
import org.scalajs.core.tools.corelib._

import org.scalajs.core.tools.sem.Semantics

import org.scalajs.core.tools.javascript
import javascript.{Trees => js, OutputMode}

/** Scala.js optimizer: does type-aware global dce. */
class ScalaJSOptimizer(val semantics: Semantics, val outputMode: OutputMode,
    optimizerFactory: ScalaJSOptimizer.OptimizerFactory) {
  import ScalaJSOptimizer._

  private[this] var withSourceMap: Boolean = _
  private[this] var linker: Linker = _
  private[this] var optimizer: GenIncOptimizer = _
  private[this] var refiner: Refiner = _
  private[this] var emitter: Emitter = _

  clean()

  def this(semantics: Semantics, mode: OutputMode) =
    this(semantics, mode, IncOptimizer.factory)

  @deprecated("Use the overload with an explicit OutputMode", "0.6.2")
  def this(semantics: Semantics,
      optimizerFactory: ScalaJSOptimizer.OptimizerFactory) =
    this(semantics, OutputMode.ECMAScript51Global, optimizerFactory)

  @deprecated("Use the overload with an explicit OutputMode", "0.6.2")
  def this(semantics: Semantics) = this(semantics, IncOptimizer.factory)

  /** Applies Scala.js-specific optimizations to a CompleteIRClasspath.
   *  See [[ScalaJSOptimizer.Config]] for details about the configuration
   *  for the output of this method.
   *  Returns a [[CompleteCIClasspath]] containing the result of the
   *  optimizations.
   *
   *  analyzes, dead code eliminates and concatenates IR content
   *  - Maintains/establishes order
   *  - No IR in result
   *  - CoreJSLibs in result (since they are implicitly in the CompleteIRCP)
   */
  def optimizeCP(classpath: IRClasspath, cfg: Config,
      logger: Logger): LinkedClasspath = {

    CacheUtils.cached(classpath.version, cfg.output, cfg.cache) {
      logger.info(s"Fast optimizing ${cfg.output.path}")
      optimizeIR(classpath.scalaJSIR, cfg, logger)
    }

    new LinkedClasspath(classpath.jsLibs, cfg.output,
        classpath.requiresDOM, classpath.version)
  }

  def optimizeIR(irFiles: Traversable[VirtualScalaJSIRFile],
      cfg: Config, logger: Logger): Unit = {

    val builder = {
      import cfg._
      if (wantSourceMap)
        new JSFileBuilderWithSourceMap(output.name,
            output.contentWriter,
            output.sourceMapWriter,
            relativizeSourceMapBase)
      else
        new JSFileBuilder(output.name, output.contentWriter)
    }

    emitter.emitCustomHeader(cfg.customOutputWrapper._1, builder)
    emitter.emitPrelude(builder, logger)
    optimizeIR(irFiles, cfg, builder, logger)
    emitter.emitPostlude(builder, logger)
    emitter.emitCustomFooter(cfg.customOutputWrapper._2, builder)

    builder.complete()
    builder.closeWriters()
  }

  def optimizeIR(irFiles: Traversable[VirtualScalaJSIRFile],
      cfg: OptimizerConfig, builder: JSTreeBuilder, logger: Logger): Unit = {

    // Update state that is a per-run configuration for the overall optimizer,
    // but not the phases
    if (cfg.wantSourceMap != withSourceMap) {
      withSourceMap = cfg.wantSourceMap
      resetState()
    }

    val linkResult = try {
      logTime(logger, "Linker") {
        linker.link(irFiles, logger,
            reachOptimizerSymbols = !cfg.disableOptimizer,
            cfg.bypassLinkingErrors, cfg.checkIR)
      }
    } catch {
      case th: Throwable =>
        resetState()
        throw th
    }

    val useOptimizer = linkResult.isComplete && !cfg.disableOptimizer

    if (cfg.batchMode)
      resetStateFromOptimizer()

    try {
      val finalResult = if (useOptimizer) {
        val rawOptimized = logTime(logger, "Inc. optimizer") {
          optimizer.update(linkResult, logger)
        }

        logTime(logger, "Refiner") {
          refiner.refine(rawOptimized, logger)
        }
      } else {
        if (!cfg.disableOptimizer)
          logger.warn("Not running the optimizer because there where linking errors.")
        linkResult
      }

      logTime(logger, "Emitter (write output)") {
        emitter.emit(finalResult, builder, logger)
      }
    } catch {
      case th: Throwable =>
        resetStateFromOptimizer()
        throw th
    }
  }

  /** Resets all persistent state of this optimizer */
  def clean(): Unit = resetState()

  private def resetState(): Unit = {
    linker = new Linker(semantics, outputMode, withSourceMap)
    resetStateFromOptimizer()
  }

  private def resetStateFromOptimizer(): Unit = {
    optimizer = optimizerFactory(semantics, outputMode, withSourceMap)
    refiner = new Refiner(semantics, outputMode)
    emitter = new Emitter(semantics, outputMode)
  }
}

object ScalaJSOptimizer {

  type OptimizerFactory = (Semantics, OutputMode, Boolean) => GenIncOptimizer

  /** Configurations relevant to the optimizer */
  trait OptimizerConfig {
    /** Ask to produce source map for the output. Is used in the incremental
     *  optimizer to decide whether a position change should trigger re-inlining
     */
    val wantSourceMap: Boolean
    /** Whether to only warn if the linker has errors. */
    val bypassLinkingErrors: Boolean
    /** If true, performs expensive checks of the IR for the used parts. */
    val checkIR: Boolean
    /** If true, no optimizations are performed */
    val disableOptimizer: Boolean
    /** If true, nothing is performed incrementally */
    val batchMode: Boolean
  }

  /** Configuration for the output of the Scala.js optimizer. */
  final class Config private[Config] (
      /** Writer for the output. */
      val output: WritableVirtualJSFile,
      /** Cache file */
      val cache: Option[WritableVirtualTextFile],
      /** Ask to produce source map for the output */
      val wantSourceMap: Boolean,
      /** Base path to relativize paths in the source map. */
      val relativizeSourceMapBase: Option[URI],
      /** Whether to only warn if the linker has errors. */
      val bypassLinkingErrors: Boolean,
      /** If true, performs expensive checks of the IR for the used parts. */
      val checkIR: Boolean,
      /** If true, no optimizations are performed */
      val disableOptimizer: Boolean,
      /** If true, nothing is performed incrementally */
      val batchMode: Boolean,
      /** Custom js code that wraps the output */
      val customOutputWrapper: (String, String)
  ) extends OptimizerConfig {

    def withCache(cache: Option[WritableVirtualTextFile]): Config =
      copyWith(cache = cache)

    def withBypassLinkingErrors(bypassLinkingErrors: Boolean): Config =
      copyWith(bypassLinkingErrors = bypassLinkingErrors)

    def withCheckIR(checkIR: Boolean): Config =
      copyWith(checkIR = checkIR)

    def withDisableOptimizer(disableOptimizer: Boolean): Config =
      copyWith(disableOptimizer = disableOptimizer)

    def withBatchMode(batchMode: Boolean): Config =
      copyWith(batchMode = batchMode)

    def withWantSourceMap(wantSourceMap: Boolean): Config =
      copyWith(wantSourceMap = wantSourceMap)

    def withRelativizeSourceMapBase(relativizeSourceMapBase: Option[URI]): Config =
      copyWith(relativizeSourceMapBase = relativizeSourceMapBase)

    def withCustomOutputWrapper(customOutputWrapper: (String, String)): Config =
      copyWith(customOutputWrapper = customOutputWrapper)

    override def toString(): String = {
        s"""Config(
           |  output                  = $output
           |  cache                   = $cache
           |  wantSourceMap           = $wantSourceMap
           |  relativizeSourceMapBase = $relativizeSourceMapBase
           |  bypassLinkingErrors     = $bypassLinkingErrors
           |  checkIR                 = $checkIR
           |  disableOptimizer        = $disableOptimizer
           |  batchMode               = $batchMode
           |  customOutputWrapper     = $customOutputWrapper
           |)""".stripMargin
    }

    private def copyWith(
        output: WritableVirtualJSFile = this.output,
        cache: Option[WritableVirtualTextFile] = this.cache,
        wantSourceMap: Boolean = this.wantSourceMap,
        relativizeSourceMapBase: Option[URI] = this.relativizeSourceMapBase,
        bypassLinkingErrors: Boolean = this.bypassLinkingErrors,
        checkIR: Boolean = this.checkIR,
        disableOptimizer: Boolean = this.disableOptimizer,
        batchMode: Boolean = this.batchMode,
        customOutputWrapper: (String, String) = this.customOutputWrapper): Config = {

      new Config(output, cache, wantSourceMap, relativizeSourceMapBase,
          bypassLinkingErrors, checkIR, disableOptimizer, batchMode,
          customOutputWrapper)
    }
  }

  object Config {
    def apply(output: WritableVirtualJSFile): Config = {
      new Config(
          output = output,
          cache = None,
          wantSourceMap = false,
          relativizeSourceMapBase = None,
          bypassLinkingErrors = false,
          checkIR = false,
          disableOptimizer = false,
          batchMode = false,
          customOutputWrapper = ("", ""))
    }
  }
}
