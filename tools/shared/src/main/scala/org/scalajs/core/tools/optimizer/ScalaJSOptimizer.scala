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

  // Components
  private[this] var cache: IRFileCache = _
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
      logTime(logger, "Cache: Read info")(cache.update(irFiles))

      logTime(logger, "Linker") {
        linker.link(cache.files, logger,
            reachOptimizerSymbols = !cfg.disableOptimizer,
            cfg.bypassLinkingErrors, cfg.noWarnMissing, cfg.checkIR)
      }
    } catch {
      case th: Throwable =>
        resetState()
        throw th
    } finally {
      // End cache run
      val stats = cache.cleanAfterUse()
      logger.debug(stats.logLine)
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
        if (cfg.noWarnMissing.isEmpty && !cfg.disableOptimizer)
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
    cache = new IRFileCache()
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

  sealed abstract class NoWarnMissing {
    def className: String
  }

  object NoWarnMissing {
    final case class Class(className: String) extends NoWarnMissing
    final case class Method(className: String, methodName: String)
        extends NoWarnMissing
  }

  type OptimizerFactory = (Semantics, OutputMode, Boolean) => GenIncOptimizer

  /** Configurations relevant to the optimizer */
  trait OptimizerConfig {
    /** Ask to produce source map for the output. Is used in the incremental
     *  optimizer to decide whether a position change should trigger re-inlining
     */
    val wantSourceMap: Boolean
    /** Whether to only warn if the linker has errors. Implicitly true, if
     *  noWarnMissing is nonEmpty
     */
    val bypassLinkingErrors: Boolean
    /** If true, performs expensive checks of the IR for the used parts. */
    val checkIR: Boolean
    /** If true, the optimizer removes trees that have not been used in the
     *  last run from the cache. Otherwise, all trees that has been used once,
     *  are kept in memory. */
    val unCache: Boolean
    /** If true, no optimizations are performed */
    val disableOptimizer: Boolean
    /** If true, nothing is performed incrementally */
    val batchMode: Boolean
    /** Elements we won't warn even if they don't exist */
    val noWarnMissing: Seq[NoWarnMissing]
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
      /** Whether to only warn if the linker has errors. Implicitly true, if
       *  noWarnMissing is nonEmpty
       */
      val bypassLinkingErrors: Boolean,
      /** If true, performs expensive checks of the IR for the used parts. */
      val checkIR: Boolean,
      /** If true, the optimizer removes trees that have not been used in the
       *  last run from the cache. Otherwise, all trees that has been used once,
       *  are kept in memory. */
      val unCache: Boolean,
      /** If true, no optimizations are performed */
      val disableOptimizer: Boolean,
      /** If true, nothing is performed incrementally */
      val batchMode: Boolean,
      /** Elements we won't warn even if they don't exist */
      val noWarnMissing: Seq[NoWarnMissing],
      /** Custom js code that wraps the output */
      val customOutputWrapper: (String, String)
  ) extends OptimizerConfig
      /* for binary compatibility */ with Product with Serializable with Equals {

    /* NOTE: This class was previously a case class and hence many useless
     * methods were implemented for binary compatibility :(
     */

    // For binary compatibility
    @deprecated("Use Config(output) and .withXYZ() methods", "0.6.5")
    def this(
        output: WritableVirtualJSFile,
        cache: Option[WritableVirtualTextFile] = None,
        wantSourceMap: Boolean = false,
        relativizeSourceMapBase: Option[URI] = None,
        bypassLinkingErrors: Boolean = false,
        checkIR: Boolean = false,
        unCache: Boolean = true,
        disableOptimizer: Boolean = false,
        batchMode: Boolean = false,
        noWarnMissing: Seq[NoWarnMissing] = Nil) = {

      this(output, cache, wantSourceMap, relativizeSourceMapBase,
          bypassLinkingErrors, checkIR, unCache, disableOptimizer, batchMode,
          noWarnMissing, customOutputWrapper = ("", ""))
    }

    def withCache(cache: Option[WritableVirtualTextFile]): Config =
      copyWith(cache = cache)

    def withBypassLinkingErrors(bypassLinkingErrors: Boolean): Config =
      copyWith(bypassLinkingErrors = bypassLinkingErrors)

    def withCheckIR(checkIR: Boolean): Config =
      copyWith(checkIR = checkIR)

    def withUnCache(unCache: Boolean): Config =
      copyWith(unCache = unCache)

    def withDisableOptimizer(disableOptimizer: Boolean): Config =
      copyWith(disableOptimizer = disableOptimizer)

    def withBatchMode(batchMode: Boolean): Config =
      copyWith(batchMode = batchMode)

    def withWantSourceMap(wantSourceMap: Boolean): Config =
      copyWith(wantSourceMap = wantSourceMap)

    def withRelativizeSourceMapBase(relativizeSourceMapBase: Option[URI]): Config =
      copyWith(relativizeSourceMapBase = relativizeSourceMapBase)

    def withNoWarnMissing(noWarnMissing: Seq[ScalaJSOptimizer.NoWarnMissing]): Config =
      copyWith(noWarnMissing = noWarnMissing)

    def withCustomOutputWrapper(customOutputWrapper: (String, String)): Config =
      copyWith(customOutputWrapper = customOutputWrapper)

    // For binary compatibility
    @deprecated("Not a case class anymore", "0.6.5")
    def copy(
        output: WritableVirtualJSFile = this.output,
        cache: Option[WritableVirtualTextFile] = this.cache,
        wantSourceMap: Boolean = this.wantSourceMap,
        relativizeSourceMapBase: Option[URI] = this.relativizeSourceMapBase,
        bypassLinkingErrors: Boolean = this.bypassLinkingErrors,
        checkIR: Boolean = this.checkIR,
        unCache: Boolean = this.unCache,
        disableOptimizer: Boolean = this.disableOptimizer,
        batchMode: Boolean = this.batchMode,
        noWarnMissing: Seq[ScalaJSOptimizer.NoWarnMissing] = this.noWarnMissing): Config = {

      copyWith(output, cache, wantSourceMap, relativizeSourceMapBase,
          bypassLinkingErrors, checkIR, unCache, disableOptimizer, batchMode,
          noWarnMissing)
    }

    private def copyWith(
        output: WritableVirtualJSFile = this.output,
        cache: Option[WritableVirtualTextFile] = this.cache,
        wantSourceMap: Boolean = this.wantSourceMap,
        relativizeSourceMapBase: Option[URI] = this.relativizeSourceMapBase,
        bypassLinkingErrors: Boolean = this.bypassLinkingErrors,
        checkIR: Boolean = this.checkIR,
        unCache: Boolean = this.unCache,
        disableOptimizer: Boolean = this.disableOptimizer,
        batchMode: Boolean = this.batchMode,
        noWarnMissing: Seq[ScalaJSOptimizer.NoWarnMissing] = this.noWarnMissing,
        customOutputWrapper: (String, String) = this.customOutputWrapper): Config = {

      new Config(output, cache, wantSourceMap, relativizeSourceMapBase,
          bypassLinkingErrors, checkIR, unCache, disableOptimizer, batchMode,
          noWarnMissing, customOutputWrapper)
    }

    // For binary compatibility
    @deprecated("Not a case class anymore", "0.6.5")
    def canEqual(that: Any): Boolean = true

    // For binary compatibility
    @deprecated("Not a case class anymore", "0.6.5")
    def productArity: Int = productArray.length

    // For binary compatibility
    @deprecated("Not a case class anymore", "0.6.5")
    def productElement(n: Int): Any = productArray(n)

    // For binary compatibility
    private def productArray: Array[Any] = {
      Array[Any](output, cache, wantSourceMap, relativizeSourceMapBase,
          bypassLinkingErrors, checkIR, unCache, disableOptimizer, batchMode,
          noWarnMissing, customOutputWrapper)
    }

    // For binary compatibility
    override def equals(other: Any): Boolean = super.equals(other)

    // For binary compatibility
    override def hashCode(): Int = super.hashCode()

    // For binary compatibility
    override def toString(): String =
      productArray.mkString("Config(", ", ", ")")
  }

  object Config extends runtime.AbstractFunction10[WritableVirtualJSFile,
      Option[WritableVirtualTextFile], Boolean, Option[URI], Boolean, Boolean,
      Boolean, Boolean, Boolean, Seq[NoWarnMissing], Config] {

    def apply(output: WritableVirtualJSFile): Config = {
      new Config(
          output = output,
          cache = None,
          wantSourceMap = false,
          relativizeSourceMapBase = None,
          bypassLinkingErrors = false,
          checkIR = false,
          unCache = true,
          disableOptimizer = false,
          batchMode = false,
          noWarnMissing = Nil,
          customOutputWrapper = ("", ""))
    }

    // For binary compatibility
    @deprecated("Use Config(output) and .withXYZ() methods", "0.6.5")
    def apply(
        output: WritableVirtualJSFile,
        cache: Option[WritableVirtualTextFile] = None,
        wantSourceMap: Boolean = false,
        relativizeSourceMapBase: Option[URI] = None,
        bypassLinkingErrors: Boolean = false,
        checkIR: Boolean = false,
        unCache: Boolean = true,
        disableOptimizer: Boolean = false,
        batchMode: Boolean = false,
        noWarnMissing: Seq[NoWarnMissing] = Nil): Config = {

      new Config(output, cache, wantSourceMap, relativizeSourceMapBase,
          bypassLinkingErrors, checkIR, unCache, disableOptimizer, batchMode,
          noWarnMissing, customOutputWrapper = ("", ""))
    }

    // For binary compatibility
    @deprecated("Not a case class anymore", "0.6.5")
    def unapply(config: Config): Option[(WritableVirtualJSFile,
        Option[WritableVirtualTextFile], Boolean, Option[URI], Boolean, Boolean,
        Boolean, Boolean, Boolean, Seq[NoWarnMissing])] = {

      Some((config.output, config.cache, config.wantSourceMap,
          config.relativizeSourceMapBase, config.bypassLinkingErrors,
          config.checkIR, config.unCache, config.disableOptimizer, config.batchMode,
          config.noWarnMissing))
    }
  }
}
