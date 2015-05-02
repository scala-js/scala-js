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

    outputMode match {
      case OutputMode.ECMAScript51Global =>
      case OutputMode.ECMAScript51Isolated | OutputMode.ECMAScript6 =>
        builder.addLine("(function(){")
      case OutputMode.ECMAScript6StrongMode =>
        builder.addLine("(function(__this, __ScalaJSEnv, __global, " +
            "$jsSelect, $jsAssign, $jsDelete, $propertiesOf, $weakFun) {")
    }

    builder.addLine("'use strict';")
    outputMode match {
      case OutputMode.ECMAScript6StrongMode =>
        builder.addLine("'use strong';")
      case _ =>
        builder.addFile(CoreJSLibs.lib(semantics, outputMode))
    }

    optimizeIR(irFiles, cfg, builder, logger)

    outputMode match {
      case OutputMode.ECMAScript51Global =>
      case OutputMode.ECMAScript51Isolated | OutputMode.ECMAScript6 =>
        builder.addLine("}).call(this);")
      case OutputMode.ECMAScript6StrongMode =>
        builder.addLine("})(this,")
        builder.addLine("  (typeof __ScalaJSEnv !== 'undefined') ? __ScalaJSEnv : void 0,")
        builder.addLine("  (typeof global !== 'undefined') ? global : void 0,")
        builder.addLine("  function(x, p) { 'use strict'; return x[p]; },")
        builder.addLine("  function(x, p, v) { 'use strict'; x[p] = v; },")
        builder.addLine("  function(x, p) { 'use strict'; delete x[p]; },")
        builder.addLine("  function(x) { 'use strict'; const r = []; for (const p in x) r['push'](p); return r; },")
        builder.addLine("  function(f) { 'use strict'; return function(...args) { return f['apply'](void 0, args); } });")
    }

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

    val linkResult = logTime(logger, "Linker") {
      linker.link(irFiles, logger,
          reachOptimizerSymbols = !cfg.disableOptimizer,
          cfg.bypassLinkingErrors, cfg.noWarnMissing, cfg.checkIR)
    }

    if (cfg.checkIR) {
      logTime(logger, "Check IR") {
        if (linkResult.isComplete) {
          val checker = new IRChecker(linkResult, logger)
          if (!checker.check())
            sys.error(s"There were ${checker.errorCount} IR checking errors.")
        } else if (cfg.noWarnMissing.isEmpty)
          sys.error("Could not check IR because there where linking errors.")
      }
    }

    val useOptimizer = linkResult.isComplete && !cfg.disableOptimizer

    if (cfg.batchMode)
      resetStateFromOptimizer()

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
  final case class Config(
      /** Writer for the output. */
      output: WritableVirtualJSFile,
      /** Cache file */
      cache: Option[WritableVirtualTextFile] = None,
      /** Ask to produce source map for the output */
      wantSourceMap: Boolean = false,
      /** Base path to relativize paths in the source map. */
      relativizeSourceMapBase: Option[URI] = None,
      /** Whether to only warn if the linker has errors. Implicitly true, if
       *  noWarnMissing is nonEmpty
       */
      bypassLinkingErrors: Boolean = false,
      /** If true, performs expensive checks of the IR for the used parts. */
      checkIR: Boolean = false,
      /** If true, the optimizer removes trees that have not been used in the
       *  last run from the cache. Otherwise, all trees that has been used once,
       *  are kept in memory. */
      unCache: Boolean = true,
      /** If true, no optimizations are performed */
      disableOptimizer: Boolean = false,
      /** If true, nothing is performed incrementally */
      batchMode: Boolean = false,
      /** Elements we won't warn even if they don't exist */
      noWarnMissing: Seq[NoWarnMissing] = Nil
  ) extends OptimizerConfig

}
