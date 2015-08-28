/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.optimizer

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.corelib.CoreJSLibs
import org.scalajs.core.tools.javascript.OutputMode

import com.google.javascript.jscomp.{
  SourceFile => ClosureSource,
  Compiler => ClosureCompiler,
  CompilerOptions => ClosureOptions,
  _
}
import scala.collection.JavaConverters._
import scala.collection.immutable.{Seq, Traversable}

import java.net.URI

/** Scala.js Closure optimizer: does advanced optimizations with Closure. */
class ScalaJSClosureOptimizer {
  import ScalaJSClosureOptimizer._

  @deprecated("The `semantics` parameter is ignored. Use the overload without parameter.", "0.6.2")
  def this(semantics: Semantics) =
    this()

  private def toClosureSource(file: VirtualJSFile) =
    ClosureSource.fromReader(file.toURI.toString(), file.reader)

  private def toClosureInput(file: VirtualJSFile) =
    new CompilerInput(toClosureSource(file))

  /** Fully optimizes an [[IRClasspath]] by asking the ScalaJSOptimizer to
   *  emit a closure AST and then compiling this AST directly
   */
  def optimizeCP(optimizer: ScalaJSOptimizer, classpath: IRClasspath,
      cfg: Config, logger: Logger): LinkedClasspath = {

    CacheUtils.cached(classpath.version, cfg.output, cfg.cache) {
      logger.info(s"Full Optimizing ${cfg.output.path}")
      optimizeIR(optimizer, classpath.scalaJSIR, cfg, logger)
    }

    new LinkedClasspath(classpath.jsLibs, cfg.output,
        classpath.requiresDOM, classpath.version)
  }

  def optimizeIR(optimizer: ScalaJSOptimizer,
      irFiles: Traversable[VirtualScalaJSIRFile], cfg: Config,
      logger: Logger): Unit = {

    // Build Closure IR via ScalaJSOptimizer
    val builder = new ClosureAstBuilder(cfg.relativizeSourceMapBase)

    optimizer.optimizeIR(irFiles, cfg, builder, logger)

    // Build a Closure JSModule which includes the core libs
    val module = new JSModule("Scala.js")

    module.add(toClosureInput(
        CoreJSLibs.lib(optimizer.semantics, optimizer.outputMode)))

    val ast = builder.closureAST
    module.add(new CompilerInput(ast, ast.getInputId(), false))

    // Compile the module
    val closureExterns = toClosureSource(ScalaJSExternsFile)
    val options = closureOptions(cfg)
    val compiler = closureCompiler(logger)

    val result = logTime(logger, "Closure: Compiler pass") {
      compiler.compileModules(
          List(closureExterns).asJava, List(module).asJava, options)
    }

    logTime(logger, "Closure: Write result") {
      writeResult(result, compiler, cfg)
    }
  }

  private def writeResult(result: Result, compiler: ClosureCompiler,
      cfg: Config): Unit = {
    val output = cfg.output

    def withNewLine(str: String): String = if (str == "") "" else str + "\n"

    val (header0, footer0) = cfg.customOutputWrapper
    val header = withNewLine(header0) + "(function(){'use strict';\n"
    val footer = "}).call(this);\n" + withNewLine(footer0)

    val outputContent =
      if (result.errors.nonEmpty) "// errors while producing source\n"
      else compiler.toSource + "\n"

    val sourceMap = Option(compiler.getSourceMap())

    // Write optimized code
    val w = output.contentWriter
    try {
      w.write(header)
      w.write(outputContent)
      w.write(footer)
      if (sourceMap.isDefined)
        w.write("//# sourceMappingURL=" + output.name + ".map\n")
    } finally w.close()

    // Write source map (if available)
    sourceMap.foreach { sm =>
      sm.setWrapperPrefix(header)
      val w = output.sourceMapWriter
      try sm.appendTo(w, output.name)
      finally w.close()
    }
  }

  private def closureOptions(optConfig: OptimizerConfig,
      noSourceMap: Boolean = false) = {

    val options = new ClosureOptions
    options.prettyPrint = optConfig.prettyPrint
    CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(options)
    options.setLanguageIn(ClosureOptions.LanguageMode.ECMASCRIPT5)
    options.setCheckGlobalThisLevel(CheckLevel.OFF)

    if (!noSourceMap && optConfig.wantSourceMap) {
      options.setSourceMapOutputPath(optConfig.output.name + ".map")
      options.setSourceMapDetailLevel(SourceMap.DetailLevel.ALL)
    }

    options
  }

  private def closureCompiler(logger: Logger) = {
    val compiler = new ClosureCompiler
    compiler.setErrorManager(new LoggerErrorManager(logger))
    compiler
  }
}

object ScalaJSClosureOptimizer {

  /** Configuration the closure part of the optimizer needs.
   *  See [[Config]] for a description of the fields.
   */
  trait OptimizerConfig {
    val output: WritableVirtualJSFile
    val cache: Option[WritableVirtualTextFile]
    val wantSourceMap: Boolean
    val prettyPrint: Boolean
    val relativizeSourceMapBase: Option[URI]
  }

  /** Configuration for the output of the Scala.js Closure optimizer */
  final class Config private[Config] (
      /** Writer for the output */
      val output: WritableVirtualJSFile,
      /** Cache file */
      val cache: Option[WritableVirtualTextFile],
      /** Whether to only warn if the linker has errors. Implicitly true, if
       *  noWarnMissing is nonEmpty */
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
      /** Ask to produce source map for the output */
      val wantSourceMap: Boolean,
      /** Pretty-print the output. */
      val prettyPrint: Boolean,
      /** Base path to relativize paths in the source map */
      val relativizeSourceMapBase: Option[URI],
      /** Elements we won't warn even if they don't exist */
      val noWarnMissing: Seq[ScalaJSOptimizer.NoWarnMissing],
      /** Custom js code that wraps the output */
      val customOutputWrapper: (String, String)
  ) extends OptimizerConfig with ScalaJSOptimizer.OptimizerConfig
      /* for binary compatibility */ with Product with Serializable with Equals {

    /* NOTE: This class was previously a case class and hence many useless
     * methods were implemented for binary compatibility :(
     */

    // For binary compatibility
    @deprecated("Use Config(output) and .withXYZ() methods", "0.6.5")
    def this(
        output: WritableVirtualJSFile,
        cache: Option[WritableVirtualTextFile] = None,
        bypassLinkingErrors: Boolean = false,
        checkIR: Boolean = false,
        unCache: Boolean = true,
        disableOptimizer: Boolean = false,
        batchMode: Boolean = false,
        wantSourceMap: Boolean = false,
        prettyPrint: Boolean = false,
        relativizeSourceMapBase: Option[URI] = None,
        noWarnMissing: Seq[ScalaJSOptimizer.NoWarnMissing] = Nil) = {

      this(
          output = output,
          cache = cache,
          bypassLinkingErrors = bypassLinkingErrors,
          checkIR = checkIR,
          unCache = unCache,
          disableOptimizer = disableOptimizer,
          batchMode = batchMode,
          wantSourceMap = wantSourceMap,
          prettyPrint = prettyPrint,
          relativizeSourceMapBase = relativizeSourceMapBase,
          noWarnMissing = noWarnMissing,
          customOutputWrapper = ("", ""))
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

    def withPrettyPrint(prettyPrint: Boolean): Config =
      copyWith(prettyPrint = prettyPrint)

    def withRelativizeSourceMapBase(relativizeSourceMapBase: Option[URI]): Config =
      copyWith(relativizeSourceMapBase = relativizeSourceMapBase)

    def withNoWarnMissing(noWarnMissing: Seq[ScalaJSOptimizer.NoWarnMissing]): Config =
      copyWith(noWarnMissing = noWarnMissing)

    def withCustomOutputWrapper(customOutputWrapper: (String, String)): Config =
      copyWith(customOutputWrapper = customOutputWrapper)

    @deprecated("Not a case class anymore", "0.6.5")
    def copy(
        output: WritableVirtualJSFile = this.output,
        cache: Option[WritableVirtualTextFile] = this.cache,
        bypassLinkingErrors: Boolean = this.bypassLinkingErrors,
        checkIR: Boolean = this.checkIR,
        unCache: Boolean = this.unCache,
        disableOptimizer: Boolean = this.disableOptimizer,
        batchMode: Boolean = this.batchMode,
        wantSourceMap: Boolean = this.wantSourceMap,
        prettyPrint: Boolean = this.prettyPrint,
        relativizeSourceMapBase: Option[URI] = this.relativizeSourceMapBase,
        noWarnMissing: Seq[ScalaJSOptimizer.NoWarnMissing] = this.noWarnMissing): Config = {

      copyWith(output, cache, bypassLinkingErrors, checkIR, unCache,
          disableOptimizer, batchMode, wantSourceMap, prettyPrint,
          relativizeSourceMapBase, noWarnMissing)
    }

    private def copyWith(
        output: WritableVirtualJSFile = this.output,
        cache: Option[WritableVirtualTextFile] = this.cache,
        bypassLinkingErrors: Boolean = this.bypassLinkingErrors,
        checkIR: Boolean = this.checkIR,
        unCache: Boolean = this.unCache,
        disableOptimizer: Boolean = this.disableOptimizer,
        batchMode: Boolean = this.batchMode,
        wantSourceMap: Boolean = this.wantSourceMap,
        prettyPrint: Boolean = this.prettyPrint,
        relativizeSourceMapBase: Option[URI] = this.relativizeSourceMapBase,
        noWarnMissing: Seq[ScalaJSOptimizer.NoWarnMissing] = this.noWarnMissing,
        customOutputWrapper: (String, String) = this.customOutputWrapper): Config = {

      new Config(output, cache, bypassLinkingErrors, checkIR, unCache,
          disableOptimizer, batchMode, wantSourceMap, prettyPrint,
          relativizeSourceMapBase, noWarnMissing, customOutputWrapper)
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

    private def productArray: Array[Any] = {
      Array[Any](output, cache, bypassLinkingErrors, checkIR, unCache,
          disableOptimizer, batchMode, wantSourceMap, prettyPrint,
          relativizeSourceMapBase, noWarnMissing, customOutputWrapper)
    }

    // For binary compatibility
    override def equals(other: Any): Boolean = super.equals(other)

    // For binary compatibility
    override def hashCode(): Int = super.hashCode()

    // For binary compatibility
    override def toString(): String =
      productArray.mkString("Config(", ", ", ")")
  }

  object Config extends runtime.AbstractFunction11[WritableVirtualJSFile,
      Option[WritableVirtualTextFile], Boolean, Boolean, Boolean,
      Boolean, Boolean, Boolean, Boolean, Option[URI],
      Seq[ScalaJSOptimizer.NoWarnMissing], Config] {

    def apply(output: WritableVirtualJSFile): Config = {
      new Config(
          output = output,
          cache = None,
          bypassLinkingErrors = false,
          checkIR = false,
          unCache = true,
          disableOptimizer = false,
          batchMode = false,
          wantSourceMap = false,
          prettyPrint = false,
          relativizeSourceMapBase = None,
          noWarnMissing = Nil,
          customOutputWrapper = ("", ""))
    }

    // For binary compatibility
    @deprecated("Use Config(output) and .withXYZ() methods", "0.6.5")
    def apply(
        output: WritableVirtualJSFile,
        cache: Option[WritableVirtualTextFile] = None,
        bypassLinkingErrors: Boolean = false,
        checkIR: Boolean = false,
        unCache: Boolean = true,
        disableOptimizer: Boolean = false,
        batchMode: Boolean = false,
        wantSourceMap: Boolean = false,
        prettyPrint: Boolean = false,
        relativizeSourceMapBase: Option[URI] = None,
        noWarnMissing: Seq[ScalaJSOptimizer.NoWarnMissing] = Nil): Config = {

      new Config(output, cache, bypassLinkingErrors, checkIR, unCache,
          disableOptimizer, batchMode, wantSourceMap, prettyPrint,
          relativizeSourceMapBase, noWarnMissing, customOutputWrapper = ("", ""))
    }

    // For binary compatibility
    @deprecated("Not a case class anymore", "0.6.5")
    def unapply(config: Config): Option[(WritableVirtualJSFile,
        Option[WritableVirtualTextFile], Boolean, Boolean, Boolean,
        Boolean, Boolean, Boolean, Boolean, Option[URI],
        Seq[ScalaJSOptimizer.NoWarnMissing])] = {

      Some((config.output, config.cache, config.bypassLinkingErrors,
          config.checkIR, config.unCache, config.disableOptimizer,
          config.batchMode, config.wantSourceMap, config.prettyPrint,
          config.relativizeSourceMapBase, config.noWarnMissing))
    }
  }

  /** Minimal set of externs to compile Scala.js-emitted code with Closure. */
  val ScalaJSExterns = """
    /** @constructor */
    function Object() {}
    Object.protoype.toString = function() {};
    /** @constructor */
    function Array() {}
    Array.prototype.length = 0;
    /** @constructor */
    function Function() {}
    Function.prototype.constructor = function() {};
    Function.prototype.call = function() {};
    Function.prototype.apply = function() {};
    var global = {};
    var __ScalaJSEnv = {};
    var NaN = 0.0/0.0, Infinity = 1.0/0.0, undefined = void 0;
    """

  val ScalaJSExternsFile = new MemVirtualJSFile("ScalaJSExterns.js").
    withContent(ScalaJSExterns)

}
