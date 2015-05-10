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
    optimizer.outputMode match {
      case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
        optimizeIRUsingClosure(optimizer, irFiles, cfg, logger)
      case OutputMode.ECMAScript6 | OutputMode.ECMAScript6StrongMode =>
        optimizer.optimizeIRInternal(irFiles, cfg, cfg.output,
            cfg.relativizeSourceMapBase, logger)
    }
  }

  private def optimizeIRUsingClosure(optimizer: ScalaJSOptimizer,
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
      writeResult(result, compiler, cfg.output)
    }
  }

  private def writeResult(result: Result, compiler: ClosureCompiler,
      output: WritableVirtualJSFile): Unit = {

    val outputContent = if (result.errors.nonEmpty) ""
    else "(function(){'use strict';" + compiler.toSource + "}).call(this);\n"

    val sourceMap = Option(compiler.getSourceMap())

    // Write optimized code
    val w = output.contentWriter
    try {
      w.write(outputContent)
      if (sourceMap.isDefined)
        w.write("//# sourceMappingURL=" + output.name + ".map\n")
    } finally w.close()

    // Write source map (if available)
    sourceMap.foreach { sm =>
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
  final case class Config(
      /** Writer for the output */
      output: WritableVirtualJSFile,
      /** Cache file */
      cache: Option[WritableVirtualTextFile] = None,
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
      /** Ask to produce source map for the output */
      wantSourceMap: Boolean = false,
      /** Pretty-print the output. */
      prettyPrint: Boolean = false,
      /** Base path to relativize paths in the source map */
      relativizeSourceMapBase: Option[URI] = None,
      /** Elements we won't warn even if they don't exist */
      noWarnMissing: Seq[ScalaJSOptimizer.NoWarnMissing] = Nil
  ) extends OptimizerConfig with ScalaJSOptimizer.OptimizerConfig

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
