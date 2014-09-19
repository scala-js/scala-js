/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.optimizer

import scala.scalajs.tools.classpath._
import scala.scalajs.tools.logging._
import scala.scalajs.tools.io._
import scala.scalajs.tools.corelib.CoreJSLibs

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

  private def toClosureSource(file: VirtualJSFile) =
    ClosureSource.fromReader(file.toURI.toString(), file.reader)

  private def toClosureInput(file: VirtualJSFile) =
    new CompilerInput(toClosureSource(file))

  /**
   * runs closure on CIJS content
   * - Maintains order
   * - Only NCJS in result
   */
  def optimizeCP(inputs: Inputs[CompleteCIClasspath],
      outCfg: OutputConfig, logger: Logger): CompleteNCClasspath = {

    val cp = inputs.input

    CacheUtils.cached(cp.version, outCfg.output, outCfg.cache) {
      logger.info(s"Optimizing ${outCfg.output.path}")
      optimizeFiles(inputs.copy(input = cp.cijsCode), outCfg, logger)
    }

    new CompleteNCClasspath(cp.jsLibs, outCfg.output :: Nil,
        cp.requiresDOM, cp.version)
  }


  /** Directly optimizes an IRClasspath by asking the ScalaJSOptimizer to
   *  emit a closure AST rather than a file and then compiling this AST directly
   */
  def directOptimizeCP(optimizer: ScalaJSOptimizer,
      inputs: Inputs[ScalaJSOptimizer.Inputs[CompleteIRClasspath]],
      outCfg: DirectOutputConfig, logger: Logger): CompleteNCClasspath = {

    val cp = inputs.input.input

    CacheUtils.cached(cp.version, outCfg.output, outCfg.cache) {
      logger.info(s"Direct Optimizing ${outCfg.output.path}")

      val irFastOptInput =
        inputs.input.copy(input = inputs.input.input.scalaJSIR)
      val irFullOptInput = inputs.copy(input = irFastOptInput)

      directOptimizeIR(optimizer, irFullOptInput, outCfg, logger)
    }

    new CompleteNCClasspath(cp.jsLibs, outCfg.output :: Nil,
        cp.requiresDOM, cp.version)
  }

  def directOptimizeIR(optimizer: ScalaJSOptimizer,
      inputs: Inputs[ScalaJSOptimizer.Inputs[Traversable[VirtualScalaJSIRFile]]],
      outCfg: DirectOutputConfig, logger: Logger): Unit = {

    // Build Closure IR via ScalaJSOptimizer
    val builder = new ClosureAstBuilder(outCfg.relativizeSourceMapBase)

    optimizer.optimizeIR(inputs.input, outCfg, builder, logger)

    // Build a Closure JSModule which includes the core libs
    val module = new JSModule("Scala.js")

    for (lib <- CoreJSLibs.libs)
      module.add(toClosureInput(lib))

    val ast = builder.closureAST
    module.add(new CompilerInput(ast, ast.getInputId(), false))

    for (export <- inputs.additionalExports)
      module.add(toClosureInput(export))

    // Compile the module
    val closureExterns =
      (ScalaJSExternsFile +: inputs.additionalExterns).map(toClosureSource)

    val options = closureOptions(outCfg)
    val compiler = closureCompiler(logger)

    val result = GenIncOptimizer.logTime(logger, "Closure Compiler pass") {
      compiler.compileModules(
          closureExterns.asJava, List(module).asJava, options)
    }

    GenIncOptimizer.logTime(logger, "Write Closure result") {
      writeResult(result, compiler, outCfg.output)
    }
  }

  def optimizeFiles(inputs: Inputs[Seq[VirtualJSFile]],
      outputConfig: OutputConfig, logger: Logger): Unit = {
    val closureExterns =
      (ScalaJSExternsFile +: inputs.additionalExterns).map(toClosureSource)
    val closureSources =
      (inputs.input ++ inputs.additionalExports).map(toClosureSource)

    val options = closureOptions(outputConfig, noSourceMap = true)
    val compiler = closureCompiler(logger)

    val result = GenIncOptimizer.logTime(logger, "Closure Compiler pass") {
      compiler.compile(
          closureExterns.asJava, closureSources.asJava, options)
    }

    GenIncOptimizer.logTime(logger, "Write Closure result") {
      writeResult(result, compiler, outputConfig.output)
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

  private def closureOptions(outputConfig: AbstractOutputConfig,
      noSourceMap: Boolean = false) = {

    val options = new ClosureOptions
    options.prettyPrint = outputConfig.prettyPrint
    CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(options)
    options.setLanguageIn(ClosureOptions.LanguageMode.ECMASCRIPT5)
    options.setCheckGlobalThisLevel(CheckLevel.OFF)

    if (!noSourceMap && outputConfig.wantSourceMap) {
      options.setSourceMapOutputPath(outputConfig.output.name + ".map")
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
  /** Inputs of the Scala.js Closure optimizer. */
  final case class Inputs[T](
      /** Input to optimize (classpath or file-list) */
      input: T,
      /** Additional externs files to be given to Closure. */
      additionalExterns: Seq[VirtualJSFile] = Nil,
      /** Additional exports to be given to Closure.
       *  These files are just appended to the classpath, given to Closure,
       *  but not used in the Scala.js optimizer pass when direct optimizing
       */
      additionalExports: Seq[VirtualJSFile] = Nil
  )

  /** Configuration the closure part of the optimizer needs.
   *  See [[OutputConfig]] for a description of the fields.
   */
  trait AbstractOutputConfig {
    val output: WritableVirtualJSFile
    val cache: Option[WritableVirtualTextFile]
    val wantSourceMap: Boolean
    val prettyPrint: Boolean
    val relativizeSourceMapBase: Option[URI]
  }

  /** Configuration for the output of the Scala.js Closure optimizer when
   *  optimizing from a JavaScript file
   */
  final case class OutputConfig(
      /** Writer for the output */
      output: WritableVirtualJSFile,
      /** Cache file */
      cache: Option[WritableVirtualTextFile] = None,
      /** Pretty-print the output. */
      prettyPrint: Boolean = false
  ) extends AbstractOutputConfig {
    val wantSourceMap: Boolean = false
    val relativizeSourceMapBase: Option[URI] = None
  }

  /** Configuration for the output of the Scala.js Closure optimizer when
   *  optimizing directly from Scala.js IR emitted by a [[ScalaJSOptimizer]]
   */
  final case class DirectOutputConfig(
      /** Writer for the output */
      output: WritableVirtualJSFile,
      /** Cache file */
      cache: Option[WritableVirtualTextFile] = None,
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
      relativizeSourceMapBase: Option[URI] = None
  ) extends AbstractOutputConfig with ScalaJSOptimizer.OptimizerConfig

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
    """

  val ScalaJSExternsFile = new MemVirtualJSFile("ScalaJSExterns.js").
    withContent(ScalaJSExterns)

}
