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

import com.google.javascript.jscomp.{
  SourceFile => ClosureSource,
  Compiler => ClosureCompiler,
  CompilerOptions => ClosureOptions,
  _
}
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

import java.net.URI

/** Scala.js Closure optimizer: does advanced optimizations with Closure. */
class ScalaJSClosureOptimizer {
  import ScalaJSClosureOptimizer._

  private def toClosureSource(file: VirtualJSFile) =
    ClosureSource.fromCode(file.name, file.content)

  /**
   * runs closure on CIJS content
   * - Maintains order
   * - Only NCJS in result
   */
  def optimizeCP(inputs: Inputs[CompleteCIClasspath],
      outCfg: OutputConfig, logger: Logger): CompleteNCClasspath = {

    val cp = inputs.input

    CacheUtils.cached(cp.version, outCfg.cache) {
      logger.info(s"Optimizing ${outCfg.output.path}")
      optimizeFiles(inputs.copy(input = cp.cijsCode), outCfg, logger)
    }

    new CompleteNCClasspath(cp.jsLibs, outCfg.output :: Nil, cp.version)
  }

  def optimizeFiles(inputs: Inputs[Seq[VirtualJSFile]],
      outputConfig: OutputConfig, logger: Logger): Unit = {
    val closureSources = inputs.input map toClosureSource
    val closureExterns =
      (ScalaJSExternsFile +: inputs.additionalExterns).map(toClosureSource)

    val options = new ClosureOptions
    options.prettyPrint = outputConfig.prettyPrint
    CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(options)
    options.setLanguageIn(ClosureOptions.LanguageMode.ECMASCRIPT5)
    options.setCheckGlobalThisLevel(CheckLevel.OFF)

    val compiler = new ClosureCompiler
    compiler.setErrorManager(new LoggerErrorManager(logger))

    val result = compiler.compile(
        closureExterns.asJava, closureSources.asJava, options)

    val outputContent = if (result.errors.nonEmpty) ""
    else "(function(){'use strict';" + compiler.toSource + "}).call(this);\n"

    // Write optimized code
    val w = outputConfig.output.contentWriter
    try w.write(outputContent)
    finally w.close()

    // don't close writer. calling code has ownership
  }
}

object ScalaJSClosureOptimizer {
  /** Inputs of the Scala.js Closure optimizer. */
  final case class Inputs[T](
      /** Input to optimize (classpath or file-list) */
      input: T,
      /** Additional externs files to be given to Closure. */
      additionalExterns: Seq[VirtualJSFile] = Nil
  )

  /** Configuration for the output of the Scala.js Closure optimizer. */
  final case class OutputConfig(
      /** Writer for the output */
      output: WritableVirtualJSFile,
      /** Cache file */
      cache: Option[WritableVirtualTextFile] = None,
      /** Ask to produce source map for the output (currently ignored). */
      wantSourceMap: Boolean = false,
      /** Pretty-print the output. */
      prettyPrint: Boolean = false,
      /** Base path to relativize paths in the source map (currently ignored) */
      relativizeSourceMapBase: Option[URI] = None
  )

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
    var __ScalaJSExportsNamespace = {};
    """

  val ScalaJSExternsFile = new MemVirtualJSFile("ScalaJSExterns.js").
    withContent(ScalaJSExterns)

}
