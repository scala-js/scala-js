/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.optimizer

import scala.scalajs.tools.logging._
import scala.scalajs.tools.io._

import scala.scalajs.tools.classpath.ScalaJSPackedClasspath.packOrderLine

import com.google.javascript.jscomp.{
  SourceFile => ClosureSource,
  Compiler => ClosureCompiler,
  CompilerOptions => ClosureOptions,
  _
}
import scala.collection.JavaConverters._

import java.net.URI

/** Scala.js Closure optimizer: does advanced optimizations with Closure. */
class ScalaJSClosureOptimizer {
  import ScalaJSClosureOptimizer._

  private def toClosureSource(file: VirtualJSFile) =
    ClosureSource.fromCode(file.name, file.content)

  def optimize(inputs: Inputs, outputConfig: OutputConfig,
      logger: Logger): Unit = {
    val closureSources = inputs.sources map toClosureSource
    val closureExterns =
      (ScalaJSExternsFile +: inputs.additionalExterns).map(toClosureSource)

    val options = new ClosureOptions
    options.prettyPrint = outputConfig.prettyPrint
    CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(options)
    options.setLanguageIn(ClosureOptions.LanguageMode.ECMASCRIPT5)
    options.setCheckGlobalThisLevel(CheckLevel.OFF)

    val compiler = new ClosureCompiler
    val result = compiler.compile(
        closureExterns.asJava, closureSources.asJava, options)

    val errors = result.errors.toList
    val warnings = result.warnings.toList

    val outputContent = if (!errors.isEmpty) {
      logger.error(errors.length + " Closure errors:")
      errors.foreach(err => logger.error(err.toString))
      ""
    } else {
      if (!warnings.isEmpty) {
        logger.warn(warnings.length + " Closure warnings:")
        warnings.foreach(err => logger.warn(err.toString))
      }
      "(function(){'use strict';" + compiler.toSource + "}).call(this);\n"
    }

    val writer = outputConfig.writer.contentWriter
    // Write out pack order line (constant: file is stand alone)
    writer.write(packOrderLine(0))
    writer.write('\n')

    // Write optimized code
    writer.write(outputContent)

    // don't close writer. calling code has ownership
  }
}

object ScalaJSClosureOptimizer {
  /** Inputs of the Scala.js Closure optimizer. */
  final case class Inputs(
      /** Sources to be fed to Closure. */
      sources: Seq[VirtualJSFile],
      /** Additional externs files to be given to Closure. */
      additionalExterns: Seq[VirtualJSFile] = Nil
  )

  /** Configuration for the output of the Scala.js Closure optimizer. */
  final case class OutputConfig(
      /** Name of the output file. (used to refer to sourcemaps) */
      name: String,
      /** Writer for the output */
      writer: VirtualJSFileWriter,
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
