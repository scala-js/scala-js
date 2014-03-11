package scala.scalajs.sbtplugin.optimizer

import scala.scalajs.sbtplugin

import sbt.{File => _, _}

import scala.scalajs.sbtplugin.FileSystem

import com.google.javascript.jscomp.{
  SourceFile => ClosureSource,
  Compiler => ClosureCompiler,
  CompilerOptions => ClosureOptions,
  _
}
import scala.collection.JavaConverters._

/** Scala.js Closure optimizer: does advanced optimizations with Closure. */
class ScalaJSClosureOptimizer[FS <: FileSystem](val fs: FS, logger: Logger) {
  import ScalaJSClosureOptimizer._
  import fs.{File, fileOps}

  private def toClosureSource(file: File) =
    ClosureSource.fromInputStream(file.name, file.input)

  def optimize(inputs: Seq[File], output: File,
      additionalExterns: Seq[File] = Nil,
      prettyPrint: Boolean = false): Unit = {
    val closureSources = inputs map toClosureSource
    val closureExterns = (
        ClosureSource.fromCode("ScalaJSExterns.js", ScalaJSExterns) +:
        additionalExterns.map(toClosureSource))

    val options = new ClosureOptions
    options.prettyPrint = prettyPrint
    CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(options)
    options.setLanguageIn(ClosureOptions.LanguageMode.ECMASCRIPT5)

    val compiler = new ClosureCompiler
    val result = compiler.compile(
        closureExterns.asJava, closureSources.asJava, options)

    val errors = result.errors.toList
    val warnings = result.warnings.toList

    if (!errors.isEmpty) {
      logger.error(errors.length + " Closure errors:")
      errors.foreach(err => logger.error(err.toString))

      writeToFile(output, "")
    } else {
      if (!warnings.isEmpty) {
        logger.warn(warnings.length + " Closure warnings:")
        warnings.foreach(err => logger.warn(err.toString))
      }

      writeToFile(output,
          "(function(){'use strict';" +
          compiler.toSource +
          "}).call(this);\n")
    }
  }

  private def writeToFile(file: File, content: String): Unit = {
    val stream = new java.io.PrintStream(file.bufferedOutput)
    try {
      stream.append(content)
    } finally {
      stream.close()
    }
  }
}

object ScalaJSClosureOptimizer {
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
}
