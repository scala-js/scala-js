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

package org.scalajs.linker.backend.closure

import scala.collection.JavaConverters._

import java.io._
import java.net.URI
import java.nio.charset.StandardCharsets

import com.google.javascript.jscomp.{
  SourceFile => ClosureSource,
  Compiler => ClosureCompiler,
  CompilerOptions => ClosureOptions,
  _
}

import org.scalajs.io._
import org.scalajs.logging.Logger

import org.scalajs.linker._
import org.scalajs.linker.standard._
import org.scalajs.linker.backend._
import org.scalajs.linker.backend.emitter.Emitter

/** The Closure backend of the Scala.js linker.
 *
 *  Runs a the Google Closure Compiler in advanced mode on the emitted code.
 *  Use this for production builds.
 */
final class ClosureLinkerBackend(config: LinkerBackendImpl.Config)
    extends LinkerBackendImpl(config) {

  import config.commonConfig.coreSpec._

  require(!esFeatures.useECMAScript2015,
      s"Cannot use features $esFeatures with the Closure Compiler" +
      "because they contain ECMAScript 2015 features")

  require(!esFeatures.allowBigIntsForLongs,
      s"Cannot use features $esFeatures with the Closure Compiler" +
      "because they allow to use BigInts")

  private[this] val emitter = {
    new Emitter(config.commonConfig)
      .withOptimizeBracketSelects(false)
  }

  val symbolRequirements: SymbolRequirement = emitter.symbolRequirements

  private val needsIIFEWrapper = moduleKind match {
    case ModuleKind.NoModule       => true
    case ModuleKind.CommonJSModule => false
  }

  /** Emit the given [[standard.LinkingUnit LinkingUnit]] to the target output.
   *
   *  @param unit [[standard.LinkingUnit LinkingUnit]] to emit
   *  @param output File to write to
   */
  def emit(unit: LinkingUnit, output: LinkerOutput, logger: Logger): Unit = {
    verifyUnit(unit)

    // Build Closure IR
    val module = logger.time("Emitter (create Closure trees)") {
      val builder = new ClosureModuleBuilder(config.relativizeSourceMapBase)
      emitter.emitForClosure(unit, builder, logger)
      builder.result()
    }

    // Compile the module
    val closureExterns = List(
        ClosureSource.fromCode("ScalaJSExterns.js", ClosureLinkerBackend.ScalaJSExterns),
        ClosureSource.fromCode("ScalaJSExportExterns.js", makeExternsForExports(unit)))
    val options = closureOptions(output)
    val compiler = closureCompiler(logger)

    val result = logger.time("Closure: Compiler pass") {
      compiler.compileModules(
          closureExterns.asJava, List(module).asJava, options)
    }

    logger.time("Closure: Write result") {
      writeResult(result, compiler, output)
    }
  }

  /** Constructs an externs file listing all exported properties in a linking
   *  unit.
   *
   *  This is necessary to avoid name clashes with renamed properties (#2491).
   */
  private def makeExternsForExports(linkingUnit: LinkingUnit): String = {
    import org.scalajs.ir.Trees._

    def exportName(memberDef: MemberDef): Option[String] = memberDef match {
      case MethodDef(_, StringLiteral(name), _, _, _) => Some(name)
      case PropertyDef(_, StringLiteral(name), _, _)  => Some(name)
      case _                                          => None
    }

    val exportedPropertyNames = for {
      classDef <- linkingUnit.classDefs
      member <- classDef.exportedMembers
      name <- exportName(member.value)
      if isValidIdentifier(name)
    } yield {
      name
    }

    val content = new java.lang.StringBuilder
    for (exportedPropertyName <- exportedPropertyNames.distinct)
      content.append(s"Object.prototype.$exportedPropertyName = 0;\n")

    content.toString()
  }

  private def closureCompiler(logger: Logger) = {
    val compiler = new ClosureCompiler
    compiler.setErrorManager(new LoggerErrorManager(logger))
    compiler
  }

  private def writeResult(result: Result, compiler: ClosureCompiler,
      output: LinkerOutput): Unit = {

    def ifIIFE(str: String): String = if (needsIIFEWrapper) str else ""

    val header = ifIIFE("(function(){") + "'use strict';\n"
    val footer = ifIIFE("}).call(this);\n")

    val outputContent =
      if (result.errors.nonEmpty) "// errors while producing source\n"
      else compiler.toSource + "\n"

    val sourceMap = Option(compiler.getSourceMap())

    def writer(out: OutputStream) =
      new OutputStreamWriter(out, StandardCharsets.UTF_8)

    // Write optimized code
    val w = writer(output.jsFile.outputStream)
    try {
      w.write(header)
      w.write(outputContent)
      w.write(footer)
      output.sourceMapURI.foreach(uri =>
          w.write("//# sourceMappingURL=" + uri.toASCIIString + "\n"))
    } finally {
      w.close()
    }

    // Write source map (if available)
    for {
      sm  <- sourceMap
      smf <- output.sourceMap
    } yield {
      sm.setWrapperPrefix(header)
      val w = writer(smf.outputStream)
      try sm.appendTo(w, output.jsFileURI.fold("")(_.toASCIIString))
      finally w.close()
    }
  }

  private def closureOptions(output: LinkerOutput) = {
    val options = new ClosureOptions
    options.prettyPrint = config.prettyPrint
    CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(options)
    options.setLanguageIn(ClosureOptions.LanguageMode.ECMASCRIPT5)
    options.setCheckGlobalThisLevel(CheckLevel.OFF)

    if (config.sourceMap && output.sourceMap.isDefined) {
      options.setSourceMapDetailLevel(SourceMap.DetailLevel.ALL)
      output.sourceMapURI.foreach(uri =>
        options.setSourceMapOutputPath(uri.toASCIIString))
    }

    options
  }
}

private object ClosureLinkerBackend {
  /** Minimal set of externs to compile Scala.js-emitted code with Closure. */
  private val ScalaJSExterns = """
    /** @constructor */
    function Object() {}
    Object.prototype.toString = function() {};
    Object.prototype.$classData = {};
    /** @constructor */
    function Array() {}
    Array.prototype.length = 0;
    /** @constructor */
    function Function() {}
    Function.prototype.constructor = function() {};
    Function.prototype.call = function() {};
    Function.prototype.apply = function() {};
    function require() {}
    var global = {};
    var exports = {};
    var NaN = 0.0/0.0, Infinity = 1.0/0.0, undefined = void 0;
    """
}
