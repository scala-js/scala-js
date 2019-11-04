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

import scala.concurrent._

import java.io._
import java.net.URI
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import com.google.javascript.jscomp.{
  SourceFile => ClosureSource,
  Compiler => ClosureCompiler,
  CompilerOptions => ClosureOptions,
  _
}

import org.scalajs.logging.Logger

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable.{IRFileImpl, OutputFileImpl}
import org.scalajs.linker.backend._
import org.scalajs.linker.backend.emitter.Emitter
import org.scalajs.linker.standard._

/** The Closure backend of the Scala.js linker.
 *
 *  Runs a the Google Closure Compiler in advanced mode on the emitted code.
 *  Use this for production builds.
 */
final class ClosureLinkerBackend(config: LinkerBackendImpl.Config)
    extends LinkerBackendImpl(config) {

  import config.commonConfig.coreSpec._

  require(!esFeatures.allowBigIntsForLongs,
      s"Cannot use features $esFeatures with the Closure Compiler " +
      "because they allow to use BigInts")

  require(moduleKind != ModuleKind.ESModule,
      s"Cannot use module kind $moduleKind with the Closure Compiler")

  private[this] val emitter = {
    new Emitter(config.commonConfig)
      .withOptimizeBracketSelects(false)
      .withTrackAllGlobalRefs(true)
  }

  val symbolRequirements: SymbolRequirement = emitter.symbolRequirements

  private val needsIIFEWrapper = moduleKind match {
    case ModuleKind.NoModule                             => true
    case ModuleKind.ESModule | ModuleKind.CommonJSModule => false
  }

  /** Emit the given [[standard.LinkingUnit LinkingUnit]] to the target output.
   *
   *  @param unit [[standard.LinkingUnit LinkingUnit]] to emit
   *  @param output File to write to
   */
  def emit(unit: LinkingUnit, output: LinkerOutput, logger: Logger)(
      implicit ec: ExecutionContext): Future[Unit] = {
    Future(compile(unit, output, logger)).flatMap { case (topLevelVarDeclarations, code, sourceMap) =>
      logger.timeFuture("Closure: Write result") {
        writeResult(topLevelVarDeclarations, code, sourceMap, output)
      }
    }
  }

  private def compile(unit: LinkingUnit, output: LinkerOutput, logger: Logger) = {
    verifyUnit(unit)

    // Build Closure IR
    val (topLevelVarDeclarations, globalRefs, module) = {
      logger.time("Closure: Emitter (create Closure trees)") {
        val builder = new ClosureModuleBuilder(config.relativizeSourceMapBase)
        val (topLevelVarDeclarations, globalRefs) =
          emitter.emitForClosure(unit, builder, logger)
        (topLevelVarDeclarations, globalRefs, builder.result())
      }
    }

    // Compile the module
    val closureExterns = java.util.Arrays.asList(
        ClosureSource.fromCode("ScalaJSExterns.js", ClosureLinkerBackend.ScalaJSExterns),
        ClosureSource.fromCode("ScalaJSGlobalRefs.js", makeExternsForGlobalRefs(globalRefs)),
        ClosureSource.fromCode("ScalaJSExportExterns.js", makeExternsForExports(topLevelVarDeclarations, unit)))
    val options = closureOptions(output)
    val compiler = closureCompiler(logger)

    val result = logger.time("Closure: Compiler pass") {
      compiler.compileModules(
          closureExterns, java.util.Arrays.asList(module), options)
    }

    if (!result.success) {
      throw new LinkingException(
          "There were errors when applying the Google Closure Compiler")
    }

    (topLevelVarDeclarations, compiler.toSource + "\n", Option(compiler.getSourceMap()))
  }

  /** Constructs an externs file listing all the global refs.
   */
  private def makeExternsForGlobalRefs(globalRefs: Set[String]): String =
    globalRefs.map("var " + _ + ";\n").mkString

  /** Constructs an externs file listing all exported properties in a linking
   *  unit.
   *
   *  This is necessary to avoid name clashes with renamed properties (#2491).
   */
  private def makeExternsForExports(topLevelVarDeclarations: List[String],
      linkingUnit: LinkingUnit): String = {
    import org.scalajs.ir.Trees._
    import org.scalajs.linker.backend.javascript.Trees.Ident.isValidJSIdentifierName

    def exportName(memberDef: MemberDef): Option[String] = memberDef match {
      case JSMethodDef(_, StringLiteral(name), _, _)   => Some(name)
      case JSPropertyDef(_, StringLiteral(name), _, _) => Some(name)
      case _                                           => None
    }

    val exportedPropertyNames = for {
      classDef <- linkingUnit.classDefs
      member <- classDef.exportedMembers
      name <- exportName(member.value)
      if isValidJSIdentifierName(name)
    } yield {
      name
    }

    val content = new java.lang.StringBuilder
    for (topLevelVarDecl <- topLevelVarDeclarations)
      content.append(s"var $topLevelVarDecl;\n")
    for (exportedPropertyName <- exportedPropertyNames.distinct)
      content.append(s"Object.prototype.$exportedPropertyName = 0;\n")

    content.toString()
  }

  private def closureCompiler(logger: Logger) = {
    import com.google.common.collect.ImmutableSet

    val compiler = new ClosureCompiler
    compiler.setErrorManager(new SortingErrorManager(ImmutableSet.of(
        new LoggerErrorReportGenerator(logger))))
    compiler
  }

  private def writeResult(topLevelVarDeclarations: List[String],
      outputContent: String, sourceMap: Option[SourceMap], output: LinkerOutput)(
      implicit ec: ExecutionContext): Future[Unit] = {

    def ifIIFE(str: String): String = if (needsIIFEWrapper) str else ""

    val header = {
      val maybeTopLevelVarDecls = if (topLevelVarDeclarations.nonEmpty) {
        val kw = if (esFeatures.useECMAScript2015) "let " else "var "
        topLevelVarDeclarations.mkString(kw, ",", ";\n")
      } else {
        ""
      }
      maybeTopLevelVarDecls + ifIIFE("(function(){") + "'use strict';\n"
    }
    val footer = ifIIFE("}).call(this);\n")

    def writeToFile(file: LinkerOutput.File)(content: Writer => Unit): Future[Unit] = {
      val out = new ByteArrayOutputStream()
      val writer = new OutputStreamWriter(out, StandardCharsets.UTF_8)
      try content(writer)
      finally writer.close()

      OutputFileImpl.fromOutputFile(file)
        .writeFull(ByteBuffer.wrap(out.toByteArray))
    }

    // Write optimized code
    val codeWritten = writeToFile(output.jsFile) { w =>
      w.write(header)
      w.write(outputContent)
      w.write(footer)
      output.sourceMapURI.foreach(uri =>
          w.write("//# sourceMappingURL=" + uri.toASCIIString + "\n"))
    }

    // Write source map (if available)
    val smWritten = for {
      sm  <- sourceMap
      smf <- output.sourceMap
    } yield {
      sm.setWrapperPrefix(header)
      writeToFile(smf) { w =>
        sm.appendTo(w, output.jsFileURI.fold("")(_.toASCIIString))
      }
    }

    smWritten.fold(codeWritten)(_.flatMap(_ => codeWritten))
  }

  private def closureOptions(output: LinkerOutput) = {
    val options = new ClosureOptions
    options.setPrettyPrint(config.prettyPrint)
    CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(options)

    val language =
      if (esFeatures.useECMAScript2015) ClosureOptions.LanguageMode.ECMASCRIPT_2015
      else ClosureOptions.LanguageMode.ECMASCRIPT5_STRICT
    options.setLanguageIn(language)
    options.setLanguageOut(language)

    options.setCheckGlobalThisLevel(CheckLevel.OFF)
    options.setWarningLevel(DiagnosticGroups.DUPLICATE_VARS, CheckLevel.OFF)
    options.setWarningLevel(DiagnosticGroups.CHECK_REGEXP, CheckLevel.OFF)
    options.setWarningLevel(DiagnosticGroups.CHECK_TYPES, CheckLevel.OFF)
    options.setWarningLevel(DiagnosticGroups.CHECK_USELESS_CODE, CheckLevel.OFF)

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
    var Object;
    Object.prototype.constructor;
    Object.prototype.toString;
    Object.prototype.$classData;
    var Array;
    Array.prototype.length;
    var Function;
    Function.prototype.call;
    Function.prototype.apply;
    var require;
    var exports;
    var NaN = 0.0/0.0, Infinity = 1.0/0.0, undefined = void 0;
    """
}
