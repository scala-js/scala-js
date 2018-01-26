/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.linker.backend.closure

import scala.collection.JavaConverters._

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

  private def toClosureSource(file: VirtualJSFile) =
    ClosureSource.fromReader(file.toURI.toString(), file.reader)

  /** Emit the given [[standard.LinkingUnit LinkingUnit]] to the target output.
   *
   *  @param unit [[standard.LinkingUnit LinkingUnit]] to emit
   *  @param output File to write to
   */
  def emit(unit: LinkingUnit, output: WritableVirtualJSFile,
      logger: Logger): Unit = {
    verifyUnit(unit)

    val builder = new ClosureAstBuilder(config.relativizeSourceMapBase)

    // Build Closure IR
    val coreJSLib = logger.time("Emitter (create Closure trees)") {
      emitter.emitForClosure(unit, builder, logger)
    }

    // Build a Closure JSModule which includes the core libs
    val module = new JSModule("Scala.js")

    module.add(new CompilerInput(toClosureSource(coreJSLib)))

    val ast = builder.closureAST
    module.add(new CompilerInput(ast, ast.getInputId(), false))

    // Compile the module
    val closureExterns = List(
        toClosureSource(ClosureLinkerBackend.ScalaJSExternsFile),
        toClosureSource(makeExternsForExports(unit)))
    val options = closureOptions(output.name)
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
  private def makeExternsForExports(linkingUnit: LinkingUnit): VirtualJSFile = {
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

    new MemVirtualJSFile("ScalaJSExportExterns.js")
      .withContent(content.toString())
  }

  private def closureCompiler(logger: Logger) = {
    val compiler = new ClosureCompiler
    compiler.setErrorManager(new LoggerErrorManager(logger))
    compiler
  }

  private def writeResult(result: Result, compiler: ClosureCompiler,
      output: WritableVirtualJSFile): Unit = {

    def ifIIFE(str: String): String = if (needsIIFEWrapper) str else ""

    val header = ifIIFE("(function(){") + "'use strict';\n"
    val footer = ifIIFE("}).call(this);\n")

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

  private def closureOptions(outputName: String) = {
    val options = new ClosureOptions
    options.prettyPrint = config.prettyPrint
    CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(options)
    options.setLanguageIn(ClosureOptions.LanguageMode.ECMASCRIPT5)
    options.setCheckGlobalThisLevel(CheckLevel.OFF)

    if (config.sourceMap) {
      options.setSourceMapOutputPath(outputName + ".map")
      options.setSourceMapDetailLevel(SourceMap.DetailLevel.ALL)
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

  private val ScalaJSExternsFile = new MemVirtualJSFile("ScalaJSExterns.js").
    withContent(ScalaJSExterns)
}
