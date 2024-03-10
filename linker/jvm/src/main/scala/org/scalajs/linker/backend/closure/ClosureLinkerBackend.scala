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

import java.io.{ByteArrayOutputStream, Writer}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.util.{Arrays, HashSet}

import com.google.javascript.jscomp.{
  SourceFile => ClosureSource,
  Compiler => ClosureCompiler,
  CompilerOptions => ClosureOptions,
  _
}

import org.scalajs.logging.Logger

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable.OutputPatternsImpl
import org.scalajs.linker.backend._
import org.scalajs.linker.backend.emitter.Emitter
import org.scalajs.linker.backend.javascript.{Trees => js}
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

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
    // Note that we do not transfer `minify` -- Closure will do its own thing anyway
    val emitterConfig = Emitter.Config(config.commonConfig.coreSpec)
      .withJSHeader(config.jsHeader)
      .withOptimizeBracketSelects(false)
      .withTrackAllGlobalRefs(true)
      .withInternalModulePattern(m => OutputPatternsImpl.moduleName(config.outputPatterns, m.id))

    // Do not apply ClosureAstTransformer eagerly:
    // The ASTs used by closure are highly mutable, so re-using them is non-trivial.
    // Since closure is slow anyways, we haven't built the optimization.
    val postTransformer = Emitter.PostTransformer.Identity

    new Emitter(emitterConfig, postTransformer)
  }

  val symbolRequirements: SymbolRequirement = emitter.symbolRequirements

  override def injectedIRFiles: Seq[IRFile] = emitter.injectedIRFiles

  private val languageMode: ClosureOptions.LanguageMode = {
    import ClosureOptions.LanguageMode._

    esFeatures.esVersion match {
      case ESVersion.ES5_1  => ECMASCRIPT5_STRICT
      case ESVersion.ES2015 => ECMASCRIPT_2015
      case ESVersion.ES2016 => ECMASCRIPT_2016
      case ESVersion.ES2017 => ECMASCRIPT_2017
      case ESVersion.ES2018 => ECMASCRIPT_2018
      case ESVersion.ES2019 => ECMASCRIPT_2019
      case ESVersion.ES2020 => ECMASCRIPT_2020
      case ESVersion.ES2021 => ECMASCRIPT_2021

      case _ =>
        throw new AssertionError(s"Unknown ES version ${esFeatures.esVersion}")
    }
  }

  /** Emit the given [[standard.ModuleSet ModuleSet]] to the target output.
   *
   *  @param moduleSet [[standard.ModuleSet ModuleSet]] to emit
   *  @param output Directory to write to
   */
  def emit(moduleSet: ModuleSet, output: OutputDirectory, logger: Logger)(
      implicit ec: ExecutionContext): Future[Report] = {
    verifyModuleSet(moduleSet)

    require(moduleSet.modules.size <= 1,
        "Cannot use multiple modules with the Closure Compiler")

    // Run Emitter even with 0 modules, to keep its internal state consistent.
    val emitterResult = logger.time("Emitter") {
      emitter.emit(moduleSet, logger)
    }

    val gccResult = for {
      sjsModule <- moduleSet.modules.headOption
    } yield {
      val closureChunk = logger.time("Closure: Create trees)") {
        val (trees, _) = emitterResult.body(sjsModule.id)
        buildChunk(trees)
      }

      logger.time("Closure: Compiler pass") {
        val options = closureOptions(sjsModule.id)

        val externs = Arrays.asList(
            ClosureSource.fromCode("ScalaJSExterns.js",
                ClosureLinkerBackend.ScalaJSExterns),
            ClosureSource.fromCode("ScalaJSGlobalRefs.js",
                makeExternsForGlobalRefs(emitterResult.globalRefs)),
            ClosureSource.fromCode("ScalaJSExportExterns.js",
                makeExternsForExports(emitterResult.topLevelVarDecls, sjsModule)))

        compile(externs, closureChunk, options, logger)
      }
    }

    logger.timeFuture("Closure: Write result") {
      writeResult(moduleSet, emitterResult.header, emitterResult.footer, gccResult, output)
    }
  }

  private def buildChunk(topLevelTrees: List[js.Tree]): JSChunk = {
    val root = ClosureAstTransformer.transformScript(topLevelTrees,
        languageMode.toFeatureSet(), config.relativizeSourceMapBase)

    val chunk = new JSChunk("Scala.js")
    chunk.add(new CompilerInput(new SyntheticAst(root)))
    chunk
  }

  private def compile(externs: java.util.List[ClosureSource], chunk: JSChunk,
      options: ClosureOptions, logger: Logger) = {
    val compiler = new ClosureCompiler
    compiler.setErrorManager(new SortingErrorManager(new HashSet(Arrays.asList(
        new LoggerErrorReportGenerator(logger)))))

    val result =
      compiler.compileModules(externs, Arrays.asList(chunk), options)

    if (!result.success) {
      throw new LinkingException(
          "There were errors when applying the Google Closure Compiler")
    }

    (compiler.toSource + "\n", compiler.getSourceMap())
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
      sjsModule: ModuleSet.Module): String = {
    import org.scalajs.ir.Trees._
    import org.scalajs.linker.backend.javascript.Trees.Ident.isValidJSIdentifierName

    def exportName(memberDef: MemberDef): Option[String] = memberDef match {
      case JSMethodDef(_, StringLiteral(name), _, _, _)   => Some(name)
      case JSPropertyDef(_, StringLiteral(name), _, _) => Some(name)
      case _                                           => None
    }

    val exportedPropertyNames = for {
      classDef <- sjsModule.classDefs
      member <- classDef.exportedMembers
      name <- exportName(member)
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

  private def writeResult(moduleSet: ModuleSet, header: String, footer: String,
      gccResult: Option[(String, SourceMap)], output: OutputDirectory)(
      implicit ec: ExecutionContext): Future[Report] = {
    /* `gccResult` is an Option, because we might have no module at all.
     * We call `.get` in the write methods to fail if we get a called anyways.
     */

    val writer = new OutputWriter(output, config, skipContentCheck = false) {
      private def writeCode(writer: Writer): Unit = {
        val code = gccResult.get._1
        writer.write(header)
        writer.write(code)
        writer.write(footer)
      }

      protected def writeModuleWithoutSourceMap(moduleID: ModuleID): Option[ByteBuffer] = {
        val jsFileWriter = new ByteArrayOutputStream()
        val jsFileStrWriter = new java.io.OutputStreamWriter(jsFileWriter, StandardCharsets.UTF_8)
        writeCode(jsFileStrWriter)
        jsFileStrWriter.flush()
        Some(ByteBuffer.wrap(jsFileWriter.toByteArray()))
      }

      protected def writeModuleWithSourceMap(moduleID: ModuleID): Option[(ByteBuffer, ByteBuffer)] = {
        val jsFileURI = OutputPatternsImpl.jsFileURI(config.outputPatterns, moduleID.id)
        val sourceMapURI = OutputPatternsImpl.sourceMapURI(config.outputPatterns, moduleID.id)

        val jsFileWriter = new ByteArrayOutputStream()
        val jsFileStrWriter = new java.io.OutputStreamWriter(jsFileWriter, StandardCharsets.UTF_8)
        writeCode(jsFileStrWriter)
        jsFileStrWriter.write("//# sourceMappingURL=" + sourceMapURI + "\n")
        jsFileStrWriter.flush()

        val sourceMapWriter = new ByteArrayOutputStream()
        val sourceMapStrWriter = new java.io.OutputStreamWriter(sourceMapWriter, StandardCharsets.UTF_8)
        val sourceMap = gccResult.get._2
        sourceMap.setWrapperPrefix(header)
        sourceMap.appendTo(sourceMapStrWriter, jsFileURI)
        sourceMapStrWriter.flush()

        Some((ByteBuffer.wrap(jsFileWriter.toByteArray()), ByteBuffer.wrap(sourceMapWriter.toByteArray())))
      }
    }

    writer.write(moduleSet)
  }

  private def closureOptions(moduleID: ModuleID) = {
    val options = new ClosureOptions
    options.setPrettyPrint(config.prettyPrint)
    CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(options)

    options.setLanguage(languageMode)
    options.setWarningLevel(DiagnosticGroups.GLOBAL_THIS, CheckLevel.OFF)
    options.setWarningLevel(DiagnosticGroups.DUPLICATE_VARS, CheckLevel.OFF)
    options.setWarningLevel(DiagnosticGroups.CHECK_REGEXP, CheckLevel.OFF)
    options.setWarningLevel(DiagnosticGroups.CHECK_TYPES, CheckLevel.OFF)
    options.setWarningLevel(DiagnosticGroups.CHECK_USELESS_CODE, CheckLevel.OFF)

    if (config.sourceMap) {
      val sourceMapFileName =
        OutputPatternsImpl.sourceMapFile(config.outputPatterns, moduleID.id)

      options.setSourceMapDetailLevel(SourceMap.DetailLevel.ALL)
      options.setSourceMapOutputPath(sourceMapFileName)
    }

    options
  }
}

private object ClosureLinkerBackend {
  /** Minimal set of externs to compile Scala.js-emitted code with Closure.
   *
   *  These must be externs in all cases because they are generated outside of
   *  global ref tracking and CoreJSLib.
   *
   *  * `constructor` is generated for classes
   *  * `toString` is used by [[java.lang.Object#toString]]
   *  * `$classData` needs to be protected from renaming because it must not
   *    be renamed to something short and ubiquitous, otherwise
   *    `$isScalaJSObject` and `$is_` functions cease to function properly.
   *  * `length` is generated by [[ArrayLength org.scalajs.ir.ArrayLength]]
   *  * `call` is generated for super calls
   *  * `apply` is generated when desugaring `...spread` arguments
   *  * `NaN`, `Infinity` and `undefined` need to be in externs for
   *    Closure not to crash in cases where it constant-folds an expression into
   *    one of these (this was confirmed to us as intended by Closure devs).
   */
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
    var NaN = 0.0/0.0, Infinity = 1.0/0.0, undefined = void 0;
    """
}
