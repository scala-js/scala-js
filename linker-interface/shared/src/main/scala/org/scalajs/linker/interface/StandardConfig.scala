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

package org.scalajs.linker.interface

import scala.language.implicitConversions

import scala.annotation.switch

import java.net.URI

import Fingerprint.FingerprintBuilder

/** Configuration of a standard linker. */
final class StandardConfig private (
    /** Scala.js semantics. */
    val semantics: Semantics,
    /** Module kind. */
    val moduleKind: ModuleKind,
    /** How to split modules (if at all). */
    val moduleSplitStyle: ModuleSplitStyle,
    /** ECMAScript features to use. */
    val esFeatures: ESFeatures,
    /** If true, performs expensive checks of the IR for the used parts. */
    val checkIR: Boolean,
    /** Whether to use the Scala.js optimizer. */
    val optimizer: Boolean,
    /** A header that will be added at the top of generated .js files. */
    val jsHeader: String,
    /** Whether things that can be parallelized should be parallelized.
     *  On the JavaScript platform, this does not have any effect.
     */
    val parallel: Boolean,
    /** Whether to emit a source map. */
    val sourceMap: Boolean,
    /** Base path to relativize paths in the source map. */
    val relativizeSourceMapBase: Option[URI],
    /** Name patterns for output. */
    val outputPatterns: OutputPatterns,
    /** Apply Scala.js-specific minification of the produced .js files.
     *
     *  When enabled, the linker more aggressively reduces the size of the
     *  generated code, at the cost of readability and debuggability. It does
     *  not perform size optimizations that would negatively impact run-time
     *  performance.
     *
     *  The focus is on optimizations that general-purpose JavaScript minifiers
     *  cannot do on their own. For the best results, we expect the Scala.js
     *  minifier to be used in conjunction with a general-purpose JavaScript
     *  minifier.
     */
    val minify: Boolean,
    /** Whether to use the Google Closure Compiler pass, if it is available.
     *  On the JavaScript platform, this does not have any effect.
     */
    val closureCompilerIfAvailable: Boolean,
    /** Pretty-print the output, for debugging purposes.
     *
     *  For the WebAssembly backend, this results in an additional `.wat` file
     *  next to each produced `.wasm` file with the WebAssembly text format
     *  representation of the latter. This file is never subsequently used,
     *  but may be inspected for debugging pruposes.
     */
    val prettyPrint: Boolean,
    /** Whether the linker should run in batch mode.
     *
     *  In batch mode, the linker can throw away intermediate state that is
     *  otherwise maintained for incremental runs.
     *
     *  This setting is only a hint. A linker and/or its subcomponents may
     *  ignore it. This applies in both directions: a linker not supporting
     *  incrementality can ignore `batchMode = false`, and a contrario, a
     *  linker mainly designed for incremental runs may ignore
     *  `batchMode = true`.
     */
    val batchMode: Boolean,
    /** The maximum number of (file) writes executed concurrently. */
    val maxConcurrentWrites: Int,
    /** If true, use the experimental WebAssembly backend. */
    val experimentalUseWebAssembly: Boolean
) {
  private def this() = {
    this(
        semantics = Semantics.Defaults,
        moduleKind = ModuleKind.NoModule,
        moduleSplitStyle = ModuleSplitStyle.FewestModules,
        esFeatures = ESFeatures.Defaults,
        checkIR = false,
        optimizer = true,
        jsHeader = "",
        parallel = true,
        sourceMap = true,
        relativizeSourceMapBase = None,
        outputPatterns = OutputPatterns.Defaults,
        minify = false,
        closureCompilerIfAvailable = false,
        prettyPrint = false,
        batchMode = false,
        maxConcurrentWrites = 50,
        experimentalUseWebAssembly = false
    )
  }

  def withSemantics(semantics: Semantics): StandardConfig =
    copy(semantics = semantics)

  def withSemantics(f: Semantics => Semantics): StandardConfig =
    copy(semantics = f(semantics))

  def withModuleKind(moduleKind: ModuleKind): StandardConfig =
    copy(moduleKind = moduleKind)

  def withModuleSplitStyle(moduleSplitStyle: ModuleSplitStyle): StandardConfig =
    copy(moduleSplitStyle = moduleSplitStyle)

  def withESFeatures(esFeatures: ESFeatures): StandardConfig =
    copy(esFeatures = esFeatures)

  def withESFeatures(f: ESFeatures => ESFeatures): StandardConfig =
    copy(esFeatures = f(esFeatures))

  def withCheckIR(checkIR: Boolean): StandardConfig =
    copy(checkIR = checkIR)

  def withOptimizer(optimizer: Boolean): StandardConfig =
    copy(optimizer = optimizer)

  /** Sets the `jsHeader` to a JS comment to add at the top of generated .js files.
   *
   *  The header must satisfy the following constraints:
   *
   *  - It must contain only valid JS whitespace and/or JS comments (single- or
   *    multi-line comment, or, at the very beginning, a hashbang comment).
   *  - It must not use new line characters that are not UNIX new lines (`"\n"`).
   *  - If non-empty, it must end with a new line.
   *  - It must not contain unpaired surrogate characters (i.e., it must be a valid UTF-16 string).
   *
   *  Those requirements can be checked with [[StandardConfig.isValidJSHeader]].
   *
   *  @throws java.lang.IllegalArgumentException
   *    if the header is not valid
   */
  def withJSHeader(jsHeader: String): StandardConfig = {
    require(StandardConfig.isValidJSHeader(jsHeader),
        "Invalid JS header; it must be a valid JS comment ending with a new line, using UNIX new lines:\n" +
        jsHeader)
    copy(jsHeader = jsHeader)
  }

  def withParallel(parallel: Boolean): StandardConfig =
    copy(parallel = parallel)

  def withSourceMap(sourceMap: Boolean): StandardConfig =
    copy(sourceMap = sourceMap)

  def withRelativizeSourceMapBase(relativizeSourceMapBase: Option[URI]): StandardConfig =
    copy(relativizeSourceMapBase = relativizeSourceMapBase)

  def withOutputPatterns(outputPatterns: OutputPatterns): StandardConfig =
    copy(outputPatterns = outputPatterns)

  def withOutputPatterns(f: OutputPatterns => OutputPatterns): StandardConfig =
    copy(outputPatterns = f(outputPatterns))

  def withMinify(minify: Boolean): StandardConfig =
    copy(minify = minify)

  def withClosureCompilerIfAvailable(closureCompilerIfAvailable: Boolean): StandardConfig =
    copy(closureCompilerIfAvailable = closureCompilerIfAvailable)

  def withPrettyPrint(prettyPrint: Boolean): StandardConfig =
    copy(prettyPrint = prettyPrint)

  def withBatchMode(batchMode: Boolean): StandardConfig =
    copy(batchMode = batchMode)

  def withMaxConcurrentWrites(maxConcurrentWrites: Int): StandardConfig =
    copy(maxConcurrentWrites = maxConcurrentWrites)

  /** Specifies whether to use the experimental WebAssembly backend.
   *
   *  When using this setting, the following settings must also be set:
   *
   *  - `withSemantics(sems)` such that the behaviors of `sems` are all set to
   *    `CheckedBehavior.Unchecked`
   *  - `withModuleKind(ModuleKind.ESModule)`
   *  - `withOptimizer(false)`
   *  - `withStrictFloats(true)` (this is the default)
   *
   *  These restrictions will be lifted in the future, except for the
   *  `ModuleKind`.
   *
   *  If any of these restrictions are not met, linking will eventually throw
   *  an `IllegalArgumentException`.
   *
   *  @note
   *    Currently, the WebAssembly backend silently ignores `@JSExport` and
   *    `@JSExportAll` annotations. This behavior may change in the future,
   *    either by making them warnings or errors, or by adding support for them.
   *    All other language features are supported.
   *
   *  @note
   *    This setting is experimental. It may be removed in an upcoming *minor*
   *    version of Scala.js. Future minor versions may also produce code that
   *    requires more recent versions of JS engines supporting newer WebAssembly
   *    standards.
   *
   *  @throws java.lang.UnsupportedOperationException
   *    In the future, if the feature gets removed.
   */
  def withExperimentalUseWebAssembly(experimentalUseWebAssembly: Boolean): StandardConfig =
    copy(experimentalUseWebAssembly = experimentalUseWebAssembly)

  override def toString(): String = {
    s"""StandardConfig(
       |  semantics                  = $semantics,
       |  moduleKind                 = $moduleKind,
       |  moduleSplitStyle           = $moduleSplitStyle,
       |  esFeatures                 = $esFeatures,
       |  checkIR                    = $checkIR,
       |  optimizer                  = $optimizer,
       |  jsHeader                   = "$jsHeader",
       |  parallel                   = $parallel,
       |  sourceMap                  = $sourceMap,
       |  relativizeSourceMapBase    = $relativizeSourceMapBase,
       |  outputPatterns             = $outputPatterns,
       |  minify                     = $minify,
       |  closureCompilerIfAvailable = $closureCompilerIfAvailable,
       |  prettyPrint                = $prettyPrint,
       |  batchMode                  = $batchMode,
       |  maxConcurrentWrites        = $maxConcurrentWrites,
       |  experimentalUseWebAssembly = $experimentalUseWebAssembly,
       |)""".stripMargin
  }

  private def copy(
      semantics: Semantics = semantics,
      moduleKind: ModuleKind = moduleKind,
      moduleSplitStyle: ModuleSplitStyle = moduleSplitStyle,
      esFeatures: ESFeatures = esFeatures,
      checkIR: Boolean = checkIR,
      optimizer: Boolean = optimizer,
      jsHeader: String = jsHeader,
      parallel: Boolean = parallel,
      sourceMap: Boolean = sourceMap,
      outputPatterns: OutputPatterns = outputPatterns,
      relativizeSourceMapBase: Option[URI] = relativizeSourceMapBase,
      minify: Boolean = minify,
      closureCompilerIfAvailable: Boolean = closureCompilerIfAvailable,
      prettyPrint: Boolean = prettyPrint,
      batchMode: Boolean = batchMode,
      maxConcurrentWrites: Int = maxConcurrentWrites,
      experimentalUseWebAssembly: Boolean = experimentalUseWebAssembly
  ): StandardConfig = {
    new StandardConfig(
        semantics,
        moduleKind,
        moduleSplitStyle,
        esFeatures,
        checkIR,
        optimizer,
        jsHeader,
        parallel,
        sourceMap,
        relativizeSourceMapBase,
        outputPatterns,
        minify,
        closureCompilerIfAvailable,
        prettyPrint,
        batchMode,
        maxConcurrentWrites,
        experimentalUseWebAssembly
    )
  }
}

object StandardConfig {
  import StandardConfigPlatformExtensions.ConfigExt

  private implicit object StandardConfigFingerprint
      extends Fingerprint[StandardConfig] {

    override def fingerprint(config: StandardConfig): String = {
      new FingerprintBuilder("StandardConfig")
        .addField("semantics", config.semantics)
        .addField("moduleKind", config.moduleKind)
        .addField("moduleSplitStyle", config.moduleSplitStyle)
        .addField("esFeatures", config.esFeatures)
        .addField("checkIR", config.checkIR)
        .addField("optimizer", config.optimizer)
        .addField("jsHeader", config.jsHeader)
        .addField("parallel", config.parallel)
        .addField("sourceMap", config.sourceMap)
        .addField("relativizeSourceMapBase",
            config.relativizeSourceMapBase.map(_.toASCIIString()))
        .addField("outputPatterns", config.outputPatterns)
        .addField("minify", config.minify)
        .addField("closureCompilerIfAvailable",
            config.closureCompilerIfAvailable)
        .addField("prettyPrint", config.prettyPrint)
        .addField("batchMode", config.batchMode)
        .addField("maxConcurrentWrites", config.maxConcurrentWrites)
        .addField("experimentalUseWebAssembly", config.experimentalUseWebAssembly)
        .build()
    }
  }

  def fingerprint(config: StandardConfig): String =
    Fingerprint.fingerprint(config)

  /** Returns the default [[StandardConfig]].
   *
   *  The defaults are:
   *
   *  - `semantics`: [[Semantics.Defaults]]
   *  - `moduleKind`: [[ModuleKind.NoModule]]
   *  - `moduleSplitStyle`: [[ModuleSplitStyle.FewestModules]]
   *  - `esFeatures`: [[ESFeatures.Defaults]]
   *  - `checkIR`: `false`
   *  - `optimizer`: `true`
   *  - `jsHeader`: `""`
   *  - `parallel`: `true`
   *  - `sourceMap`: `true`
   *  - `relativizeSourceMapBase`: `None`
   *  - `outputPatterns`: [[OutputPatterns.Defaults]]
   *  - `minify`: `false`
   *  - `closureCompilerIfAvailable`: `false`
   *  - `prettyPrint`: `false`
   *  - `batchMode`: `false`
   *  - `maxConcurrentWrites`: `50`
   *  - `experimentalUseWebAssembly`: `false`
   */
  def apply(): StandardConfig = new StandardConfig()

  /** Tests whether a string is a valid JS header.
   *
   *  A header is valid if and only if it satisfies the following constraints:
   *
   *  - It must contain only valid JS whitespace and/or JS comments (single- or
   *    multi-line comment, or, at the very beginning, a hashbang comment).
   *  - It must not use new line characters that are not UNIX new lines (`"\n"`).
   *  - If non-empty, it must end with a new line.
   *  - It must not contain unpaired surrogate characters (i.e., it must be a valid UTF-16 string).
   */
  def isValidJSHeader(jsHeader: String): Boolean = {
    // scalastyle:off return

    /* First, reject any non-UNIX Unicode new line code point, wherever they
     * appear (in comments or not). This includes VT and FF, which JavaScript
     * considers as whitespace but not line separators, as well as NEL.
     * https://www.unicode.org/reports/tr14/tr14-32.html#BK
     * VT | FF | CR | NEL | LS | PS
     *
     * Also reject unpaired surrogate chars, wherever they appear.
     */
    val len = jsHeader.length()
    var i = 0
    while (i != len) {
      def isNewLine(c: Char): Boolean = (c: @switch) match {
        case '\u000B' | '\u000C' | '\r' | '\u0085' | '\u2028' | '\u2029' => true
        case _                                                           => false
      }

      val cp = jsHeader.codePointAt(i)
      i += Character.charCount(cp)
      if (Character.isBmpCodePoint(cp)) {
        if (isNewLine(cp.toChar) || Character.isSurrogate(cp.toChar))
          return false
      }
    }

    // Now, parse whitespace and comments, and reject anything else

    i = 0
    while (i != len) {
      val cp = jsHeader.codePointAt(i)
      i += Character.charCount(cp)

      (cp: @switch) match {
        // Accept the UNIX new line
        case '\n' =>
          ()

        /* Accept JavaScript comments
         * https://262.ecma-international.org/12.0/#sec-comments
         */
        case '/' =>
          if (i == len)
            return false

          jsHeader.charAt(i) match {
            // Single-line comment
            case '/' =>
              while (i != len && jsHeader.charAt(i) != '\n')
                i += 1

            // Multi-line comment
            case '*' =>
              i += 1
              val closingMarkerPos = jsHeader.indexOf("*/", i)
              if (closingMarkerPos < 0)
                return false
              i = closingMarkerPos + 2

            case _ =>
              return false
          }

        /* Accept a hashbang comment, but only at the very beginning
         * This is a Stage 3 proposal:
         * https://github.com/tc39/proposal-hashbang
         * Documentation on MDN:
         * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#hashbang_comments
         */
        case '#' if i == 1 && len >= 2 && jsHeader.charAt(1) == '!' =>
          i += 1
          while (i != len && jsHeader.charAt(i) != '\n')
            i += 1

        /* Accept JavaScript Whitespace that are not Unicode new lines
         * https://262.ecma-international.org/12.0/#sec-white-space
         * TAB | SP | NBSP | ZWNBSP | <Unicode category Zs>
         */
        case '\t' | ' ' | 0x00a0 | 0xfeff =>
          ()
        case _ if Character.getType(cp) == Character.SPACE_SEPARATOR =>
          () // General category 'Zs'

        // Reject anything else
        case _ =>
          return false
      }
    }

    // Non-empty headers must end with a new line
    len == 0 || jsHeader.charAt(len - 1) == '\n'

    // scalastyle:on return
  }

  implicit def configExt(config: StandardConfig): ConfigExt =
    new ConfigExt(config)
}
