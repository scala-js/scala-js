/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js CLI               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.cli

import org.scalajs.core.ir.ScalaJSVersions

import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.logging._

import CheckedBehavior.Compliant

import org.scalajs.core.tools.linker.Linker
import org.scalajs.core.tools.linker.frontend.LinkerFrontend
import org.scalajs.core.tools.linker.backend.{LinkerBackend, OutputMode, ModuleKind}

import scala.collection.immutable.Seq

import java.io.File
import java.net.URI

object Scalajsld {

  private case class Options(
    cp: Seq[File] = Seq.empty,
    output: File = null,
    jsoutput: Boolean = false,
    semantics: Semantics = Semantics.Defaults,
    outputMode: OutputMode = OutputMode.ECMAScript51Isolated,
    moduleKind: ModuleKind = ModuleKind.NoModule,
    noOpt: Boolean = false,
    fullOpt: Boolean = false,
    prettyPrint: Boolean = false,
    sourceMap: Boolean = false,
    relativizeSourceMap: Option[URI] = None,
    bypassLinkingErrors: Boolean = false,
    checkIR: Boolean = false,
    stdLib: Option[File] = None,
    logLevel: Level = Level.Info)

  private implicit object OutputModeRead extends scopt.Read[OutputMode] {
    val arity = 1
    val reads = { (s: String) =>
      OutputMode.All.find(_.toString() == s).getOrElse(
          throw new IllegalArgumentException(s"$s is not a valid output mode"))
    }
  }

  private implicit object ModuleKindRead extends scopt.Read[ModuleKind] {
    val arity = 1
    val reads = { (s: String) =>
      ModuleKind.All.find(_.toString() == s).getOrElse(
          throw new IllegalArgumentException(s"$s is not a valid module kind"))
    }
  }

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Options]("scalajsld") {
      head("scalajsld", ScalaJSVersions.current)
      arg[File]("<value> ...")
        .unbounded()
        .action { (x, c) => c.copy(cp = c.cp :+ x) }
        .text("Entries of Scala.js classpath to link")
      opt[File]('o', "output")
        .valueName("<file>")
        .required()
        .action { (x, c) => c.copy(output = x) }
        .text("Output file of linker (required)")
      opt[File]("jsoutput")
        .hidden()
        .valueName("<file>")
        .abbr("jo")
        .action { (_, c) => c.copy(jsoutput = true) }
        .text("Deprecated: Does nothing but printing a warning")
      opt[Unit]('f', "fastOpt")
        .action { (_, c) => c.copy(noOpt = false, fullOpt = false) }
        .text("Optimize code (this is the default)")
      opt[Unit]('n', "noOpt")
        .action { (_, c) => c.copy(noOpt = true, fullOpt = false) }
        .text("Don't optimize code")
      opt[Unit]('u', "fullOpt")
        .action { (_, c) => c.copy(noOpt = false, fullOpt = true) }
        .text("Fully optimize code (uses Google Closure Compiler)")
      opt[Unit]('p', "prettyPrint")
        .action { (_, c) => c.copy(prettyPrint = true) }
        .text("Pretty print full opted code (meaningful with -u)")
      opt[Unit]('s', "sourceMap")
        .action { (_, c) => c.copy(sourceMap = true) }
        .text("Produce a source map for the produced code")
      opt[Unit]("compliantAsInstanceOfs")
        .action { (_, c) => c.copy(semantics =
          c.semantics.withAsInstanceOfs(Compliant))
        }
        .text("Use compliant asInstanceOfs")
      opt[OutputMode]('m', "outputMode")
        .action { (mode, c) => c.copy(outputMode = mode) }
        .text("Output mode " + OutputMode.All.mkString("(", ", ", ")"))
      opt[ModuleKind]('k', "moduleKind")
        .action { (kind, c) => c.copy(moduleKind = kind) }
        .text("Module kind " + ModuleKind.All.mkString("(", ", ", ")"))
      opt[Unit]('b', "bypassLinkingErrors")
        .action { (_, c) => c.copy(bypassLinkingErrors = true) }
        .text("Only warn if there are linking errors (deprecated)")
      opt[Unit]('c', "checkIR")
        .action { (_, c) => c.copy(checkIR = true) }
        .text("Check IR before optimizing")
      opt[File]('r', "relativizeSourceMap")
        .valueName("<path>")
        .action { (x, c) => c.copy(relativizeSourceMap = Some(x.toURI)) }
        .text("Relativize source map with respect to given path (meaningful with -s)")
      opt[Unit]("noStdlib")
        .action { (_, c) => c.copy(stdLib = None) }
        .text("Don't automatically include Scala.js standard library")
      opt[File]("stdlib")
        .valueName("<scala.js stdlib jar>")
        .hidden()
        .action { (x, c) => c.copy(stdLib = Some(x)) }
        .text("Location of Scala.js standard libarary. This is set by the " +
            "runner script and automatically prepended to the classpath. " +
            "Use -n to not include it.")
      opt[Unit]('d', "debug")
        .action { (_, c) => c.copy(logLevel = Level.Debug) }
        .text("Debug mode: Show full log")
      opt[Unit]('q', "quiet")
        .action { (_, c) => c.copy(logLevel = Level.Warn) }
        .text("Only show warnings & errors")
      opt[Unit]("really-quiet")
        .abbr("qq")
        .action { (_, c) => c.copy(logLevel = Level.Error) }
        .text("Only show errors")
      version("version")
        .abbr("v")
        .text("Show scalajsld version")
      help("help")
        .abbr("h")
        .text("prints this usage text")

      override def showUsageOnError = true
    }

    for (options <- parser.parse(args, Options())) {
      val classpath = options.stdLib.toList ++ options.cp
      val irContainers = IRFileCache.IRContainer.fromClasspath(classpath)

      // Warn if writing JS dependencies was requested.
      if (options.jsoutput) {
        Console.err.println(
            "Support for the --jsoutput flag has been dropped. " +
            "JS dependencies will not be written to disk. " +
            "Comment on https://github.com/scala-js/scala-js/issues/2163 " +
            "if you rely on this feature.")
      }

      // Warn if bypassing linking errors was requested.
      if (options.bypassLinkingErrors) {
        Console.err.println(
            "Support for bypassing linking errors with -b or " +
            "--bypassLinkingErrors will be dropped in the next major version.")
      }

      val semantics =
        if (options.fullOpt) options.semantics.optimized
        else options.semantics

      val frontendConfig = LinkerFrontend.Config()
        .withBypassLinkingErrorsInternal(options.bypassLinkingErrors)
        .withCheckIR(options.checkIR)

      val backendConfig = LinkerBackend.Config()
        .withRelativizeSourceMapBase(options.relativizeSourceMap)
        .withPrettyPrint(options.prettyPrint)

      val config = Linker.Config()
        .withSourceMap(options.sourceMap)
        .withOptimizer(!options.noOpt)
        .withParallel(true)
        .withClosureCompiler(options.fullOpt)
        .withFrontendConfig(frontendConfig)
        .withBackendConfig(backendConfig)

      val linker = Linker(semantics, options.outputMode, options.moduleKind,
          config)

      val logger = new ScalaConsoleLogger(options.logLevel)
      val outFile = WritableFileVirtualJSFile(options.output)
      val cache = (new IRFileCache).newCache

      linker.link(cache.cached(irContainers), outFile, logger)
    }
  }
}
