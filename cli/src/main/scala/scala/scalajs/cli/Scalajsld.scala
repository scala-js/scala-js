/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js CLI               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.cli

import scala.scalajs.ir.ScalaJSVersions

import scala.scalajs.tools.sem._
import scala.scalajs.tools.io._
import scala.scalajs.tools.logging._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.classpath.builder._

import CheckedBehaviors.Compliant

import scala.scalajs.tools.optimizer.{
  ScalaJSOptimizer,
  ScalaJSClosureOptimizer,
  ParIncOptimizer
}

import scala.collection.immutable.Seq

import java.io.File
import java.net.URI

object Scalajsld {

  case class Options(
    cp: Seq[File] = Seq.empty,
    output: File = null,
    jsoutput: Option[File] = None,
    semantics: Semantics = Semantics.Defaults,
    noOpt: Boolean = false,
    fullOpt: Boolean = false,
    prettyPrint: Boolean = false,
    sourceMap: Boolean = false,
    relativizeSourceMap: Option[URI] = None,
    checkIR: Boolean = false,
    stdLib: Option[File] = None,
    logLevel: Level = Level.Info)

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
        .valueName("<file>")
        .abbr("jo")
        .action { (x, c) => c.copy(jsoutput = Some(x)) }
        .text("Concatenate all JavaScript libary dependencies to this file")
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
          c.semantics.transformCheckedBehaviors(_.withAsInstanceOfs(Compliant)))
        }
        .text("Use compliant asInstanceOfs")
      opt[Unit]('c', "checkIR")
        .action { (_, c) => c.copy(checkIR = true) }
        .text("Check IR before optimizing")
      opt[File]('r', "relativizeSourceMap")
        .valueName("<path>")
        .action { (x, c) => c.copy(relativizeSourceMap = Some(x.toURI)) }
        .text("Relativize source map with respect to given path (meaningful with -s)")
      opt[Unit]("noStdlib")
        .action { (_, c) => c.copy(stdLib = None) }
        .text("Don't automatcially include Scala.js standard library")
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
      val cpFiles = options.stdLib.toList ++ options.cp
      // Load and resolve classpath
      val cp = PartialClasspathBuilder.build(cpFiles).resolve()

      // Write JS dependencies if requested
      for (jsout <- options.jsoutput)
        IO.concatFiles(WritableFileVirtualJSFile(jsout), cp.jsLibs.map(_._1))

      // Link Scala.js code
      val outFile = WritableFileVirtualJSFile(options.output)
      if (options.fullOpt)
        fullOpt(cp, outFile, options)
      else
        fastOpt(cp, outFile, options)
    }
  }

  private def fullOpt(cp: IRClasspath,
      output: WritableVirtualJSFile, options: Options) = {
    import ScalaJSClosureOptimizer._

    val semantics = options.semantics.optimized

    new ScalaJSClosureOptimizer(semantics).optimizeCP(
        newScalaJSOptimizer(semantics),
        Inputs(ScalaJSOptimizer.Inputs(cp)),
        OutputConfig(
            output = output,
            wantSourceMap = options.sourceMap,
            relativizeSourceMapBase = options.relativizeSourceMap,
            checkIR = options.checkIR,
            prettyPrint = options.prettyPrint),
        newLogger(options))
  }

  private def fastOpt(cp: IRClasspath,
      output: WritableVirtualJSFile, options: Options) = {
    import ScalaJSOptimizer._

    newScalaJSOptimizer(options.semantics).optimizeCP(
        Inputs(cp),
        OutputConfig(
            output = output,
            wantSourceMap = options.sourceMap,
            checkIR = options.checkIR,
            disableOptimizer = options.noOpt,
            relativizeSourceMapBase = options.relativizeSourceMap),
        newLogger(options))
  }

  private def newLogger(options: Options) =
    new ScalaConsoleLogger(options.logLevel)

  private def newScalaJSOptimizer(semantics: Semantics) =
    new ScalaJSOptimizer(semantics, new ParIncOptimizer(_))

}
