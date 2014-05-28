/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js CLI               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.cli

import scala.scalajs.ir
import ir.ScalaJSVersions

import scala.scalajs.tools.io._
import scala.scalajs.tools.logging._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.classpath.builder._

import scala.scalajs.tools.optimizer.{ScalaJSOptimizer, ScalaJSClosureOptimizer}
import scala.scalajs.tools.packager.ScalaJSPackager

import scala.collection.immutable.Seq

import java.io.File
import java.net.URI

object Scalajsld {

  case class Options(
    cp: Seq[File] = Seq.empty,
    output: File = null,
    jsoutput: Option[File] = None,
    noOpt: Boolean = false,
    fullOpt: Boolean = false,
    prettyPrint: Boolean = false,
    sourceMap: Boolean = false,
    relativizeSourceMap: Option[URI] = None,
    checkIR: Boolean = false,
    stdLib: Option[File] = None)

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
        .text("Don't optimize code, just concatenate")
      opt[Unit]('u', "fullOpt")
        .action { (_, c) => c.copy(noOpt = false, fullOpt = true) }
        .text("Fully optimize code (uses Google Closure Compiler)")
      opt[Unit]('p', "prettyPrint")
        .action { (_, c) => c.copy(prettyPrint = true) }
        .text("Pretty print full opted code (meaningful with -u)")
      opt[Unit]('s', "sourceMap")
        .action { (_, c) => c.copy(sourceMap = true) }
        .text("Produce a source map for the produced code (with fastOpt and noOpt)")
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
      val cp = PartialClasspathBuilder.buildIR(cpFiles).resolve()

      // Write JS dependencies if requested
      for (jsout <- options.jsoutput) {
        import ScalaJSPackager._
        (new ScalaJSPackager).packageJS(cp.jsLibs,
            OutputConfig(WritableFileVirtualJSFile(jsout)),
            new ScalaConsoleLogger)
      }

      // Link Scala.js code
      val outFile = WritableFileVirtualJSFile(options.output)
      if (options.fullOpt) {
        val fastOptCP =
          fastOpt(cp, WritableMemVirtualJSFile("temporary file"),
              options.copy(sourceMap = false))
        fullOpt(fastOptCP, outFile, options)
      } else if (options.noOpt)
        noOpt(cp, outFile, options)
      else
        fastOpt(cp, outFile, options)
    }
  }

  private def noOpt(cp: CompleteIRClasspath,
      output: WritableVirtualJSFile, options: Options) = {
    import ScalaJSPackager.OutputConfig

    (new ScalaJSPackager).packageCP(cp,
        OutputConfig(
            output = output,
            wantSourceMap = options.sourceMap,
            relativizeSourceMapBase = options.relativizeSourceMap),
        new ScalaConsoleLogger)
  }

  private def fullOpt(cp: CompleteCIClasspath,
      output: WritableVirtualJSFile, options: Options) = {
    import ScalaJSClosureOptimizer._

    (new ScalaJSClosureOptimizer).optimizeCP(
        Inputs(cp),
        OutputConfig(
            output = output,
            wantSourceMap = options.sourceMap,
            prettyPrint = options.prettyPrint,
            relativizeSourceMapBase = options.relativizeSourceMap),
        new ScalaConsoleLogger)
  }

  private def fastOpt(cp: CompleteIRClasspath,
      output: WritableVirtualJSFile, options: Options) = {
    import ScalaJSOptimizer._

    (new ScalaJSOptimizer).optimizeCP(
        Inputs(cp),
        OutputConfig(
            output = output,
            wantSourceMap = options.sourceMap,
            checkIR = options.checkIR,
            relativizeSourceMapBase = options.relativizeSourceMap),
        new ScalaConsoleLogger)
  }

}
