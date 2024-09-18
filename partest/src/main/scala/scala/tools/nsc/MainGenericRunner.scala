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

package scala.tools.nsc

/* Super hacky overriding of the MainGenericRunner used by partest */

import org.scalajs.ir

import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.interface._

import org.scalajs.jsenv._
import org.scalajs.jsenv.nodejs.NodeJSEnv

import com.google.common.jimfs.Jimfs

import scala.tools.partest.scalajs.ScalaJSPartestOptions._

import java.net.URL
import java.nio.file._

import scala.io.Source
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

import Properties.{ versionString, copyrightString }
import GenericRunnerCommand._

class MainGenericRunner {
  def errorFn(ex: Throwable): Boolean = {
    ex.printStackTrace()
    false
  }
  def errorFn(str: String): Boolean = {
    scala.Console.err println str
    false
  }

  val useWasm = System.getProperty("scalajs.partest.useWasm").toBoolean
  val optMode = OptMode.fromId(System.getProperty("scalajs.partest.optMode"))

  def readSemantics() = {
    import org.scalajs.linker.interface.CheckedBehavior.Compliant

    val opt = Option(System.getProperty("scalajs.partest.compliantSems"))
    val compliantSems =
      opt.fold[List[String]](Nil)(_.split(',').toList.filter(_.nonEmpty))

    compliantSems.foldLeft(Semantics.Defaults) { (prev, compliantSem) =>
      compliantSem match {
        case "asInstanceOfs"          => prev.withAsInstanceOfs(Compliant)
        case "arrayIndexOutOfBounds"  => prev.withArrayIndexOutOfBounds(Compliant)
        case "arrayStores"            => prev.withArrayStores(Compliant)
        case "negativeArraySizes"     => prev.withNegativeArraySizes(Compliant)
        case "nullPointers"           => prev.withNullPointers(Compliant)
        case "stringIndexOutOfBounds" => prev.withStringIndexOutOfBounds(Compliant)
        case "moduleInit"             => prev.withModuleInit(Compliant)
      }
    }
  }

  def process(args: Array[String]): Boolean = {
    val command = new GenericRunnerCommand(args.toList, (x: String) => errorFn(x))

    if (!command.ok) return errorFn("\n" + command.shortUsageMsg)
    else if (command.settings.version.value) return errorFn("Scala code runner %s -- %s".format(versionString, copyrightString))
    else if (command.shouldStopWithInfo) return errorFn("shouldStopWithInfo")

    if (command.howToRun != AsObject)
      return errorFn("Scala.js runner can only run an object")

    val logger = new ScalaConsoleLogger(Level.Warn)
    val semantics0 = readSemantics()
    val semantics = if (optMode == FullOpt) semantics0.optimized else semantics0

    val useESModule = useWasm

    val moduleInitializers = Seq(ModuleInitializer.mainMethodWithArgs(
        command.thingToRun, "main", command.arguments))

    val linkerConfig = StandardConfig()
      .withCheckIR(true)
      .withSemantics(semantics)
      .withExperimentalUseWebAssembly(useWasm)
      .withModuleKind(if (useESModule) ModuleKind.ESModule else ModuleKind.NoModule)
      .withSourceMap(false)
      .withOptimizer(optMode != NoOpt)
      .withClosureCompiler(optMode == FullOpt)
      .withBatchMode(true)
      .withOutputPatterns(OutputPatterns.fromJSFile(if (useESModule) "%s.mjs" else "%s.js"))

    val linker = StandardImpl.linker(linkerConfig)

    val dir: Path = if (!useWasm) {
      val tempDir = Jimfs.newFileSystem().getPath("tmp")
      Files.createDirectory(tempDir)
      tempDir
    } else {
      /* The Wasm output needs to read other files in the same directory,
       * with predictable names. Therefore, we need to use real files.
       */
      val tempDir = Files.createTempDirectory("tmp-scalajs-partest")
      tempDir.toFile().deleteOnExit()
      tempDir
    }

    val sjsCode = {
      val cache = StandardImpl.irFileCache().newCache
      val result = PathIRContainer
        .fromClasspath(command.settings.classpathURLs.map(urlToPath _))
        .map(_._1)
        .flatMap(cache.cached _)
        .flatMap(linker.link(_, moduleInitializers, PathOutputDirectory(dir), logger))

      val report = Await.result(result, Duration.Inf)

      if (report.publicModules.size != 1)
        throw new AssertionError(s"got other than 1 module: $report")

      dir.resolve(report.publicModules.head.jsFileName)
    }

    val jsEnvConfig: NodeJSEnv.Config = if (!useWasm) {
      NodeJSEnv.Config()
    } else {
      NodeJSEnv.Config().withArgs(List(
        "--experimental-wasm-exnref",
        "--experimental-wasm-imported-strings", // for JS string builtins
        /* Force using the Turboshaft infrastructure for the optimizing compiler.
         * It appears to be more stable for the Wasm that we throw at it.
         * See also the use of this flag in Build.scala.
         */
        "--turboshaft-wasm"
      ))
    }

    val input =
      if (useESModule) Input.ESModule(sjsCode) :: Nil
      else Input.Script(sjsCode) :: Nil
    val config = RunConfig().withLogger(logger)

    val run = new NodeJSEnv(jsEnvConfig).start(input, config)
    try {
      Await.result(run.future, Duration.Inf)
    } finally {
      run.close()
    }

    true
  }

  private def urlToPath(url: java.net.URL) = {
    try {
      Paths.get(url.toURI())
    } catch {
      case e: java.net.URISyntaxException => Paths.get(url.getPath())
    }
  }
}

object MainGenericRunner extends MainGenericRunner {
  def main(args: Array[String]): Unit = {
    if (!process(args))
      System.exit(1)
  }
}
