/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.packager

import java.io._
import java.net.URI

import scala.scalajs.ir

import scala.scalajs.tools.logging._
import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.sourcemap._
import scala.scalajs.tools.corelib.CoreJSLibs

import scala.collection.immutable.{Seq, Traversable}

/** Scala.js packager: blindly concatenates all Scala.js code in a classpath */
class ScalaJSPackager {
  import ScalaJSPackager._

  /**
   *  Desugars any IR into JS and concatenates all scalaJSCode
   *  - Maintains order
   *  - No IR in result
   *  See [[ScalaJSOptimizer.OutputConfig]] for details about the configuration
   *  for the output of this method.
   */
  def packageCP(pcp: PartialClasspath,
      outCfg: OutputConfig, logger: Logger): PartialClasspath = {

    CacheUtils.cached(pcp.version, outCfg.cache) {
      logPackageMsg(outCfg, logger)
      pcp match {
        case pircp: PartialIRClasspath =>
          packageIR(pircp.scalaJSIR, outCfg, logger, addCoreJSLib = false)
        case _ =>
          packageJS(pcp.scalaJSCode, outCfg, logger)
      }
    }

    PartialClasspath(pcp.dependencies, pcp.availableLibs,
        outCfg.output :: Nil, pcp.version)
  }

  /** Desugars any IR into JS and concatenates all cijsCode / scalaJSIR
   *  - Maintains order
   *  - No IR in result
   *  See [[ScalaJSOptimizer.OutputConfig]] for details about the configuration
   *  for the output of this method.
   */
  def packageCP(ccp: CompleteCIClasspath,
      outCfg: OutputConfig, logger: Logger): CompleteCIClasspath = {

    CacheUtils.cached(ccp.version, outCfg.cache) {
      logPackageMsg(outCfg, logger)
      ccp match {
        case circp: CompleteIRClasspath =>
          // The corejslib is implicit in the CompleteIRClasspath
          // therefore we need to add it here.
          packageIR(circp.scalaJSIR, outCfg, logger, addCoreJSLib = true)
        case _ =>
          packageJS(ccp.cijsCode, outCfg, logger)
      }
    }

    CompleteCIClasspath(ccp.jsLibs, outCfg.output :: Nil, ccp.version)
  }

  /** Concatenates ncjsCode
   *  - Maintains order
   *  - No IR in result
   *  See [[ScalaJSOptimizer.OutputConfig]] for details about the configuration
   *  for the output of this method.
   */
  def packageCP(ccp: CompleteNCClasspath,
      outCfg: OutputConfig, logger: Logger): CompleteNCClasspath = {

    CacheUtils.cached(ccp.version, outCfg.cache) {
      logPackageMsg(outCfg, logger)
      packageJS(ccp.ncjsCode, outCfg, logger)
    }

    new CompleteNCClasspath(ccp.jsLibs, outCfg.output :: Nil, ccp.version)
  }

  def packageJS(js: Seq[VirtualJSFile], outCfg: OutputConfig,
      logger: Logger): Unit = {

    val builder = mkBuilder(outCfg)
    builder.addLine("'use strict';")
    js.foreach(builder.addFile _)
    builder.complete()
    builder.closeWriters()
  }

  def packageIR(ir: Traversable[VirtualScalaJSIRFile],
      outCfg: OutputConfig, logger: Logger,
      addCoreJSLib: Boolean = false): Unit = {

    val builder = mkBuilder(outCfg)
    builder.addLine("'use strict';")

    if (addCoreJSLib)
      CoreJSLibs.libs.foreach(builder.addFile _)

    addIR(ir, builder)

    builder.complete()
    builder.closeWriters()
  }

  private def addIR(ir: Traversable[VirtualScalaJSIRFile],
      builder: JSFileBuilder) = {
    /* We can emit the IR tree directly to our builder, instead of emitting each
     * in a virtual file then appending that to the builder.
     * This is mostly important for the source map, because otherwise the
     * intermediate source map has to be parsed again.
     */
     val infoAndTrees = ir.map(_.infoAndTree).toList

     val dummyParents = createDummyParents(infoAndTrees.map(_._1))
     builder.addFile(dummyParents)

     for ((_, tree) <- infoAndTrees.sortBy(_._1.ancestorCount))
       builder.addIRTree(tree)
  }

  private def mkBuilder(outputConfig: OutputConfig) = {
    import outputConfig._

    if (wantSourceMap)
      new JSFileBuilderWithSourceMap(output.name,
          output.contentWriter,
          output.sourceMapWriter,
          relativizeSourceMapBase)
    else
      new JSFileBuilder(output.name, output.contentWriter)
  }

  private def createDummyParents(infos: Seq[ir.Infos.ClassInfo]) = {
    // Hardcode existence of java.lang.Object (O) and java.lang.String (T)
    val existingClasses = Set("O", "T") ++ infos.map(_.encodedName)

    val buf = new StringBuilder
    buf.append("// Generated DummyParents.js file\n\n")

    for {
      info   <- infos
      parent  = info.superClass
      if parent != "" && !existingClasses.contains(parent)
    } {
      buf.append("/** @constructor */\n")
      buf.append(s"ScalaJS.h.$parent = ScalaJS.h.$parent || function() {};\n")
    }

    new MemVirtualJSFile("DummyParents.js").withContent(buf.toString)
  }

  private def logPackageMsg(outCfg: OutputConfig, log: Logger) =
    log.info(s"Packaging ${outCfg.output.path} ...")
}

object ScalaJSPackager {

  /** Configuration for the output of the Scala.js packager. */
  final case class OutputConfig(
      /** Virtual writer for the output file. */
      output: WritableVirtualJSFile,
      /** Cache file */
      cache: Option[WritableVirtualTextFile] = None,
      /** Ask to produce source map for the output. */
      wantSourceMap: Boolean = false,
      /** Base path to relativize paths in the source map. */
      relativizeSourceMapBase: Option[URI] = None
  )

}
