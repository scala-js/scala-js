package scala.scalajs.sbtplugin.optimizer

import scala.collection.mutable

import sbt._

import scala.scalajs.sbtplugin.Utils._
import OptData._

/** Scala.js optimizer: does type-aware global dce. */
class ScalaJSOptimizer(logger: Logger) {
  def optimize(inputs: Seq[File], output: File,
      relativeSourceMaps: Boolean): Unit = {

    val (classFiles, otherFiles) = inputs.partition(isScalaJSClassFile)
    val infoFiles0 = classFiles.map(changeExt(_, ".js", ".sjsinfo"))

    val coreJSLibDir = otherFiles.head.getParentFile
    val infoFiles = Seq(
        coreJSLibDir / "javalangObject.sjsinfo",
        coreJSLibDir / "javalangString.sjsinfo") ++ infoFiles0

    val analyzer = parseInfoFiles(infoFiles)
    analyzer.computeReachability()
  }

  def parseInfoFiles(infoFiles: Seq[File]): Analyzer = {
    val allData = infoFiles map readData
    new Analyzer(logger, allData)
  }
}
