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

import Fingerprint.FingerprintBuilder

final class OutputPatterns private (
    private[interface] val jsFile: String,
    private[interface] val sourceMapFile: String,
    private[interface] val moduleName: String,
    private[interface] val jsFileURI: String,
    private[interface] val sourceMapURI: String
) {
  def withJSFile(jsFile: String): OutputPatterns =
    copy(jsFile = jsFile)

  def withSourceMapFile(sourceMapFile: String): OutputPatterns =
    copy(sourceMapFile = sourceMapFile)

  def withModuleName(moduleName: String): OutputPatterns =
    copy(moduleName = moduleName)

  def withJSFileURI(jsFileURI: String): OutputPatterns =
    copy(jsFileURI = jsFileURI)

  def withSourceMapURI(sourceMapURI: String): OutputPatterns =
    copy(sourceMapURI = sourceMapURI)

  override def toString(): String = {
    s"""OutputPatterns(
       |  jsFile        = $jsFile,
       |  sourceMapFile = $sourceMapFile,
       |  moduleName    = $moduleName,
       |  jsFileURI     = $jsFileURI,
       |  sourceMapURI  = $sourceMapURI,
       |)""".stripMargin
  }

  private def copy(
      jsFile: String = jsFile,
      sourceMapFile: String = sourceMapFile,
      moduleName: String = moduleName,
      jsFileURI: String = jsFileURI,
      sourceMapURI: String = sourceMapURI): OutputPatterns = {
    new OutputPatterns(jsFile, sourceMapFile, moduleName, jsFileURI, sourceMapURI)
  }
}

object OutputPatterns {
  val Defaults: OutputPatterns = fromJSFile("%s.js")

  def fromJSFile(jsFile: String): OutputPatterns = {
    new OutputPatterns(
        jsFile = jsFile,
        sourceMapFile = s"$jsFile.map",
        moduleName = s"./$jsFile",
        jsFileURI = jsFile,
        sourceMapURI = s"$jsFile.map"
    )
  }

  private[interface] implicit object OutputPatternsFingerprint
      extends Fingerprint[OutputPatterns] {

    override def fingerprint(outputPatterns: OutputPatterns): String = {
      new FingerprintBuilder("OutputPatterns")
        .addField("jsFile", outputPatterns.jsFile)
        .addField("sourceMapFile", outputPatterns.sourceMapFile)
        .addField("moduleName", outputPatterns.moduleName)
        .addField("jsFileURI", outputPatterns.jsFileURI)
        .addField("sourceMapURI", outputPatterns.sourceMapURI)
        .build()
    }
  }
}
