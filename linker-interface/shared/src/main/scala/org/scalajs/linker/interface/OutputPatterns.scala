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

/** Output patterns configure how the linker names/refers to modules it created.
 *
 *  Internally, the linker refers to every module using an abstract module ID.
 *
 *  For public modules (i.e. modules that can be imported), the module ID is
 *  provided by the user.
 *
 *  For internal modules (i.e. modules used to share code), the module ID is
 *  generated by the linker.
 *
 *  Currently, all `with*` methods expect a [[java.util.Formatter Formatter]]
 *  pattern taking the module ID as sole string (`%s`) argument.
 */
final class OutputPatterns private (
    private[interface] val jsFile: String,
    private[interface] val sourceMapFile: String,
    private[interface] val moduleName: String,
    private[interface] val jsFileURI: String,
    private[interface] val sourceMapURI: String
) {
  /** Pattern for the JS file name (the file containing the module's code). */
  def withJSFile(jsFile: String): OutputPatterns =
    copy(jsFile = jsFile)

  /** Pattern for the file name of the source map file of the JS file. */
  def withSourceMapFile(sourceMapFile: String): OutputPatterns =
    copy(sourceMapFile = sourceMapFile)

  /** Pattern for the module name (the string used to import a module). */
  def withModuleName(moduleName: String): OutputPatterns =
    copy(moduleName = moduleName)

  /** Pattern for the "file" field in the source map. */
  def withJSFileURI(jsFileURI: String): OutputPatterns =
    copy(jsFileURI = jsFileURI)

  /** Pattern for the source map URI in the JS file. */
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
  /** Default [[OutputPatterns]]; equivalent to `fromJSFile("%s.js")`. */
  val Defaults: OutputPatterns = fromJSFile("%s.js")

  /** Creates [[OutputPatterns]] from a JS file pattern.
   *
   *  Other patterns are derived from the JS file pattern as follows:
   *  - `sourceMapFile`: ".map" is appended.
   *  - `moduleName`: "./" is prepended (relative path import).
   *  - `jsFileURI`: relative URI (same as the provided pattern).
   *  - `sourceMapURI`: relative URI (same as `sourceMapFile`).
   */
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
