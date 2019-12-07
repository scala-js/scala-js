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

package org.scalajs.nscplugin

import scala.tools.nsc._
import scala.tools.nsc.io.AbstractFile
import scala.reflect.internal.pickling.PickleBuffer

import java.io._

import org.scalajs.ir

/** Send JS ASTs to files
 *
 *  @author Sébastien Doeraene
 */
trait GenJSFiles[G <: Global with Singleton] extends SubComponent {
  self: GenJSCode[G] =>

  import global._
  import jsAddons._

  def genIRFile(cunit: CompilationUnit, tree: ir.Trees.ClassDef): Unit = {
    val outfile = getFileFor(cunit, tree.name.name, ".sjsir")
    val output = outfile.bufferedOutput
    try ir.Serializers.serialize(output, tree)
    finally output.close()
  }

  private def getFileFor(cunit: CompilationUnit, className: ir.Names.ClassName,
      suffix: String): AbstractFile = {
    val baseDir: AbstractFile =
      settings.outputDirs.outputDirFor(cunit.source.file)

    val pathParts = className.nameString.split('.')
    val dir = pathParts.init.foldLeft(baseDir)(_.subdirectoryNamed(_))
    val filename = pathParts.last
    dir.fileNamed(filename + suffix)
  }
}
