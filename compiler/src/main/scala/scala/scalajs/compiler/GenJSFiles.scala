/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._
import scala.tools.nsc.io.AbstractFile
import scala.reflect.internal.pickling.PickleBuffer

import java.io._

import scala.scalajs.ir.{Trees => js, Printers, SourceMapWriter}

/** Send JS ASTs to files
 *
 *  @author Sébastien Doeraene
 */
trait GenJSFiles extends SubComponent { self: GenJSCode =>
  import global._
  import jsAddons._

  def genIRFileText(cunit: CompilationUnit, sym: Symbol, tree: js.Tree): Unit = {
    val outfile = getFileFor(cunit, sym, ".ir.js")
    val output = bufferedWriter(outfile)
    try {
      val printer = new Printers.IRTreePrinter(output)
      printer.printTopLevelTree(tree)
      printer.close()
    } finally {
      output.close()
    }
  }

  def genJSFile(cunit: CompilationUnit, sym: Symbol, tree: js.Tree,
      infoBuilder: ClassInfoBuilder) {
    val outfile = getFileFor(cunit, sym, ".js")
    val output = bufferedWriter(outfile)
    var sourceMapFile: AbstractFile = null
    var sourceMapOutput: Writer = null
    try {
      val printer =
        if (!scalaJSOpts.noSourceMap) {
          // With source map
          sourceMapFile = getFileFor(cunit, sym, ".js.map")
          sourceMapOutput = bufferedWriter(sourceMapFile)
          val smWriter = new SourceMapWriter(sourceMapOutput, outfile.name)
          new Printers.IRTreePrinterWithSourceMap(output, smWriter)
        } else {
          // Without source map
          new Printers.IRTreePrinter(output)
        }

      printer.printTopLevelTree(tree)

      if (sourceMapFile ne null) {
        printer.print(s"//@ sourceMappingURL=${sourceMapFile.name}")
        printer.println()
      }

      printer.close()
    } finally {
      output.close()
      if (sourceMapOutput ne null)
        sourceMapOutput.close()
    }

    val infofile = getFileFor(cunit, sym, ".sjsinfo")
    val infoWriter = bufferedWriter(infofile)
    try {
      val printer = new Printers.IRTreePrinter(infoWriter)
      printer.printTree(infoBuilder.toJSON)
      printer.println()
      printer.close()
    } finally {
      infoWriter.close()
    }
  }

  private def bufferedWriter(file: AbstractFile): Writer =
    new OutputStreamWriter(file.bufferedOutput, "UTF-8")

  private def getFileFor(cunit: CompilationUnit, sym: Symbol,
      suffix: String) = {
    val baseDir: AbstractFile =
      settings.outputDirs.outputDirFor(cunit.source.file)

    val pathParts = sym.fullName.split("[./]")
    val dir = (baseDir /: pathParts.init)(_.subdirectoryNamed(_))

    var filename = pathParts.last
    if (sym.isModuleClass && !sym.isImplClass)
      filename = filename + nme.MODULE_SUFFIX_STRING

    dir fileNamed (filename + suffix)
  }
}
