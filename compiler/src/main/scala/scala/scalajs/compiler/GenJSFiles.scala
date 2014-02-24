/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._
import scala.tools.nsc.io.AbstractFile
import scala.reflect.internal.pickling.PickleBuffer

import java.io.{ File, PrintWriter, BufferedOutputStream, FileOutputStream }

/** Send JS ASTs to files
 *
 *  @author Sébastien Doeraene
 */
trait GenJSFiles extends SubComponent { self: GenJSCode =>
  import global._
  import jsAddons._

  def genJSFile(cunit: CompilationUnit, sym: Symbol, tree: js.Tree,
      infoBuilder: ClassInfoBuilder) {
    val outfile = getFileFor(cunit, sym, ".js")
    val output = bufferedPrintWriter(outfile)
    var sourceMapFile: AbstractFile = null
    var sourceMapOutput: PrintWriter = null
    try {
      val printer =
        if (true) { // TODO Some option
          // With source map
          sourceMapFile = getFileFor(cunit, sym, ".js.map")
          sourceMapOutput = bufferedPrintWriter(sourceMapFile)
          new JSTreePrinterWithSourceMap(output, sourceMapOutput,
              outfile.name)
        } else {
          // Without source map
          new JSTreePrinter(output)
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
    val infoWriter = bufferedPrintWriter(infofile)
    try {
      val printer = new JSTreePrinter(infoWriter)
      printer.printTree(infoBuilder.toJSON)
      printer.println()
      printer.close()
    } finally {
      infoWriter.close()
    }
  }

  private def bufferedPrintWriter(file: AbstractFile) =
    new PrintWriter(file.bufferedOutput)

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
