package scala.tools.nsc
package backend
package js

import scala.tools.nsc.io.AbstractFile
import scala.reflect.internal.pickling.PickleBuffer

import java.io.PrintWriter

/** Send JS ASTs to files */
trait GenJSFiles extends SubComponent { self: GenJSCode =>
  import global._
  import jsAddons._

  def genJSTypeFile(cunit: CompilationUnit, representative: Symbol) {
    val pickleSym =
      if (representative.isModuleClass) representative.companionModule
      else representative

    currentRun.symData.get(pickleSym) foreach { pickleBuffer =>
      genJSTypeFile(cunit, representative, pickleBuffer)
    }
  }

  def genJSFile(cunit: CompilationUnit, sym: Symbol, tree: js.Tree) {
    val outfile = getFileFor(cunit, sym, ".js", true)
    val output = new PrintWriter(outfile.bufferedOutput)
    var sourceMapFile: AbstractFile = null
    var sourceMapOutput: PrintWriter = null
    try {
      val printer =
        if (true) { // TODO Some option
          // With source map
          sourceMapFile = getFileFor(cunit, sym, ".js.map", true)
          sourceMapOutput = new PrintWriter(sourceMapFile.bufferedOutput)
          new JSTreePrinterWithSourceMap(output, sourceMapOutput, outfile.name)
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
  }

  private def genJSTypeFile(cunit: CompilationUnit, sym: Symbol,
      pickleBuffer: PickleBuffer) {
    val outfile = getFileFor(cunit, sym, ".jstype", false)
    val output = outfile.bufferedOutput
    try {
      output.write(pickleBuffer.bytes, 0, pickleBuffer.writeIndex)
    } finally {
      output.close()
    }
  }

  private def getFileFor(cunit: CompilationUnit, sym: Symbol,
      suffix: String, withOrderingPrefix: Boolean) = {
    val baseDir: AbstractFile =
      settings.outputDirs.outputDirFor(cunit.source.file)

    val pathParts = sym.fullName.split("[./]")
    val dir = (baseDir /: pathParts.init)(_.subdirectoryNamed(_))

    var filename = pathParts.last
    if (withOrderingPrefix) {
      filename = getOrderingPrefixFor(sym) + filename
      if (sym.isModuleClass && !sym.isImplClass)
        filename = filename + nme.MODULE_SUFFIX_STRING
    }

    dir.fileNamed(filename + suffix)
  }

  private def getOrderingPrefixFor(sym: Symbol): String = {
    val ordering = sym.ancestors.count(!_.isInterface) + 1
    "%04d-" format ordering
  }
}
