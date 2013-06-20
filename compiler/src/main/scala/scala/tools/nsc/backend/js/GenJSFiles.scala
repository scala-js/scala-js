package scala.tools.nsc
package backend
package js

import scala.tools.nsc.io.AbstractFile
import scala.reflect.internal.pickling.PickleBuffer

import java.io.PrintWriter

/** Send JS ASTs to files */
trait GenJSFiles extends SubComponent {
  val global: scalajs.JSGlobal

  import global._

  def genJSFiles(cunit: CompilationUnit, representative: Symbol, tree: js.Tree) {
    val jsClassName = representative.fullName

    if (tree != js.EmptyTree)
      genJSFile(cunit, jsClassName, tree)

    val pickleSym =
      if (representative.isModuleClass) representative.companionModule
      else representative

    currentRun.symData.get(pickleSym) match {
      case Some(pickleBuffer) =>
        genJSTypeFile(cunit, jsClassName, pickleBuffer)

      case None => ()
    }
  }

  private def genJSFile(cunit: CompilationUnit, jsClassName: String,
      tree: js.Tree) {
    val outfile = getFileFor(cunit, jsClassName, ".js")
    val output = new PrintWriter(outfile.bufferedOutput)
    var sourceMapFile: AbstractFile = null
    var sourceMapOutput: PrintWriter = null
    try {
      val printer =
        if (true) { // TODO Some option
          // With source map
          sourceMapFile = getFileFor(cunit, jsClassName, ".js.map")
          sourceMapOutput = new PrintWriter(sourceMapFile.bufferedOutput)
          new JSTreePrinterWithSourceMap(output, sourceMapOutput, outfile.name)
        } else {
          // Without source map
          new JSTreePrinter(output)
        }

      printer.printTree(tree)
      printer.print(";")
      printer.println()

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

  private def genJSTypeFile(cunit: CompilationUnit, jsClassName: String,
      pickleBuffer: PickleBuffer) {
    val outfile = getFileFor(cunit, jsClassName, ".jstype")
    val output = outfile.bufferedOutput
    try {
      output.write(pickleBuffer.bytes, 0, pickleBuffer.writeIndex)
    } finally {
      output.close()
    }
  }

  private def getFileFor(cunit: CompilationUnit, jsClassName: String,
      suffix: String) = {
    val baseDir: AbstractFile =
      settings.outputDirs.outputDirFor(cunit.source.file)

    val pathParts = jsClassName.split("[./]")
    val dir = (baseDir /: pathParts.init)(_.subdirectoryNamed(_))

    dir.fileNamed(pathParts.last + suffix)
  }
}
