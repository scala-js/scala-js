package scala.tools.nsc
package backend
package js

import scala.tools.nsc.io.AbstractFile

import java.io.DataOutputStream

/** Send JS ASTs to files */
trait GenJSFiles extends SubComponent {
  val global: scalajs.JSGlobal

  import global._

  def genJSFile(cunit: CompilationUnit, classSym: Symbol, tree: js.Tree) {
    val outfile = getFileFor(cunit, classSym, ".js")
    val output = new java.io.PrintWriter(outfile.bufferedOutput)
    try {
      val printer = new JSTreePrinter(output)
      printer.printTree(tree)
      printer.println()
    } finally {
      output.close()
    }
  }

  private def getFileFor(cunit: CompilationUnit, classSym: Symbol,
      suffix: String) = {
    val baseDir: AbstractFile =
      settings.outputDirs.outputDirFor(cunit.source.file)

    val pathParts = classSym.fullName.split("[./]")
    val dir = (baseDir /: pathParts.init)(_.subdirectoryNamed(_))
    val $_? = if (classSym.hasModuleFlag && !classSym.isImplClass) "$" else ""

    dir.fileNamed(pathParts.last + $_? + suffix)
  }
}
