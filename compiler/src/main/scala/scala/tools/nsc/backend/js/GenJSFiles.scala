package scala.tools.nsc
package backend
package js

import scala.tools.nsc.io.AbstractFile
import scala.reflect.internal.pickling.PickleBuffer

import java.io.DataOutputStream

/** Send JS ASTs to files */
trait GenJSFiles extends SubComponent {
  val global: scalajs.JSGlobal

  import global._

  def genJSFiles(cunit: CompilationUnit, classSym: Symbol, tree: js.Tree) {
    val jsClassName =
      if (classSym.hasModuleFlag && !classSym.isImplClass) classSym.fullName+"$"
      else classSym.fullName
    val isModuleName = jsClassName.endsWith("$")

    genJSFile(cunit, jsClassName, tree)

    def shouldEmitPickle(): Boolean = {
      /* This should return
       * (!isModuleName || (classSym.linkedClassOfClass == NoSymbol))
       * but this condition will sometimes crash with an assertion error for
       * package object classes.
       * Hence we always return true. This might emit the same pickle twice.
       */
      true
    }

    if (shouldEmitPickle()) {
      val pickleSym = if (isModuleName) classSym.companionModule else classSym
      currentRun.symData.get(pickleSym) match {
        case Some(pickleBuffer) =>
          val nonModuleClassName =
            if (isModuleName) jsClassName.substring(0, jsClassName.length()-1)
            else jsClassName
          genJSTypeFile(cunit, nonModuleClassName, pickleBuffer)

        case None => ()
      }
    }
  }

  private def genJSFile(cunit: CompilationUnit, jsClassName: String,
      tree: js.Tree) {
    val outfile = getFileFor(cunit, jsClassName, ".js")
    val output = new java.io.PrintWriter(outfile.bufferedOutput)
    try {
      val printer = new JSTreePrinter(output)
      printer.printTree(tree)
      printer.print(";")
      printer.println()
    } finally {
      output.close()
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
