/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package org.scalajs.core.compiler

import scala.tools.nsc._
import scala.tools.nsc.io.AbstractFile
import scala.reflect.internal.pickling.PickleBuffer

import java.io._

import org.scalajs.core.ir
import ir.Infos

/** Send JS ASTs to files
 *
 *  @author Sébastien Doeraene
 */
trait GenJSFiles extends SubComponent { self: GenJSCode =>
  import global._
  import jsAddons._

  def genIRFile(cunit: CompilationUnit, sym: Symbol, suffix: Option[String],
      tree: ir.Trees.ClassDef): Unit = {
    val outfile = getFileFor(cunit, sym, suffix.getOrElse("") + ".sjsir")
    val output = outfile.bufferedOutput
    try {
      ir.InfoSerializers.serialize(output, Infos.generateClassInfo(tree))
      ir.Serializers.serialize(output, tree)
    } catch {
      case e: ir.InvalidIRException =>
        e.tree match {
          case ir.Trees.UndefinedParam() =>
            reporter.error(sym.pos, "Found a dangling UndefinedParam at " +
                s"${e.tree.pos}. This is likely due to a bad interaction " +
                "between a macro or a compiler plugin and the Scala.js " +
                "compiler plugin. If you hit this, please let us know.")

          case _ =>
            reporter.error(sym.pos, "The Scala.js compiler generated " +
                "invalid IR for this class. Please report this as a bug. IR: " +
                e.tree)
        }
    } finally {
      output.close()
    }
  }

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
