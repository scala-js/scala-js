/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend

import scalajs.JSGlobal

import io.AbstractFile

import scalajs.JSClassPath
import JSClassPath._
import util.{ ClassPath, MergedClassPath, DeltaClassPath }
import scala.tools.util.PathResolver

import scala.reflect.internal.util.Statistics
import symtab.SymbolLoadersStats._

/** The JavaScript platform
 *  Since we try to emulate the Java platform as closely as possible (for now),
 *  we inherit from JavaPlatform.
 *
 *  @author Sébastien Doeraene
 */
trait JSPlatform extends JavaPlatform {
  val global: JSGlobal

  import global._

  private var currentClassPath: Option[MergedClassPath[BinaryRepr]] = None

  override def classPath: ClassPath[BinaryRepr] = {
    if (currentClassPath.isEmpty)
      currentClassPath = Some(new PathResolver(settings,
          if (settings.inline.value) new JavaScriptContext else DefaultJavaScriptContext).result)
    currentClassPath.get
  }

  /** Update classpath with a substituted subentry */
  override def updateClassPath(subst: Map[ClassPath[BinaryRepr], ClassPath[BinaryRepr]]) =
    currentClassPath = Some(new DeltaClassPath(currentClassPath.get, subst))

  override def platformPhases = List(
    flatten,
    genJSCode
  )

  override def newClassLoader(bin: AbstractFile): loaders.SymbolLoader =
    if (bin.name endsWith ".jstype") new JSTypefileLoader(bin)
    else new loaders.ClassfileLoader(bin)

  /** A SymbolLoader for .jstype files */
  class JSTypefileLoader(val typefile: AbstractFile) extends loaders.SymbolLoader with FlagAssigningCompleter {
    import loaders._

    private object typefileParser extends scalajs.JSTypefileParser {
      val global: JSPlatform.this.global.type = JSPlatform.this.global
    }

    protected def description = "jstype file "+ typefile.toString

    protected def doComplete(root: Symbol) {
      val start = if (Statistics.canEnable) Statistics.startTimer(classReadNanos) else null
      typefileParser.parse(typefile, root)
      if (root.associatedFile eq null) {
        root match {
          // In fact, the ModuleSymbol forwards its setter to the module class
          case _: ClassSymbol | _: ModuleSymbol =>
            debuglog("ClassfileLoader setting %s.associatedFile = %s".format(root.name, typefile))
            root.associatedFile = typefile
          case _ =>
            debuglog("Not setting associatedFile to %s because %s is a %s".format(typefile, root.name, root.shortSymbolClass))
        }
      }
      if (Statistics.canEnable) Statistics.stopTimer(classReadNanos, start)
    }

    override def sourcefile: Option[AbstractFile] = typefileParser.srcfile
  }
}
