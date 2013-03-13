/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package scalajs

import scala.collection.{ mutable, immutable }

import util._
import io.AbstractFile

object JSClassPath {
  import ClassPath._

  /** A classpath context for JavaScript compilation
   *
   *  It is aware of .jstype files.
   */
  class JavaScriptContext extends JavaContext {
    override def toBinaryName(rep: AbstractFile) = {
      val name = rep.name
      if (endsJSType(name))
        name.substring(0, name.length - 7)
      else {
        assert(endsClass(name), name)
        name.substring(0, name.length - 6)
      }
    }

    override def newClassPath(dir: AbstractFile) =
      new JSDirectoryClassPath(dir, this)
  }

  object DefaultJavaScriptContext extends JavaScriptContext {
    override def isValidName(name: String) = !isTraitImplementation(name)
  }

  private[scalajs] def endsClass(s: String) = s.length > 6 && s.substring(s.length - 6) == ".class"
  private[scalajs] def endsJSType(s: String) = s.length > 7 && s.substring(s.length - 7) == ".jstype"
}

import ClassPath._
import JSClassPath._

/** A directory classpath for JavaScript compilation
 *
 *  It enables loading symbols from .jstype files in addition to .class files.
 *
 *  This class should not be needed. It exists solely so that we can
 *  override validClassFile(), and we need to to that because the initial
 *  definition tests whether the file name ends in ".class".
 *
 *  I strongly think this check should be delegated to the *context*, not the
 *  classpath. This would get us rid of this class.
 */
class JSDirectoryClassPath(dir: AbstractFile,
    context: ClassPathContext[AbstractFile])
    extends DirectoryClassPath(dir, context) {

  override def validClassFile(name: String) = {
    super.validClassFile(name) ||
      (endsJSType(name) && context.isValidName(name))
  }

  /* !!! Horrible duplicate of the algorithm in DirectoryClassPath
   */

  // calculates (packages, classes) in one traversal.
  private def traverse() = {
    val classBuf   = immutable.Vector.newBuilder[ClassRep]
    val packageBuf = immutable.Vector.newBuilder[DirectoryClassPath]
    dir foreach { f =>
      if (!f.isDirectory && validClassFile(f.name))
        classBuf += ClassRep(Some(f), None)
      else if (f.isDirectory && validPackage(f.name))
        packageBuf += new JSDirectoryClassPath(f, context)
    }
    (packageBuf.result, classBuf.result)
  }

  override lazy val (packages, classes) = traverse()
}
