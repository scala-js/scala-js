/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package scalajs

import java.io.IOException
import io.AbstractFile

/** This abstract class implements a jstype file parser.
 *
 *  @author Sébastien Doeraene
 *  @version 1.0
 */
abstract class JSTypefileParser {
  val global: Global

  import global._

  protected var busy: Option[Symbol] = None // lock to detect recursive reads

  protected var srcfile0: Option[AbstractFile] = None
  def srcfile = srcfile0

  private object unpickler extends scala.reflect.internal.pickling.UnPickler {
    val global: JSTypefileParser.this.global.type = JSTypefileParser.this.global
  }

  @inline private def pushBusy[T](sym: Symbol)(body: => T): T = {
    busy match {
      case Some(`sym`)  => throw new IOException("unsatisfiable cyclic dependency in '%s'".format(sym))
      case Some(sym1)   => throw new IOException("illegal class file dependency between '%s' and '%s'".format(sym, sym1))
      case _            => ()
    }

    busy = Some(sym)
    try body
    finally busy = None
  }

  def parse(file: AbstractFile, root: Symbol): Unit = {
    debuglog("[class] >> " + root.fullName)

    pushBusy(root) {
      definitions.init()

      val fileBytes = file.toByteArray
      val clazz = if (root.isModule) root.companionClass else root
      // WARNING! do no use clazz.companionModule to find staticModule.
      // In a situation where root can be defined, but its companionClass not,
      // this would give incorrect results (see SI-5031 in separate compilation scenario)
      val staticModule = if (root.isModule) root else root.companionModule

      unpickler.unpickle(fileBytes, 0, clazz, staticModule, file.name)
    }
  }
}
