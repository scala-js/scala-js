/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2018, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.core.tools.linker.backend.emitter

import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Printers._

object Transients {

  final case class CallHelper(helper: String, args: List[Tree])
      extends Transient.Value {

    def printIR(out: IRTreePrinter): Unit = {
      out.print("$callHelper(")
      out.print(helper)
      for (arg <- args) {
        out.print(", ")
        out.print(arg)
      }
      out.print(")")
    }
  }

}
