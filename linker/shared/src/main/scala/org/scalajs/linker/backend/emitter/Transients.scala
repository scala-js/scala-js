/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.backend.emitter

import org.scalajs.ir.Trees._
import org.scalajs.ir.Printers._

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
