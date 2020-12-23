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

import org.scalajs.ir.Position
import org.scalajs.ir.Printers._
import org.scalajs.ir.Transformers._
import org.scalajs.ir.Traversers._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

object Transients {

  final case class CallHelper(helper: String, args: List[Tree])(val tpe: Type)
      extends Transient.Value {

    def traverse(traverser: Traverser): Unit =
      args.foreach(traverser.traverse(_))

    def transform(transformer: Transformer, isStat: Boolean)(
        implicit pos: Position): Tree = {
      Transient(CallHelper(helper, args.map(transformer.transformExpr(_)))(tpe))
    }

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
