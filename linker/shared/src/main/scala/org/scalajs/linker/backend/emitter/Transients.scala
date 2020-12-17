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

  /** A fully resolved method call.
   *
   *  This tree wraps an `ApplyStatically` for which it is guaranteed that the
   *  method is found in the mentioned class (as opposed to being inherited).
   *
   *  This is used as an optimization for `Apply` and `ApplyStatically` nodes:
   *  the emitter will compile those as calls to *static* methods when the
   *  target is only ever called from `ResolvedApply`s.
   *
   *  If the above optimization cannot be applied, the call is compiled as the
   *  underlying `ApplyStatically`. In that case, the `canBeApply` flag
   *  indicates that the underlying call be compiled as a regular method call.
   *  This is useful because, ironically, in JavaScript a monomorphic "virtual"
   *  method call `x.m()` is faster than its "statified" variant
   *  `Class.prototype.m.call(x)`).
   */
  final case class ResolvedApply(canBeApply: Boolean, tree: ApplyStatically)
      extends Transient.Value {

    def printIR(out: IRTreePrinter): Unit = {
      import out._

      val ApplyStatically(flags, receiver, className, method, args) = tree
      print(receiver)
      print(".")
      print(className)
      print("::!")
      print(flags)
      print(method)
      printArgs(args)
    }
  }

  object ResolvedApply {
    object AsTree {
      def apply(canBeApply: Boolean, tree: ApplyStatically): Transient =
        Transient(ResolvedApply(canBeApply, tree))(tree.tpe)(tree.pos)

      def unapply(tree: Transient): Option[(Boolean, ApplyStatically)] = {
        tree match {
          case Transient(ResolvedApply(canBeApply, innerTree)) =>
            Some((canBeApply, innerTree))
          case _ =>
            None
        }
      }
    }
  }

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
