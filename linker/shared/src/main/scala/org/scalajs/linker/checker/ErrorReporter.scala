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

package org.scalajs.linker.checker

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.standard.LinkedClass

private[checker] trait ErrorReporter {
  import ErrorReporter.ErrorContext

  def reportError(msg: String)(implicit ctx: ErrorContext): Unit
}

private[checker] object ErrorReporter {

  /** A string interpolator that displays IR concepts in a nice way. */
  implicit final class InfoStringContext(
      private val self: StringContext)
      extends AnyVal {

    def i(args: Any*): String =
      self.s(args.map(format(_)): _*)

    private def format(arg: Any): String = {
      arg match {
        case arg: LocalName if arg.isThis => "`this`"

        case arg: Name       => arg.nameString
        case arg: FieldName  => arg.nameString
        case arg: MethodName => arg.displayName
        case arg: IRNode     => arg.show
        case arg: TypeRef    => arg.displayName
        case arg: Type       => arg.show()
        case _               => arg.toString()
      }
    }
  }

  /** The context in which to report IR check errors.
   *
   *  The way this class is written is optimized for the happy path, where no
   *  error occurs. In that case, `toString()` is never called, and we avoid
   *  any kind of allocation.
   *
   *  The parameter is an `Any` for that reason. It should be an
   *  `Either[IRNode, LinkedClass]`, but that would also require an allocation
   *  of the `Left` or `Right` (in fact, we'd love to type it as
   *  `IRNode | LinkedClass`). `ErrorContext` is also made an `AnyVal` for the
   *  same reasons, again.
   *
   *  If `toString()` is called, we're in a bad situation anyway, because the
   *  IR is invalid, so all bets are off and we can be slow and allocate stuff;
   *  we don't care.
   */
  final class ErrorContext private (private val nodeOrLinkedClass: Any) extends AnyVal {

    override def toString(): String = {
      val (pos, name) = nodeOrLinkedClass match {
        case tree: IRNode             => (tree.pos, tree.getClass.getSimpleName)
        case linkedClass: LinkedClass => (linkedClass.pos, "ClassDef")
      }
      s"${pos.source}(${pos.line + 1}:${pos.column + 1}:$name)"
    }
  }

  object ErrorContext {
    def apply(node: IRNode): ErrorContext =
      new ErrorContext(node)

    def apply(linkedClass: LinkedClass): ErrorContext =
      new ErrorContext(linkedClass)
  }
}
