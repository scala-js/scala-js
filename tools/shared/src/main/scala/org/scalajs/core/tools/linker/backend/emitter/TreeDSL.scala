/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.backend.emitter

import scala.language.implicitConversions

import org.scalajs.core.ir
import org.scalajs.core.ir.Position

import org.scalajs.core.tools.javascript.Trees._

private[emitter] object TreeDSL {
  implicit class TreeOps(val self: Tree) extends AnyVal {
    /** Select a member */
    def DOT(field: Ident)(implicit pos: Position): DotSelect =
      DotSelect(self, field)

    /** Select a member */
    def DOT(field: String)(implicit pos: Position): DotSelect =
      DotSelect(self, Ident(field))

    // Some operators that we use

    def ===(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.===, self, that)
    def ===(that: String)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.===, self, StringLiteral(that))

    def !==(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.!==, self, that)

    def unary_!()(implicit pos: Position): Tree =
      UnaryOp(ir.Trees.JSUnaryOp.!, self)
    def &&(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.&&, self, that)
    def ||(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.||, self, that)

    // Other constructs

    def :=(that: Tree)(implicit pos: Position): Tree =
      Assign(self, that)
  }

  def typeof(expr: Tree)(implicit pos: Position): Tree =
    UnaryOp(ir.Trees.JSUnaryOp.typeof, expr)
}
