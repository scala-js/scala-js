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

    def instanceof(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.instanceof, self, that)

    // Other constructs

    def :=(that: Tree)(implicit pos: Position): Tree =
      Assign(self, that)

    def prototype(implicit pos: Position): Tree = self DOT "prototype"
  }

  def typeof(expr: Tree)(implicit pos: Position): Tree =
    UnaryOp(ir.Trees.JSUnaryOp.typeof, expr)
}
