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

import scala.language.implicitConversions

import org.scalajs.ir
import org.scalajs.ir.Position

import org.scalajs.linker.backend.javascript.Trees._

private[emitter] object TreeDSL {
  implicit class TreeOps private[TreeDSL] (val __private_self: Tree)
      extends AnyVal {

    @inline private def self: Tree = __private_self

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

    def unary_+()(implicit pos: Position): Tree =
      UnaryOp(ir.Trees.JSUnaryOp.+, self)
    def unary_-()(implicit pos: Position): Tree =
      UnaryOp(ir.Trees.JSUnaryOp.-, self)
    def unary_!()(implicit pos: Position): Tree =
      UnaryOp(ir.Trees.JSUnaryOp.!, self)

    def &&(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.&&, self, that)
    def ||(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.||, self, that)

    def +(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.+, self, that)
    def -(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.-, self, that)
    def *(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.*, self, that)
    def /(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp./, self, that)
    def %(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.%, self, that)

    def &(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.&, self, that)
    def |(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.|, self, that)
    def |(that: Int)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.|, self, IntLiteral(that))

    def <<(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.<<, self, that)
    def >>(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.>>, self, that)
    def >>>(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.>>>, self, that)

    def <(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.<, self, that)
    def >(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.>, self, that)
    def <=(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.<=, self, that)
    def >=(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.>=, self, that)

    def prefix_++(implicit pos: Position): Tree =
      IncDec(prefix = true, inc = true, self)
    def prefix_--(implicit pos: Position): Tree =
      IncDec(prefix = true, inc = false, self)
    def ++(implicit pos: Position): Tree =
      IncDec(prefix = false, inc = true, self)
    def --(implicit pos: Position): Tree =
      IncDec(prefix = false, inc = false, self)

    def in(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(ir.Trees.JSBinaryOp.in, self, that)
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
