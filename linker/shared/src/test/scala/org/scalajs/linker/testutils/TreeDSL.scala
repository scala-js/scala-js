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

package org.scalajs.linker.testutils

import org.scalajs.ir.Position
import org.scalajs.ir.Trees._

object TreeDSL {
  implicit class TreeOps(private val self: Tree) {
    def +(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(BinaryOp.Int_+, self, that)

    def -(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(BinaryOp.Int_-, self, that)

    def *(that: Tree)(implicit pos: Position): Tree =
      BinaryOp(BinaryOp.Int_*, self, that)

    def unary_-(implicit pos: Position): Tree =
      BinaryOp(BinaryOp.Int_-, IntLiteral(0), self)

    def &(that: Int)(implicit pos: Position): Tree =
      BinaryOp(BinaryOp.Int_&, self, IntLiteral(that))

    def <<(that: Int)(implicit pos: Position): Tree =
      BinaryOp(BinaryOp.Int_<<, self, IntLiteral(that))

    def >>>(that: Int)(implicit pos: Position): Tree =
      BinaryOp(BinaryOp.Int_>>>, self, IntLiteral(that))

    def >>(that: Int)(implicit pos: Position): Tree =
      BinaryOp(BinaryOp.Int_>>, self, IntLiteral(that))
  }
}
