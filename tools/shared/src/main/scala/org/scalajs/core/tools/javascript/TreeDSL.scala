/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.javascript

import scala.language.implicitConversions

import org.scalajs.core.ir.Position

import Trees._

private[javascript] object TreeDSL {
  implicit class TreeOps(val self: Tree) extends AnyVal {
    /** Select a member */
    def DOT(field: Ident)(implicit pos: Position): DotSelect =
      DotSelect(self, field)

    /** Select a member */
    def DOT(field: String)(implicit pos: Position): DotSelect =
      DotSelect(self, Ident(field))

    // Some operators that we use

    def ===(that: Tree)(implicit pos: Position): Tree =
      BinaryOp("===", self, that)
    def ===(that: String)(implicit pos: Position): Tree =
      BinaryOp("===", self, StringLiteral(that))

    def unary_!()(implicit pos: Position): Tree =
      UnaryOp("!", self)
    def &&(that: Tree)(implicit pos: Position): Tree =
      BinaryOp("&&", self, that)
    def ||(that: Tree)(implicit pos: Position): Tree =
      BinaryOp("||", self, that)

    // Other constructs

    def :=(that: Tree)(implicit pos: Position): Tree =
      Assign(self, that)
  }

  def typeof(expr: Tree)(implicit pos: Position): Tree =
    UnaryOp("typeof", expr)
}
