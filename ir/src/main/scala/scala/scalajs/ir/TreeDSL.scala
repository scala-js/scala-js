/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

import scala.language.implicitConversions

import Trees._
import Types._

private[ir] object TreeDSL {
  implicit class TreeOps(val self: Tree) extends AnyVal {
    /** Select a member */
    def DOT(field: Ident)(implicit pos: Position): JSDotSelect =
      JSDotSelect(self, field)

    /** Select a member */
    def DOT(field: String)(implicit pos: Position): JSDotSelect =
      JSDotSelect(self, Ident(field))

    // Some operators that we use

    def ===(that: Tree)(implicit pos: Position): Tree =
      JSBinaryOp("===", self, that)
    def ===(that: String)(implicit pos: Position): Tree =
      JSBinaryOp("===", self, StringLiteral(that))

    def unary_!()(implicit pos: Position): Tree =
      JSUnaryOp("!", self)
    def &&(that: Tree)(implicit pos: Position): Tree =
      JSBinaryOp("&&", self, that)
    def ||(that: Tree)(implicit pos: Position): Tree =
      JSBinaryOp("||", self, that)

    // Other constructs

    def :=(that: Tree)(implicit pos: Position): Tree =
      Assign(self, that)
  }

  def typeof(expr: Tree)(implicit pos: Position): Tree =
    JSUnaryOp("typeof", expr)
}
