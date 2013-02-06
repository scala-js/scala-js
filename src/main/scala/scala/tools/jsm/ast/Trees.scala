/* JSM - JavaScript manipulation library
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.tools.jsm
package ast

import scala.util.parsing.input.{ Position, NoPosition }

/** JavaScript ASTs
 *
 *  @author Sébastien Doeraene
 */
object Trees {
  import Names._

  abstract class Tree {
    def pos: Position
  }

  // Definitions

  case class VarDef(name: IdentifierName)(implicit val pos: Position) extends Tree

  case class FunDef(name: IdentifierName, args: List[IdentifierName], body: Tree)(implicit val pos: Position) extends Tree

  // Statement-only language constructs

  case class Block(stats: List[Tree])(implicit val pos: Position) extends Tree

  case class Assign(lhs: Tree, rhs: Tree)(implicit val pos: Position) extends Tree

  case class Return(expr: Tree)(implicit val pos: Position) extends Tree

  case class If(cond: Tree, thenp: Tree, elsep: Tree)(implicit val pos: Position) extends Tree

  case class While(cond: Tree, body: Tree)(implicit val pos: Position) extends Tree

  case class Try(block: Tree, errVar: IdentifierName, handler: Tree)(implicit val pos: Position) extends Tree

  case class Break()(implicit val pos: Position) extends Tree

  case class Continue()(implicit val pos: Position) extends Tree

  // Expressions

  case class Ident(name: IdentifierName)(implicit val pos: Position) extends Tree

  case class Select(qualifier: Tree, item: Tree)(implicit val pos: Position) extends Tree

  case class Apply(fun: Tree, args: List[Tree])(implicit val pos: Position) extends Tree

  case class Function(args: List[IdentifierName], body: Tree)(implicit val pos: Position) extends Tree

  case class UnaryOp(op: String, lhs: Tree)(implicit val pos: Position) extends Tree

  case class BinaryOp(op: String, lhs: Tree, rhs: Tree)(implicit val pos: Position) extends Tree

  case class New(fun: IdentifierName, args: List[Tree])(implicit val pos: Position) extends Tree

  case class This()(implicit val pos: Position) extends Tree

  // Literals

  case class IntLiteral(value: Long)(implicit val pos: Position) extends Tree

  case class DoubleLiteral(value: Double)(implicit val pos: Position) extends Tree

  case class StringLiteral(value: String)(implicit val pos: Position) extends Tree

  // Compounds

  case class ArrayConstr(items: List[Tree])(implicit val pos: Position) extends Tree

  case class ObjectConstr(fields: List[(PropertyName, Tree)])(implicit val pos: Position) extends Tree

  // Classes - from ECMAScript 6, can be desugared into other concepts

  case class ClassDef(name: IdentifierName, parents: List[Tree], defs: List[Tree])(implicit val pos: Position) extends Tree

  case class MethodDef(name: PropertyName, args: List[IdentifierName], body: Tree)(implicit val pos: Position) extends Tree

  case class GetterDef(name: PropertyName, body: Tree)(implicit val pos: Position) extends Tree

  case class SetterDef(name: PropertyName, arg: IdentifierName, body: Tree)(implicit val pos: Position) extends Tree

  case class Super()(implicit val pos: Position) extends Tree
}
