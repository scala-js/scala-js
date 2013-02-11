/* JSM - JavaScript manipulation library
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.tools.jsm
package ast

/** JavaScript ASTs
 *
 *  @author Sébastien Doeraene
 */
object Trees {
  import Names._

  type Position = scala.util.parsing.input.Position
  val NoPosition = scala.util.parsing.input.NoPosition

  abstract sealed class Tree {
    def pos: Position
  }

  case object EmptyTree extends Tree {
    def pos = NoPosition
  }

  // Definitions

  case class VarDef(name: IdentifierName, rhs: Tree)(implicit val pos: Position) extends Tree

  case class FunDef(name: IdentifierName, args: List[IdentifierName], body: Tree)(implicit val pos: Position) extends Tree

  // Statement-only language constructs

  case class Skip()(implicit val pos: Position) extends Tree

  case class Block(stats: List[Tree], expr: Tree)(implicit val pos: Position) extends Tree

  case class Assign(lhs: Tree, rhs: Tree)(implicit val pos: Position) extends Tree

  case class Return(expr: Tree)(implicit val pos: Position) extends Tree

  case class If(cond: Tree, thenp: Tree, elsep: Tree)(implicit val pos: Position) extends Tree

  case class While(cond: Tree, body: Tree)(implicit val pos: Position) extends Tree

  case class Try(block: Tree, errVar: IdentifierName, handler: Tree, finalizer: Tree)(implicit val pos: Position) extends Tree

  case class Throw(expr: Tree)(implicit val pos: Position) extends Tree

  case class Break()(implicit val pos: Position) extends Tree

  case class Continue()(implicit val pos: Position) extends Tree

  // Expressions

  case class Ident(name: IdentifierName)(implicit val pos: Position) extends Tree

  case class PropIdent(name: PropertyName)(implicit val pos: Position) extends Tree

  case class Select(qualifier: Tree, item: Tree)(implicit val pos: Position) extends Tree

  case class Apply(fun: Tree, args: List[Tree])(implicit val pos: Position) extends Tree

  case class Function(args: List[IdentifierName], body: Tree)(implicit val pos: Position) extends Tree

  case class UnaryOp(op: String, lhs: Tree)(implicit val pos: Position) extends Tree

  case class BinaryOp(op: String, lhs: Tree, rhs: Tree)(implicit val pos: Position) extends Tree

  case class New(fun: IdentifierName, args: List[Tree])(implicit val pos: Position) extends Tree

  case class This()(implicit val pos: Position) extends Tree

  // Literals

  sealed trait Literal extends Tree

  case class Undefined()(implicit val pos: Position) extends Literal

  case class Null()(implicit val pos: Position) extends Literal

  case class BooleanLiteral(value: Boolean)(implicit val pos: Position) extends Literal

  case class IntLiteral(value: Long)(implicit val pos: Position) extends Literal

  case class DoubleLiteral(value: Double)(implicit val pos: Position) extends Literal

  case class StringLiteral(value: String)(implicit val pos: Position) extends Literal

  // Compounds

  case class ArrayConstr(items: List[Tree])(implicit val pos: Position) extends Tree

  case class ObjectConstr(fields: List[(PropertyName, Tree)])(implicit val pos: Position) extends Tree

  // Classes - from ECMAScript 6, can be desugared into other concepts

  case class ClassDef(name: IdentifierName, parents: List[Tree], defs: List[Tree])(implicit val pos: Position) extends Tree

  case class MethodDef(name: PropertyName, args: List[IdentifierName], body: Tree)(implicit val pos: Position) extends Tree

  case class GetterDef(name: PropertyName, body: Tree)(implicit val pos: Position) extends Tree

  case class SetterDef(name: PropertyName, arg: IdentifierName, body: Tree)(implicit val pos: Position) extends Tree

  case class Super()(implicit val pos: Position) extends Tree

  // Some derivatives

  object ApplyMethod {
    def apply(receiver: Tree, method: PropIdent, args: List[Tree])(implicit pos: Position) =
      Apply(Select(receiver, method), args)

    def unapply(tree: Apply): Option[(Tree, PropIdent, List[Tree])] = {
      tree match {
        case Apply(Select(receiver, method : PropIdent), args) =>
          Some((receiver, method, args))
        case _ =>
          None
      }
    }
  }
}
