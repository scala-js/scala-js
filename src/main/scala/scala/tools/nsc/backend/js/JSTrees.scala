/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package js

/** JavaScript ASTs
 *
 *  @author Sébastien Doeraene
 */
trait JSTrees { self: scalajs.JSGlobal =>
  object js {
    // Tree

    abstract sealed class Tree {
      def pos: Position
    }

    case object EmptyTree extends Tree {
      def pos = NoPosition
    }

    // Identifiers and properties

    sealed trait PropertyName extends Tree {
      def name: String
    }

    object PropertyName {
      def apply(name: String)(implicit pos: Position): PropertyName = {
        if (isValidIdentifier(name)) Ident(name)
        else StringLiteral(name)
      }

      def unapply(tree: PropertyName): Some[String] =
        Some(tree.name)
    }

    case class Ident(name: String)(implicit val pos: Position) extends Tree with PropertyName {
      requireValidIdent(name)
    }

    final def isValidIdentifier(name: String): Boolean = {
      !name.head.isDigit && name.tail.forall(isIdentChar)
    }

    private def isIdentChar(c: Char) =
      c.isLetterOrDigit || c == '$' || c == '_'

    @inline final def requireValidIdent(name: String) {
      require(isValidIdentifier(name), s"${name} is not a valid identifier")
    }

    // Definitions

    case class VarDef(name: Ident, rhs: Tree)(implicit val pos: Position) extends Tree

    case class FunDef(name: Ident, args: List[Ident], body: Tree)(implicit val pos: Position) extends Tree

    // Statement-only language constructs

    case class Skip()(implicit val pos: Position) extends Tree

    case class Block(stats: List[Tree], expr: Tree)(implicit val pos: Position) extends Tree

    case class Assign(lhs: Tree, rhs: Tree)(implicit val pos: Position) extends Tree

    case class Return(expr: Tree)(implicit val pos: Position) extends Tree

    case class If(cond: Tree, thenp: Tree, elsep: Tree)(implicit val pos: Position) extends Tree

    case class While(cond: Tree, body: Tree)(implicit val pos: Position) extends Tree

    case class Try(block: Tree, errVar: Ident, handler: Tree, finalizer: Tree)(implicit val pos: Position) extends Tree

    case class Throw(expr: Tree)(implicit val pos: Position) extends Tree

    case class Break()(implicit val pos: Position) extends Tree

    case class Continue()(implicit val pos: Position) extends Tree

    // Expressions

    case class DotSelect(qualifier: Tree, item: Ident)(implicit val pos: Position) extends Tree

    case class BracketSelect(qualifier: Tree, item: Tree)(implicit val pos: Position) extends Tree

    case class Apply(fun: Tree, args: List[Tree])(implicit val pos: Position) extends Tree

    case class Function(args: List[Ident], body: Tree)(implicit val pos: Position) extends Tree

    case class UnaryOp(op: String, lhs: Tree)(implicit val pos: Position) extends Tree

    case class BinaryOp(op: String, lhs: Tree, rhs: Tree)(implicit val pos: Position) extends Tree

    case class New(fun: Ident, args: List[Tree])(implicit val pos: Position) extends Tree

    case class This()(implicit val pos: Position) extends Tree

    // Literals

    sealed trait Literal extends Tree

    case class Undefined()(implicit val pos: Position) extends Literal

    case class Null()(implicit val pos: Position) extends Literal

    case class BooleanLiteral(value: Boolean)(implicit val pos: Position) extends Literal

    case class IntLiteral(value: Long)(implicit val pos: Position) extends Literal

    case class DoubleLiteral(value: Double)(implicit val pos: Position) extends Literal

    case class StringLiteral(value: String)(implicit val pos: Position) extends Literal with PropertyName {
      override def name = value
    }

    // Compounds

    case class ArrayConstr(items: List[Tree])(implicit val pos: Position) extends Tree

    case class ObjectConstr(fields: List[(PropertyName, Tree)])(implicit val pos: Position) extends Tree

    // Classes - from ECMAScript 6, can be desugared into other concepts

    case class ClassDef(name: Ident, parents: List[Tree], defs: List[Tree])(implicit val pos: Position) extends Tree

    case class MethodDef(name: PropertyName, args: List[Ident], body: Tree)(implicit val pos: Position) extends Tree

    case class GetterDef(name: PropertyName, body: Tree)(implicit val pos: Position) extends Tree

    case class SetterDef(name: PropertyName, arg: Ident, body: Tree)(implicit val pos: Position) extends Tree

    case class Super()(implicit val pos: Position) extends Tree

    // Some derivatives

    object Select {
      def apply(qualifier: Tree, item: PropertyName)(implicit pos: Position) = {
        item match {
          case ident : Ident =>
            DotSelect(qualifier, ident)
          case StringLiteral(name) =>
            if (isValidIdentifier(name)) DotSelect(qualifier, Ident(name)(item.pos))
            else BracketSelect(qualifier, item)
        }
      }

      def unapply(tree: Tree): Option[(Tree, PropertyName)] = tree match {
        case DotSelect(qualifier, item) => Some((qualifier, item))
        case BracketSelect(qualifier, item : StringLiteral) => Some((qualifier, item))
        case _ => None
      }
    }

    object ApplyMethod {
      def apply(receiver: Tree, method: PropertyName, args: List[Tree])(implicit pos: Position) =
        Apply(Select(receiver, method), args)

      def unapply(tree: Apply): Option[(Tree, PropertyName, List[Tree])] = {
        tree match {
          case Apply(Select(receiver, method : PropertyName), args) =>
            Some((receiver, method, args))
          case _ =>
            None
        }
      }
    }
  }
}
