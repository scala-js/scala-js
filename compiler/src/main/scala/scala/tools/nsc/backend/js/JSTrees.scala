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

      override def toString() = {
        val baos = new java.io.ByteArrayOutputStream()
        val writer = new java.io.PrintWriter(baos)
        val printer = new JSTreePrinter(writer)
        printer.printTree(this)
        writer.close()
        baos.toString()
      }
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

    object Block {
      def apply(stats: List[Tree])(implicit pos: Position): Tree = stats match {
        case Nil => Skip()
        case only :: Nil => only
        case _ => Block(stats.init, stats.last)
      }
    }

    case class Assign(lhs: Tree, rhs: Tree)(implicit val pos: Position) extends Tree

    case class Return(expr: Tree)(implicit val pos: Position) extends Tree

    case class If(cond: Tree, thenp: Tree, elsep: Tree)(implicit val pos: Position) extends Tree

    case class While(cond: Tree, body: Tree, label: Option[Ident] = None)(implicit val pos: Position) extends Tree

    case class DoWhile(body: Tree, cond: Tree, label: Option[Ident] = None)(implicit val pos: Position) extends Tree

    case class Try(block: Tree, errVar: Ident, handler: Tree, finalizer: Tree)(implicit val pos: Position) extends Tree

    case class Throw(expr: Tree)(implicit val pos: Position) extends Tree

    case class Break(label: Option[Ident] = None)(implicit val pos: Position) extends Tree

    case class Continue(label: Option[Ident] = None)(implicit val pos: Position) extends Tree

    case class Switch(selector: Tree, cases: List[(Tree, Tree)], default: Tree)(implicit val pos: Position) extends Tree

    /** Like match in Scala (i.e., a break-free switch) */
    case class Match(selector: Tree, cases: List[(List[Tree], Tree)], default: Tree)(implicit val pos: Position) extends Tree

    // Expressions

    case class DotSelect(qualifier: Tree, item: Ident)(implicit val pos: Position) extends Tree

    case class BracketSelect(qualifier: Tree, item: Tree)(implicit val pos: Position) extends Tree

    case class Apply(fun: Tree, args: List[Tree])(implicit val pos: Position) extends Tree

    case class Function(args: List[Ident], body: Tree)(implicit val pos: Position) extends Tree

    case class UnaryOp(op: String, lhs: Tree)(implicit val pos: Position) extends Tree

    case class BinaryOp(op: String, lhs: Tree, rhs: Tree)(implicit val pos: Position) extends Tree

    case class New(fun: Tree, args: List[Tree])(implicit val pos: Position) extends Tree

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

    case class ClassDef(name: Ident, parent: Tree, defs: List[Tree])(implicit val pos: Position) extends Tree

    case class MethodDef(name: PropertyName, args: List[Ident], body: Tree)(implicit val pos: Position) extends Tree

    case class GetterDef(name: PropertyName, body: Tree)(implicit val pos: Position) extends Tree

    case class SetterDef(name: PropertyName, arg: Ident, body: Tree)(implicit val pos: Position) extends Tree

    case class CustomDef(name: PropertyName, rhs: Tree)(implicit val pos: Position) extends Tree

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

    object DynamicSelect {
      def apply(qualifier: Tree, item: Tree)(implicit pos: Position) = {
        item match {
          case StringLiteral(name) if isValidIdentifier(name) =>
            DotSelect(qualifier, Ident(name)(item.pos))
          case _ =>
            BracketSelect(qualifier, item)
        }
      }

      def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
        case DotSelect(qualifier, item) =>
          Some((qualifier, StringLiteral(item.name)(item.pos)))
        case BracketSelect(qualifier, item) => Some((qualifier, item))
        case _ => None
      }
    }

    object ApplyMethod {
      def apply(receiver: Tree, method: PropertyName, args: List[Tree])(implicit pos: Position) =
        Apply(Select(receiver, method), args)

      def unapply(tree: Apply): Option[(Tree, PropertyName, List[Tree])] = {
        tree match {
          case Apply(Select(receiver, method), args) =>
            Some((receiver, method, args))
          case _ =>
            None
        }
      }
    }

    object DynamicApplyMethod {
      def apply(receiver: Tree, method: Tree, args: List[Tree])(implicit pos: Position) =
        Apply(DynamicSelect(receiver, method), args)

      def unapply(tree: Apply): Option[(Tree, Tree, List[Tree])] = {
        tree match {
          case Apply(DynamicSelect(receiver, method), args) =>
            Some((receiver, method, args))
          case _ =>
            None
        }
      }
    }

    // Transformer

    class Transformer {
      def transformStat(tree: Tree): Tree = {
        implicit val pos = tree.pos

        tree match {
          // Definitions

          case VarDef(ident, rhs) =>
            VarDef(ident, transformExpr(rhs))

          case FunDef(name, args, body) =>
            FunDef(name, args, transformStat(body))

          // Statement-only language constructs

          case Block(stats, stat) =>
            Block(stats map transformStat, transformStat(stat))

          case Assign(lhs, rhs) =>
            Assign(transformExpr(lhs), transformExpr(rhs))

          case Return(expr) =>
            Return(transformExpr(expr))

          case If(cond, thenp, elsep) =>
            If(transformExpr(cond), transformStat(thenp), transformStat(elsep))

          case While(cond, body, label) =>
            While(transformExpr(cond), transformStat(body), label)

          case DoWhile(body, cond, label) =>
            DoWhile(transformStat(body), transformExpr(cond), label)

          case Try(block, errVar, handler, finalizer) =>
            Try(transformStat(block), errVar, transformStat(handler), transformStat(finalizer))

          case Throw(expr) =>
            Throw(transformExpr(expr))

          case Switch(selector, cases, default) =>
            Switch(transformExpr(selector),
                cases map (c => (transformExpr(c._1), transformStat(c._2))),
                transformStat(default))

          case Match(selector, cases, default) =>
            Match(transformExpr(selector),
                cases map (c => (c._1 map transformExpr, transformStat(c._2))),
                transformStat(default))

          // Expressions

          case DotSelect(qualifier, item) =>
            DotSelect(transformExpr(qualifier), item)

          case BracketSelect(qualifier, item) =>
            BracketSelect(transformExpr(qualifier), transformExpr(item))

          case Apply(fun, args) =>
            Apply(transformExpr(fun), args map transformExpr)

          case Function(args, body) =>
            Function(args, transformStat(body))

          case UnaryOp(op, lhs) =>
            UnaryOp(op, transformExpr(lhs))

          case BinaryOp(op, lhs, rhs) =>
            BinaryOp(op, transformExpr(lhs), transformExpr(rhs))

          case New(fun, args) =>
            New(fun, args map transformExpr)

          // Compounds

          case ArrayConstr(items) =>
            ArrayConstr(items map transformExpr)

          case ObjectConstr(fields) =>
            ObjectConstr(fields map {
              case (name, value) => (name, transformExpr(value))
            })

          // Classes

          case ClassDef(name, parent, defs) =>
            ClassDef(name, transformExpr(parent), defs map transformDef)

          case _ =>
            tree
        }
      }

      def transformExpr(tree: Tree): Tree = {
        implicit val pos = tree.pos

        tree match {
          // Things that really should always be statements
          // But actually it could be meaningful to have them as expressions

          case VarDef(ident, rhs) =>
            VarDef(ident, transformExpr(rhs))

          case FunDef(name, args, body) =>
            FunDef(name, args, transformStat(body))

          case Assign(lhs, rhs) =>
            Assign(transformExpr(lhs), transformExpr(rhs))

          case While(cond, body, label) =>
            While(transformExpr(cond), transformStat(body), label)

          case DoWhile(body, cond, label) =>
            DoWhile(transformStat(body), transformExpr(cond), label)

          // Language constructs that are statement-only in standard JavaScript

          case Block(stats, expr) =>
            Block(stats map transformStat, transformExpr(expr))

          case Return(expr) =>
            Return(transformExpr(expr))

          case If(cond, thenp, elsep) =>
            If(transformExpr(cond), transformExpr(thenp), transformExpr(elsep))

          case Try(block, errVar, handler, finalizer) =>
            Try(transformExpr(block), errVar, transformExpr(handler), transformStat(finalizer))

          case Throw(expr) =>
            Throw(transformExpr(expr))

          case Switch(selector, cases, default) =>
            /* Given that there are 'break's in JS switches, a switch
             * expression can definitely have no meaning whatsoever.
             */
            abort("A switch expression has no meaning: " + tree)

          case Match(selector, cases, default) =>
            Match(transformExpr(selector),
                cases map (c => (c._1 map transformExpr, transformExpr(c._2))),
                transformExpr(default))

          // Expressions

          case DotSelect(qualifier, item) =>
            DotSelect(transformExpr(qualifier), item)

          case BracketSelect(qualifier, item) =>
            BracketSelect(transformExpr(qualifier), transformExpr(item))

          case Apply(fun, args) =>
            Apply(transformExpr(fun), args map transformExpr)

          case Function(args, body) =>
            Function(args, transformStat(body))

          case UnaryOp(op, lhs) =>
            UnaryOp(op, transformExpr(lhs))

          case BinaryOp(op, lhs, rhs) =>
            BinaryOp(op, transformExpr(lhs), transformExpr(rhs))

          case New(fun, args) =>
            New(fun, args map transformExpr)

          // Compounds

          case ArrayConstr(items) =>
            ArrayConstr(items map transformExpr)

          case ObjectConstr(fields) =>
            ObjectConstr(fields map {
              case (name, value) => (name, transformExpr(value))
            })

          // Classes

          case ClassDef(name, parent, defs) =>
            ClassDef(name, transformExpr(parent), defs map transformDef)

          case _ =>
            tree
        }
      }

      def transformDef(tree: Tree): Tree = {
        implicit val pos = tree.pos

        tree match {
          case MethodDef(name, args, body) =>
            MethodDef(name, args, transformStat(body))

          case GetterDef(name, body) =>
            GetterDef(name, transformStat(body))

          case SetterDef(name, arg, body) =>
            SetterDef(name, arg, transformStat(body))

          case CustomDef(name, rhs) =>
            CustomDef(name, transformExpr(rhs))

          case _ =>
            tree
        }
      }
    }
  }
}
