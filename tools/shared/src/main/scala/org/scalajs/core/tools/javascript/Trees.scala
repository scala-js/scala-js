/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.javascript

import scala.annotation.switch

import org.scalajs.core.ir
import ir.Position
import ir.Position.NoPosition

object Trees {
  import ir.Trees.requireValidIdent

  /** AST node of JavaScript. */
  abstract sealed class Tree {
    val pos: Position

    def show: String = {
      val writer = new java.io.StringWriter
      val printer = new Printers.JSTreePrinter(writer)
      printer.printTree(this, isStat = true)
      writer.toString()
    }
  }

  case object EmptyTree extends Tree {
    val pos = NoPosition
  }

  // Comments

  case class DocComment(text: String)(implicit val pos: Position) extends Tree

  // Identifiers and properties

  sealed trait PropertyName {
    def name: String
    def pos: Position
  }

  case class Ident(name: String, originalName: Option[String])(
      implicit val pos: Position) extends PropertyName {
    requireValidIdent(name)
  }

  object Ident {
    def apply(name: String)(implicit pos: Position): Ident =
      new Ident(name, Some(name))
  }

  // Definitions

  sealed trait LocalDef extends Tree {
    def name: Ident
    def mutable: Boolean

    def ref(implicit pos: Position): Tree = VarRef(name)
  }

  case class VarDef(name: Ident, rhs: Tree)(implicit val pos: Position) extends LocalDef {
    def mutable: Boolean = true
  }

  /** ES6 let or const (depending on the mutable flag). */
  case class Let(name: Ident, mutable: Boolean, rhs: Tree)(implicit val pos: Position) extends LocalDef

  case class ParamDef(name: Ident, rest: Boolean)(implicit val pos: Position) extends LocalDef {
    def mutable: Boolean = true
  }

  // Control flow constructs

  case class Skip()(implicit val pos: Position) extends Tree

  class Block private (val stats: List[Tree])(implicit val pos: Position) extends Tree {
    override def toString(): String =
      stats.mkString("Block(", ",", ")")
  }

  object Block {
    def apply(stats: List[Tree])(implicit pos: Position): Tree = {
      val flattenedStats = stats flatMap {
        case Skip() => Nil
        case Block(subStats) => subStats
        case other => other :: Nil
      }
      flattenedStats match {
        case Nil => Skip()
        case only :: Nil => only
        case _ => new Block(flattenedStats)
      }
    }

    def apply(stats: Tree*)(implicit pos: Position): Tree =
      apply(stats.toList)

    def unapply(block: Block): Some[List[Tree]] = Some(block.stats)
  }

  case class Labeled(label: Ident, body: Tree)(implicit val pos: Position) extends Tree

  case class Assign(lhs: Tree, rhs: Tree)(implicit val pos: Position) extends Tree {
    require(lhs match {
      case _:VarRef | _:DotSelect | _:BracketSelect => true
      case _ => false
    }, s"Invalid lhs for Assign: $lhs")
  }

  case class Return(expr: Tree)(implicit val pos: Position) extends Tree

  case class If(cond: Tree, thenp: Tree, elsep: Tree)(implicit val pos: Position) extends Tree

  case class While(cond: Tree, body: Tree, label: Option[Ident] = None)(implicit val pos: Position) extends Tree

  case class DoWhile(body: Tree, cond: Tree, label: Option[Ident] = None)(implicit val pos: Position) extends Tree

  case class Try(block: Tree, errVar: Ident, handler: Tree, finalizer: Tree)(implicit val pos: Position) extends Tree

  case class Throw(expr: Tree)(implicit val pos: Position) extends Tree

  case class Break(label: Option[Ident] = None)(implicit val pos: Position) extends Tree

  case class Continue(label: Option[Ident] = None)(implicit val pos: Position) extends Tree

  case class Switch(selector: Tree, cases: List[(Tree, Tree)], default: Tree)(implicit val pos: Position) extends Tree

  case class Debugger()(implicit val pos: Position) extends Tree

  // Expressions

  case class New(ctor: Tree, args: List[Tree])(implicit val pos: Position) extends Tree

  case class DotSelect(qualifier: Tree, item: Ident)(implicit val pos: Position) extends Tree

  case class BracketSelect(qualifier: Tree, item: Tree)(implicit val pos: Position) extends Tree

  /** Syntactic apply.
   *  It is a method call if fun is a dot-select or bracket-select. It is a
   *  function call otherwise.
   */
  case class Apply(fun: Tree, args: List[Tree])(implicit val pos: Position) extends Tree

  /** `...items`, the "spread" operator of ECMAScript 6.
   *
   *  It is only valid in ECMAScript 6, in the `args`/`items` of a [[New]],
   *  [[Apply]], or [[ArrayConstr]].
   *
   *  @param items An iterable whose items will be spread
   */
  case class Spread(items: Tree)(implicit val pos: Position) extends Tree

  case class Delete(prop: Tree)(implicit val pos: Position) extends Tree {
    require(prop match {
      case _:DotSelect | _:BracketSelect => true
      case _ => false
    }, s"Invalid prop for Delete: $prop")
  }

  /** Unary operation (always preserves pureness).
   *
   *  Operations which do not preserve pureness are not allowed in this tree.
   *  These are notably ++ and --
   */
  case class UnaryOp(op: UnaryOp.Code, lhs: Tree)(implicit val pos: Position) extends Tree

  object UnaryOp {
    /** Codes are the same as in the IR. */
    type Code = ir.Trees.JSUnaryOp.Code
  }

  /** Binary operation (always preserves pureness).
   *
   *  Operations which do not preserve pureness are not allowed in this tree.
   *  These are notably +=, -=, *=, /= and %=
   */
  case class BinaryOp(op: BinaryOp.Code, lhs: Tree, rhs: Tree)(implicit val pos: Position) extends Tree

  object BinaryOp {
    /** Codes are the same as in the IR. */
    type Code = ir.Trees.JSBinaryOp.Code
  }

  case class ArrayConstr(items: List[Tree])(implicit val pos: Position) extends Tree

  case class ObjectConstr(fields: List[(PropertyName, Tree)])(implicit val pos: Position) extends Tree

  // Literals

  /** Marker for literals. Literals are always pure. */
  sealed trait Literal extends Tree

  case class Undefined()(implicit val pos: Position) extends Literal

  case class Null()(implicit val pos: Position) extends Literal

  case class BooleanLiteral(value: Boolean)(implicit val pos: Position) extends Literal

  case class IntLiteral(value: Int)(implicit val pos: Position) extends Literal

  case class DoubleLiteral(value: Double)(implicit val pos: Position) extends Literal

  case class StringLiteral(value: String)(
      implicit val pos: Position) extends Literal with PropertyName {
    override def name = value
  }

  // Atomic expressions

  case class VarRef(ident: Ident)(implicit val pos: Position) extends Tree

  case class This()(implicit val pos: Position) extends Tree

  case class Function(args: List[ParamDef], body: Tree)(implicit val pos: Position) extends Tree

  // Named function definition

  case class FunctionDef(name: Ident, args: List[ParamDef], body: Tree)(
      implicit val pos: Position) extends Tree

  // ECMAScript 6 classes

  case class ClassDef(className: Option[Ident], parentClass: Option[Tree],
      members: List[Tree])(implicit val pos: Position) extends Tree

  case class MethodDef(static: Boolean, name: PropertyName, args: List[ParamDef],
      body: Tree)(implicit val pos: Position) extends Tree

  case class GetterDef(static: Boolean, name: PropertyName,
      body: Tree)(implicit val pos: Position) extends Tree

  case class SetterDef(static: Boolean, name: PropertyName, param: ParamDef,
      body: Tree)(implicit val pos: Position) extends Tree

  case class Super()(implicit val pos: Position) extends Tree
}
