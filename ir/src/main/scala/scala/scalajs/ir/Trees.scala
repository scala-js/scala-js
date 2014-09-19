/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

import scala.annotation.switch

import Position.NoPosition
import Types._

object Trees {
  /** AST node of the IR. */
  abstract sealed class Tree {
    val pos: Position
    val tpe: Type

    def show(jsMode: Boolean): String = {
      val writer = new java.io.StringWriter
      val printer = new Printers.IRTreePrinter(writer, jsMode)
      printer.printTree(this, isStat = true)
      writer.toString()
    }
  }

  case object EmptyTree extends Tree {
    val pos = NoPosition
    val tpe = NoType
  }

  // Comments

  case class DocComment(text: String)(implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  // Identifiers and properties

  sealed trait PropertyName {
    def name: String
    def originalName: Option[String]
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

  final def isValidIdentifier(name: String): Boolean = {
    val c = name.head
    (c == '$' || c == '_' || c.isUnicodeIdentifierStart) &&
    name.tail.forall(c => (c == '$') || c.isUnicodeIdentifierPart) &&
    !isKeyword(name)
  }

  @inline final def requireValidIdent(name: String) {
    require(isValidIdentifier(name), s"${name} is not a valid identifier")
  }

  final val isKeyword: Set[String] = Set(
      // Value keywords
      "true", "false", "null", "undefined",

      // Current JavaScript keywords
      "break", "case", "catch", "continue", "debugger", "default", "delete",
      "do", "else", "finally", "for", "function", "if", "in", "instanceof",
      "new", "return", "switch", "this", "throw", "try", "typeof", "var",
      "void", "while", "with",

      // Future reserved keywords
      "class", "const", "enum", "export", "extends", "import", "super",

      // Future reserved keywords in Strict mode
      "implements", "interface", "let", "package", "private", "protected",
      "public", "static", "yield",

      // Other reserved keywords found on the Web but not in the spec
      "abstract", "boolean", "byte", "char", "double", "final", "float",
      "goto", "int", "long", "native", "short", "synchronized", "throws",
      "transient", "volatile"
  )

  // Definitions

  case class VarDef(name: Ident, vtpe: Type, mutable: Boolean, rhs: Tree)(implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position

    def ref(implicit pos: Position): Tree =
      VarRef(name, mutable = mutable)(vtpe)
  }

  case class ParamDef(name: Ident, ptpe: Type, mutable: Boolean)(implicit val pos: Position) extends Tree {
    val tpe = NoType

    def ref(implicit pos: Position): Tree =
      VarRef(name, mutable = mutable)(ptpe)
  }

  // Control flow constructs

  case class Skip()(implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position
  }

  class Block private (val stats: List[Tree])(implicit val pos: Position) extends Tree {
    val tpe = stats.last.tpe
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

  case class Labeled(label: Ident, tpe: Type, body: Tree)(implicit val pos: Position) extends Tree

  case class Assign(lhs: Tree, rhs: Tree)(implicit val pos: Position) extends Tree {
    require(lhs match {
      case _:VarRef | _:Select | _:ArraySelect |
           _:JSDotSelect | _:JSBracketSelect => true
      case _ => false
    }, s"Invalid lhs for Assign: $lhs")

    val tpe = NoType // cannot be in expression position
  }

  case class Return(expr: Tree, label: Option[Ident] = None)(implicit val pos: Position) extends Tree {
    val tpe = NothingType
  }

  case class If(cond: Tree, thenp: Tree, elsep: Tree)(val tpe: Type)(implicit val pos: Position) extends Tree

  case class While(cond: Tree, body: Tree, label: Option[Ident] = None)(implicit val pos: Position) extends Tree {
    // cannot be in expression position, unless it is infinite
    val tpe = cond match {
      case BooleanLiteral(true) => NothingType
      case _                    => NoType
    }
  }

  case class DoWhile(body: Tree, cond: Tree, label: Option[Ident] = None)(implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position
  }

  case class Try(block: Tree, errVar: Ident, handler: Tree, finalizer: Tree)(val tpe: Type)(implicit val pos: Position) extends Tree

  case class Throw(expr: Tree)(implicit val pos: Position) extends Tree {
    val tpe = NothingType
  }

  case class Break(label: Option[Ident] = None)(implicit val pos: Position) extends Tree {
    val tpe = NothingType
  }

  case class Continue(label: Option[Ident] = None)(implicit val pos: Position) extends Tree {
    val tpe = NothingType
  }

  case class Switch(selector: Tree, cases: List[(Tree, Tree)], default: Tree)(implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position
  }

  /** A break-free switch (without fallthrough behavior).
   *  Unlike [[Switch]], it can be used in expression position.
   *  It supports alternatives explicitly (hence the List[Tree] in cases),
   *  whereas in a [[Switch]] one would use the fallthrough behavior to
   *  implement alternatives.
   *  (This is not a pattern matching construct like in Scala.)
   */
  case class Match(selector: Tree, cases: List[(List[Tree], Tree)], default: Tree)(val tpe: Type)(implicit val pos: Position) extends Tree

  case class Debugger()(implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position
  }

  // Scala expressions

  case class New(cls: ClassType, ctor: Ident, args: List[Tree])(implicit val pos: Position) extends Tree {
    val tpe = cls
  }

  case class LoadModule(cls: ClassType)(implicit val pos: Position) extends Tree {
    val tpe = cls
  }

  case class StoreModule(cls: ClassType, value: Tree)(implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position
  }

  case class Select(qualifier: Tree, item: Ident, mutable: Boolean)(val tpe: Type)(implicit val pos: Position) extends Tree

  case class Apply(receiver: Tree, method: Ident, args: List[Tree])(val tpe: Type)(implicit val pos: Position) extends Tree

  case class StaticApply(receiver: Tree, cls: ClassType, method: Ident, args: List[Tree])(val tpe: Type)(implicit val pos: Position) extends Tree

  case class TraitImplApply(impl: ClassType, method: Ident, args: List[Tree])(val tpe: Type)(implicit val pos: Position) extends Tree

  /** Unary operation (always preserves pureness). */
  case class UnaryOp(op: UnaryOp.Code, lhs: Tree)(implicit val pos: Position) extends Tree {
    import UnaryOp._
    val tpe = (op: @switch) match {
      case `typeof`                            => StringType
      case Int_+ | Int_- | Int_~ | DoubleToInt => IntType
      case Double_+ | Double_-                 => DoubleType
      case Boolean_!                           => BooleanType
    }
  }

  object UnaryOp {
    /** Codes are raw Ints to be able to write switch matches on them. */
    type Code = Int

    final val typeof = 1

    final val Int_+ = 2
    final val Int_- = 3
    final val Int_~ = 4

    final val Double_+ = 5
    final val Double_- = 6

    final val Boolean_! = 7

    final val DoubleToInt = 8
  }

  /** Binary operation (always preserves pureness). */
  case class BinaryOp(op: BinaryOp.Code, lhs: Tree, rhs: Tree)(implicit val pos: Position) extends Tree {
    import BinaryOp._
    val tpe = (op: @switch) match {
      case === | !== | < | <= | > | >= | `in` | `instanceof` |
          Boolean_| | Boolean_& | Boolean_^ | Boolean_|| | Boolean_&& =>
        BooleanType
      case String_+ =>
        StringType
      case Int_+ | Int_- | Int_* | Int_/ | Int_% |
          Int_| | Int_& | Int_^ | Int_<< | Int_>>> | Int_>> =>
        IntType
      case Double_+ | Double_- | Double_* | Double_/ | Double_% =>
        DoubleType
    }
  }

  object BinaryOp {
    /** Codes are raw Ints to be able to write switch matches on them. */
    type Code = Int

    final val === = 1
    final val !== = 2

    final val <  = 3
    final val <= = 4
    final val >  = 5
    final val >= = 6

    final val String_+ = 7

    final val in         = 8
    final val instanceof = 9

    final val Int_+ = 10
    final val Int_- = 11
    final val Int_* = 12
    final val Int_/ = 13
    final val Int_% = 14

    final val Int_|   = 15
    final val Int_&   = 16
    final val Int_^   = 17
    final val Int_<<  = 18
    final val Int_>>> = 19
    final val Int_>>  = 20

    final val Double_+ = 21
    final val Double_- = 22
    final val Double_* = 23
    final val Double_/ = 24
    final val Double_% = 25

    final val Boolean_|  = 26
    final val Boolean_&  = 27
    final val Boolean_^  = 28
    final val Boolean_|| = 29
    final val Boolean_&& = 30
  }

  case class NewArray(tpe: ArrayType, lengths: List[Tree])(implicit val pos: Position) extends Tree {
    require(lengths.nonEmpty && lengths.size <= tpe.dimensions)
  }

  case class ArrayValue(tpe: ArrayType, elems: List[Tree])(implicit val pos: Position) extends Tree

  case class ArrayLength(array: Tree)(implicit val pos: Position) extends Tree {
    val tpe = IntType
  }

  case class ArraySelect(array: Tree, index: Tree)(val tpe: Type)(implicit val pos: Position) extends Tree

  case class RecordValue(tpe: RecordType, elems: List[Tree])(implicit val pos: Position) extends Tree

  case class IsInstanceOf(expr: Tree, cls: ReferenceType)(implicit val pos: Position) extends Tree {
    val tpe = BooleanType
  }

  case class AsInstanceOf(expr: Tree, cls: ReferenceType)(implicit val pos: Position) extends Tree {
    val tpe = cls match {
      case ClassType(Definitions.RuntimeNullClass)    => NullType
      case ClassType(Definitions.RuntimeNothingClass) => NothingType
      case _                                          => cls
    }
  }

  case class ClassOf(cls: ReferenceType)(implicit val pos: Position) extends Tree {
    val tpe = ClassType(Definitions.ClassClass)
  }

  case class CallHelper(helper: String, args: List[Tree])(val tpe: Type)(implicit val pos: Position) extends Tree

  object CallHelper {
    def apply(helper: String, args: Tree*)(tpe: Type)(
      implicit pos: Position): CallHelper = {
      CallHelper(helper, args.toList)(tpe)
    }
  }

  // JavaScript expressions

  case class JSGlobal()(implicit val pos: Position) extends Tree {
    val tpe = DynType
  }

  case class JSNew(ctor: Tree, args: List[Tree])(implicit val pos: Position) extends Tree {
    val tpe = DynType
  }

  case class JSDotSelect(qualifier: Tree, item: Ident)(implicit val pos: Position) extends Tree {
    val tpe = DynType
  }

  case class JSBracketSelect(qualifier: Tree, item: Tree)(implicit val pos: Position) extends Tree {
    val tpe = DynType
  }

  case class JSFunctionApply(fun: Tree, args: List[Tree])(implicit val pos: Position) extends Tree {
    val tpe = DynType
  }

  case class JSDotMethodApply(receiver: Tree, method: Ident, args: List[Tree])(implicit val pos: Position) extends Tree {
    val tpe = DynType
  }

  case class JSBracketMethodApply(receiver: Tree, method: Tree, args: List[Tree])(implicit val pos: Position) extends Tree {
    val tpe = DynType
  }

  /** Syntactic apply.
   *  It is a method call if fun is a dot-select or bracket-select. It is a
   *  function call otherwise. Just like in JavaScript.
   */
  case class JSApply(fun: Tree, args: List[Tree])(implicit val pos: Position) extends Tree {
    val tpe = DynType
  }

  case class JSDelete(prop: Tree)(implicit val pos: Position) extends Tree {
    require(prop match {
      case _:JSDotSelect | _:JSBracketSelect => true
      case _ => false
    }, s"Invalid prop for JSDelete: $prop")

    val tpe = NoType // cannot be in expression position
  }

  /** Unary operation (always preserves pureness).
   *
   *  Operations which do not preserve pureness are not allowed in this tree.
   *  These are notably ++ and --
   */
  case class JSUnaryOp(op: String, lhs: Tree)(implicit val pos: Position) extends Tree {
    val tpe = DynType
  }

  /** Binary operation (always preserves pureness).
   *
   *  Operations which do not preserve pureness are not allowed in this tree.
   *  These are notably +=, -=, *=, /= and %=
   */
  case class JSBinaryOp(op: String, lhs: Tree, rhs: Tree)(implicit val pos: Position) extends Tree {
    val tpe = DynType
  }

  case class JSArrayConstr(items: List[Tree])(implicit val pos: Position) extends Tree {
    val tpe = DynType
  }

  case class JSObjectConstr(fields: List[(PropertyName, Tree)])(implicit val pos: Position) extends Tree {
    val tpe = DynType
  }

  // Literals

  /** Marker for literals. Literals are always pure. */
  sealed trait Literal extends Tree

  case class Undefined()(implicit val pos: Position) extends Literal {
    val tpe = UndefType
  }

  case class UndefinedParam()(val tpe: Type)(implicit val pos: Position) extends Literal

  case class Null()(implicit val pos: Position) extends Literal {
    val tpe = NullType
  }

  case class BooleanLiteral(value: Boolean)(implicit val pos: Position) extends Literal {
    val tpe = BooleanType
  }

  case class IntLiteral(value: Int)(implicit val pos: Position) extends Literal {
    val tpe = IntType
  }

  case class DoubleLiteral(value: Double)(implicit val pos: Position) extends Literal {
    val tpe = DoubleType
  }

  case class StringLiteral(value: String, originalName: Option[String])(
      implicit val pos: Position) extends Literal with PropertyName {
    val tpe = StringType
    override def name = value
  }

  object StringLiteral {
    def apply(value: String)(implicit pos: Position): StringLiteral =
      new StringLiteral(value, None)
  }

  // Atomic expressions

  case class VarRef(ident: Ident, mutable: Boolean)(val tpe: Type)(implicit val pos: Position) extends Tree

  case class This()(val tpe: Type)(implicit val pos: Position) extends Tree

  /** Closure with explicit captures.
   *  The n captures map to the n first formal arguments.
   */
  case class Closure(
      thisType: Type, args: List[ParamDef], resultType: Type, body: Tree,
      captures: List[Tree])(implicit val pos: Position) extends Tree {
    val tpe = DynType
  }

  case class Function(thisType: Type, args: List[ParamDef], resultType: Type, body: Tree)(implicit val pos: Position) extends Tree {
    val tpe = DynType
  }

  // Type-related

  /** Hard-cast (unchecked at compile-time, erased at runtime).
   *  Used for low-level stuff.
   */
  case class Cast(expr: Tree, tpe: Type)(implicit val pos: Position) extends Tree

  // Classes

  case class ClassDef(name: Ident, kind: ClassKind, parent: Option[Ident], ancestors: List[Ident], defs: List[Tree])(implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  case class MethodDef(name: PropertyName, args: List[ParamDef], resultType: Type, body: Tree)(implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  case class PropertyDef(name: PropertyName, getterBody: Tree, setterArg: ParamDef, setterBody: Tree)(implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  case class ConstructorExportDef(name: String, args: List[ParamDef], body: Tree)(implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  case class ModuleExportDef(fullName: String)(implicit val pos: Position) extends Tree {
    val tpe = NoType
  }
}
