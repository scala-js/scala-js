/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.ir

import scala.annotation.switch

import Position.NoPosition
import Types._

object Trees {
  /** AST node of the IR. */
  abstract sealed class Tree {
    val pos: Position
    val tpe: Type

    def show: String = {
      val writer = new java.io.StringWriter
      val printer = new Printers.IRTreePrinter(writer)
      printer.print(this)
      writer.toString()
    }
  }

  // Identifiers and properties

  sealed trait PropertyName {
    /** Encoded name of this PropertyName within its owner's scope.
     *
     *  For [[ComputedName]]s, the value of `encodedName` cannot be relied on
     *  beyond equality tests, and the fact that it starts with `"__computed_"`.
     */
    def encodedName: String

    def pos: Position
  }

  case class Ident(name: String, originalName: Option[String])(
      implicit val pos: Position) extends PropertyName {
    requireValidIdent(name)
    def encodedName: String = name
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

  @inline final def requireValidIdent(name: String): Unit = {
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

  case class ComputedName(tree: Tree, logicalName: String) extends PropertyName {
    requireValidIdent(logicalName)
    def pos: Position = tree.pos
    override def encodedName: String = "__computed_" + logicalName
  }

  // Definitions

  case class VarDef(name: Ident, vtpe: Type, mutable: Boolean, rhs: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position

    def ref(implicit pos: Position): VarRef = VarRef(name)(vtpe)
  }

  case class ParamDef(name: Ident, ptpe: Type, mutable: Boolean, rest: Boolean)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType

    def ref(implicit pos: Position): VarRef = VarRef(name)(ptpe)
  }

  // Control flow constructs

  case class Skip()(implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position
  }

  class Block private (val stats: List[Tree])(
      implicit val pos: Position) extends Tree {
    val tpe = stats.last.tpe

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

  case class Labeled(label: Ident, tpe: Type, body: Tree)(
      implicit val pos: Position) extends Tree

  case class Assign(lhs: Tree, rhs: Tree)(
      implicit val pos: Position) extends Tree {
    require(lhs match {
      case _:VarRef | _:Select | _:SelectStatic | _:ArraySelect |
           _:JSDotSelect | _:JSBracketSelect | _:JSSuperBracketSelect => true
      case _ => false
    }, s"Invalid lhs for Assign: $lhs")

    val tpe = NoType // cannot be in expression position
  }

  case class Return(expr: Tree, label: Option[Ident] = None)(
      implicit val pos: Position) extends Tree {
    val tpe = NothingType
  }

  case class If(cond: Tree, thenp: Tree, elsep: Tree)(val tpe: Type)(
      implicit val pos: Position) extends Tree

  case class While(cond: Tree, body: Tree, label: Option[Ident] = None)(
      implicit val pos: Position) extends Tree {
    // cannot be in expression position, unless it is infinite
    val tpe = cond match {
      case BooleanLiteral(true) => NothingType
      case _                    => NoType
    }
  }

  case class DoWhile(body: Tree, cond: Tree, label: Option[Ident] = None)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position
  }

  case class TryCatch(block: Tree, errVar: Ident, handler: Tree)(
      val tpe: Type)(implicit val pos: Position) extends Tree

  case class TryFinally(block: Tree, finalizer: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = block.tpe
  }

  case class Throw(expr: Tree)(implicit val pos: Position) extends Tree {
    val tpe = NothingType
  }

  case class Continue(label: Option[Ident] = None)(
      implicit val pos: Position) extends Tree {
    val tpe = NothingType
  }

  /** A break-free switch (without fallthrough behavior).
   *  Unlike a JavaScript switch, it can be used in expression position.
   *  It supports alternatives explicitly (hence the List[Tree] in cases),
   *  whereas in a switch one would use the fallthrough behavior to
   *  implement alternatives.
   *  (This is not a pattern matching construct like in Scala.)
   */
  case class Match(selector: Tree, cases: List[(List[Literal], Tree)],
      default: Tree)(val tpe: Type)(implicit val pos: Position) extends Tree

  case class Debugger()(implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position
  }

  // Scala expressions

  case class New(cls: ClassType, ctor: Ident, args: List[Tree])(
      implicit val pos: Position) extends Tree {
    val tpe = cls
  }

  case class LoadModule(cls: ClassType)(
      implicit val pos: Position) extends Tree {
    val tpe = cls
  }

  case class StoreModule(cls: ClassType, value: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position
  }

  case class Select(qualifier: Tree, item: Ident)(val tpe: Type)(
      implicit val pos: Position) extends Tree

  case class SelectStatic(cls: ClassType, item: Ident)(val tpe: Type)(
      implicit val pos: Position) extends Tree

  /** Apply an instance method with dynamic dispatch (the default). */
  case class Apply(receiver: Tree, method: Ident, args: List[Tree])(
      val tpe: Type)(implicit val pos: Position) extends Tree

  /** Apply an instance method with static dispatch (e.g., super calls). */
  case class ApplyStatically(receiver: Tree, cls: ClassType, method: Ident,
      args: List[Tree])(val tpe: Type)(implicit val pos: Position) extends Tree

  /** Apply a static method. */
  case class ApplyStatic(cls: ClassType, method: Ident, args: List[Tree])(
      val tpe: Type)(implicit val pos: Position) extends Tree

  /** Unary operation (always preserves pureness). */
  case class UnaryOp(op: UnaryOp.Code, lhs: Tree)(
      implicit val pos: Position) extends Tree {

    val tpe = UnaryOp.resultTypeOf(op)
  }

  object UnaryOp {
    /** Codes are raw Ints to be able to write switch matches on them. */
    type Code = Int

    final val Boolean_! = 1

    final val IntToLong     = 2
    final val LongToInt     = 3
    final val LongToDouble  = 4
    final val DoubleToInt   = 5
    final val DoubleToFloat = 6
    final val DoubleToLong  = 7

    def resultTypeOf(op: Code): Type = (op: @switch) match {
      case LongToInt | DoubleToInt  => IntType
      case IntToLong | DoubleToLong => LongType
      case DoubleToFloat            => FloatType
      case LongToDouble             => DoubleType
      case Boolean_!                => BooleanType
    }
  }

  /** Binary operation (always preserves pureness). */
  case class BinaryOp(op: BinaryOp.Code, lhs: Tree, rhs: Tree)(
      implicit val pos: Position) extends Tree {

    val tpe = BinaryOp.resultTypeOf(op)
  }

  object BinaryOp {
    /** Codes are raw Ints to be able to write switch matches on them. */
    type Code = Int

    final val === = 1
    final val !== = 2

    final val String_+ = 3

    final val Int_+ = 4
    final val Int_- = 5
    final val Int_* = 6
    final val Int_/ = 7
    final val Int_% = 8

    final val Int_|   = 9
    final val Int_&   = 10
    final val Int_^   = 11
    final val Int_<<  = 12
    final val Int_>>> = 13
    final val Int_>>  = 14

    final val Float_+ = 15
    final val Float_- = 16
    final val Float_* = 17
    final val Float_/ = 18
    final val Float_% = 19

    final val Double_+ = 20
    final val Double_- = 21
    final val Double_* = 22
    final val Double_/ = 23
    final val Double_% = 24

    final val Num_== = 25
    final val Num_!= = 26
    final val Num_<  = 27
    final val Num_<= = 28
    final val Num_>  = 29
    final val Num_>= = 30

    final val Long_+ = 31
    final val Long_- = 32
    final val Long_* = 33
    final val Long_/ = 34
    final val Long_% = 35

    final val Long_|   = 36
    final val Long_&   = 37
    final val Long_^   = 38
    final val Long_<<  = 39
    final val Long_>>> = 40
    final val Long_>>  = 41

    final val Long_== = 42
    final val Long_!= = 43
    final val Long_<  = 44
    final val Long_<= = 45
    final val Long_>  = 46
    final val Long_>= = 47

    final val Boolean_== = 48
    final val Boolean_!= = 49
    final val Boolean_|  = 50
    final val Boolean_&  = 51

    def resultTypeOf(op: Code): Type = (op: @switch) match {
      case === | !== |
          Num_== | Num_!= | Num_< | Num_<= | Num_> | Num_>= |
          Long_== | Long_!= | Long_< | Long_<= | Long_> | Long_>= |
          Boolean_== | Boolean_!= | Boolean_| | Boolean_& =>
        BooleanType
      case String_+ =>
        StringType
      case Int_+ | Int_- | Int_* | Int_/ | Int_% |
          Int_| | Int_& | Int_^ | Int_<< | Int_>>> | Int_>> =>
        IntType
      case Float_+ | Float_- | Float_* | Float_/ | Float_% =>
        FloatType
      case Double_+ | Double_- | Double_* | Double_/ | Double_% =>
        DoubleType
      case Long_+ | Long_- | Long_* | Long_/ | Long_% |
          Long_| | Long_& | Long_^ | Long_<< | Long_>>> | Long_>> =>
        LongType
    }
  }

  case class NewArray(tpe: ArrayType, lengths: List[Tree])(
      implicit val pos: Position) extends Tree {
    require(lengths.nonEmpty && lengths.size <= tpe.dimensions)
  }

  case class ArrayValue(tpe: ArrayType, elems: List[Tree])(
      implicit val pos: Position) extends Tree

  case class ArrayLength(array: Tree)(implicit val pos: Position) extends Tree {
    val tpe = IntType
  }

  case class ArraySelect(array: Tree, index: Tree)(val tpe: Type)(
      implicit val pos: Position) extends Tree

  case class RecordValue(tpe: RecordType, elems: List[Tree])(
      implicit val pos: Position) extends Tree

  case class IsInstanceOf(expr: Tree, cls: ReferenceType)(
      implicit val pos: Position) extends Tree {
    val tpe = BooleanType
  }

  case class AsInstanceOf(expr: Tree, cls: ReferenceType)(
      implicit val pos: Position) extends Tree {
    val tpe = cls match {
      case ClassType(Definitions.RuntimeNullClass)    => NullType
      case ClassType(Definitions.RuntimeNothingClass) => NothingType
      case _                                          => cls.asInstanceOf[Type]
    }
  }

  case class Unbox(expr: Tree, charCode: Char)(
      implicit val pos: Position) extends Tree {
    val tpe = (charCode: @switch) match {
      case 'Z'             => BooleanType
      case 'B' | 'S' | 'I' => IntType
      case 'J'             => LongType
      case 'F'             => FloatType
      case 'D'             => DoubleType
    }
  }

  case class GetClass(expr: Tree)(implicit val pos: Position) extends Tree {
    val tpe = ClassType(Definitions.ClassClass)
  }

  case class CallHelper(helper: String, args: List[Tree])(val tpe: Type)(
      implicit val pos: Position) extends Tree

  object CallHelper {
    def apply(helper: String, args: Tree*)(tpe: Type)(
        implicit pos: Position): CallHelper = {
      CallHelper(helper, args.toList)(tpe)
    }
  }

  // JavaScript expressions

  case class JSNew(ctor: Tree, args: List[Tree])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  case class JSDotSelect(qualifier: Tree, item: Ident)(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  case class JSBracketSelect(qualifier: Tree, item: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  case class JSFunctionApply(fun: Tree, args: List[Tree])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  case class JSDotMethodApply(receiver: Tree, method: Ident,
      args: List[Tree])(implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  case class JSBracketMethodApply(receiver: Tree, method: Tree,
      args: List[Tree])(implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** Selects a property inherited from the parent class of `cls` on `receiver`.
   *
   *  `cls` must be a Scala.js-defined JS class.
   *
   *  Given the Scala.js-defined JS class
   *
   *  {{{
   *  @ScalaJSDefined
   *  class Foo extends Bar
   *  }}}
   *
   *  The node
   *
   *  {{{
   *  JSSuperBrackerSelect(ClassType(Foo), qualifier, item)
   *  }}}
   *
   *  which is printed as
   *
   *  {{{
   *  qualifier.Foo::super[item]
   *  }}}
   *
   *  has the semantics of an ES6 super reference
   *
   *  {{{
   *  super[item]
   *  }}}
   *
   *  as if it were in an instance method of `Foo` with `qualifier` as the
   *  `this` value.
   */
  case class JSSuperBracketSelect(cls: ClassType, receiver: Tree, item: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** Calls a method inherited from the parent class of `cls` on `receiver`.
   *
   *  `cls` must be a Scala.js-defined JS class.
   *
   *  Given the Scala.js-defined JS class
   *
   *  {{{
   *  @ScalaJSDefined
   *  class Foo extends Bar
   *  }}}
   *
   *  The node
   *
   *  {{{
   *  JSSuperBrackerCall(ClassType(Foo), receiver, method, args)
   *  }}}
   *
   *  which is printed as
   *
   *  {{{
   *  receiver.Foo::super[method](...args)
   *  }}}
   *
   *  has the following semantics:
   *
   *  {{{
   *  Bar.prototype[method].call(receiver, ...args)
   *  }}}
   *
   *  If this happens to be located in an instance method of `Foo`, *and*
   *  `receiver` happens to be `This()`, this is equivalent to the ES6
   *  statement
   *
   *  {{{
   *  super[method](...args)
   *  }}}
   */
  case class JSSuperBracketCall(cls: ClassType, receiver: Tree, method: Tree,
      args: List[Tree])(implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** Super constructor call in the constructor of a Scala.js-defined JS class.
   *
   *  Exactly one such node must appear in the constructor of a
   *  Scala.js-defined JS class, at the top-level (possibly as a direct child
   *  of a top-level `Block`). Any other use of this node is invalid.
   *
   *  Statements before this node, as well as the `args`, cannot contain any
   *  `This()` node. Statements after this node can use `This()`.
   *
   *  After the execution of this node, it is guaranteed that all fields
   *  declared in the current class have been created and initialized. Up to
   *  that point, accessing any field declared in this class (e.g., through an
   *  overridden method called from the super constructor) is undefined
   *  behavior.
   *
   *  All in all, the shape of a constructor is therefore:
   *
   *  {{{
   *  {
   *    statementsNotUsingThis();
   *    JSSuperConstructorCall(...argsNotUsingThis);
   *    statementsThatMayUseThis()
   *  }
   *  }}}
   *
   *  which currently translates to something of the following shape:
   *
   *  {{{
   *  {
   *    statementsNotUsingThis();
   *    super(...argsNotUsingThis);
   *    this.privateField1 = 0;
   *    this["publicField2"] = false;
   *    statementsThatMayUseThis()
   *  }
   *  }}}
   */
  case class JSSuperConstructorCall(args: List[Tree])(
      implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  /** Loads the constructor of a JS class (native or not).
   *
   *  `cls` must represent a non-trait JS class (native or not).
   *
   *  This is used typically to instantiate a JS class, and most importantly
   *  if it is a Scala.js-defined JS class. Given the class
   *
   *  {{{
   *  @ScalaJSDefined
   *  class Foo(x: Int) extends js.Object
   *  }}}
   *
   *  The instantiation `new Foo(1)` would be represented as
   *
   *  {{{
   *  JSNew(LoadJSConstructor(ClassType("Foo")), List(IntLiteral(1)))
   *  }}}
   *
   *  This node is also useful to encode `o.isInstanceOf[Foo]`:
   *
   *  {{{
   *  JSBinaryOp(instanceof, o, LoadJSConstructor(ClassType("Foo")))
   *  }}}
   *
   *  If `Foo` is Scala.js-defined, the presence of this node makes it
   *  instantiable, and therefore reachable.
   */
  case class LoadJSConstructor(cls: ClassType)(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** Like [[LoadModule]] but for a JS module class. */
  case class LoadJSModule(cls: ClassType)(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** `...items`, the "spread" operator of ECMAScript 6.
   *
   *  It is only valid in the `args`/`items` of a [[JSNew]], [[JSFunctionApply]],
   *  [[JSDotMethodApply]], [[JSBracketMethodApply]], or [[JSArrayConstr]].
   *
   *  @param items An Array whose items will be spread (not an arbitrary iterable)
   */
  case class JSSpread(items: Tree)(implicit val pos: Position) extends Tree {
    val tpe = NoType // there is no reasonable type for this tree
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
  case class JSUnaryOp(op: JSUnaryOp.Code, lhs: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  object JSUnaryOp {
    /** Codes are raw Ints to be able to write switch matches on them. */
    type Code = Int

    final val + = 1
    final val - = 2
    final val ~ = 3
    final val ! = 4

    final val typeof = 5
  }

  /** Binary operation (always preserves pureness).
   *
   *  Operations which do not preserve pureness are not allowed in this tree.
   *  These are notably +=, -=, *=, /= and %=
   */
  case class JSBinaryOp(op: JSBinaryOp.Code, lhs: Tree, rhs: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  object JSBinaryOp {
    /** Codes are raw Ints to be able to write switch matches on them. */
    type Code = Int

    final val === = 1
    final val !== = 2

    final val + = 3
    final val - = 4
    final val * = 5
    final val / = 6
    final val % = 7

    final val |   = 8
    final val &   = 9
    final val ^   = 10
    final val <<  = 11
    final val >>  = 12
    final val >>> = 13

    final val <  = 14
    final val <= = 15
    final val >  = 16
    final val >= = 17

    final val && = 18
    final val || = 19

    final val in         = 20
    final val instanceof = 21
  }

  case class JSArrayConstr(items: List[Tree])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  case class JSObjectConstr(fields: List[(PropertyName, Tree)])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  case class JSLinkingInfo()(implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  // Literals

  /** Marker for literals. Literals are always pure. */
  sealed trait Literal extends Tree

  case class Undefined()(implicit val pos: Position) extends Literal {
    val tpe = UndefType
  }

  case class Null()(implicit val pos: Position) extends Literal {
    val tpe = NullType
  }

  case class BooleanLiteral(value: Boolean)(
      implicit val pos: Position) extends Literal {
    val tpe = BooleanType
  }

  case class IntLiteral(value: Int)(
      implicit val pos: Position) extends Literal {
    val tpe = IntType
  }

  case class LongLiteral(value: Long)(
      implicit val pos: Position) extends Literal {
    val tpe = LongType
  }

  case class FloatLiteral(value: Float)(
      implicit val pos: Position) extends Literal {
    val tpe = FloatType
  }

  case class DoubleLiteral(value: Double)(
      implicit val pos: Position) extends Literal {
    val tpe = DoubleType
  }

  case class StringLiteral(value: String)(
      implicit val pos: Position) extends Literal with PropertyName {
    val tpe = StringType
    override def encodedName: String = value
  }

  case class ClassOf(cls: ReferenceType)(
      implicit val pos: Position) extends Literal {
    val tpe = ClassType(Definitions.ClassClass)
  }

  // Specials

  case class UndefinedParam()(val tpe: Type)(
      implicit val pos: Position) extends Tree

  // Atomic expressions

  case class VarRef(ident: Ident)(val tpe: Type)(
      implicit val pos: Position) extends Tree

  case class This()(val tpe: Type)(implicit val pos: Position) extends Tree

  /** Closure with explicit captures.
   *  The n captures map to the n first formal arguments.
   */
  case class Closure(captureParams: List[ParamDef], params: List[ParamDef],
      body: Tree, captureValues: List[Tree])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  // Classes

  case class ClassDef(name: Ident, kind: ClassKind, superClass: Option[Ident],
      interfaces: List[Ident], jsNativeLoadSpec: Option[JSNativeLoadSpec],
      defs: List[Tree])(
      val optimizerHints: OptimizerHints)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  case class FieldDef(static: Boolean, name: PropertyName, ftpe: Type,
      mutable: Boolean)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  case class MethodDef(static: Boolean, name: PropertyName,
      args: List[ParamDef], resultType: Type, body: Option[Tree])(
      val optimizerHints: OptimizerHints, val hash: Option[TreeHash])(
      implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  case class PropertyDef(static: Boolean, name: PropertyName,
      getterBody: Option[Tree], setterArgAndBody: Option[(ParamDef, Tree)])(
      implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  case class ConstructorExportDef(name: String, args: List[ParamDef],
      body: Tree)(implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  case class JSClassExportDef(fullName: String)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  /** Traditional `@JSExport` for top-level objects, as a 0-arg function.
   *
   *  This exports a module as a 0-arg function that returns the module
   *  instance. It is initialized lazily in that case.
   *
   *  This alternative should eventually disappear.
   */
  case class ModuleExportDef(fullName: String)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  /** New-style `@JSExportTopLevel` for top-level objects, directly as the
   *  object.
   *
   *  This exports a module directly as a variable holding the module instance.
   *  The instance is initialized during ES module instantiation.
   */
  case class TopLevelModuleExportDef(fullName: String)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  case class TopLevelMethodExportDef(methodDef: MethodDef)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  case class TopLevelFieldExportDef(fullName: String, field: Ident)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  final class OptimizerHints(val bits: Int) extends AnyVal {
    import OptimizerHints._

    def inline: Boolean = (bits & InlineMask) != 0
    def noinline: Boolean = (bits & NoinlineMask) != 0

    def withInline(value: Boolean): OptimizerHints =
      if (value) new OptimizerHints(bits | InlineMask)
      else new OptimizerHints(bits & ~InlineMask)

    def withNoinline(value: Boolean): OptimizerHints =
      if (value) new OptimizerHints(bits | NoinlineMask)
      else new OptimizerHints(bits & ~NoinlineMask)

    override def toString(): String =
      s"OptimizerHints($bits)"
  }

  object OptimizerHints {
    final val InlineShift = 0
    final val InlineMask = 1 << InlineShift

    final val NoinlineShift = 1
    final val NoinlineMask = 1 << NoinlineShift

    final val empty: OptimizerHints =
      new OptimizerHints(0)
  }

  /** Loading specification for a native JS class or object. */
  sealed abstract class JSNativeLoadSpec

  object JSNativeLoadSpec {

    /** Load from the global scope.
     *
     *  The `path` is a series of nested property names starting from the
     *  global object.
     *
     *  The path can be empty, in which case this denotes the global object
     *  itself.
     *
     *  Any element in the path is a property selection from there. A global
     *  scope loading spec with one path element is therefore a global variable.
     *
     *  Examples:
     *  {{{
     *  // <global>
     *  Global(None, Nil)
     *
     *  // <global>.Date
     *  Global(None, List("Date"))
     *
     *  // <global>.cp.Vect
     *  Global(None, List("cp", "Vect"))
     *  }}}
     */
    final case class Global(path: List[String]) extends JSNativeLoadSpec

    /** Load from a module import.
     *
     *  The `module` is the ES module identifier. The `path` is a series of
     *  nested property names starting from the module object.
     *
     *  The path can be empty, in which case the specification denotes the
     *  namespace import, i.e., import a special object whose fields are all
     *  the exports of the module.
     *
     *  Any element in the path is a property selection from there. A module
     *  import info with one path element is importing that particular value
     *  from the module.
     *
     *  Examples:
     *  {{{
     *  // import { Bar as x } from 'foo'
     *  Import("foo", List("Bar"))
     *
     *  // import { Bar as y } from 'foo'
     *  // y.Baz
     *  Import("foo", List("Bar", "Baz"))
     *
     *  // import * as x from 'foo' (namespace import)
     *  Import("foo", Nil)
     *
     *  // import x from 'foo' (default import)
     *  Import("foo", List("default"))
     *  }}}
     */
    final case class Import(module: String, path: List[String])
        extends JSNativeLoadSpec

  }

  /** A hash of a tree (usually a MethodDef). Contains two SHA-1 hashes */
  final class TreeHash(val treeHash: Array[Byte], val posHash: Array[Byte]) {
    assert(treeHash.length == 20)
    assert(posHash.length == 20)
  }
}
