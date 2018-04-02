/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.ir

import scala.annotation.switch

import Position.NoPosition
import Types._

object Trees {
  /** Base class for all nodes in the IR.
   *
   *  Usually, one of the direct subclasses of `IRNode` should be used instead.
   */
  abstract sealed class IRNode {
    def pos: Position

    def show: String = {
      val writer = new java.io.StringWriter
      val printer = new Printers.IRTreePrinter(writer)
      printer.printAnyNode(this)
      writer.toString()
    }
  }

  /** Either a `Tree` or a `JSSpread`.
   *
   *  This is the type of actual arguments to JS applications.
   */
  sealed trait TreeOrJSSpread extends IRNode

  /** Node for a statement or expression in the IR. */
  abstract sealed class Tree extends IRNode with TreeOrJSSpread {
    val tpe: Type
  }

  // Identifiers and properties

  sealed trait PropertyName extends IRNode {
    /** Encoded name of this PropertyName within its owner's scope.
     *
     *  For [[ComputedName]]s, the value of `encodedName` cannot be relied on
     *  beyond equality tests, and the fact that it starts with `"__computed_"`.
     */
    def encodedName: String
  }

  case class Ident(name: String, originalName: Option[String])(
      implicit val pos: Position)
      extends IRNode with PropertyName {

    requireValidIdent(name)

    def encodedName: String = name
  }

  object Ident {
    def apply(name: String)(implicit pos: Position): Ident =
      new Ident(name, Some(name))
  }

  final def isValidIdentifier(name: String): Boolean = {
    name.nonEmpty && {
      val c = name.head
      (c == '$' || c == '_' || c.isUnicodeIdentifierStart) &&
      name.tail.forall(c => (c == '$') || c.isUnicodeIdentifierPart) &&
      !isKeyword(name)
    }
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

  case class ComputedName(tree: Tree, logicalName: String)
      extends IRNode with PropertyName {

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
      implicit val pos: Position) extends IRNode {
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
           _:JSDotSelect | _:JSBracketSelect | _:JSSuperBracketSelect |
           _:JSGlobalRef =>
        true
      case _ =>
        false
    }, s"Invalid lhs for Assign: $lhs")

    val tpe = NoType // cannot be in expression position
  }

  case class Return(expr: Tree, label: Option[Ident])(
      implicit val pos: Position) extends Tree {
    val tpe = NothingType
  }

  case class If(cond: Tree, thenp: Tree, elsep: Tree)(val tpe: Type)(
      implicit val pos: Position) extends Tree

  case class While(cond: Tree, body: Tree)(
      implicit val pos: Position) extends Tree {
    // cannot be in expression position, unless it is infinite
    val tpe = cond match {
      case BooleanLiteral(true) => NothingType
      case _                    => NoType
    }
  }

  case class DoWhile(body: Tree, cond: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position
  }

  case class ForIn(obj: Tree, keyVar: Ident, body: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType
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

    // Widening conversions
    final val CharToInt = 2
    final val ByteToInt = 3
    final val ShortToInt = 4
    final val IntToLong = 5
    final val IntToDouble = 6
    final val FloatToDouble = 7

    // Narrowing conversions
    final val IntToChar = 8
    final val IntToByte = 9
    final val IntToShort = 10
    final val LongToInt = 11
    final val DoubleToInt = 12
    final val DoubleToFloat = 13

    // Long <-> Double (neither widening nor narrowing)
    final val LongToDouble = 14
    final val DoubleToLong = 15

    def resultTypeOf(op: Code): Type = (op: @switch) match {
      case Boolean_! =>
        BooleanType
      case IntToChar =>
        CharType
      case IntToByte =>
        ByteType
      case IntToShort =>
        ShortType
      case CharToInt | ByteToInt | ShortToInt | LongToInt | DoubleToInt =>
        IntType
      case IntToLong | DoubleToLong =>
        LongType
      case DoubleToFloat =>
        FloatType
      case IntToDouble | LongToDouble | FloatToDouble =>
        DoubleType
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

    final val Boolean_== = 4
    final val Boolean_!= = 5
    final val Boolean_|  = 6
    final val Boolean_&  = 7

    final val Int_+ = 8
    final val Int_- = 9
    final val Int_* = 10
    final val Int_/ = 11
    final val Int_% = 12

    final val Int_|   = 13
    final val Int_&   = 14
    final val Int_^   = 15
    final val Int_<<  = 16
    final val Int_>>> = 17
    final val Int_>>  = 18

    final val Int_== = 19
    final val Int_!= = 20
    final val Int_<  = 21
    final val Int_<= = 22
    final val Int_>  = 23
    final val Int_>= = 24

    final val Long_+ = 25
    final val Long_- = 26
    final val Long_* = 27
    final val Long_/ = 28
    final val Long_% = 29

    final val Long_|   = 30
    final val Long_&   = 31
    final val Long_^   = 32
    final val Long_<<  = 33
    final val Long_>>> = 34
    final val Long_>>  = 35

    final val Long_== = 36
    final val Long_!= = 37
    final val Long_<  = 38
    final val Long_<= = 39
    final val Long_>  = 40
    final val Long_>= = 41

    final val Float_+ = 42
    final val Float_- = 43
    final val Float_* = 44
    final val Float_/ = 45
    final val Float_% = 46

    final val Double_+ = 47
    final val Double_- = 48
    final val Double_* = 49
    final val Double_/ = 50
    final val Double_% = 51

    final val Double_== = 52
    final val Double_!= = 53
    final val Double_<  = 54
    final val Double_<= = 55
    final val Double_>  = 56
    final val Double_>= = 57

    def resultTypeOf(op: Code): Type = (op: @switch) match {
      case === | !== |
          Boolean_== | Boolean_!= | Boolean_| | Boolean_& |
          Int_== | Int_!= | Int_< | Int_<= | Int_> | Int_>= |
          Long_== | Long_!= | Long_< | Long_<= | Long_> | Long_>= |
          Double_== | Double_!= | Double_< | Double_<= | Double_> | Double_>= =>
        BooleanType
      case String_+ =>
        StringType
      case Int_+ | Int_- | Int_* | Int_/ | Int_% |
          Int_| | Int_& | Int_^ | Int_<< | Int_>>> | Int_>> =>
        IntType
      case Long_+ | Long_- | Long_* | Long_/ | Long_% |
          Long_| | Long_& | Long_^ | Long_<< | Long_>>> | Long_>> =>
        LongType
      case Float_+ | Float_- | Float_* | Float_/ | Float_% =>
        FloatType
      case Double_+ | Double_- | Double_* | Double_/ | Double_% =>
        DoubleType
    }
  }

  case class NewArray(tpe: ArrayType, lengths: List[Tree])(
      implicit val pos: Position) extends Tree {
    require(lengths.nonEmpty && lengths.size <= tpe.arrayTypeRef.dimensions)
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

  case class IsInstanceOf(expr: Tree, typeRef: TypeRef)(
      implicit val pos: Position) extends Tree {
    val tpe = BooleanType
  }

  case class AsInstanceOf(expr: Tree, typeRef: TypeRef)(
      implicit val pos: Position) extends Tree {
    val tpe = typeRef match {
      case ClassRef(className)   => ClassType(className)
      case typeRef: ArrayTypeRef => ArrayType(typeRef)
    }
  }

  case class Unbox(expr: Tree, charCode: Char)(
      implicit val pos: Position) extends Tree {
    val tpe = (charCode: @switch) match {
      case 'Z' => BooleanType
      case 'C' => CharType
      case 'B' => ByteType
      case 'S' => ShortType
      case 'I' => IntType
      case 'J' => LongType
      case 'F' => FloatType
      case 'D' => DoubleType
    }
  }

  case class GetClass(expr: Tree)(implicit val pos: Position) extends Tree {
    val tpe = ClassType(Definitions.ClassClass)
  }

  // JavaScript expressions

  case class JSNew(ctor: Tree, args: List[TreeOrJSSpread])(
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

  case class JSFunctionApply(fun: Tree, args: List[TreeOrJSSpread])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  case class JSDotMethodApply(receiver: Tree, method: Ident,
      args: List[TreeOrJSSpread])(implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  case class JSBracketMethodApply(receiver: Tree, method: Tree,
      args: List[TreeOrJSSpread])(implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** Selects a property inherited from the given `superClass` on `receiver`.
   *
   *  Given the non-native JS classes
   *
   *  {{{
   *  class Bar extends js.Object
   *  class Foo extends Bar
   *  }}}
   *
   *  The node
   *
   *  {{{
   *  JSSuperBrackerSelect(LoadJSConstructor(ClassType(Bar)), qualifier, item)
   *  }}}
   *
   *  which is printed as
   *
   *  {{{
   *  super(constructorOf[Bar])::qualifier[item]
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
  case class JSSuperBracketSelect(superClass: Tree, receiver: Tree, item: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** Calls a method inherited from the given `superClass` on `receiver`.
   *
   *  Intuitively, this corresponds to
   *
   *  {{{
   *  superClass.prototype[method].call(receiver, ...args)
   *  }}}
   *
   *  but retains more structure at the IR level than using an explicit
   *  encoding of the above expression.
   *
   *  Given the non-native JS classes
   *
   *  {{{
   *  class Bar extends js.Object
   *  class Foo extends Bar
   *  }}}
   *
   *  The node
   *
   *  {{{
   *  JSSuperBrackerCall(LoadJSConstructor(ClassType(Bar)), receiver, method, args)
   *  }}}
   *
   *  which is printed as
   *
   *  {{{
   *  super(constructorOf[Bar])::receiver[method](...args)
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
  case class JSSuperBracketCall(superClass: Tree, receiver: Tree, method: Tree,
      args: List[TreeOrJSSpread])(implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** Super constructor call in the constructor of a non-native JS class.
   *
   *  Exactly one such node must appear in the constructor of a
   *  non-native JS class, at the top-level (possibly as a direct child
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
  case class JSSuperConstructorCall(args: List[TreeOrJSSpread])(
      implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  /** Loads the constructor of a JS class (native or not).
   *
   *  `cls` must represent a non-trait JS class (native or not).
   *
   *  This is used typically to instantiate a JS class, and most importantly
   *  if it is a non-native JS class. Given the class
   *
   *  {{{
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
   *  If `Foo` is non-native, the presence of this node makes it instantiable,
   *  and therefore reachable.
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
  case class JSSpread(items: Tree)(implicit val pos: Position)
      extends IRNode with TreeOrJSSpread

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

  case class JSArrayConstr(items: List[TreeOrJSSpread])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  case class JSObjectConstr(fields: List[(PropertyName, Tree)])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  case class JSGlobalRef(ident: Ident)(
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

  case class CharLiteral(value: Char)(
      implicit val pos: Position) extends Literal {
    val tpe = CharType
  }

  case class ByteLiteral(value: Byte)(
      implicit val pos: Position) extends Literal {
    val tpe = ByteType
  }

  case class ShortLiteral(value: Short)(
      implicit val pos: Position) extends Literal {
    val tpe = ShortType
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

  case class ClassOf(typeRef: TypeRef)(
      implicit val pos: Position) extends Literal {
    val tpe = ClassType(Definitions.ClassClass)
  }

  // Atomic expressions

  case class VarRef(ident: Ident)(val tpe: Type)(
      implicit val pos: Position) extends Tree

  case class This()(val tpe: Type)(implicit val pos: Position) extends Tree

  /** Closure with explicit captures.
   *
   *  @param arrow
   *    If `true`, the closure is an Arrow Function (`=>`), which does not have
   *    an `this` parameter, and cannot be constructed (called with `new`).
   *    If `false`, it is a regular Function (`function`).
   */
  case class Closure(arrow: Boolean, captureParams: List[ParamDef],
      params: List[ParamDef], body: Tree, captureValues: List[Tree])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** Creates a JavaScript class value.
   *
   *  @param cls
   *    Reference to the `ClassDef` for the class definition, which must have
   *    `jsClassCaptures.nonEmpty`
   *
   *  @param captureValues
   *    Actual values for the captured parameters (in the `ClassDef`'s
   *    `jsClassCaptures.get`)
   */
  case class CreateJSClass(cls: ClassRef, captureValues: List[Tree])(
      implicit val pos: Position)
      extends Tree {
    val tpe = AnyType
  }

  // Transient, a special one

  /** A transient node for custom purposes.
   *
   *  A transient node is never a valid input to the [[Serializers]] nor to the
   *  linker, but can be used in a transient state for internal purposes.
   *
   *  @param value
   *    The payload of the transient node, without any specified meaning.
   */
  case class Transient(value: Transient.Value)(val tpe: Type)(
      implicit val pos: Position)
      extends Tree

  object Transient {
    /** Common interface for the values that can be stored in [[Transient]]
     *  nodes.
     */
    trait Value {
      /** Prints the IR representation of this transient node.
       *  This method is called by the IR printers when encountering a
       *  [[Transient]] node.
       *
       *  @param out
       *    The [[Printers.IRTreePrinter]] to which the transient node must be
       *    printed. It can be used to print raw strings or nested IR nodes.
       */
      def printIR(out: Printers.IRTreePrinter): Unit
    }
  }

  // Classes

  final class ClassDef(
      val name: Ident,
      val kind: ClassKind,
      /** JS class captures.
       *
       *  - If `kind != ClassKind.JSClass`, must be `None`.
       *  - Otherwise, if `None`, this is a top-level class, whose JS class
       *    value is unique in the world and can be loaded with
       *    `LoadJSConstructor`.
       *  - If `Some(params)`, this is a nested JS class. New class values for
       *    this class def can be created with `CreateJSClass`.
       *    `LoadJSConstructor` is not valid for such a class def, since it
       *    does not have a unique JS class value to load.
       *
       *  Note that `Some(Nil)` is valid and is a nested JS class that happens
       *  to have no captures. It will still have zero to many JS class values
       *  created with `CreateJSClass`.
       */
      val jsClassCaptures: Option[List[ParamDef]],
      val superClass: Option[Ident],
      val interfaces: List[Ident],
      /** If defined, an expression returning the JS class value of the super
       *  class.
       *
       *  If `kind` is neither `ClassKind.JSClass` nor `ClassKind.JSModule`,
       *  this field must be `None`.
       *
       *  The expression can depend on JS class captures.
       *
       *  If empty for a non-native JS class, the JS super class value is
       *  implicitly `LoadJSConstructor(superClass.get)`. In that case the
       *  class def for `superClass` must have `jsClassCaptures.isEmpty`.
       */
      val jsSuperClass: Option[Tree],
      val jsNativeLoadSpec: Option[JSNativeLoadSpec],
      val memberDefs: List[MemberDef],
      val topLevelExportDefs: List[TopLevelExportDef]
  )(
      val optimizerHints: OptimizerHints
  )(implicit val pos: Position) extends IRNode

  object ClassDef {
    def apply(
        name: Ident,
        kind: ClassKind,
        jsClassCaptures: Option[List[ParamDef]],
        superClass: Option[Ident],
        interfaces: List[Ident],
        jsSuperClass: Option[Tree],
        jsNativeLoadSpec: Option[JSNativeLoadSpec],
        memberDefs: List[MemberDef],
        topLevelExportDefs: List[TopLevelExportDef])(
        optimizerHints: OptimizerHints)(
        implicit pos: Position): ClassDef = {
      new ClassDef(name, kind, jsClassCaptures, superClass, interfaces,
          jsSuperClass, jsNativeLoadSpec, memberDefs, topLevelExportDefs)(
          optimizerHints)
    }
  }

  // Class members

  sealed abstract class MemberDef extends IRNode {
    val static: Boolean
    val name: PropertyName

    def encodedName: String = name.encodedName
  }

  case class FieldDef(static: Boolean, name: PropertyName, ftpe: Type,
      mutable: Boolean)(
      implicit val pos: Position) extends MemberDef

  case class MethodDef(static: Boolean, name: PropertyName,
      args: List[ParamDef], resultType: Type, body: Option[Tree])(
      val optimizerHints: OptimizerHints, val hash: Option[TreeHash])(
      implicit val pos: Position) extends MemberDef

  case class PropertyDef(static: Boolean, name: PropertyName,
      getterBody: Option[Tree], setterArgAndBody: Option[(ParamDef, Tree)])(
      implicit val pos: Position) extends MemberDef

  // Top-level export defs

  sealed abstract class TopLevelExportDef extends IRNode {
    final def topLevelExportName: String = this match {
      case TopLevelConstructorExportDef(name, _, _) => name
      case TopLevelModuleExportDef(name)            => name
      case TopLevelJSClassExportDef(name)           => name

      case TopLevelMethodExportDef(MethodDef(_, propName, _, _, _)) =>
        val StringLiteral(name) = propName
        name

      case TopLevelFieldExportDef(name, _) => name
    }
  }

  case class TopLevelConstructorExportDef(name: String, args: List[ParamDef],
      body: Tree)(implicit val pos: Position) extends TopLevelExportDef

  case class TopLevelJSClassExportDef(fullName: String)(
      implicit val pos: Position) extends TopLevelExportDef

  /** Export for a top-level object.
   *
   *  This exports the singleton instance of the containing module class.
   *  The instance is initialized during ES module instantiation.
   */
  case class TopLevelModuleExportDef(fullName: String)(
      implicit val pos: Position) extends TopLevelExportDef

  case class TopLevelMethodExportDef(methodDef: MethodDef)(
      implicit val pos: Position) extends TopLevelExportDef

  case class TopLevelFieldExportDef(fullName: String, field: Ident)(
      implicit val pos: Position) extends TopLevelExportDef

  // Miscellaneous

  final class OptimizerHints private (val __private_bits: Int) extends AnyVal {
    import OptimizerHints._

    @inline private def bits: Int = __private_bits

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
    private final val InlineShift = 0
    private final val InlineMask = 1 << InlineShift

    private final val NoinlineShift = 1
    private final val NoinlineMask = 1 << NoinlineShift

    final val empty: OptimizerHints =
      new OptimizerHints(0)

    private[ir] def fromBits(bits: Int): OptimizerHints =
      new OptimizerHints(bits)

    private[ir] def toBits(hints: OptimizerHints): Int =
      hints.bits
  }

  /** Loading specification for a native JS class or object. */
  sealed abstract class JSNativeLoadSpec

  object JSNativeLoadSpec {

    /** Load from the global scope.
     *
     *  The `globalRef` is the name of a global variable (found in the global
     *  scope).
     *
     *  The `path` is a series of nested property names starting from that
     *  variable.
     *
     *  The path can be empty, in which case this denotes the specified global
     *  variable itself.
     *
     *  Examples:
     *  {{{
     *  // Foo
     *  Global("Foo", Nil)
     *
     *  // cp.Vect
     *  Global("cp", List("Vect"))
     *  }}}
     */
    final case class Global(globalRef: String, path: List[String])
        extends JSNativeLoadSpec

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

    /** Like [[Import]], but with a [[Global]] fallback when linking without
     *  modules.
     *
     *  When linking with a module kind that supports modules, the `importSpec`
     *  is used. When modules are not supported, use the fallback `globalSpec`.
     */
    final case class ImportWithGlobalFallback(importSpec: Import,
        globalSpec: Global)
        extends JSNativeLoadSpec

  }

  /** A hash of a tree (usually a MethodDef).
   *
   *  Contains a SHA-1 hash.
   */
  final class TreeHash(val hash: Array[Byte]) {
    assert(hash.length == 20)
  }
}
