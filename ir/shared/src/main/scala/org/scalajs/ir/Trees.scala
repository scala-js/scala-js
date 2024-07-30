/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.ir

import scala.annotation.switch

import Names._
import OriginalName.NoOriginalName
import Position.NoPosition
import Types._

object Trees {
  /* The case classes for IR Nodes are sealed instead of final because making
   * them final triggers bugs with Scala 2.12.{1-4}, in combination
   * with their `implicit val pos`.
   */

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

  // Identifiers

  sealed case class LocalIdent(name: LocalName)(implicit val pos: Position)
      extends IRNode

  sealed case class LabelIdent(name: LabelName)(implicit val pos: Position)
      extends IRNode

  sealed case class SimpleFieldIdent(name: SimpleFieldName)(implicit val pos: Position)
      extends IRNode

  sealed case class FieldIdent(name: FieldName)(implicit val pos: Position)
      extends IRNode

  sealed case class MethodIdent(name: MethodName)(implicit val pos: Position)
      extends IRNode

  sealed case class ClassIdent(name: ClassName)(implicit val pos: Position)
      extends IRNode

  /** Tests whether the given name is a valid JavaScript identifier name.
   *
   *  This test does *not* exclude keywords.
   */
  def isJSIdentifierName(name: String): Boolean = {
    // scalastyle:off return
    /* This method is called in the constructor of some IR node classes, such
     * as JSGlobalRef; it should be fast.
     */
    val len = name.length()
    if (len == 0)
      return false
    val c = name.charAt(0)
    if (c != '$' && c != '_' && !Character.isUnicodeIdentifierStart(c))
      return false
    var i = 1
    while (i != len) {
      val c = name.charAt(i)
      if (c != '$' && !Character.isUnicodeIdentifierPart(c))
        return false
      i += 1
    }
    true
    // scalastyle:on return
  }

  // Definitions

  sealed case class VarDef(name: LocalIdent, originalName: OriginalName,
      vtpe: Type, mutable: Boolean, rhs: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position

    def ref(implicit pos: Position): VarRef = VarRef(name)(vtpe)
  }

  sealed case class ParamDef(name: LocalIdent, originalName: OriginalName,
      ptpe: Type, mutable: Boolean)(
      implicit val pos: Position) extends IRNode {
    def ref(implicit pos: Position): VarRef = VarRef(name)(ptpe)
  }

  // Control flow constructs

  sealed case class Skip()(implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position
  }

  sealed class Block private (val stats: List[Tree])(
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

    def apply(stats: List[Tree], expr: Tree)(implicit pos: Position): Tree =
      apply(stats :+ expr)

    def apply(stats: Tree*)(implicit pos: Position): Tree =
      apply(stats.toList)

    def unapply(block: Block): Some[List[Tree]] = Some(block.stats)
  }

  sealed case class Labeled(label: LabelIdent, tpe: Type, body: Tree)(
      implicit val pos: Position) extends Tree

  sealed trait AssignLhs extends Tree

  sealed case class Assign(lhs: AssignLhs, rhs: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position
  }

  sealed case class Return(expr: Tree, label: LabelIdent)(
      implicit val pos: Position) extends Tree {
    val tpe = NothingType
  }

  sealed case class If(cond: Tree, thenp: Tree, elsep: Tree)(val tpe: Type)(
      implicit val pos: Position) extends Tree

  sealed case class LinkTimeIf(cond: LinkTimeTree, thenp: Tree,
      elsep: Tree)(val tpe: Type)(implicit val pos: Position) extends Tree

  sealed abstract class LinkTimeTree extends IRNode {
    val pos: Position
    val tpe: Type
  }

  object LinkTimeTree {
    final case class BinaryOp(op: LinkTimeOp.Code, lhs: LinkTimeTree, rhs: LinkTimeTree)(
        implicit val pos: Position) extends LinkTimeTree {
      val tpe = BooleanType
    }

    final case class Property(name: String, tpe: Type)(implicit val pos: Position)
      extends LinkTimeTree

    final case class IntConst(v: Int)(implicit val pos: Position) extends LinkTimeTree {
      val tpe = IntType
    }

    final case class BooleanConst(v: Boolean)(implicit val pos: Position) extends LinkTimeTree {
      val tpe = BooleanType
    }
  }

  object LinkTimeOp {
    type Code = Int

    final val Boolean_== = 1
    final val Boolean_!= = 2
    final val Boolean_&& = 3
    final val Boolean_|| = 4

    final val Int_== = 5
    final val Int_!= = 6
    final val Int_<  = 7
    final val Int_<= = 8
    final val Int_>  = 9
    final val Int_>= = 10
  }

  sealed case class While(cond: Tree, body: Tree)(
      implicit val pos: Position) extends Tree {
    // cannot be in expression position, unless it is infinite
    val tpe = cond match {
      case BooleanLiteral(true) => NothingType
      case _                    => NoType
    }
  }

  sealed case class ForIn(obj: Tree, keyVar: LocalIdent,
      keyVarOriginalName: OriginalName, body: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  sealed case class TryCatch(block: Tree, errVar: LocalIdent,
      errVarOriginalName: OriginalName, handler: Tree)(
      val tpe: Type)(implicit val pos: Position) extends Tree

  sealed case class TryFinally(block: Tree, finalizer: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = block.tpe
  }

  sealed case class Throw(expr: Tree)(implicit val pos: Position) extends Tree {
    val tpe = NothingType
  }

  /** A break-free switch (without fallthrough behavior).
   *
   *  Unlike a JavaScript switch, it can be used in expression position.
   *  It supports alternatives explicitly (hence the `List[MatchableLiteral]`
   *  in cases), whereas in a switch one would use the fallthrough behavior to
   *  implement alternatives.
   *  (This is not a pattern matching construct like in Scala.)
   *
   *  The selector must be either an `int` (`IntType`) or a `java.lang.String`.
   *  The cases can be any `MatchableLiteral`, even if they do not make sense
   *  for the type of the selecter (they simply will never match).
   *
   *  Because `+0.0 === -0.0` in JavaScript, and because those semantics are
   *  used in a JS `switch`, we have to prevent the selector from ever being
   *  `-0.0`. Otherwise, it would be matched by a `case IntLiteral(0)`. At the
   *  same time, we must allow at least `int` and `java.lang.String` to support
   *  all switchable `match`es from Scala. Since the latter two have no common
   *  super type that does not allow `-0.0`, we really have to special-case
   *  those two types.
   *
   *  This is also why we restrict `MatchableLiteral`s to `IntLiteral`,
   *  `StringLiteral` and `Null`. Allowing more cases would only make IR
   *  checking more complicated, without bringing any added value.
   */
  sealed case class Match(selector: Tree, cases: List[(List[MatchableLiteral], Tree)],
      default: Tree)(val tpe: Type)(implicit val pos: Position) extends Tree

  sealed case class Debugger()(implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position
  }

  // Scala expressions

  sealed case class New(className: ClassName, ctor: MethodIdent,
      args: List[Tree])(
      implicit val pos: Position) extends Tree {
    val tpe = ClassType(className)
  }

  sealed case class LoadModule(className: ClassName)(
      implicit val pos: Position) extends Tree {
    val tpe = ClassType(className)
  }

  sealed case class StoreModule()(implicit val pos: Position) extends Tree {
    val tpe = NoType // cannot be in expression position
  }

  sealed case class Select(qualifier: Tree, field: FieldIdent)(val tpe: Type)(
      implicit val pos: Position) extends AssignLhs

  sealed case class SelectStatic(field: FieldIdent)(val tpe: Type)(
      implicit val pos: Position) extends AssignLhs

  sealed case class SelectJSNativeMember(className: ClassName, member: MethodIdent)(
      implicit val pos: Position)
      extends Tree {
    val tpe = AnyType
  }

  /** Apply an instance method with dynamic dispatch (the default). */
  sealed case class Apply(flags: ApplyFlags, receiver: Tree, method: MethodIdent,
      args: List[Tree])(
      val tpe: Type)(implicit val pos: Position) extends Tree

  /** Apply an instance method with static dispatch (e.g., super calls). */
  sealed case class ApplyStatically(flags: ApplyFlags, receiver: Tree,
      className: ClassName, method: MethodIdent, args: List[Tree])(
      val tpe: Type)(implicit val pos: Position) extends Tree

  /** Apply a static method. */
  sealed case class ApplyStatic(flags: ApplyFlags, className: ClassName,
      method: MethodIdent, args: List[Tree])(
      val tpe: Type)(implicit val pos: Position) extends Tree

  /** Apply a static method via dynamic import. */
  sealed case class ApplyDynamicImport(flags: ApplyFlags, className: ClassName,
      method: MethodIdent, args: List[Tree])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** Unary operation (always preserves pureness). */
  sealed case class UnaryOp(op: UnaryOp.Code, lhs: Tree)(
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

    // Long -> Float (neither widening nor narrowing), introduced in 1.6
    final val LongToFloat = 16

    // String.length, introduced in 1.11
    final val String_length = 17

    def resultTypeOf(op: Code): Type = (op: @switch) match {
      case Boolean_! =>
        BooleanType
      case IntToChar =>
        CharType
      case IntToByte =>
        ByteType
      case IntToShort =>
        ShortType
      case CharToInt | ByteToInt | ShortToInt | LongToInt | DoubleToInt | String_length =>
        IntType
      case IntToLong | DoubleToLong =>
        LongType
      case DoubleToFloat | LongToFloat =>
        FloatType
      case IntToDouble | LongToDouble | FloatToDouble =>
        DoubleType
    }
  }

  /** Binary operation (always preserves pureness). */
  sealed case class BinaryOp(op: BinaryOp.Code, lhs: Tree, rhs: Tree)(
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

    // New in 1.11
    final val String_charAt = 58

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
      case String_charAt =>
        CharType
    }
  }

  sealed case class NewArray(typeRef: ArrayTypeRef, lengths: List[Tree])(
      implicit val pos: Position) extends Tree {
    val tpe = ArrayType(typeRef)
  }

  sealed case class ArrayValue(typeRef: ArrayTypeRef, elems: List[Tree])(
      implicit val pos: Position) extends Tree {
    val tpe = ArrayType(typeRef)
  }

  sealed case class ArrayLength(array: Tree)(implicit val pos: Position)
      extends Tree {
    val tpe = IntType
  }

  sealed case class ArraySelect(array: Tree, index: Tree)(val tpe: Type)(
      implicit val pos: Position) extends AssignLhs

  sealed case class RecordValue(tpe: RecordType, elems: List[Tree])(
      implicit val pos: Position) extends Tree

  sealed case class RecordSelect(record: Tree, field: SimpleFieldIdent)(
      val tpe: Type)(
      implicit val pos: Position)
      extends AssignLhs

  sealed case class IsInstanceOf(expr: Tree, testType: Type)(
      implicit val pos: Position)
      extends Tree {
    val tpe = BooleanType
  }

  sealed case class AsInstanceOf(expr: Tree, tpe: Type)(
      implicit val pos: Position)
      extends Tree

  sealed case class GetClass(expr: Tree)(implicit val pos: Position)
      extends Tree {
    val tpe = ClassType(ClassClass)
  }

  sealed case class Clone(expr: Tree)(implicit val pos: Position)
      extends Tree {
    val tpe: Type = expr.tpe // this is OK because our type system does not have singleton types
  }

  sealed case class IdentityHashCode(expr: Tree)(implicit val pos: Position)
      extends Tree {
    val tpe = IntType
  }

  sealed case class WrapAsThrowable(expr: Tree)(implicit val pos: Position)
      extends Tree {
    val tpe = ClassType(ThrowableClass)
  }

  sealed case class UnwrapFromThrowable(expr: Tree)(implicit val pos: Position)
      extends Tree {
    val tpe = AnyType
  }

  // JavaScript expressions

  sealed case class JSNew(ctor: Tree, args: List[TreeOrJSSpread])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  sealed case class JSPrivateSelect(qualifier: Tree, field: FieldIdent)(
      implicit val pos: Position) extends AssignLhs {
    val tpe = AnyType
  }

  sealed case class JSSelect(qualifier: Tree, item: Tree)(
      implicit val pos: Position) extends AssignLhs {
    val tpe = AnyType
  }

  sealed case class JSFunctionApply(fun: Tree, args: List[TreeOrJSSpread])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  sealed case class JSMethodApply(receiver: Tree, method: Tree,
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
   *  JSSuperBrackerSelect(LoadJSConstructor(ClassName("Bar")), qualifier, item)
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
  sealed case class JSSuperSelect(superClass: Tree, receiver: Tree, item: Tree)(
      implicit val pos: Position) extends AssignLhs {
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
   *  JSSuperBrackerCall(LoadJSConstructor(ClassName("Bar")), receiver, method, args)
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
  sealed case class JSSuperMethodCall(superClass: Tree, receiver: Tree,
      method: Tree, args: List[TreeOrJSSpread])(
      implicit val pos: Position)
      extends Tree {
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
  sealed case class JSSuperConstructorCall(args: List[TreeOrJSSpread])(
      implicit val pos: Position) extends Tree {
    val tpe = NoType
  }

  /** JavaScript dynamic import of the form `import(arg)`.
   *
   *  This form is its own node, rather than using something like
   *  {{{
   *  JSFunctionApply(JSImport())
   *  }}}
   *  because `import` is not a first-class term in JavaScript.
   *  `ImportCall` is a dedicated syntactic form that cannot be
   *  dissociated.
   */
  sealed case class JSImportCall(arg: Tree)(implicit val pos: Position)
      extends Tree {
    val tpe = AnyType // it is a JavaScript Promise
  }

  /** JavaScript meta-property `new.target`.
   *
   *  This form is its own node, rather than using something like
   *  {{{
   *  JSSelect(JSNew(), StringLiteral("target"))
   *  }}}
   *  because `new` is not a first-class term in JavaScript. `new.target`
   *  is a dedicated syntactic form that cannot be dissociated.
   */
  sealed case class JSNewTarget()(implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** JavaScript meta-property `import.meta`.
   *
   *  This form is its own node, rather than using something like
   *  {{{
   *  JSSelect(JSImport(), StringLiteral("meta"))
   *  }}}
   *  because `import` is not a first-class term in JavaScript. `import.meta`
   *  is a dedicated syntactic form that cannot be dissociated.
   */
  sealed case class JSImportMeta()(implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** Loads the constructor of a JS class (native or not).
   *
   *  `className` must represent a non-trait JS class (native or not).
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
   *  JSNew(LoadJSConstructor(ClassName("Foo")), List(IntLiteral(1)))
   *  }}}
   *
   *  This node is also useful to encode `o.isInstanceOf[Foo]`:
   *
   *  {{{
   *  JSBinaryOp(instanceof, o, LoadJSConstructor(ClassName("Foo")))
   *  }}}
   *
   *  If `Foo` is non-native, the presence of this node makes it instantiable,
   *  and therefore reachable.
   */
  sealed case class LoadJSConstructor(className: ClassName)(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** Like [[LoadModule]] but for a JS module class. */
  sealed case class LoadJSModule(className: ClassName)(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** `...items`, the "spread" operator of ECMAScript 6.
   *
   *  @param items An Array whose items will be spread (not an arbitrary iterable)
   */
  sealed case class JSSpread(items: Tree)(implicit val pos: Position)
      extends IRNode with TreeOrJSSpread

  /** `delete qualifier[item]` */
  sealed case class JSDelete(qualifier: Tree, item: Tree)(
      implicit val pos: Position)
      extends Tree {

    val tpe = NoType // cannot be in expression position
  }

  /** Unary operation (always preserves pureness).
   *
   *  Operations which do not preserve pureness are not allowed in this tree.
   *  These are notably ++ and --
   */
  sealed case class JSUnaryOp(op: JSUnaryOp.Code, lhs: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = JSUnaryOp.resultTypeOf(op)
  }

  object JSUnaryOp {
    /** Codes are raw Ints to be able to write switch matches on them. */
    type Code = Int

    final val + = 1
    final val - = 2
    final val ~ = 3
    final val ! = 4

    final val typeof = 5

    def resultTypeOf(op: Code): Type =
      AnyType
  }

  /** Binary operation (always preserves pureness).
   *
   *  Operations which do not preserve pureness are not allowed in this tree.
   *  These are notably +=, -=, *=, /= and %=
   */
  sealed case class JSBinaryOp(op: JSBinaryOp.Code, lhs: Tree, rhs: Tree)(
      implicit val pos: Position) extends Tree {
    val tpe = JSBinaryOp.resultTypeOf(op)
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

    // New in 1.12
    final val ** = 22

    def resultTypeOf(op: Code): Type = op match {
      case === | !== =>
        /* We assume that ECMAScript will never pervert `===` and `!==` to the
         * point of them not returning a primitive boolean. This is important
         * for the trees resulting from optimizing `BinaryOp.===` into
         * `JSBinaryOp.===` to be well-typed.
         */
        BooleanType
      case _ =>
        AnyType
    }
  }

  sealed case class JSArrayConstr(items: List[TreeOrJSSpread])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  sealed case class JSObjectConstr(fields: List[(Tree, Tree)])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  sealed case class JSGlobalRef(name: String)(
      implicit val pos: Position) extends AssignLhs {
    import JSGlobalRef._

    val tpe = AnyType

    require(isValidJSGlobalRefName(name),
        s"`$name` is not a valid global ref name")
  }

  object JSGlobalRef {
    /** Set of identifier names that can never be accessed from the global
     *  scope.
     *
     *  This set includes and is limited to:
     *
     *  - All ECMAScript 2015 keywords;
     *  - Identifier names that are treated as keywords or reserved identifier
     *    names in ECMAScript 2015 Strict Mode;
     *  - The identifier `arguments`, because any attempt to refer to it always
     *    refers to the magical `arguments` pseudo-array from the enclosing
     *    function, rather than a global variable.
     *
     *  This set does *not* contain `await`, although it is a reserved word
     *  within ES modules. It used to be allowed before 1.11.0, and even
     *  browsers do not seem to reject it. For compatibility reasons, we only
     *  warn about it at compile time, but the IR allows it.
     */
    final val ReservedJSIdentifierNames: Set[String] = Set(
        "arguments", "break", "case", "catch", "class", "const", "continue",
        "debugger", "default", "delete", "do", "else", "enum", "export",
        "extends", "false", "finally", "for", "function", "if", "implements",
        "import", "in", "instanceof", "interface", "let", "new", "null",
        "package", "private", "protected", "public", "return", "static",
        "super", "switch", "this", "throw", "true", "try", "typeof", "var",
        "void", "while", "with", "yield"
    )

    /** Tests whether the given name is a valid name for a `JSGlobalRef`.
     *
     *  A name is valid iff it is a JavaScript identifier name (see
     *  [[isJSIdentifierName]]) *and* it is not reserved (see
     *  [[ReservedJSIdentifierNames]]).
     */
    def isValidJSGlobalRefName(name: String): Boolean =
      isJSIdentifierName(name) && !ReservedJSIdentifierNames.contains(name)
  }

  sealed case class JSTypeOfGlobalRef(globalRef: JSGlobalRef)(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  sealed case class JSLinkingInfo()(implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  // Literals

  /** Marker for literals. Literals are always pure.
   *
   *  All `Literal`s can be compared for equality. The equality does not take
   *  the `pos` into account.
   */
  sealed trait Literal extends Tree

  /** Marker for literals that can be used in a [[Match]] case.
   *
   *  Matchable literals are:
   *
   *  - `IntLiteral`
   *  - `StringLiteral`
   *  - `Null`
   *
   *  See [[Match]] for the rationale about that specific set.
   */
  sealed trait MatchableLiteral extends Literal

  sealed case class Undefined()(implicit val pos: Position) extends Literal {
    val tpe = UndefType
  }

  sealed case class Null()(implicit val pos: Position) extends MatchableLiteral {
    val tpe = NullType
  }

  sealed case class BooleanLiteral(value: Boolean)(
      implicit val pos: Position) extends Literal {
    val tpe = BooleanType
  }

  sealed case class CharLiteral(value: Char)(
      implicit val pos: Position) extends Literal {
    val tpe = CharType
  }

  sealed case class ByteLiteral(value: Byte)(
      implicit val pos: Position) extends Literal {
    val tpe = ByteType
  }

  sealed case class ShortLiteral(value: Short)(
      implicit val pos: Position) extends Literal {
    val tpe = ShortType
  }

  sealed case class IntLiteral(value: Int)(
      implicit val pos: Position) extends MatchableLiteral {
    val tpe = IntType
  }

  sealed case class LongLiteral(value: Long)(
      implicit val pos: Position) extends Literal {
    val tpe = LongType
  }

  sealed case class FloatLiteral(value: Float)(
      implicit val pos: Position) extends Literal {
    val tpe = FloatType

    override def equals(that: Any): Boolean = that match {
      case that: FloatLiteral => java.lang.Float.compare(this.value, that.value) == 0
      case _                  => false
    }

    override def hashCode(): Int = java.lang.Float.hashCode(value)
  }

  sealed case class DoubleLiteral(value: Double)(
      implicit val pos: Position) extends Literal {
    val tpe = DoubleType

    override def equals(that: Any): Boolean = that match {
      case that: DoubleLiteral => java.lang.Double.compare(this.value, that.value) == 0
      case _                   => false
    }

    override def hashCode(): Int = java.lang.Double.hashCode(value)
  }

  sealed case class StringLiteral(value: String)(
      implicit val pos: Position) extends MatchableLiteral {
    val tpe = StringType
  }

  sealed case class ClassOf(typeRef: TypeRef)(
      implicit val pos: Position) extends Literal {
    val tpe = ClassType(ClassClass)
  }

  // Atomic expressions

  sealed case class VarRef(ident: LocalIdent)(val tpe: Type)(
      implicit val pos: Position) extends AssignLhs

  sealed case class This()(val tpe: Type)(implicit val pos: Position)
      extends Tree

  /** Closure with explicit captures.
   *
   *  @param arrow
   *    If `true`, the closure is an Arrow Function (`=>`), which does not have
   *    an `this` parameter, and cannot be constructed (called with `new`).
   *    If `false`, it is a regular Function (`function`).
   */
  sealed case class Closure(arrow: Boolean, captureParams: List[ParamDef],
      params: List[ParamDef], restParam: Option[ParamDef], body: Tree,
      captureValues: List[Tree])(
      implicit val pos: Position) extends Tree {
    val tpe = AnyType
  }

  /** Creates a JavaScript class value.
   *
   *  @param className
   *    Reference to the `ClassDef` for the class definition, which must have
   *    `jsClassCaptures.nonEmpty`
   *
   *  @param captureValues
   *    Actual values for the captured parameters (in the `ClassDef`'s
   *    `jsClassCaptures.get`)
   */
  sealed case class CreateJSClass(className: ClassName,
      captureValues: List[Tree])(
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
  sealed case class Transient(value: Transient.Value)(
      implicit val pos: Position) extends Tree {
    val tpe = value.tpe
  }

  object Transient {
    /** Common interface for the values that can be stored in [[Transient]]
     *  nodes.
     */
    trait Value {
      /** Type of this transient value. */
      val tpe: Type

      /** Traverses this transient value.
       *
       *  Implementations should delegate traversal to contained trees.
       */
      def traverse(traverser: Traversers.Traverser): Unit

      /** Transforms this transient value.
       *
       *  Implementations should transform contained trees and potentially adjust the result.
       */
      def transform(transformer: Transformers.Transformer, isStat: Boolean)(
          implicit pos: Position): Tree

      /** Prints the IR representation of this transient node.
       *  This method is called by the IR printers when encountering a
       *  [[org.scalajs.ir.Trees.Transient Transient]] node.
       *
       *  @param out
       *    The [[org.scalajs.ir.Printers.IRTreePrinter IRTreePrinter]] to
       *    which the transient node must be printed. It can be used to print
       *    raw strings or nested IR nodes.
       */
      def printIR(out: Printers.IRTreePrinter): Unit
    }
  }

  // Classes

  final class ClassDef(
      val name: ClassIdent,
      val originalName: OriginalName,
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
      val superClass: Option[ClassIdent],
      val interfaces: List[ClassIdent],
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
      val fields: List[AnyFieldDef],
      val methods: List[MethodDef],
      val jsConstructor: Option[JSConstructorDef],
      val jsMethodProps: List[JSMethodPropDef],
      val jsNativeMembers: List[JSNativeMemberDef],
      val topLevelExportDefs: List[TopLevelExportDef]
  )(
      val optimizerHints: OptimizerHints
  )(implicit val pos: Position) extends IRNode {
    def className: ClassName = name.name
  }

  object ClassDef {
    def apply(
        name: ClassIdent,
        originalName: OriginalName,
        kind: ClassKind,
        jsClassCaptures: Option[List[ParamDef]],
        superClass: Option[ClassIdent],
        interfaces: List[ClassIdent],
        jsSuperClass: Option[Tree],
        jsNativeLoadSpec: Option[JSNativeLoadSpec],
        fields: List[AnyFieldDef],
        methods: List[MethodDef],
        jsConstructor: Option[JSConstructorDef],
        jsMethodProps: List[JSMethodPropDef],
        jsNativeMembers: List[JSNativeMemberDef],
        topLevelExportDefs: List[TopLevelExportDef])(
        optimizerHints: OptimizerHints)(
        implicit pos: Position): ClassDef = {
      new ClassDef(name, originalName, kind, jsClassCaptures, superClass,
          interfaces, jsSuperClass, jsNativeLoadSpec, fields, methods,
          jsConstructor, jsMethodProps, jsNativeMembers, topLevelExportDefs)(
          optimizerHints)
    }
  }

  // Class members

  /** Any member of a `ClassDef`.
   *
   *  Partitioned into `AnyFieldDef`, `MethodDef`, `JSConstructorDef` and
   *  `JSMethodPropDef`.
   */
  sealed abstract class MemberDef extends IRNode {
    val flags: MemberFlags
  }

  sealed trait VersionedMemberDef extends MemberDef {
    val version: Version
  }

  sealed abstract class AnyFieldDef extends MemberDef {
    // val name: Ident | Tree
    val ftpe: Type
  }

  sealed case class FieldDef(flags: MemberFlags, name: FieldIdent,
      originalName: OriginalName, ftpe: Type)(
      implicit val pos: Position) extends AnyFieldDef

  sealed case class JSFieldDef(flags: MemberFlags, name: Tree, ftpe: Type)(
      implicit val pos: Position) extends AnyFieldDef

  sealed case class MethodDef(flags: MemberFlags, name: MethodIdent,
      originalName: OriginalName, args: List[ParamDef], resultType: Type,
      body: Option[Tree])(
      val optimizerHints: OptimizerHints, val version: Version)(
      implicit val pos: Position) extends VersionedMemberDef {
    def methodName: MethodName = name.name
  }

  sealed case class JSConstructorDef(flags: MemberFlags,
      args: List[ParamDef], restParam: Option[ParamDef], body: JSConstructorBody)(
      val optimizerHints: OptimizerHints, val version: Version)(
      implicit val pos: Position)
      extends VersionedMemberDef

  sealed case class JSConstructorBody(
      beforeSuper: List[Tree], superCall: JSSuperConstructorCall, afterSuper: List[Tree])(
      implicit val pos: Position)
      extends IRNode {
    val allStats: List[Tree] = beforeSuper ::: superCall :: afterSuper
  }

  sealed abstract class JSMethodPropDef extends VersionedMemberDef

  sealed case class JSMethodDef(flags: MemberFlags, name: Tree,
      args: List[ParamDef], restParam: Option[ParamDef], body: Tree)(
      val optimizerHints: OptimizerHints, val version: Version)(
      implicit val pos: Position)
      extends JSMethodPropDef

  sealed case class JSPropertyDef(flags: MemberFlags, name: Tree,
      getterBody: Option[Tree], setterArgAndBody: Option[(ParamDef, Tree)])(
      val version: Version)(
      implicit val pos: Position)
      extends JSMethodPropDef

  sealed case class JSNativeMemberDef(flags: MemberFlags, name: MethodIdent,
      jsNativeLoadSpec: JSNativeLoadSpec)(
      implicit val pos: Position)
      extends MemberDef

  // Top-level export defs

  sealed abstract class TopLevelExportDef extends IRNode {
    import TopLevelExportDef._

    def moduleID: String

    final def topLevelExportName: String = this match {
      case TopLevelModuleExportDef(_, name)  => name
      case TopLevelJSClassExportDef(_, name) => name

      case TopLevelMethodExportDef(_, JSMethodDef(_, propName, _, _, _)) =>
        val StringLiteral(name) = propName: @unchecked  // unchecked is needed for Scala 3.2+
        name

      case TopLevelFieldExportDef(_, name, _) => name
    }

    require(isValidTopLevelExportName(topLevelExportName),
        s"`$topLevelExportName` is not a valid top-level export name")
  }

  object TopLevelExportDef {
    def isValidTopLevelExportName(exportName: String): Boolean =
      isJSIdentifierName(exportName)
  }

  sealed case class TopLevelJSClassExportDef(moduleID: String, exportName: String)(
      implicit val pos: Position) extends TopLevelExportDef

  /** Export for a top-level object.
   *
   *  This exports the singleton instance of the containing module class.
   *  The instance is initialized during ES module instantiation.
   */
  sealed case class TopLevelModuleExportDef(moduleID: String, exportName: String)(
      implicit val pos: Position) extends TopLevelExportDef

  sealed case class TopLevelMethodExportDef(moduleID: String,
      methodDef: JSMethodDef)(
      implicit val pos: Position) extends TopLevelExportDef

  sealed case class TopLevelFieldExportDef(moduleID: String,
      exportName: String, field: FieldIdent)(
      implicit val pos: Position) extends TopLevelExportDef

  // Miscellaneous

  final class OptimizerHints private (private val bits: Int) extends AnyVal {
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

  final class ApplyFlags private (private val bits: Int) extends AnyVal {
    import ApplyFlags._

    def isPrivate: Boolean = (bits & PrivateBit) != 0

    def isConstructor: Boolean = (bits & ConstructorBit) != 0

    def inline: Boolean = (bits & InlineBit) != 0

    def noinline: Boolean = (bits & NoinlineBit) != 0

    def withPrivate(value: Boolean): ApplyFlags =
      if (value) new ApplyFlags((bits & ~ConstructorBit) | PrivateBit)
      else new ApplyFlags(bits & ~PrivateBit)

    def withConstructor(value: Boolean): ApplyFlags =
      if (value) new ApplyFlags((bits & ~PrivateBit) | ConstructorBit)
      else new ApplyFlags(bits & ~ConstructorBit)

    def withInline(value: Boolean): ApplyFlags =
      if (value) new ApplyFlags(bits | InlineBit)
      else new ApplyFlags(bits & ~InlineBit)

    def withNoinline(value: Boolean): ApplyFlags =
      if (value) new ApplyFlags(bits | NoinlineBit)
      else new ApplyFlags(bits & ~NoinlineBit)
  }

  object ApplyFlags {
    private final val PrivateShift = 0
    private final val PrivateBit = 1 << PrivateShift

    private final val ConstructorShift = 1
    private final val ConstructorBit = 1 << ConstructorShift

    private final val InlineShift = 2
    private final val InlineBit = 1 << InlineShift

    private final val NoinlineShift = 3
    private final val NoinlineBit = 1 << NoinlineShift

    final val empty: ApplyFlags =
      new ApplyFlags(0)

    private[ir] def fromBits(bits: Int): ApplyFlags =
      new ApplyFlags(bits)

    private[ir] def toBits(flags: ApplyFlags): Int =
      flags.bits
  }

  final class MemberNamespace private (
      val ordinal: Int) // intentionally public
      extends AnyVal {

    import MemberNamespace._

    def isStatic: Boolean = (ordinal & StaticFlag) != 0

    def isPrivate: Boolean = (ordinal & PrivateFlag) != 0

    def isConstructor: Boolean = (ordinal & ConstructorFlag) != 0

    def prefixString: String = this match {
      case Public            => ""
      case Private           => "private "
      case PublicStatic      => "static "
      case PrivateStatic     => "private static "
      case Constructor       => "constructor "
      case StaticConstructor => "static constructor "
    }
  }

  object MemberNamespace {
    private final val StaticShift = 0
    private final val StaticFlag = 1 << StaticShift

    private final val PrivateShift = 1
    private final val PrivateFlag = 1 << PrivateShift

    private final val ConstructorShift = 2
    private final val ConstructorFlag = 1 << ConstructorShift

    final val Public: MemberNamespace =
      new MemberNamespace(0)

    final val PublicStatic: MemberNamespace =
      new MemberNamespace(StaticFlag)

    final val Private: MemberNamespace =
      new MemberNamespace(PrivateFlag)

    final val PrivateStatic: MemberNamespace =
      new MemberNamespace(PrivateFlag | StaticFlag)

    final val Constructor: MemberNamespace =
      new MemberNamespace(ConstructorFlag)

    final val StaticConstructor: MemberNamespace =
      new MemberNamespace(ConstructorFlag | StaticFlag)

    final val Count = 6

    def fromOrdinal(ordinal: Int): MemberNamespace = {
      require(0 <= ordinal && ordinal < Count,
          s"Invalid namespace ordinal $ordinal")
      new MemberNamespace(ordinal)
    }

    private[Trees] def fromOrdinalUnchecked(ordinal: Int): MemberNamespace =
      new MemberNamespace(ordinal)

    def forNonStaticCall(flags: ApplyFlags): MemberNamespace = {
        if (flags.isPrivate) Private
        else if (flags.isConstructor) Constructor
        else Public
      }

    def forStaticCall(flags: ApplyFlags): MemberNamespace =
      if (flags.isPrivate) PrivateStatic else PublicStatic
  }

  final class MemberFlags private (private val bits: Int) extends AnyVal {
    import MemberFlags._

    def namespace: MemberNamespace =
      MemberNamespace.fromOrdinalUnchecked(bits & NamespaceMask)

    def isMutable: Boolean = (bits & MutableBit) != 0

    def withNamespace(namespace: MemberNamespace): MemberFlags =
      new MemberFlags((bits & ~NamespaceMask) | namespace.ordinal)

    def withMutable(value: Boolean): MemberFlags =
      if (value) new MemberFlags(bits | MutableBit)
      else new MemberFlags(bits & ~MutableBit)
  }

  object MemberFlags {
    /* NamespaceMask must remain with no shift, for easy conversion between
     * MemberFlags and MemberNamespace.
     */
    private final val NamespaceMask = 7

    private final val MutableShift = 3
    private final val MutableBit = 1 << MutableShift

    final val empty: MemberFlags =
      new MemberFlags(0)

    private[ir] def fromBits(bits: Int): MemberFlags =
      new MemberFlags(bits)

    private[ir] def toBits(flags: MemberFlags): Int =
      flags.bits
  }

  /** Loading specification for a native JS class or object. */
  sealed abstract class JSNativeLoadSpec

  object JSNativeLoadSpec {

    /** Load from the global scope.
     *
     *  The `globalRef` is the name of a global variable (found in the global
     *  scope). It must be valid according to
     *  [[JSGlobalRef.isValidJSGlobalRefName]].
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
        extends JSNativeLoadSpec {

      require(JSGlobalRef.isValidJSGlobalRefName(globalRef))
    }

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
}
