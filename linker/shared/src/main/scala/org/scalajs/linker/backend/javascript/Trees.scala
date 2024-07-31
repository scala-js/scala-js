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

package org.scalajs.linker.backend.javascript

import scala.annotation.switch

import java.nio.charset.StandardCharsets

import org.scalajs.ir
import org.scalajs.ir.{OriginalName, Position}
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Position.NoPosition

object Trees {
  /* The case classes for JS Trees are sealed instead of final because making
   * them final triggers bugs with 2.12.{1-4}, in combination
   * with their `implicit val pos`.
   */

  /** AST node of JavaScript. */
  abstract sealed class Tree {
    val pos: Position

    def show: String = Printers.showTree(this)
  }

  // Constructor comment / annotation.

  sealed case class JSDocConstructor(tree: Tree)(implicit val pos: Position)
      extends Tree

  // Identifiers and properties

  sealed trait PropertyName {
    def pos: Position
  }

  sealed trait MaybeDelayedIdent extends PropertyName {
    def resolveName(): String
  }

  sealed case class Ident(name: String, originalName: OriginalName)(
      implicit val pos: Position) extends MaybeDelayedIdent {
    Ident.requireValidJSIdentifierName(name)

    def resolveName(): String = name
  }

  object Ident {
    def apply(name: String)(implicit pos: Position): Ident =
      new Ident(name, NoOriginalName)

    def requireValidJSIdentifierName(name: String): Unit = {
      require(isValidJSIdentifierName(name),
          s"'$name' is not a valid JS identifier name")
    }

    /** Tests whether the given string is a valid `IdentifierName` for the
     *  ECMAScript language specification.
     *
     *  This does not exclude keywords, as they can be used as identifiers in
     *  some productions, notably as property names.
     */
    def isValidJSIdentifierName(name: String): Boolean = {
      // scalastyle:off return
      // This method is called on every `Ident` creation; it should be fast.
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
  }

  /** An ident whose real name will be resolved later. */
  sealed case class DelayedIdent(resolver: DelayedIdent.Resolver, originalName: OriginalName)(
      implicit val pos: Position)
      extends MaybeDelayedIdent {

    def resolveName(): String = {
      val name = resolver.resolve()
      Ident.requireValidJSIdentifierName(name)
      name
    }
  }

  object DelayedIdent {
    def apply(resolver: DelayedIdent.Resolver)(implicit pos: Position): DelayedIdent =
      new DelayedIdent(resolver, NoOriginalName)

    /** Resolver for the eventual name of a `DelayedIdent`. */
    trait Resolver {
      /** Resolves the eventual name of the delayed ident.
       *
       *  @throws java.lang.IllegalStateException
       *    if this resolver is not yet ready to resolve a name
       */
      def resolve(): String

      /** A string representing this resolver for debugging purposes.
       *
       *  The result of this method is used when calling `show` on the
       *  associated `DelayedIdent`. Once the resolver is ready, this method is
       *  encouraged to return the same string as `resolve()`, but it is not
       *  mandatory to do so.
       */
      def debugString: String
    }
  }

  sealed case class ComputedName(tree: Tree) extends PropertyName {
    def pos: Position = tree.pos
  }

  // Definitions

  sealed trait LocalDef extends Tree {
    def name: Ident
    def mutable: Boolean

    def ref(implicit pos: Position): Tree = VarRef(name)
  }

  sealed case class VarDef(name: Ident, rhs: Option[Tree])(
      implicit val pos: Position)
      extends LocalDef {
    def mutable: Boolean = true
  }

  /** ES6 let or const (depending on the mutable flag). */
  sealed case class Let(name: Ident, mutable: Boolean, rhs: Option[Tree])(
      implicit val pos: Position)
      extends LocalDef

  sealed case class ParamDef(name: Ident)(implicit val pos: Position)
      extends LocalDef {
    def mutable: Boolean = true
  }

  // Control flow constructs

  sealed case class Skip()(implicit val pos: Position) extends Tree

  sealed class Block private (val stats: List[Tree])(
      implicit val pos: Position)
      extends Tree {
    override def toString(): String =
      stats.mkString("Block(", ",", ")")
  }

  object Block {
    def apply(stats: Iterable[Tree])(implicit pos: Position): Tree =
      apply(stats.iterator)

    def apply(stats: Iterator[Tree])(implicit pos: Position): Tree = {
      /* Do a fused _.flatMap(...).toList on our own. This is the only
       * implementation I could come up with that:
       * - Is efficient.
       * - Works on 2.12 and 2.13.
       * - Does not duplicate code.
       *
       * Not fusing this would produce a relatively complex iterator construct for no reason.
       */
      val builder = List.newBuilder[Tree]

      val flattenedStats = stats.foreach {
        case Skip()          => // skip :)
        case Block(subStats) => builder ++= subStats
        case other           => builder += other
      }

      builder.result() match {
        case Nil         => Skip()
        case only :: Nil => only
        case stats       => new Block(stats)
      }
    }

    def apply(stats: Tree*)(implicit pos: Position): Tree =
      apply(stats)

    def unapply(block: Block): Some[List[Tree]] = Some(block.stats)
  }

  sealed case class Labeled(label: Ident, body: Tree)(
      implicit val pos: Position)
      extends Tree

  sealed case class Assign(lhs: Tree, rhs: Tree)(implicit val pos: Position)
      extends Tree {
    require(lhs match {
      case _:VarRef | _:DotSelect | _:BracketSelect => true
      case _ => false
    }, s"Invalid lhs for Assign: $lhs")
  }

  sealed case class Return(expr: Tree)(implicit val pos: Position) extends Tree

  sealed case class If(cond: Tree, thenp: Tree, elsep: Tree)(
      implicit val pos: Position)
      extends Tree

  object If {
    def apply(cond: Tree, thenp: Tree)(implicit pos: Position): If =
      If(cond, thenp, Skip())
  }

  sealed case class While(cond: Tree, body: Tree, label: Option[Ident] = None)(
      implicit val pos: Position)
      extends Tree

  sealed case class DoWhile(body: Tree, cond: Tree, label: Option[Ident] = None)(
      implicit val pos: Position)
      extends Tree

  sealed case class ForIn(lhs: Tree, obj: Tree, body: Tree)(
      implicit val pos: Position)
      extends Tree

  sealed case class For(init: Tree, guard: Tree, update: Tree, body: Tree)(
      implicit val pos: Position)
      extends Tree

  sealed case class TryCatch(block: Tree, errVar: Ident, handler: Tree)(
      implicit val pos: Position)
      extends Tree

  sealed case class TryFinally(block: Tree, finalizer: Tree)(
      implicit val pos: Position)
      extends Tree

  sealed case class Throw(expr: Tree)(implicit val pos: Position) extends Tree

  sealed case class Break(label: Option[Ident] = None)(
      implicit val pos: Position)
      extends Tree

  sealed case class Continue(label: Option[Ident] = None)(
      implicit val pos: Position)
      extends Tree

  sealed case class Switch(selector: Tree, cases: List[(Tree, Tree)],
      default: Tree)(
      implicit val pos: Position)
      extends Tree

  sealed case class Debugger()(implicit val pos: Position) extends Tree

  // Expressions

  sealed case class New(ctor: Tree, args: List[Tree])(
      implicit val pos: Position)
      extends Tree

  sealed case class DotSelect(qualifier: Tree, item: MaybeDelayedIdent)(
      implicit val pos: Position)
      extends Tree

  sealed case class BracketSelect(qualifier: Tree, item: Tree)(
      implicit val pos: Position)
      extends Tree

  /** Syntactic apply.
   *  It is a method call if fun is a dot-select or bracket-select. It is a
   *  function call otherwise.
   */
  sealed case class Apply(fun: Tree, args: List[Tree])(
      implicit val pos: Position)
      extends Tree

  /** Dynamic `import(arg)`. */
  sealed case class ImportCall(arg: Tree)(implicit val pos: Position)
      extends Tree

  /** Meta-property `new.target`. */
  sealed case class NewTarget()(implicit val pos: Position) extends Tree

  /** Meta-property `import.meta`. */
  sealed case class ImportMeta()(implicit val pos: Position) extends Tree

  /** `...items`, the "spread" operator of ECMAScript 6.
   *
   *  It is only valid in ECMAScript 6, in the `args`/`items` of a [[New]],
   *  [[Apply]], or [[ArrayConstr]].
   *
   *  @param items An iterable whose items will be spread
   */
  sealed case class Spread(items: Tree)(implicit val pos: Position) extends Tree

  sealed case class Delete(prop: Tree)(implicit val pos: Position) extends Tree {
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
  sealed case class UnaryOp(op: UnaryOp.Code, lhs: Tree)(
      implicit val pos: Position)
      extends Tree

  object UnaryOp {
    /** Codes are the same as in the IR. */
    type Code = ir.Trees.JSUnaryOp.Code
  }

  sealed case class Await(expr: Tree)(implicit val pos: Position) extends Tree

  /** `++x`, `x++`, `--x` or `x--`. */
  sealed case class IncDec(prefix: Boolean, inc: Boolean, arg: Tree)(
      implicit val pos: Position)
      extends Tree

  /** Binary operation (always preserves pureness).
   *
   *  Operations which do not preserve pureness are not allowed in this tree.
   *  These are notably +=, -=, *=, /= and %=
   */
  sealed case class BinaryOp(op: BinaryOp.Code, lhs: Tree, rhs: Tree)(
      implicit val pos: Position)
      extends Tree

  object BinaryOp {
    /** Codes are the same as in the IR. */
    type Code = ir.Trees.JSBinaryOp.Code
  }

  sealed case class ArrayConstr(items: List[Tree])(implicit val pos: Position)
      extends Tree

  sealed case class ObjectConstr(fields: List[(PropertyName, Tree)])(
      implicit val pos: Position)
      extends Tree

  // Literals

  /** Marker for literals. Literals are always pure. */
  sealed trait Literal extends Tree

  sealed case class Undefined()(implicit val pos: Position) extends Literal

  sealed case class Null()(implicit val pos: Position) extends Literal

  sealed case class BooleanLiteral(value: Boolean)(implicit val pos: Position)
      extends Literal

  sealed case class IntLiteral(value: Int)(implicit val pos: Position)
      extends Literal

  sealed case class DoubleLiteral(value: Double)(implicit val pos: Position)
      extends Literal

  sealed case class StringLiteral(value: String)(
      implicit val pos: Position) extends Literal with PropertyName

  sealed case class BigIntLiteral(value: BigInt)(
      implicit val pos: Position) extends Literal

  // Atomic expressions

  sealed case class VarRef(ident: Ident)(implicit val pos: Position)
      extends Tree

  sealed case class This()(implicit val pos: Position) extends Tree

  sealed case class Function(arrow: Boolean, args: List[ParamDef],
      restParam: Option[ParamDef], body: Tree)(
      implicit val pos: Position) extends Tree

  // Named function definition

  sealed case class FunctionDef(name: Ident, args: List[ParamDef],
      restParam: Option[ParamDef], body: Tree)(
      implicit val pos: Position) extends Tree

  // ECMAScript 6 classes

  sealed case class ClassDef(className: Option[Ident],
      parentClass: Option[Tree], members: List[Tree])(
      implicit val pos: Position)
      extends Tree

  sealed case class MethodDef(static: Boolean, name: PropertyName,
      args: List[ParamDef], restParam: Option[ParamDef], body: Tree)(
      implicit val pos: Position)
      extends Tree

  sealed case class GetterDef(static: Boolean, name: PropertyName, body: Tree)(
      implicit val pos: Position)
      extends Tree

  sealed case class SetterDef(static: Boolean, name: PropertyName,
      param: ParamDef, body: Tree)(
      implicit val pos: Position)
      extends Tree

  sealed case class Super()(implicit val pos: Position) extends Tree

  // ECMAScript 6 modules

  /** The name of an ES module export.
   *
   *  It must be a valid `IdentifierName`, as tested by
   *  [[ExportName.isValidExportName]].
   */
  sealed case class ExportName(name: String)(implicit val pos: Position) {
    require(ExportName.isValidExportName(name),
        s"'$name' is not a valid export name")
  }

  object ExportName {
    /** Tests whether a string is a valid export name.
     *
     *  A string is a valid export name if and only if it is a valid ECMAScript
     *  `IdentifierName`, which is defined in
     *  [[http://www.ecma-international.org/ecma-262/6.0/#sec-names-and-keywords
     *  Section 11.6 of the ECMAScript 2015 specification]].
     *
     *  Currently, this implementation is buggy in some corner cases, as it does
     *  not accept code points with the Unicode properties `Other_ID_Start` and
     *  `Other_ID_Continue`. For example,
     *  `isValidIdentifierName(0x2118.toChar.toString)` will return `false`
     *  instead of `true`.
     *
     *  In theory, it does not really account for code points with the Unicode
     *  properties `Pattern_Syntax` and `Pattern_White_Space`, which should be
     *  rejected. However, with the current version of Unicode (9.0.0), there
     *  seems to be no such character that would be accepted by this method.
     */
    final def isValidExportName(name: String): Boolean = {
      // scalastyle:off return
      import java.lang.Character._

      def isJSIdentifierStart(cp: Int): Boolean =
        isUnicodeIdentifierStart(cp) || cp == '$' || cp == '_'

      def isJSIdentifierPart(cp: Int): Boolean = {
        val ZWNJ = 0x200c
        val ZWJ = 0x200d
        isUnicodeIdentifierPart(cp) || cp == '$' || cp == '_' || cp == ZWNJ || cp == ZWJ
      }

      if (name.isEmpty)
        return false

      val firstCP = name.codePointAt(0)
      if (!isJSIdentifierStart(firstCP))
        return false

      var i = charCount(firstCP)
      while (i < name.length) {
        val cp = name.codePointAt(i)
        if (!isJSIdentifierPart(cp))
          return false
        i += charCount(cp)
      }

      true
      // scalastyle:on return
    }
  }

  /** `import` statement, except namespace import.
   *
   *  This corresponds to the following syntax:
   *  {{{
   *  import { <binding1_1> as <binding1_2>, ..., <bindingN_1> as <bindingN_2> } from <from>
   *  }}}
   *  The `_1` parts of bindings are therefore the identifier names that are
   *  imported, as specified in `export` clauses of the module. The `_2` parts
   *  are the names under which they are imported in the current module.
   *
   *  Special cases:
   *  - When `_1.name == _2.name`, there is shorter syntax in ES, i.e.,
   *    `import { binding } from 'from'`.
   *  - When `_1.name == "default"`, it is equivalent to a default import.
   */
  sealed case class Import(bindings: List[(ExportName, Ident)],
      from: StringLiteral)(
      implicit val pos: Position)
      extends Tree

  /** Namespace `import` statement.
   *
   *  This corresponds to the following syntax:
   *  {{{
   *  import * as <binding> from <from>
   *  }}}
   */
  sealed case class ImportNamespace(binding: Ident, from: StringLiteral)(
      implicit val pos: Position)
      extends Tree

  /** `export` statement.
   *
   *  This corresponds to the following syntax:
   *  {{{
   *  export { <binding1_1> as <binding1_2>, ..., <bindingN_1> as <bindingN_2> }
   *  }}}
   *  The `_1` parts of bindings are therefore the identifiers from the current
   *  module that are exported. The `_2` parts are the names under which they
   *  are exported to other modules.
   */
  sealed case class Export(bindings: List[(Ident, ExportName)])(
      implicit val pos: Position)
      extends Tree

  /** "forwarding" `export` statement.
   *
   *  This corresponds to the following syntax:
   *  {{{
   *  export { <binding1_1> as <binding1_2>, ..., <bindingN_1> as <bindingN_2> } from <from>
   *  }}}
   *  The `_1` parts of bindings are the identifiers that are imported.
   *  The `_2` parts are the names under which they are exported.
   */
  sealed case class ExportImport(bindings: List[(ExportName, ExportName)],
      from: StringLiteral)(
      implicit val pos: Position)
      extends Tree

  /** An already printed tree.
   *
   *  This is a special purpose node to store partially transformed trees.
   *
   *  A cleaner abstraction would be to have something like ir.Tree.Transient
   *  (for different output formats), but for now, we do not need this.
   */
  sealed case class PrintedTree(jsCode: Array[Byte],
      sourceMapFragment: SourceMapWriter.Fragment) extends Tree {
    val pos: Position = Position.NoPosition

    override def show: String = new String(jsCode, StandardCharsets.UTF_8)
  }

  object PrintedTree {
    def empty: PrintedTree = PrintedTree(Array(), SourceMapWriter.Fragment.Empty)
  }
}
