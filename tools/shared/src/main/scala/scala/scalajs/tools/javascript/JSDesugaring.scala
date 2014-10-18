/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.javascript

import scala.language.implicitConversions

import scala.annotation.switch

import scala.scalajs.ir._
import Position._
import Transformers._
import scala.scalajs.ir.Trees._
import scala.scalajs.ir.Types._

import scala.scalajs.tools.sem._
import CheckedBehaviors._

import scala.scalajs.tools.javascript.{Trees => js}

/** Desugaring of the IR to regular ES5 JavaScript.
 *
 *  The major difference between the IR and JS is that most constructs can be
 *  used in expression position. The main work of the desugaring is to
 *  unnest complex constructs in expression position so that they become
 *  statements.
 *
 *  The general idea is two-folded:
 *  1) Unnest complex constructs in "argument position":
 *     When a complex construct is used in a non-rhs expression position
 *     (argument to a function, operand, condition of an if, etc.), that we
 *     call "argument position", declare a variable before the statement,
 *     assign the complex construct to it and then use that variable in the
 *     argument position instead.
 *  2) Push LHS's inside complex RHS's:
 *     When an rhs is a complex construct, push the lhs inside the complex
 *     construct. Are considered lhs:
 *     * Assign, i.e., `x =`
 *     * VarDef, i.e., `var x =`
 *     * Return, i.e., `return`
 *     * (EmptyTree is also used as a trick for code reuse)
 *     In fact, think that, in this context, LHS means: what to do with the
 *     result of evaluating the RHS.
 *
 *  --------------------------------------------------------------------------
 *
 *  Typical example, consider the method call:
 *
 *  obj.meth({
 *    var x = foo(42);
 *    x*x
 *  });
 *
 *  According to rule 1), the block that is passed as a parameter to obj.meth
 *  is first extracted in a synthetic var:
 *
 *  var x\$1 = {
 *    var x = foo(42);
 *    x*x
 *  }
 *  obj.meth(x\$1);
 *
 *  Then, according to rule 2), the lhs `var x\$1 =` is pushed inside the block:
 *
 *  {
 *    var x = foo(42);
 *    var x\$1 = x*x;
 *  }
 *  obj.meth(x\$1);
 *
 *  Because bare blocks are non-significant in JS, this is equivalent to
 *
 *  var x = foo(42);
 *  var x\$1 = x*x;
 *  obj.meth(x\$1);
 *
 *  --------------------------------------------------------------------------
 *
 *  JSDesugaring does all this in a single pass, but it helps to think that:
 *  * Rule 1) is implemented by unnest(), and used most notably in
 *    * transformStat() for statement-only constructs
 *    * pushLhsInto() for statement-or-expression constructs
 *  * Rule 2) is implemented by pushLhsInto()
 *  * Emitting the class structure is delegated to [[ScalaJSClassEmitter]].
 *
 *  There are a few other things that JSDesugaring takes care of:
 *  * Transform Scala expressions into their JS equivalent, taking the
 *    Scala.js class encoding into account.
 *  * And tiny details.
 *
 *  @author Sébastien Doeraene
 */
object JSDesugaring {

  private final val ScalaJSEnvironmentName = "ScalaJS"

  /** Desugars a statement of the IR into ES5 JavaScript. */
  def desugarJavaScript(tree: Tree, semantics: Semantics): js.Tree = {
    new JSDesugar(semantics).transformStat(tree)
  }

  private[javascript] implicit def transformIdent(ident: Ident): js.Ident =
    js.Ident(ident.name, ident.originalName)(ident.pos)

  private[javascript] def transformParamDef(paramDef: ParamDef): js.ParamDef =
    js.ParamDef(paramDef.name, paramDef.mutable)(paramDef.pos)

  private class JSDesugar(semantics: Semantics) {
    private val behaviors = semantics.checkedBehaviors

    // Synthetic variables

    var syntheticVarCounter: Int = 0

    def newSyntheticVar()(implicit pos: Position): Ident = {
      syntheticVarCounter += 1
      Ident("jsx$" + syntheticVarCounter, None)
    }

    def resetSyntheticVarCounterIn[A](f: => A): A = {
      val savedCounter = syntheticVarCounter
      syntheticVarCounter = 0
      try f
      finally syntheticVarCounter = savedCounter
    }

    // Record names

    def makeRecordFieldIdent(recIdent: Ident, fieldIdent: Ident)(
        implicit pos: Position): Ident =
      makeRecordFieldIdent(recIdent.name, recIdent.originalName,
          fieldIdent.name, fieldIdent.originalName)

    def makeRecordFieldIdent(recIdent: Ident,
        fieldName: String, fieldOrigiName: Option[String])(
        implicit pos: Position): Ident =
      makeRecordFieldIdent(recIdent.name, recIdent.originalName,
          fieldName, fieldOrigiName)

    def makeRecordFieldIdent(recName: String, recOrigName: Option[String],
        fieldName: String, fieldOrigName: Option[String])(
        implicit pos: Position): Ident = {
      val name = recName + "_$_" + fieldName
      val originalName = Some(recOrigName.getOrElse(recName) + "." +
          fieldOrigName.getOrElse(fieldName))
      Ident(name, originalName)
    }

    // LHS'es for labeled expressions

    var labeledExprLHSes: Map[Ident, Tree] = Map.empty

    // Now the work

    /** Desugar a statement of the IR into ES5 JS */
    def transformStat(tree: Tree): js.Tree = {
      implicit val pos = tree.pos

      tree match {
        // Statement-only language constructs

        case Skip() =>
          js.Skip()

        case VarDef(varIdent, RecordType(fields), recMutable, EmptyTree) =>
          js.Block(for {
            RecordType.Field(fieldName, fieldOrigName, tpe, fieldMutable) <- fields
          } yield {
            transformStat {
              VarDef(makeRecordFieldIdent(varIdent, fieldName, fieldOrigName),
                  tpe, recMutable || fieldMutable, EmptyTree)
            }
          })

        case VarDef(name, _, mutable, EmptyTree) =>
          js.VarDef(name, mutable, js.EmptyTree)

        case VarDef(_, _, _, rhs) =>
          pushLhsInto(tree, rhs)

        case Assign(RecordFieldVarRef(lhs), rhs) =>
          pushLhsInto(Assign(lhs, EmptyTree), rhs)

        case Assign(select @ Select(qualifier, item, mutable), rhs) =>
          unnest(qualifier, rhs) { (newQualifier, newRhs) =>
            js.Assign(
                js.DotSelect(transformExpr(newQualifier), item)(select.pos),
                transformExpr(newRhs))
          }

        case Assign(select @ ArraySelect(array, index), rhs) =>
          unnest(List(array, index, rhs)) {
            case List(newArray, newIndex, newRhs) =>
              js.Assign(
                  js.BracketSelect(js.DotSelect(transformExpr(newArray),
                      js.Ident("u"))(select.pos),
                      transformExpr(newIndex))(select.pos),
                  transformExpr(newRhs))
          }

        case Assign(select @ JSDotSelect(qualifier, item), rhs) =>
          unnest(qualifier, rhs) { (newQualifier, newRhs) =>
            js.Assign(
                js.DotSelect(transformExpr(newQualifier), item)(select.pos),
                transformExpr(newRhs))
          }

        case Assign(select @ JSBracketSelect(qualifier, item), rhs) =>
          unnest(List(qualifier, item, rhs)) {
            case List(newQualifier, newItem, newRhs) =>
              js.Assign(
                  js.BracketSelect(transformExpr(newQualifier),
                      transformExpr(newItem))(select.pos),
                  transformExpr(newRhs))
          }

        case Assign(_ : VarRef, rhs) =>
          pushLhsInto(tree, rhs)

        case Assign(_, _) =>
          sys.error(s"Illegal Assign in transformStat: $tree")

        case StoreModule(cls, value) =>
          assert(cls.className.endsWith("$"),
              s"Trying to store module for non-module class $cls")
          val moduleName = cls.className.dropRight(1)
          unnest(value) { newValue =>
            js.Assign(
                js.DotSelect(envField("n"), Ident(moduleName)),
                transformExpr(newValue))
          }

        case While(cond, body, label) =>
          /* We cannot simply unnest(cond) here, because that would eject the
           * evaluation of the condition out of the loop.
           */
          val newLabel = label.map(transformIdent)
          if (isExpression(cond)) {
            js.While(transformExpr(cond), transformStat(body), newLabel)
          } else {
            js.While(js.BooleanLiteral(true), {
              unnest(cond) { newCond =>
                js.If(transformExpr(newCond), transformStat(body), js.Break())
              }
            }, newLabel)
          }

        case DoWhile(body, cond, label) =>
          /* We cannot simply unnest(cond) here, because that would eject the
           * evaluation of the condition out of the loop.
           */
          val newLabel = label.map(transformIdent)
          if (isExpression(cond)) {
            js.DoWhile(transformStat(body), transformExpr(cond), newLabel)
          } else {
            /* This breaks 'continue' statements for this loop, but we don't
             * care because we never emit continue statements for do..while
             * loops.
             */
            js.While(js.BooleanLiteral(true), {
              js.Block(transformStat(body), {
                unnest(cond) { newCond =>
                  js.If(transformExpr(newCond), js.Skip(), js.Break())
                }
              })
            }, newLabel)
          }

        case Debugger() =>
          js.Debugger()

        case JSDelete(JSDotSelect(obj, prop)) =>
          unnest(obj) { (newObj) =>
            js.Delete(js.DotSelect(transformExpr(newObj), prop))
          }

        case JSDelete(JSBracketSelect(obj, prop)) =>
          unnest(obj, prop) { (newObj, newProp) =>
            js.Delete(js.BracketSelect(
                transformExpr(newObj), transformExpr(newProp)))
          }

        // Treat 'return' as an LHS

        case Return(expr, label) =>
          pushLhsInto(tree, expr)

        /* Anything else is an expression => pushLhsInto(EmptyTree, _)
         * In order not to duplicate all the code of pushLhsInto() here, we
         * use a trick: EmptyTree is a dummy LHS that says "do nothing
         * with the result of the rhs".
         * This is exactly what an expression statement is doing: it evaluates
         * the expression, but does nothing with its result.
         */

        case _ =>
          pushLhsInto(EmptyTree, tree)
      }
    }

    private object RecordFieldVarRef {
      def unapply(tree: Tree): Option[VarRef] = {
        tree match {
          case Select(RecordVarRef(VarRef(recIdent, recMutable)),
              fieldIdent, fieldMutable) =>
            implicit val pos = tree.pos
            Some(VarRef(makeRecordFieldIdent(recIdent, fieldIdent),
                recMutable || fieldMutable)(tree.tpe))
          case _ =>
            None
        }
      }
    }

    private object RecordVarRef {
      def unapply(tree: Tree): Option[VarRef] = {
        if (!tree.tpe.isInstanceOf[RecordType]) None
        else {
          tree match {
            case tree: VarRef => Some(tree)
            case Select(RecordVarRef(VarRef(recIdent, recMutable)),
                fieldIdent, fieldMutable) =>
              implicit val pos = tree.pos
              Some(VarRef(makeRecordFieldIdent(recIdent, fieldIdent),
                  recMutable || fieldMutable)(tree.tpe))
          }
        }
      }
    }

    /** Unnest complex constructs in argument position in temporary variables
     *
     *  If all the arguments are JS expressions, there is nothing to do.
     *  Any argument that is not a JS expression must be unnested and stored
     *  in a temporary variable before the statement produced by `makeStat`.
     *
     *  But *this changes the evaluation order!* In order not to lose it, it
     *  is necessary to also unnest arguments that are expressions but that
     *  are supposed to be evaluated before the argument-to-be-unnested and
     *  could have side-effects or even whose evaluation could be influenced
     *  by the side-effects of another unnested argument.
     *
     *  Without deep effect analysis, which we do not do, we need to take
     *  a very pessimistic approach, and unnest any expression that contains
     *  an identifier (except those after the last non-expression argument).
     *  Hence the predicate `isPureExpressionWithoutIdent`.
     */
    def unnest(args: List[Tree])(
        makeStat: List[Tree] => js.Tree): js.Tree = {
      if (args forall isExpression) makeStat(args)
      else {
        val extractedStatements = new scala.collection.mutable.ListBuffer[js.Tree]

        /* Attention! Everything must be processed recursively
         * *right-to-left*! Indeed, the point is that noExtractYet will tell
         * whether anything supposed to be evaluated *after* the currently
         * being processed expression has been (at least partly) extracted
         * in temporary variables (or simply statements, in the Block case).
         * If nothing has, we can keep more in place without having to extract
         * that expression in a temporary variable.
         */

        def rec(arg: Tree): Tree = {
          def noExtractYet = extractedStatements.isEmpty

          if (if (noExtractYet) isExpression(arg) else isPureExpression(arg)) {
            arg
          } else {
            implicit val pos = arg.pos
            arg match {
              case Block(stats :+ expr) =>
                val result = rec(expr) // right-to-left, remember?
                // Put the stats in a Block because ++=: is not smart
                js.Block(stats.map(transformStat)) +=: extractedStatements
                result

              case UnaryOp(op, lhs) =>
                UnaryOp(op, rec(lhs))
              case BinaryOp(op, lhs, rhs) =>
                val newRhs = rec(rhs)
                BinaryOp(op, rec(lhs), newRhs)
              case JSBinaryOp(op, lhs, rhs) =>
                val newRhs = rec(rhs)
                JSBinaryOp(op, rec(lhs), newRhs)
              case JSUnaryOp(op, lhs) =>
                JSUnaryOp(op, rec(lhs))
              case IsInstanceOf(expr, tpe) =>
                IsInstanceOf(rec(expr), tpe)
              case AsInstanceOf(expr, tpe) =>
                AsInstanceOf(rec(expr), tpe)

              case NewArray(tpe, lengths) =>
                NewArray(tpe, recs(lengths))
              case ArrayValue(tpe, elems) =>
                ArrayValue(tpe, recs(elems))
              case JSArrayConstr(items) =>
                JSArrayConstr(recs(items))
              case JSObjectConstr(items) =>
                val newValues = recs(items.map(_._2))
                JSObjectConstr(items.map(_._1) zip newValues)
              case Closure(thisType, args, resultType, body, captures) =>
                Closure(thisType, args, resultType, body, recs(captures))

              case CallHelper(helper, args)
                  if noExtractYet || isHelperThatPreservesSideEffectFreedom(helper) =>
                CallHelper(helper, recs(args))(arg.tpe)

              case New(cls, constr, args) if noExtractYet =>
                New(cls, constr, recs(args))
              case Select(qualifier, item, mutable) if noExtractYet =>
                Select(rec(qualifier), item, mutable)(arg.tpe)
              case Apply(receiver, method, args) if noExtractYet =>
                val newArgs = recs(args)
                Apply(rec(receiver), method, newArgs)(arg.tpe)
              case StaticApply(receiver, cls, method, args) if noExtractYet =>
                val newArgs = recs(args)
                StaticApply(rec(receiver), cls, method, newArgs)(arg.tpe)
              case TraitImplApply(impl, method, args) if noExtractYet =>
                TraitImplApply(impl, method, recs(args))(arg.tpe)
              case ArrayLength(array) if noExtractYet =>
                ArrayLength(rec(array))
              case ArraySelect(array, index) if noExtractYet =>
                val newIndex = rec(index)
                ArraySelect(rec(array), newIndex)(arg.tpe)

              case If(cond, thenp, elsep)
                  if noExtractYet && isExpression(thenp) && isExpression(elsep) =>
                If(rec(cond), thenp, elsep)(arg.tpe)

              case _ =>
                val temp = newSyntheticVar()
                val computeTemp =
                  pushLhsInto(VarDef(temp, arg.tpe, mutable = false, EmptyTree), arg)
                computeTemp +=: extractedStatements
                VarRef(temp, mutable = false)(arg.tpe)
            }
          }
        }

        def recs(args: List[Tree]): List[Tree] = {
          // This is a right-to-left map
          args.foldRight[List[Tree]](Nil) { (arg, acc) =>
            rec(arg) :: acc
          }
        }

        val newArgs = recs(args)

        assert(extractedStatements.nonEmpty,
            "Reached computeTemps with no temp to compute")

        val newStatement = makeStat(newArgs)
        js.Block(extractedStatements.result() ::: List(newStatement))(newStatement.pos)
      }
    }

    /** Same as above, for a single argument */
    def unnest(arg: Tree)(
        makeStat: Tree => js.Tree): js.Tree = {
      unnest(List(arg)) {
        case List(newArg) => makeStat(newArg)
      }
    }

    /** Same as above, for two arguments */
    def unnest(lhs: Tree, rhs: Tree)(
        makeStat: (Tree, Tree) => js.Tree): js.Tree = {
      unnest(List(lhs, rhs)) {
        case List(newLhs, newRhs) => makeStat(newLhs, newRhs)
      }
    }

    /** Same as above, for one head argument and a list of arguments */
    def unnest(arg0: Tree, args: List[Tree])(
        makeStat: (Tree, List[Tree]) => js.Tree): js.Tree = {
      unnest(arg0 :: args) { newArgs =>
        makeStat(newArgs.head, newArgs.tail)
      }
    }

    private val isHelperThatPreservesPureness: Set[String] = Set(
        "isByte", "isShort", "isInt",
        "asUnit", "asBoolean", "asByte", "asShort", "asInt",
        "asFloat", "asDouble",
        "uZ", "uB", "uS", "uI", "uJ", "uF", "uD",
        "imul"
    )

    private val isHelperThatPreservesSideEffectFreedom: Set[String] = Set(
        "wrapJavaScriptException", "unwrapJavaScriptException",
        "makeNativeArrayWrapper", "newArrayObject"
    ) ++ isHelperThatPreservesPureness

    /** Common implementation for the functions below.
     *  A pure expression can be moved around or executed twice, because it
     *  will always produce the same result and never have side-effects.
     *  A side-effect free expression can be elided if its result is not used.
     */
    private def isExpressionInternal(tree: Tree,
        allowUnpure: Boolean, allowSideEffects: Boolean): Boolean = {

      require(!allowSideEffects || allowUnpure)

      def test(tree: Tree): Boolean = tree match {
        // Atomic expressions
        case _: Literal  => true
        case _: This     => true
        case _: JSGlobal => true

        // Vars and fields (side-effect free, pure if immutable)
        case VarRef(_, mutable) =>
          allowUnpure || !mutable
        case Select(qualifier, item, mutable) =>
          (allowUnpure || !mutable) && test(qualifier)

        // Expressions preserving pureness
        case Block(trees)            => trees forall test
        case If(cond, thenp, elsep)  => test(cond) && test(thenp) && test(elsep)
        case BinaryOp(_, lhs, rhs)   => test(lhs) && test(rhs)
        case UnaryOp(_, lhs)         => test(lhs)
        case JSBinaryOp(_, lhs, rhs) => test(lhs) && test(rhs)
        case JSUnaryOp(_, lhs)       => test(lhs)
        case ArrayLength(array)      => test(array)
        case IsInstanceOf(expr, _)   => test(expr)

        // Expressions preserving side-effect freedom
        case NewArray(tpe, lengths) =>
          allowUnpure && (lengths forall test)
        case ArrayValue(tpe, elems) =>
          allowUnpure && (elems forall test)
        case ArraySelect(array, index) =>
          allowUnpure && test(array) && test(index)
        case JSArrayConstr(items) =>
          allowUnpure && (items forall test)
        case JSObjectConstr(items) =>
          allowUnpure && (items forall (item => test(item._2)))
        case Closure(thisType, args, resultType, body, captures) =>
          allowUnpure && (captures forall test)

        // Call helper
        case CallHelper(helper, args) =>
          val shallowTest =
            if (allowSideEffects) true
            else if (allowUnpure) isHelperThatPreservesSideEffectFreedom(helper)
            else isHelperThatPreservesPureness(helper)
          shallowTest && (args forall test)

        // Scala expressions that can always have side-effects
        case New(cls, constr, args) =>
          allowSideEffects && (args forall test)
        case LoadModule(cls) => // unfortunately
          allowSideEffects
        case Apply(receiver, method, args) =>
          allowSideEffects && test(receiver) && (args forall test)
        case StaticApply(receiver, cls, method, args) =>
          allowSideEffects && test(receiver) && (args forall test)
        case TraitImplApply(impl, method, args) =>
          allowSideEffects && (args forall test)
        case AsInstanceOf(expr, _) =>
          allowSideEffects && test(expr)

        // JavaScript expressions that can always have side-effects
        case JSNew(fun, args) =>
          allowSideEffects && test(fun) && (args forall test)
        case JSDotSelect(qualifier, item) =>
          allowSideEffects && test(qualifier)
        case JSBracketSelect(qualifier, item) =>
          allowSideEffects && test(qualifier) && test(item)
        case JSFunctionApply(fun, args) =>
          allowSideEffects && test(fun) && (args forall test)
        case JSDotMethodApply(receiver, method, args) =>
          allowSideEffects && test(receiver) && (args forall test)
        case JSBracketMethodApply(receiver, method, args) =>
          allowSideEffects && test(receiver) && test(method) && (args forall test)

        // Non-expressions
        case _ => false
      }
      test(tree)
    }

    /** Test whether the given tree is a standard JS expression.
     */
    def isExpression(tree: Tree): Boolean =
      isExpressionInternal(tree, allowUnpure = true, allowSideEffects = true)

    /** Test whether the given tree is a side-effect-free standard JS expression.
     */
    def isSideEffectFreeExpression(tree: Tree): Boolean =
      isExpressionInternal(tree, allowUnpure = true, allowSideEffects = false)

    /** Test whether the given tree is a pure standard JS expression.
     */
    def isPureExpression(tree: Tree): Boolean =
      isExpressionInternal(tree, allowUnpure = false, allowSideEffects = false)

    def doVarDef(ident: Ident, tpe: Type, mutable: Boolean, rhs: Tree): js.Tree = {
      implicit val pos = rhs.pos
      tpe match {
        case RecordType(fields) =>
          val elems = (rhs: @unchecked) match {
            case RecordValue(_, elems) =>
              elems
            case VarRef(rhsIdent, rhsMutable) =>
              for (RecordType.Field(fName, fOrigName, fTpe, fMutable) <- fields)
                yield VarRef(makeRecordFieldIdent(rhsIdent, fName, fOrigName),
                    rhsMutable || fMutable)(fTpe)
          }
          js.Block(for {
            (RecordType.Field(fName, fOrigName, fTpe, fMutable),
                fRhs) <- fields zip elems
          } yield {
            doVarDef(makeRecordFieldIdent(ident, fName, fOrigName),
                fTpe, mutable || fMutable, fRhs)
          })

        case _ =>
          js.VarDef(ident, mutable, transformExpr(rhs))
      }
    }

    def doAssign(lhs: Tree, rhs: Tree): js.Tree = {
      implicit val pos = rhs.pos
      lhs.tpe match {
        case RecordType(fields) =>
          val VarRef(ident, mutable) = lhs
          val elems = (rhs: @unchecked) match {
            case VarRef(rhsIdent, rhsMutable) =>
              for (RecordType.Field(fName, fOrigName, fTpe, fMutable) <- fields)
                yield VarRef(makeRecordFieldIdent(rhsIdent, fName, fOrigName),
                    rhsMutable || fMutable)(fTpe)
          }
          js.Block(for {
            (RecordType.Field(fName, fOrigName, fTpe, fMutable),
                fRhs) <- fields zip elems
          } yield {
            doAssign(VarRef(makeRecordFieldIdent(ident, fName, fOrigName),
                mutable || fMutable)(fTpe), fRhs)
          })

        case _ =>
          js.Assign(transformExpr(lhs), transformExpr(rhs))
      }
    }

    /** Push an lhs into a (potentially complex) rhs
     *  lhs can be either a EmptyTree, a VarDef, a Assign or a
     *  Return
     */
    def pushLhsInto(lhs: Tree, rhs: Tree): js.Tree = {
      implicit val rhsPos = rhs.pos

      /** Push the current lhs further into a deeper rhs */
      @inline def redo(newRhs: Tree) = pushLhsInto(lhs, newRhs)

      if (rhs.tpe == NothingType && lhs != EmptyTree) {
        /* A touch of peephole dead code elimination.
         * Actually necessary to handle pushing an lhs into an infinite loop,
         * for example.
         */
        val transformedRhs = pushLhsInto(EmptyTree, rhs)
        lhs match {
          case VarDef(name, _, mutable, _) =>
            /* We still need to declare the var, in case it is used somewhere
             * else in the function, where we can't dce it.
             */
            js.Block(js.VarDef(name, true, js.EmptyTree), transformedRhs)
          case _ =>
            transformedRhs
        }
      } else (rhs match {
        // Handle the Block before testing whether it is an expression

        case Block(stats :+ expr) =>
          js.Block((stats map transformStat) :+ redo(expr))

        // Base case, rhs is already a regular JS expression

        case _ if isExpression(rhs) =>
          (lhs: @unchecked) match {
            case EmptyTree =>
              if (isSideEffectFreeExpression(rhs)) js.Skip()
              else transformExpr(rhs)
            case VarDef(name, tpe, mutable, _) =>
              doVarDef(name, tpe, mutable, rhs)
            case Assign(lhs, _) =>
              doAssign(lhs, rhs)
            case Return(_, None) =>
              js.Return(transformExpr(rhs))
            case Return(_, label @ Some(l)) =>
              labeledExprLHSes(l) match {
                case newLhs @ Return(_, _) =>
                  pushLhsInto(newLhs, rhs) // no need to break here
                case newLhs =>
                  js.Block(pushLhsInto(newLhs, rhs),
                      js.Break(label.map(transformIdent)))
              }
          }

        // Almost base case with RecordValue

        case RecordValue(recTpe, elems) =>
          (lhs: @unchecked) match {
            case EmptyTree =>
              js.Block(elems map transformStat)
            case VarDef(name, tpe, mutable, _) =>
              unnest(elems) { newElems =>
                doVarDef(name, tpe, mutable, RecordValue(recTpe, newElems))
              }
            case Assign(lhs, _) =>
              unnest(elems) { newElems =>
                val temp = newSyntheticVar()
                js.Block(
                    doVarDef(temp, recTpe, false, RecordValue(recTpe, newElems)),
                    doAssign(lhs, VarRef(temp, false)(recTpe)))
              }
            case Return(_, label @ Some(l)) =>
              val newLhs = labeledExprLHSes(l)
              js.Block(pushLhsInto(newLhs, rhs),
                  js.Break(label.map(transformIdent)))
          }

        // Control flow constructs

        case Labeled(label, tpe, body) =>
          val savedMap = labeledExprLHSes
          labeledExprLHSes = labeledExprLHSes + (label -> lhs)
          try {
            lhs match {
              case Return(_, _) => redo(body)
              case _            => js.Labeled(label, redo(body))
            }
          } finally {
            labeledExprLHSes = savedMap
          }

        case Return(expr, _) =>
          pushLhsInto(rhs, expr)

        case Continue(label) =>
          js.Continue(label.map(transformIdent))

        case If(cond, thenp, elsep) =>
          unnest(cond) { newCond =>
            js.If(transformExpr(newCond), redo(thenp), redo(elsep))
          }

        case Try(block, errVar, handler, finalizer) =>
          val newHandler =
            if (handler == EmptyTree) js.EmptyTree else redo(handler)
          val newFinalizer =
            if (finalizer == EmptyTree) js.EmptyTree else transformStat(finalizer)

          if (newHandler != js.EmptyTree && newFinalizer != js.EmptyTree) {
            /* The Google Closure Compiler wrongly eliminates finally blocks, if
             * the catch block throws an exception.
             * Issues: #563, google/closure-compiler#186
             *
             * Therefore, we desugar
             *
             *   try { ... } catch { ... } finally { ... }
             *
             * into
             *
             *   try { try { ... } catch { ... } } finally { ... }
             */
            js.Try(js.Try(redo(block), errVar, newHandler, js.EmptyTree),
                errVar, js.EmptyTree, newFinalizer)
          } else
            js.Try(redo(block), errVar, newHandler, newFinalizer)

        // TODO Treat throw as an LHS?
        case Throw(expr) =>
          unnest(expr) { newExpr =>
            js.Throw(transformExpr(newExpr))
          }

        /** Matches are desugared into switches
         *
         *  A match is different from a switch in two respects, both linked
         *  to match being designed to be used in expression position in
         *  Extended-JS.
         *
         *  * There is no fall-through from one case to the next one, hence,
         *    no break statement.
         *  * Match supports _alternatives_ explicitly (with a switch, one
         *    would use the fall-through behavior to implement alternatives).
         */
        case Match(selector, cases, default) =>
          unnest(selector) { newSelector =>
            val newCases = {
              for {
                (values, body) <- cases
                newValues = (values map transformExpr)
                // add the break statement
                newBody = js.Block(redo(body), js.Break())
                // desugar alternatives into several cases falling through
                caze <- (newValues.init map (v => (v, js.Skip()))) :+ (newValues.last, newBody)
              } yield {
                caze
              }
            }
            val newDefault =
              if (default == EmptyTree) js.EmptyTree
              else redo(default)
            js.Switch(transformExpr(newSelector), newCases, newDefault)
          }

        // Scala expressions (if we reach here their arguments are not expressions)

        case New(cls, ctor, args) =>
          unnest(args) { newArgs =>
            redo(New(cls, ctor, newArgs))
          }

        case Select(qualifier, item, mutable) =>
          unnest(qualifier) { newQualifier =>
            redo(Select(newQualifier, item, mutable)(rhs.tpe))
          }

        case Apply(receiver, method, args) =>
          unnest(receiver, args) { (newReceiver, newArgs) =>
            redo(Apply(newReceiver, method, newArgs)(rhs.tpe))
          }

        case StaticApply(receiver, cls, method, args) =>
          unnest(receiver, args) { (newReceiver, newArgs) =>
            redo(StaticApply(newReceiver, cls, method, newArgs)(rhs.tpe))
          }

        case TraitImplApply(impl, method, args) =>
          unnest(args) { newArgs =>
            redo(TraitImplApply(impl, method, newArgs)(rhs.tpe))
          }

        case UnaryOp(op, lhs) =>
          unnest(lhs) { newLhs =>
            redo(UnaryOp(op, newLhs))
          }

        case BinaryOp(op, lhs, rhs) =>
          unnest(lhs, rhs) { (newLhs, newRhs) =>
            redo(BinaryOp(op, newLhs, newRhs))
          }

        case NewArray(tpe, lengths) =>
          unnest(lengths) { newLengths =>
            redo(NewArray(tpe, newLengths))
          }

        case ArrayValue(tpe, elems) =>
          unnest(elems) { newElems =>
            redo(ArrayValue(tpe, newElems))
          }

        case ArrayLength(array) =>
          unnest(array) { newArray =>
            redo(ArrayLength(newArray))
          }

        case ArraySelect(array, index) =>
          unnest(array, index) { (newArray, newIndex) =>
            redo(ArraySelect(newArray, newIndex)(rhs.tpe))
          }

        case IsInstanceOf(expr, cls) =>
          unnest(expr) { newExpr =>
            redo(IsInstanceOf(newExpr, cls))
          }

        case AsInstanceOf(expr, cls) =>
          if (behaviors.asInstanceOfs == Unchecked) {
            redo(expr)
          } else {
            unnest(expr) { newExpr =>
              redo(AsInstanceOf(newExpr, cls))
            }
          }

        case CallHelper(helper, args) =>
          unnest(args) { newArgs =>
            redo(CallHelper(helper, newArgs)(rhs.tpe))
          }

        // JavaScript expressions (if we reach here their arguments are not expressions)

        case JSNew(ctor, args) =>
          unnest(ctor :: args) { newCtorAndArgs =>
            val newCtor :: newArgs = newCtorAndArgs
            redo(JSNew(newCtor, newArgs))
          }

        case JSFunctionApply(fun, args) =>
          unnest(fun :: args) { newFunAndArgs =>
            val newFun :: newArgs = newFunAndArgs
            redo(JSFunctionApply(newFun, newArgs))
          }

        case JSDotMethodApply(receiver, method, args) =>
          unnest(receiver :: args) { newReceiverAndArgs =>
            val newReceiver :: newArgs = newReceiverAndArgs
            redo(JSDotMethodApply(newReceiver, method, newArgs))
          }

        case JSBracketMethodApply(receiver, method, args) =>
          unnest(receiver :: method :: args) { newReceiverAndArgs =>
            val newReceiver :: newMethod :: newArgs = newReceiverAndArgs
            redo(JSBracketMethodApply(newReceiver, newMethod, newArgs))
          }

        case JSDotSelect(qualifier, item) =>
          unnest(qualifier) { newQualifier =>
            redo(JSDotSelect(newQualifier, item))
          }

        case JSBracketSelect(qualifier, item) =>
          unnest(qualifier, item) { (newQualifier, newItem) =>
            redo(JSBracketSelect(newQualifier, newItem))
          }

        case JSUnaryOp(op, lhs) =>
          unnest(lhs) { newLhs =>
            redo(JSUnaryOp(op, newLhs))
          }

        case JSBinaryOp("&&", lhs, rhs) =>
          if (lhs.tpe == BooleanType) {
            redo(If(lhs, rhs, BooleanLiteral(false))(AnyType))
          } else {
            unnest(lhs) { newLhs =>
              redo(If(newLhs, rhs, newLhs)(AnyType))
            }
          }

        case JSBinaryOp("||", lhs, rhs) =>
          if (lhs.tpe == BooleanType) {
            redo(If(lhs, BooleanLiteral(true), rhs)(AnyType))
          } else {
            unnest(lhs) { newLhs =>
              redo(If(newLhs, newLhs, rhs)(AnyType))
            }
          }

        case JSBinaryOp(op, lhs, rhs) =>
          unnest(lhs, rhs) { (newLhs, newRhs) =>
            redo(JSBinaryOp(op, newLhs, newRhs))
          }

        case JSArrayConstr(items) =>
          unnest(items) { newItems =>
            redo(JSArrayConstr(newItems))
          }

        case JSObjectConstr(fields) =>
          val names = fields map (_._1)
          val items = fields map (_._2)
          unnest(items) { newItems =>
            redo(JSObjectConstr(names.zip(newItems)))
          }

        // Closures

        case Closure(thisType, args, resultType, body, captures) =>
          unnest(captures) { newCaptures =>
            redo(Closure(thisType, args, resultType, body, newCaptures))
          }

        case _ =>
          if (lhs == EmptyTree) {
            /* Go "back" to transformStat() after having dived into
             * expression statements. Remember that (lhs == EmptyTree)
             * is a trick that we use to "add" all the code of pushLhsInto()
             * to transformStat().
             */
            rhs match {
              case _:Skip | _:VarDef | _:Assign | _:While | _:DoWhile |
                  _:Debugger | _:JSDelete | _:StoreModule | _:ClassDef =>
                transformStat(rhs)
              case _ =>
                sys.error("Illegal tree in JSDesugar.pushLhsInto():\n" +
                    "lhs = " + lhs + "\n" + "rhs = " + rhs +
                    " of class " + rhs.getClass)
            }
          } else {
            sys.error("Illegal tree in JSDesugar.pushLhsInto():\n" +
                "lhs = " + lhs + "\n" + "rhs = " + rhs +
                " of class " + rhs.getClass)
          }
      })
    }

    // Desugar Scala operations to JavaScript operations -----------------------

    /** Desugar an expression of the IR into ES5 JS */
    def transformExpr(tree: Tree): js.Tree = {
      import TreeDSL._

      implicit val pos = tree.pos

      def or0(tree: js.Tree): js.Tree =
        js.BinaryOp("|", tree, js.IntLiteral(0))

      tree match {
        // Control flow constructs

        case Block(stats :+ expr) =>
          js.Block((stats map transformStat) :+ transformExpr(expr))

        // Note that these work even if thenp/elsep is not a BooleanType
        case If(cond, BooleanLiteral(true), elsep) =>
          js.BinaryOp("||", transformExpr(cond), transformExpr(elsep))
        case If(cond, thenp, BooleanLiteral(false)) =>
          js.BinaryOp("&&", transformExpr(cond), transformExpr(thenp))

        case If(cond, thenp, elsep) =>
          js.If(transformExpr(cond), transformExpr(thenp), transformExpr(elsep))

        // Scala expressions

        case New(cls, ctor, args) =>
          js.Apply(js.New(encodeClassVar(cls.className), Nil) DOT ctor,
              args map transformExpr)

        case LoadModule(cls) =>
          genLoadModule(cls.className)

        case RecordFieldVarRef(VarRef(name, mutable)) =>
          js.VarRef(name, mutable)

        case Select(qualifier, item, _) =>
          transformExpr(qualifier) DOT item

        case Apply(receiver, method, args) =>
          js.Apply(transformExpr(receiver) DOT method, args map transformExpr)

        case StaticApply(receiver, cls, method, args) =>
          val fun = encodeClassVar(cls.className).prototype DOT method
          js.Apply(fun DOT "call", (receiver :: args) map transformExpr)

        case TraitImplApply(impl, method, args) =>
          js.Apply(envField("i") DOT method, args map transformExpr)

        case UnaryOp(op, lhs) =>
          import UnaryOp._
          val newLhs = transformExpr(lhs)
          (op: @switch) match {
            case `typeof`    => js.UnaryOp("typeof", newLhs)
            case Boolean_!   => js.UnaryOp("!", newLhs)
            case DoubleToInt => js.BinaryOp("|", newLhs, js.IntLiteral(0))

            case LongToInt    => genLongMethodApply(newLhs, LongImpl.toInt)
            case LongToDouble => genLongMethodApply(newLhs, LongImpl.toDouble)

            case IntToLong =>
              genNewLong(LongImpl.initFromInt, newLhs)
            case DoubleToLong =>
              genLongModuleApply(LongImpl.fromDouble, newLhs)
          }

        case BinaryOp(op, lhs, rhs) =>
          import BinaryOp._
          val lhs1 = lhs match {
            case UnaryOp(UnaryOp.DoubleToInt, inner)
                if op == Int_& || op == Int_<< =>
              /* This case is emitted typically by conversions from
               * Float/Double to Char/Byte/Short. We have to introduce an
               * (int) cast in the IR so that it typechecks, but in JavaScript
               * this is redundant because & and << already convert both their
               * operands to ints. So we get rid of the conversion here.
               */
              inner
            case _ =>
              lhs
          }

          val newLhs = transformExpr(lhs1)
          val newRhs = transformExpr(rhs)

          (op: @switch) match {
            case === | Num_== | Boolean_== => js.BinaryOp("===", newLhs, newRhs)
            case !== | Num_!= | Boolean_!= => js.BinaryOp("!==", newLhs, newRhs)

            case String_+ =>
              if (lhs.tpe == StringType || rhs.tpe == StringType)
                js.BinaryOp("+", newLhs, newRhs)
              else
                js.BinaryOp("+", js.BinaryOp("+", js.StringLiteral(""), newLhs), newRhs)

            case `in`         => js.BinaryOp("in", newLhs, newRhs)
            case `instanceof` => js.BinaryOp("instanceof", newLhs, newRhs)

            case Int_+ => or0(js.BinaryOp("+", newLhs, newRhs))
            case Int_- =>
              lhs match {
                case IntLiteral(0) => or0(js.UnaryOp("-", newRhs))
                case _             => or0(js.BinaryOp("-", newLhs, newRhs))
              }
            case Int_* => genCallHelper("imul", newLhs, newRhs)
            case Int_/ => or0(js.BinaryOp("/", newLhs, newRhs))
            case Int_% => js.BinaryOp("%", newLhs, newRhs)

            case Int_|   => js.BinaryOp("|", newLhs, newRhs)
            case Int_&   => js.BinaryOp("&", newLhs, newRhs)
            case Int_^   =>
              lhs match {
                case IntLiteral(-1) => js.UnaryOp("~", newRhs)
                case _              => js.BinaryOp("^", newLhs, newRhs)
              }
            case Int_<<  => js.BinaryOp("<<", newLhs, newRhs)
            case Int_>>> => or0(js.BinaryOp(">>>", newLhs, newRhs))
            case Int_>>  => js.BinaryOp(">>", newLhs, newRhs)

            case Double_+ => js.BinaryOp("+", newLhs, newRhs)
            case Double_- =>
              lhs match {
                case DoubleLiteral(0.0) => js.UnaryOp("-", newRhs)
                case _                  => js.BinaryOp("-", newLhs, newRhs)
              }
            case Double_* => js.BinaryOp("*", newLhs, newRhs)
            case Double_/ => js.BinaryOp("/", newLhs, newRhs)
            case Double_% => js.BinaryOp("%", newLhs, newRhs)

            case Num_<  => js.BinaryOp("<", newLhs, newRhs)
            case Num_<= => js.BinaryOp("<=", newLhs, newRhs)
            case Num_>  => js.BinaryOp(">", newLhs, newRhs)
            case Num_>= => js.BinaryOp(">=", newLhs, newRhs)

            case Long_+ => genLongMethodApply(newLhs, LongImpl.+, newRhs)
            case Long_- =>
              lhs match {
                case LongLiteral(0L) => genLongMethodApply(newRhs, LongImpl.UNARY_-)
                case _               => genLongMethodApply(newLhs, LongImpl.-, newRhs)
              }
            case Long_* => genLongMethodApply(newLhs, LongImpl.*, newRhs)
            case Long_/ => genLongMethodApply(newLhs, LongImpl./, newRhs)
            case Long_% => genLongMethodApply(newLhs, LongImpl.%, newRhs)

            case Long_|   => genLongMethodApply(newLhs, LongImpl.|,   newRhs)
            case Long_&   => genLongMethodApply(newLhs, LongImpl.&,   newRhs)
            case Long_^   =>
              lhs match {
                case LongLiteral(-1L) => genLongMethodApply(newRhs, LongImpl.UNARY_~)
                case _                => genLongMethodApply(newLhs, LongImpl.^, newRhs)
              }
            case Long_<<  => genLongMethodApply(newLhs, LongImpl.<<,  newRhs)
            case Long_>>> => genLongMethodApply(newLhs, LongImpl.>>>, newRhs)
            case Long_>>  => genLongMethodApply(newLhs, LongImpl.>>,  newRhs)

            case Long_== => genLongMethodApply(newLhs, LongImpl.===, newRhs)
            case Long_!= => genLongMethodApply(newLhs, LongImpl.!==, newRhs)
            case Long_<  => genLongMethodApply(newLhs, LongImpl.<,   newRhs)
            case Long_<= => genLongMethodApply(newLhs, LongImpl.<=,  newRhs)
            case Long_>  => genLongMethodApply(newLhs, LongImpl.>,   newRhs)
            case Long_>= => genLongMethodApply(newLhs, LongImpl.>=,  newRhs)

            case Boolean_| => !(!js.BinaryOp("|", newLhs, newRhs))
            case Boolean_& => !(!js.BinaryOp("&", newLhs, newRhs))
          }

        case NewArray(tpe, lengths) =>
          genCallHelper("newArrayObject",
              genClassDataOf(tpe), js.ArrayConstr(lengths map transformExpr))

        case ArrayValue(tpe, elems) =>
          genCallHelper("makeNativeArrayWrapper",
              genClassDataOf(tpe), js.ArrayConstr(elems map transformExpr))

        case ArrayLength(array) =>
          js.BracketSelect(js.DotSelect(transformExpr(array),
              Ident("u")), js.StringLiteral("length"))

        case ArraySelect(array, index) =>
          js.BracketSelect(js.DotSelect(transformExpr(array),
              Ident("u")), transformExpr(index))

        case IsInstanceOf(expr, cls) =>
          genIsInstanceOf(transformExpr(expr), cls)

        case AsInstanceOf(expr, cls) =>
          val newExpr = transformExpr(expr)
          if (behaviors.asInstanceOfs == Unchecked) newExpr
          else genAsInstanceOf(newExpr, cls)

        case CallHelper(helper, args) =>
          val newArgs = args map transformExpr
          @inline def default = genCallHelper(helper, newArgs: _*)

          if (behaviors.asInstanceOfs == Unchecked) {
            helper match {
              case "uZ" =>
                !(!newArgs.head)
              case "uB" | "uS" | "uI" =>
                js.BinaryOp("|", newArgs.head, js.IntLiteral(0))
              case "uF" | "uD" =>
                js.UnaryOp("+", newArgs.head)
              case _ =>
                default
            }
          } else {
            default
          }

        // JavaScript expressions

        case JSGlobal() =>
          envField("g")

        case JSNew(constr, args) =>
          js.New(transformExpr(constr), args map transformExpr)

        case JSDotSelect(qualifier, item) =>
          js.DotSelect(transformExpr(qualifier), item)

        case JSBracketSelect(qualifier, item) =>
          js.BracketSelect(transformExpr(qualifier), transformExpr(item))

        case JSFunctionApply(fun, args) =>
          /* Protect the fun so that if it is, e.g.,
           * path.f
           * we emit
           * (0, path.f)(args...)
           * instead of
           * path.f(args...)
           * If we emit the latter, then `this` will be bound to `path` in
           * `f`, which is sometimes extremely harmful (e.g., for builtin
           * methods of `window`).
           */
          val transformedFun = transformExpr(fun)
          val protectedFun = transformedFun match {
            case _:js.DotSelect | _:js.BracketSelect =>
              js.Block(js.IntLiteral(0), transformedFun)
            case _ =>
              transformedFun
          }
          js.Apply(protectedFun, args map transformExpr)

        case JSDotMethodApply(receiver, method, args) =>
          js.Apply(js.DotSelect(transformExpr(receiver), method),
              args map transformExpr)

        case JSBracketMethodApply(receiver, method, args) =>
          js.Apply(js.BracketSelect(transformExpr(receiver),
              transformExpr(method)), args map transformExpr)

        case JSUnaryOp(op, lhs) =>
          js.UnaryOp(op, transformExpr(lhs))

        case JSBinaryOp(op, lhs, rhs) =>
          js.BinaryOp(op, transformExpr(lhs), transformExpr(rhs))

        case JSArrayConstr(items) =>
          js.ArrayConstr(items map transformExpr)

        case JSObjectConstr(fields) =>
          js.ObjectConstr(fields map {
            case (name: Ident, value) =>
              (transformIdent(name), transformExpr(value))
            case (StringLiteral(name), value) =>
              (js.StringLiteral(name), transformExpr(value))
          })

        // Literals

        case Undefined()            => js.Undefined()
        case Null()                 => js.Null()
        case BooleanLiteral(value)  => js.BooleanLiteral(value)
        case IntLiteral(value)      => js.IntLiteral(value)
        case DoubleLiteral(value)   => js.DoubleLiteral(value)
        case StringLiteral(value)   => js.StringLiteral(value)

        case LongLiteral(0L) =>
          genLongModuleApply(LongImpl.Zero)
        case LongLiteral(value) =>
          val (l, m, h) = LongImpl.extractParts(value)
          genNewLong(LongImpl.initFromParts,
              js.IntLiteral(l), js.IntLiteral(m), js.IntLiteral(h))

        case ClassOf(cls) =>
          js.Apply(js.DotSelect(genClassDataOf(cls), Ident("getClassOf")), Nil)

        // Atomic expressions

        case VarRef(name, mutable) =>
          js.VarRef(name, mutable)

        case This() =>
          js.This()

        case Closure(thisType, args, resultType, body, captures) =>
          val transformedArgs = args.map(transformParamDef)

          val transformedBody = {
            val withReturn =
              if (resultType == NoType) body
              else Return(body, None)
            transformStat(withReturn) match {
              case js.Block(stats :+ js.Return(js.Undefined())) => js.Block(stats)
              case other                                        => other
            }
          }

          if (captures.isEmpty) {
            js.Function(transformedArgs, transformedBody)
          } else {
            val (formalCaptures, formalArgs) =
              transformedArgs.splitAt(captures.size)
            js.Apply(
                js.Function(formalCaptures, {
                  js.Return(js.Function(formalArgs, transformedBody))
                }),
                captures.map(transformExpr))
          }

        // Invalid trees

        case _ =>
          sys.error("Invalid tree in JSDesugar.transformExpr() "+
              s"of class ${tree.getClass}")
      }
    }

    def genClassDataOf(cls: ReferenceType)(implicit pos: Position): js.Tree = {
      cls match {
        case ClassType(className) =>
          encodeClassField("d", className)
        case ArrayType(base, dims) =>
          (1 to dims).foldLeft(encodeClassField("d", base)) { (prev, _) =>
            js.Apply(js.DotSelect(prev, js.Ident("getArrayOf")), Nil)
          }
      }
    }

    private def genNewLong(ctor: String, args: js.Tree*)(
        implicit pos: Position): js.Tree = {
      import TreeDSL._
      js.Apply(
          js.New(encodeClassVar(LongImpl.RuntimeLongClass), Nil) DOT ctor,
          args.toList)
    }

    private def genLongMethodApply(receiver: js.Tree, methodName: String,
        args: js.Tree*)(implicit pos: Position): js.Tree = {
      import TreeDSL._
      js.Apply(receiver DOT methodName, args.toList)
    }

    private def genLongModuleApply(methodName: String, args: js.Tree*)(
        implicit pos: Position): js.Tree = {
      import TreeDSL._
      js.Apply(
          genLoadModule(LongImpl.RuntimeLongModuleClass) DOT methodName,
          args.toList)
    }

    private def genLoadModule(moduleClass: String)(
        implicit pos: Position): js.Tree = {
      import TreeDSL._
      assert(moduleClass.endsWith("$"),
          s"Trying to load module for non-module class $moduleClass")
      val moduleName = moduleClass.dropRight(1)
      js.Apply(envField("m") DOT moduleName, Nil)
    }

  }

  // Helpers

  private[javascript] def genIsInstanceOf(expr: js.Tree, cls: ReferenceType)(
      implicit pos: Position): js.Tree =
    genIsAsInstanceOf(expr, cls, test = true)

  private def genAsInstanceOf(expr: js.Tree, cls: ReferenceType)(
      implicit pos: Position): js.Tree =
    genIsAsInstanceOf(expr, cls, test = false)

  private def genIsAsInstanceOf(expr: js.Tree, cls: ReferenceType, test: Boolean)(
      implicit pos: Position): js.Tree = {
    import Definitions._
    import TreeDSL._

    cls match {
      case ClassType(className0) =>
        val className =
          if (className0 == BoxedLongClass) LongImpl.RuntimeLongClass
          else className0

        if (HijackedBoxedClasses.contains(className)) {
          if (test) {
            className match {
              case BoxedUnitClass    => expr === js.Undefined()
              case BoxedBooleanClass => typeof(expr) === "boolean"
              case BoxedByteClass    => genCallHelper("isByte", expr)
              case BoxedShortClass   => genCallHelper("isShort", expr)
              case BoxedIntegerClass => genCallHelper("isInt", expr)
              case BoxedFloatClass   => typeof(expr) === "number"
              case BoxedDoubleClass  => typeof(expr) === "number"
            }
          } else {
            className match {
              case BoxedUnitClass    => genCallHelper("asUnit", expr)
              case BoxedBooleanClass => genCallHelper("asBoolean", expr)
              case BoxedByteClass    => genCallHelper("asByte", expr)
              case BoxedShortClass   => genCallHelper("asShort", expr)
              case BoxedIntegerClass => genCallHelper("asInt", expr)
              case BoxedFloatClass   => genCallHelper("asFloat", expr)
              case BoxedDoubleClass  => genCallHelper("asDouble", expr)
            }
          }
        } else {
          js.Apply(
              envField(if (test) "is" else "as") DOT js.Ident(className),
              List(expr))
        }

      case ArrayType(base, depth) =>
        js.Apply(
            envField(if (test) "isArrayOf" else "asArrayOf") DOT js.Ident(base),
            List(expr, js.IntLiteral(depth)))
    }
  }

  private[javascript] def genCallHelper(helperName: String, args: js.Tree*)(
      implicit pos: Position): js.Tree =
    js.Apply(envField(helperName), args.toList)

  private[javascript] def encodeClassVar(className: String)(
      implicit pos: Position): js.Tree =
    encodeClassField("c", className)

  private[javascript] def encodeClassField(field: String, className: String)(
      implicit pos: Position): js.Tree =
    js.DotSelect(envField(field), js.Ident(className))

  private[javascript] def envField(field: String)(implicit pos: Position): js.Tree =
    js.DotSelect(js.VarRef(js.Ident(ScalaJSEnvironmentName), false),
        js.Ident(field))

  private[javascript] implicit class MyTreeOps(val self: js.Tree) {
    def prototype(implicit pos: Position): js.Tree =
      js.DotSelect(self, js.Ident("prototype"))
  }
}
