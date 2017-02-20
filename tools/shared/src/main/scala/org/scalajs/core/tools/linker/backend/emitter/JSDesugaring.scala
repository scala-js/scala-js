/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.backend.emitter

import scala.language.implicitConversions

import scala.annotation.{switch, tailrec}

import scala.collection.mutable

import org.scalajs.core.ir
import ir._
import ir.Position._
import ir.Transformers._
import ir.Trees._
import ir.Types._

import org.scalajs.core.tools.sem._
import CheckedBehavior._

import org.scalajs.core.tools.linker.LinkedClass
import org.scalajs.core.tools.linker.backend.OutputMode
import org.scalajs.core.tools.javascript.{Trees => js}

import java.io.StringWriter

/** Desugaring of the IR to JavaScript.
 *
 *  The general shape and compliance to standards is chosen with an
 *  [[OutputMode]].
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
 *     * VarDef, i.e., `val x =` or `var x =`
 *     * Return, i.e., `return`
 *     * Discard, i.e. just evaluate and discard
 *     In fact, think that, in this context, LHS means: what to do with the
 *     result of evaluating the RHS.
 *
 *  When VarDefs are emitted as Lets (i.e., in ES 6 mode), they cannot be
 *  pushed in all complex constructs, since that would alter their scope.
 *  In those cases, they are first declared without an initial value, then
 *  an Assign is pushed instead.
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
 *  --------------------------------------------------------------------------
 *
 *  About `Labeled` blocks, this phase maintains two sets of label names.
 *
 *  First, `tailPosLabels`, a set of labels for which we are in "tail
 *  position", i.e., breaking to that label is equivalent to no-op. `Break`s to
 *  labels in that set can be replaced by `Skip()`, allowing the removal of
 *  the label altogether. For example, in
 *
 *  {{{
 *  var y;
 *  lbl: {
 *    var x = 5;
 *    if (b) {
 *      y = x + 3;
 *      break lbl;
 *    } else {
 *      y = x + 5;
 *    }
 *  }
 *  }}}
 *
 *  the `break lbl` is in tail position for the label `lbl`, and can therefore
 *  be removed. After removal of the labeled block itself, the snippet rewrites
 *  as:
 *
 *  {{{
 *  var y;
 *  var x = 5;
 *  if (b) {
 *    y = x + 3;
 *  } else {
 *    y = x + 5;
 *  }
 *  }}}
 *
 *  `tailPosLabels` is a property of the *evaluation order*, and is therefore
 *  passed explicitly in `transform` methods (as opposed to being stored in the
 *  environment).
 *
 *  Second, `defaultBreakTargets`, a set of "default" labels, for which `break`
 *  is equivalent to `break lbl`. This is the set of tail-position labels of the
 *  closest enclosing `while/do..while/switch` "loop". `Break`s to labels in
 *  that set can be replaced by `break` without label, also allowing the removal
 *  of the labeled block. For example, in
 *
 *  {{{
 *  var y;
 *  lbl: {
 *    var i = 0;
 *    while (true) {
 *      if (x == 5) {
 *        y = 3;
 *        break lbl;
 *      }
 *      i = i - 1;
 *    }
 *  }
 *  }}}
 *
 *  the `break lbl` is in "default" position of the label `lbl`, since the
 *  closest enclosing `while/do..while/switch` is in tail position of `lbl`.
 *  The snippet can therefore be rewritten as:
 *
 *  {{{
 *  var y;
 *  var i = 0;
 *  while (true) {
 *    if (x == 5) {
 *      y = 3;
 *      break;
 *    }
 *    i = i - 1;
 *  }
 *  }}}
 *
 *  This is particularly interesting for `Range.foreach`, because, when inlined,
 *  its `return` statement becomes a `break` to an `inlinereturn` label, which
 *  is a default label at the point of the `return`. We can therefore avoid the
 *  useless labeled block for this common case.
 *
 *  `defaultBreakTargets` is property of the *scope*, and is therefore stored
 *  in the environment.
 *
 *  @author Sébastien Doeraene
 */
private[emitter] class JSDesugaring(semantics: Semantics,
    outputMode: OutputMode, internalOptions: InternalOptions) {
  import JSDesugaring._

  private final val ScalaJSEnvironmentName = "ScalaJS"

  /** Desugars parameters and body to a JS function.
   */
  private[emitter] def desugarToFunction(
      enclosingClassName: String,
      params: List[ParamDef], body: Tree, isStat: Boolean)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): js.Function = {
    desugarToFunction(enclosingClassName,
        None, params, body, isStat)
  }

  /** Desugars parameters and body to a JS function.
   */
  private[emitter] def desugarToFunction(
      enclosingClassName: String,
      thisIdent: Option[js.Ident], params: List[ParamDef],
      body: Tree, isStat: Boolean)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): js.Function = {
    val env = Env.empty
      .withThisIdent(thisIdent)
      .withEnclosingClassName(Some(enclosingClassName))

    new JSDesugar().desugarToFunction(params, body, isStat, env)
  }

  /** Desugars parameters and body to a JS function.
   */
  private[emitter] def desugarToFunction(
      params: List[ParamDef],
      body: Tree, isStat: Boolean)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): js.Function = {
    new JSDesugar().desugarToFunction(params, body, isStat, Env.empty)
  }

  /** Desugars a statement or an expression. */
  private[emitter] def desugarTree(
      enclosingClassName: Option[String],
      tree: Tree, isStat: Boolean)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    val env = Env.empty.withEnclosingClassName(enclosingClassName)
    val desugar = new JSDesugar()
    if (isStat)
      desugar.transformStat(tree, Set.empty)(env)
    else
      desugar.transformExpr(tree)(env)
  }

  private[emitter] implicit def transformIdent(ident: Ident): js.Ident =
    js.Ident(ident.name, ident.originalName)(ident.pos)

  private[emitter] def transformParamDef(paramDef: ParamDef): js.ParamDef =
    js.ParamDef(paramDef.name, paramDef.rest)(paramDef.pos)

  private class JSDesugar()(implicit globalKnowledge: GlobalKnowledge) {

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

    val usedLabels = mutable.Set.empty[String]

    // Now the work

    /** Desugars parameters and body to a JS function.
     */
    def desugarToFunction(
        params: List[ParamDef], body: Tree, isStat: Boolean, env0: Env)(
        implicit pos: Position): js.Function = {

      val env = env0.withParams(params)

      val withReturn =
        if (isStat) body
        else Return(body)

      val translateRestParam = outputMode match {
        case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
          params.nonEmpty && params.last.rest
        case _ =>
          false
      }

      val extractRestParam =
        if (translateRestParam) makeExtractRestParam(params)
        else js.Skip()

      val newParams =
        (if (translateRestParam) params.init else params).map(transformParamDef)

      val newBody = transformStat(withReturn, Set.empty)(env) match {
        case js.Block(stats :+ js.Return(js.Undefined())) => js.Block(stats)
        case other                                        => other
      }

      js.Function(newParams, js.Block(extractRestParam, newBody))
    }

    private def makeExtractRestParam(params: List[ParamDef])(
        implicit pos: Position): js.Tree = {
      val offset = params.size - 1
      val restParamDef = params.last

      val lenIdent = newSyntheticVar()
      val len = js.VarRef(lenIdent)

      val counterIdent = newSyntheticVar()
      val counter = js.VarRef(counterIdent)

      val restParamIdent = restParamDef.name
      val restParam = js.VarRef(restParamIdent)

      val arguments = js.VarRef(js.Ident("arguments"))

      def or0(tree: js.Tree): js.Tree =
        js.BinaryOp(JSBinaryOp.|, tree, js.IntLiteral(0)(tree.pos))(tree.pos)

      js.Block(
        // const len = arguments.length | 0
        genLet(lenIdent, mutable = false,
            or0(genIdentBracketSelect(arguments, "length"))),
        // let i = <offset>
        genLet(counterIdent, mutable = true, js.IntLiteral(offset)),
        // const restParam = []
        genLet(restParamIdent, mutable = false, js.ArrayConstr(Nil)),
        // while (i < len)
        js.While(js.BinaryOp(JSBinaryOp.<, counter, len), js.Block(
          // restParam.push(arguments[i]);
          js.Apply(
              genIdentBracketSelect(restParam, "push"), List(
              js.BracketSelect(arguments, counter))),
          // i = (i + 1) | 0
          js.Assign(counter, or0(js.BinaryOp(JSBinaryOp.+,
              counter, js.IntLiteral(1))))
        ))
      )
    }

    /** Desugar a statement of the IR into ES5 JS */
    def transformStat(tree: Tree, tailPosLabels: Set[String])(
        implicit env: Env): js.Tree = {
      import TreeDSL._

      implicit val pos = tree.pos

      tree match {
        // VarDefs at the end of block. Normal VarDefs are handled in
        // transformBlockStats

        case VarDef(_, _, _, rhs) =>
          pushLhsInto(Lhs.Discard, rhs, tailPosLabels)

        // Statement-only language constructs

        case Skip() =>
          js.Skip()

        case Assign(RecordFieldVarRef(lhs), rhs) =>
          pushLhsInto(Lhs.Assign(lhs), rhs, tailPosLabels)

        case Assign(select @ Select(qualifier, item), rhs) =>
          unnest(qualifier, rhs) { (newQualifier, newRhs, env0) =>
            implicit val env = env0
            js.Assign(
                js.DotSelect(transformExpr(newQualifier), item)(select.pos),
                transformExpr(newRhs))
          }

        case Assign(select @ ArraySelect(array, index), rhs) =>
          unnest(List(array, index, rhs)) {
            case (List(newArray, newIndex, newRhs), env0) =>
              implicit val env = env0
              val genArray = transformExpr(newArray)
              val genIndex = transformExpr(newIndex)
              val genRhs = transformExpr(newRhs)
              semantics.arrayIndexOutOfBounds match {
                case CheckedBehavior.Compliant | CheckedBehavior.Fatal =>
                  js.Apply(js.DotSelect(genArray, js.Ident("set")),
                      List(genIndex, genRhs))
                case CheckedBehavior.Unchecked =>
                  js.Assign(
                      js.BracketSelect(
                          js.DotSelect(genArray, js.Ident("u"))(select.pos),
                          genIndex)(select.pos),
                      genRhs)
              }
          }

        case Assign(select @ JSDotSelect(qualifier, item), rhs) =>
          unnest(qualifier, rhs) { (newQualifier, newRhs, env0) =>
            implicit val env = env0
            js.Assign(
                js.DotSelect(transformExpr(newQualifier), item)(select.pos),
                transformExpr(newRhs))
          }

        case Assign(select @ JSBracketSelect(qualifier, item), rhs) =>
          unnest(List(qualifier, item, rhs)) {
            case (List(newQualifier, newItem, newRhs), env0) =>
              implicit val env = env0
              js.Assign(
                  genBracketSelect(transformExpr(newQualifier),
                      transformExpr(newItem))(select.pos),
                  transformExpr(newRhs))
          }

        case Assign(select @ JSSuperBracketSelect(cls, qualifier, item), rhs) =>
          unnest(List(qualifier, item, rhs)) {
            case (List(newQualifier, newItem, newRhs), env0) =>
              implicit val env = env0
              val ctor = genRawJSClassConstructor(cls.className)
              genCallHelper("superSet", ctor DOT "prototype",
                  transformExpr(newQualifier), transformExpr(item),
                  transformExpr(rhs))
          }

        case Assign(lhs @ (_:VarRef | _:SelectStatic), rhs) =>
          pushLhsInto(Lhs.Assign(lhs), rhs, tailPosLabels)

        case Assign(_, _) =>
          sys.error(s"Illegal Assign in transformStat: $tree")

        case StoreModule(cls, value) =>
          unnest(value) { (newValue, env0) =>
            implicit val env = env0
            js.Assign(
                envField("n", cls.className),
                transformExpr(newValue))
          }

        case While(cond, body, label) =>
          /* We cannot simply unnest(cond) here, because that would eject the
           * evaluation of the condition out of the loop.
           */
          val newLabel = label.map(transformIdent)
          val bodyBreakTargets = tailPosLabels ++ label.map(_.name)
          if (isExpression(cond)) {
            js.While(transformExpr(cond),
                transformStat(body, Set.empty)(
                    env.withDefaultBreakTargets(bodyBreakTargets)),
                newLabel)
          } else {
            js.While(js.BooleanLiteral(true), {
              unnest(cond) { (newCond, env0) =>
                implicit val env = env0
                js.If(transformExpr(newCond),
                    transformStat(body, Set.empty)(
                        env.withDefaultBreakTargets(bodyBreakTargets)),
                    js.Break())
              }
            }, newLabel)
          }

        case DoWhile(body, cond, label) =>
          /* We cannot simply unnest(cond) here, because that would eject the
           * evaluation of the condition out of the loop.
           */
          val newLabel = label.map(transformIdent)
          val bodyBreakTargets = tailPosLabels ++ label.map(_.name)
          if (isExpression(cond)) {
            js.DoWhile(
                transformStat(body, Set.empty)(
                    env.withDefaultBreakTargets(bodyBreakTargets)),
                transformExpr(cond), newLabel)
          } else {
            /* This breaks 'continue' statements for this loop, but we don't
             * care because we never emit continue statements for do..while
             * loops.
             */
            js.While(js.BooleanLiteral(true), {
              js.Block(
                  transformStat(body, Set.empty)(
                      env.withDefaultBreakTargets(bodyBreakTargets)),
                  unnest(cond) { (newCond, env0) =>
                    implicit val env = env0
                    js.If(transformExpr(newCond), js.Skip(), js.Break())
                  })
            }, newLabel)
          }

        case Debugger() =>
          js.Debugger()

        case JSSuperConstructorCall(args) =>
          unnestOrSpread(args) { (newArgs, env0) =>
            implicit val env = env0

            val enclosingClassName = env.enclosingClassName.getOrElse {
              sys.error("Need enclosing class for super constructor call.")
            }

            val superCtorCall = {
              outputMode match {
                case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
                  val superCtor = genRawJSClassConstructor(
                      globalKnowledge.getSuperClassOfJSClass(enclosingClassName))

                  if (containsAnySpread(newArgs)) {
                    val argArray = spreadToArgArray(newArgs)
                    js.Apply(
                        genIdentBracketSelect(superCtor, "apply"),
                        List(js.This(), transformExpr(argArray)))
                  } else {
                    js.Apply(
                        genIdentBracketSelect(superCtor, "call"),
                        js.This() :: newArgs.map(transformExpr))
                  }

                case OutputMode.ECMAScript6 =>
                  js.Apply(js.Super(), newArgs.map(transformExpr))
              }
            }

            val enclosingClassFieldDefs =
              globalKnowledge.getJSClassFieldDefs(enclosingClassName)

            val fieldDefs = for {
              field @ FieldDef(false, name, ftpe, mutable) <-
                enclosingClassFieldDefs
            } yield {
              implicit val pos = field.pos
              /* Here, a naive translation would emit something like this:
               *
               *   this["field"] = 0;
               *
               * However, this won't work if we override a getter from the
               * superclass with a val in this class, because the assignment
               * would try to set the property which has a getter but no
               * setter.
               * Instead, we must force the creation of a field on the object,
               * irrespective of the presence of a getter/setter in the
               * prototype chain. This is why we use `defineProperty`:
               *
               *   Object.defineProperty(this, "field", {
               *     "configurable": true,
               *     "enumerable": true,
               *     "writable": true,
               *     "value": 0
               *   });
               *
               * which has all the same semantics as the assignment, except
               * it disregards the prototype chain.
               *
               * If the field is an identifier, we cannot directly translate
               * it to a string for use in `defineProperty`, because Closure
               * would fail to rename it. In that case, we use
               * `defineProperties` instead, as follows:
               *
               *   Object.defineProperties(this, {
               *     field: {
               *       "configurable": true,
               *       "enumerable": true,
               *       "writable": true,
               *       "value": 0
               *     }
               *   });
               */

              def makeObjectMethodApply(methodName: String,
                  args: List[js.Tree]): js.Tree = {
                js.Apply(
                  genIdentBracketSelect(genIdentBracketSelect(
                      envField("g"),
                      "Object"),
                      methodName),
                  args)
              }

              val descriptor = js.ObjectConstr(List(
                  js.StringLiteral("configurable") -> js.BooleanLiteral(true),
                  js.StringLiteral("enumerable") -> js.BooleanLiteral(true),
                  js.StringLiteral("writable") -> js.BooleanLiteral(true),
                  js.StringLiteral("value") -> genZeroOf(ftpe)
              ))

              unnestPropertyName(name) { (newName, env0) =>
                implicit val env = env0
                newName match {
                  case newName: Ident =>
                    val descriptors = js.ObjectConstr(List(
                        transformIdent(newName) -> descriptor))
                    makeObjectMethodApply("defineProperties",
                        List(js.This(), descriptors))

                  case newName: StringLiteral =>
                    makeObjectMethodApply("defineProperty",
                        List(js.This(), transformExpr(newName), descriptor))

                  case ComputedName(nameTree, _) =>
                    makeObjectMethodApply("defineProperty",
                        List(js.This(), transformExpr(nameTree), descriptor))
                }
              }
            }

            js.Block(superCtorCall :: fieldDefs)
          }

        case JSDelete(JSDotSelect(obj, prop)) =>
          unnest(obj) { (newObj, env0) =>
            implicit val env = env0
            js.Delete(js.DotSelect(transformExpr(newObj), prop))
          }

        case JSDelete(JSBracketSelect(obj, prop)) =>
          unnest(obj, prop) { (newObj, newProp, env0) =>
            implicit val env = env0
            js.Delete(genBracketSelect(
                transformExpr(newObj), transformExpr(newProp)))
          }

        // Treat 'return' as an LHS

        case Return(expr, label) =>
          pushLhsInto(Lhs.Return(label), expr, tailPosLabels)

        /* Anything else is an expression => pushLhsInto(Lhs.Discard, _)
         * In order not to duplicate all the code of pushLhsInto() here, we
         * use a trick: Lhs.Discard is a dummy LHS that says "do nothing
         * with the result of the rhs".
         * This is exactly what an expression statement is doing: it evaluates
         * the expression, but does nothing with its result.
         */

        case _ =>
          pushLhsInto(Lhs.Discard, tree, tailPosLabels)
      }
    }

    private object RecordFieldVarRef {
      def unapply(tree: Tree): Option[VarRef] = {
        tree match {
          case Select(RecordVarRef(VarRef(recIdent)), fieldIdent) =>
            implicit val pos = tree.pos
            Some(VarRef(makeRecordFieldIdent(recIdent, fieldIdent))(tree.tpe))
          case _ =>
            None
        }
      }
    }

    def transformBlockStats(trees: List[Tree])(
        implicit env: Env): (List[js.Tree], Env) = {

      @tailrec
      def transformLoop(trees: List[Tree], env: Env,
          acc: List[js.Tree]): (List[js.Tree], Env) = trees match {
        case VarDef(ident, tpe, mutable, rhs) :: ts =>
          val newEnv = env.withDef(ident, tpe, mutable)
          val lhs = Lhs.VarDef(ident, tpe, mutable)
          val newTree = pushLhsInto(lhs, rhs, Set.empty)(env)
          transformLoop(ts, newEnv, newTree :: acc)

        case tree :: ts =>
          transformLoop(ts, env, transformStat(tree, Set.empty)(env) :: acc)

        case Nil =>
          (acc.reverse, env)
      }

      transformLoop(trees, env, Nil)
    }

    private object RecordVarRef {
      def unapply(tree: Tree): Option[VarRef] = {
        if (!tree.tpe.isInstanceOf[RecordType]) None
        else {
          tree match {
            case tree: VarRef => Some(tree)
            case Select(RecordVarRef(VarRef(recIdent)), fieldIdent) =>
              implicit val pos = tree.pos
              Some(VarRef(makeRecordFieldIdent(recIdent, fieldIdent))(tree.tpe))
          }
        }
      }
    }

    /** Same as `unnest`, but allows (and preserves) [[JSSpread]]s at the
     *  top-level.
     */
    def unnestOrSpread(args: List[Tree])(makeStat: (List[Tree], Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      val (argsNoSpread, argsWereSpread) = args.map {
        case JSSpread(items) => (items, true)
        case arg             => (arg, false)
      }.unzip

      unnest(argsNoSpread) { (newArgsNoSpread, env) =>
        val newArgs = newArgsNoSpread.zip(argsWereSpread).map {
          case (newItems, true) => JSSpread(newItems)(newItems.pos)
          case (newArg, false)  => newArg
        }
        makeStat(newArgs, env)
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
    def unnest(args: List[Tree])(makeStat: (List[Tree], Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      if (args forall isExpression) makeStat(args, env)
      else {
        val extractedStatements = new scala.collection.mutable.ListBuffer[js.Tree]
        var innerEnv = env

        /* Attention! Everything must be processed recursively
         * *right-to-left*! Indeed, the point is that noExtractYet will tell
         * whether anything supposed to be evaluated *after* the currently
         * being processed expression has been (at least partly) extracted
         * in temporary variables (or simply statements, in the Block case).
         * If nothing has, we can keep more in place without having to extract
         * that expression in a temporary variable.
         *
         * Also note that environments are handled the wrong way around. This is
         * ok, since the same local name may not appear multiple times inside a
         * single method.
         */

        def rec(arg: Tree)(implicit env: Env): Tree = {
          def noExtractYet = extractedStatements.isEmpty

          if (if (noExtractYet) isExpression(arg) else isPureExpression(arg)) {
            arg
          } else {
            implicit val pos = arg.pos
            arg match {
              case Block(stats :+ expr) =>
                val (jsStats, newEnv) = transformBlockStats(stats)
                innerEnv = newEnv
                val result = rec(expr)(newEnv) // right-to-left, remember?
                // Put the stats in a Block because ++=: is not smart
                js.Block(jsStats) +=: extractedStatements
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

              case AsInstanceOf(expr, tpe)
                  if noExtractYet || semantics.asInstanceOfs == Unchecked =>
                AsInstanceOf(rec(expr), tpe)
              case Unbox(expr, tpe)
                  if noExtractYet || semantics.asInstanceOfs == Unchecked =>
                Unbox(rec(expr), tpe)

              case NewArray(tpe, lengths) =>
                NewArray(tpe, recs(lengths))
              case ArrayValue(tpe, elems) =>
                ArrayValue(tpe, recs(elems))
              case JSArrayConstr(items) if !containsAnySpread(items) =>
                JSArrayConstr(recs(items))

              case arg @ JSObjectConstr(items)
                  if !doesObjectConstrRequireDesugaring(arg) =>
                // We need to properly interleave keys and values here
                val newItems = items.foldRight[List[(PropertyName, Tree)]](Nil) {
                  case ((key, value), acc) =>
                    val newValue = rec(value) // value first!
                    val newKey = key match {
                      case _:Ident | _:StringLiteral =>
                        key
                      case ComputedName(keyExpr, logicalName) =>
                        ComputedName(rec(keyExpr), logicalName)
                    }
                    (newKey, newValue) :: acc
                }
                JSObjectConstr(newItems)

              case Closure(captureParams, params, body, captureValues) =>
                Closure(captureParams, params, body, recs(captureValues))

              case New(cls, constr, args) if noExtractYet =>
                New(cls, constr, recs(args))
              case Select(qualifier, item) if noExtractYet =>
                Select(rec(qualifier), item)(arg.tpe)
              case Apply(receiver, method, args) if noExtractYet =>
                val newArgs = recs(args)
                Apply(rec(receiver), method, newArgs)(arg.tpe)
              case ApplyStatically(receiver, cls, method, args) if noExtractYet =>
                val newArgs = recs(args)
                ApplyStatically(rec(receiver), cls, method, newArgs)(arg.tpe)
              case ApplyStatic(cls, method, args) if noExtractYet =>
                ApplyStatic(cls, method, recs(args))(arg.tpe)
              case ArrayLength(array) if noExtractYet =>
                ArrayLength(rec(array))
              case ArraySelect(array, index) if noExtractYet =>
                val newIndex = rec(index)
                ArraySelect(rec(array), newIndex)(arg.tpe)
              case CallHelper(helper, args) if noExtractYet =>
                CallHelper(helper, recs(args))(arg.tpe)

              case If(cond, thenp, elsep)
                  if noExtractYet && isExpression(thenp) && isExpression(elsep) =>
                If(rec(cond), thenp, elsep)(arg.tpe)

              case _ =>
                val temp = newSyntheticVar()
                val newEnv = env.withDef(temp, arg.tpe, false)
                innerEnv = newEnv
                val computeTemp = pushLhsInto(
                    Lhs.VarDef(temp, arg.tpe, mutable = false), arg,
                    Set.empty)
                computeTemp +=: extractedStatements
                VarRef(temp)(arg.tpe)
            }
          }
        }

        def recs(args: List[Tree])(implicit env: Env): List[Tree] = {
          // This is a right-to-left map
          args.foldRight[List[Tree]](Nil) { (arg, acc) =>
            rec(arg) :: acc
          }
        }

        val newArgs = recs(args)

        assert(extractedStatements.nonEmpty,
            "Reached computeTemps with no temp to compute")

        val newStatement = makeStat(newArgs, innerEnv)
        js.Block(extractedStatements.result() ::: List(newStatement))(newStatement.pos)
      }
    }

    /** Same as above, for a single argument */
    def unnest(arg: Tree)(makeStat: (Tree, Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      unnest(List(arg)) {
        case (List(newArg), env) => makeStat(newArg, env)
      }
    }

    /** Same as above, for two arguments */
    def unnest(lhs: Tree, rhs: Tree)(
        makeStat: (Tree, Tree, Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      unnest(List(lhs, rhs)) {
        case (List(newLhs, newRhs), env) => makeStat(newLhs, newRhs, env)
      }
    }

    /** Same as above, for one head argument and a list of arguments */
    def unnest(arg0: Tree, args: List[Tree])(
        makeStat: (Tree, List[Tree], Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      unnest(arg0 :: args) { (newArgs, env) =>
        makeStat(newArgs.head, newArgs.tail, env)
      }
    }

    /** Unnest for the fields of a `JSObjectConstr`. */
    def unnestJSObjectConstrFields(fields: List[(PropertyName, Tree)])(
        makeStat: (List[(PropertyName, Tree)], Env) => js.Tree)(
        implicit env: Env): js.Tree = {

      // Collect all the trees that need unnesting, in evaluation order
      val trees = fields.flatMap {
        case (ComputedName(tree, _), value) => List(tree, value)
        case (_, value)                     => List(value)
      }

      unnest(trees) { (newTrees, env) =>
        val newTreesIterator = newTrees.iterator

        val newFields = fields.map {
          case (propName, value) =>
            val newPropName = propName match {
              case ComputedName(_, logicalName) =>
                val newTree = newTreesIterator.next()
                ComputedName(newTree, logicalName)
              case _:StringLiteral | _:Ident =>
                propName
            }
            val newValue = newTreesIterator.next()
            (newPropName, newValue)
        }

        assert(!newTreesIterator.hasNext)
        makeStat(newFields, env)
      }
    }

    /** Unnest for a `PropertyName`. */
    def unnestPropertyName(arg: PropertyName)(
        makeStat: (PropertyName, Env) => js.Tree)(
        implicit env: Env): js.Tree = {

      arg match {
        case _:StringLiteral | _:Ident =>
          makeStat(arg, env)
        case ComputedName(tree, logicalName) =>
          unnest(tree) { (newTree, env) =>
            makeStat(ComputedName(newTree, logicalName), env)
          }
      }
    }

    /** Common implementation for the functions below.
     *  A pure expression can be moved around or executed twice, because it
     *  will always produce the same result and never have side-effects.
     *  A side-effect free expression can be elided if its result is not used.
     */
    private def isExpressionInternal(tree: Tree, allowUnpure: Boolean,
        allowSideEffects: Boolean)(implicit env: Env): Boolean = {

      require(!allowSideEffects || allowUnpure)

      def test(tree: Tree): Boolean = tree match {
        // Atomic expressions
        case _: Literal       => true
        case _: This          => true
        case _: JSLinkingInfo => true

        // Vars (side-effect free, pure if immutable)
        case VarRef(name) =>
          allowUnpure || !env.isLocalMutable(name)

        // Fields may throw if qualifier is null
        case Select(qualifier, item) =>
          allowSideEffects && test(qualifier)

        // Static fields are side-effect free
        case SelectStatic(_, _) =>
          allowUnpure

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
        case tree @ JSObjectConstr(items) =>
          allowUnpure &&
          !doesObjectConstrRequireDesugaring(tree) &&
          items.forall { item =>
            test(item._2) && (item._1 match {
              case ComputedName(tree, _) => test(tree)
              case _                     => true
            })
          }
        case Closure(captureParams, params, body, captureValues) =>
          allowUnpure && (captureValues forall test)

        // Scala expressions that can always have side-effects
        case New(cls, constr, args) =>
          allowSideEffects && (args forall test)
        case LoadModule(cls) => // unfortunately
          allowSideEffects
        case Apply(receiver, method, args) =>
          allowSideEffects && test(receiver) && (args forall test)
        case ApplyStatically(receiver, cls, method, args) =>
          allowSideEffects && test(receiver) && (args forall test)
        case ApplyStatic(cls, method, args) =>
          allowSideEffects && (args forall test)
        case GetClass(arg) =>
          allowSideEffects && test(arg)
        case CallHelper(helper, args) =>
          allowSideEffects && (args forall test)

        // Casts
        case AsInstanceOf(expr, _) =>
          (allowSideEffects || semantics.asInstanceOfs == Unchecked) && test(expr)
        case Unbox(expr, _) =>
          (allowSideEffects || semantics.asInstanceOfs == Unchecked) && test(expr)

        // Because the linking info is a frozen object, linkingInfo["envInfo"] is pure
        case JSBracketSelect(JSLinkingInfo(), StringLiteral("envInfo")) => true

        // And because the env is a frozen object too, linkingInfo["envInfo"]["global"] is pure
        case JSBracketSelect(
            JSBracketSelect(JSLinkingInfo(), StringLiteral("envInfo")),
            StringLiteral("global")) =>
          true

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
        case JSSuperBracketSelect(_, qualifier, item) =>
          allowSideEffects && test(qualifier) && test(item)
        case LoadJSModule(_) =>
          allowSideEffects

        /* LoadJSConstructor is pure only for Scala.js-defined JS classes,
         * which do not have a native load spec. Note that this test makes
         * sense per se, as the actual desugaring of `LoadJSConstructor` is
         * based on the jsNativeLoadSpec of the class.
         */
        case LoadJSConstructor(cls) =>
          allowUnpure || {
            globalKnowledge.getJSNativeLoadSpec(cls.className).isEmpty
          }

        // Non-expressions
        case _ => false
      }
      test(tree)
    }

    /** Test whether the given tree is a standard JS expression.
     */
    def isExpression(tree: Tree)(implicit env: Env): Boolean =
      isExpressionInternal(tree, allowUnpure = true, allowSideEffects = true)

    /** Test whether the given tree is a side-effect-free standard JS expression.
     */
    def isSideEffectFreeExpression(tree: Tree)(implicit env: Env): Boolean =
      isExpressionInternal(tree, allowUnpure = true, allowSideEffects = false)

    /** Test whether the given tree is a pure standard JS expression.
     */
    def isPureExpression(tree: Tree)(implicit env: Env): Boolean =
      isExpressionInternal(tree, allowUnpure = false, allowSideEffects = false)

    def doVarDef(ident: Ident, tpe: Type, mutable: Boolean, rhs: Tree)(
        implicit env: Env): js.Tree = {
      implicit val pos = rhs.pos
      tpe match {
        case RecordType(fields) =>
          val elems = (rhs: @unchecked) match {
            case RecordValue(_, elems) =>
              elems
            case VarRef(rhsIdent) =>
              for (RecordType.Field(fName, fOrigName, fTpe, _) <- fields)
                yield VarRef(makeRecordFieldIdent(rhsIdent, fName, fOrigName))(fTpe)
          }
          js.Block(for {
            (RecordType.Field(fName, fOrigName, fTpe, fMutable),
                fRhs) <- fields zip elems
          } yield {
            doVarDef(makeRecordFieldIdent(ident, fName, fOrigName), fTpe,
                mutable || fMutable, fRhs)
          })

        case _ =>
          genLet(ident, mutable, transformExpr(rhs))
      }
    }

    def doEmptyVarDef(ident: Ident, tpe: Type)(
        implicit pos: Position, env: Env): js.Tree = {
      tpe match {
        case RecordType(fields) =>
          js.Block(for {
            RecordType.Field(fName, fOrigName, fTpe, fMutable) <- fields
          } yield {
            doEmptyVarDef(makeRecordFieldIdent(ident, fName, fOrigName), fTpe)
          })

        case _ =>
          genEmptyMutableLet(ident)
      }
    }

    def doAssign(lhs: Tree, rhs: Tree)(implicit env: Env): js.Tree = {
      implicit val pos = rhs.pos
      lhs.tpe match {
        case RecordType(fields) =>
          val VarRef(ident) = lhs
          val elems = (rhs: @unchecked) match {
            case VarRef(rhsIdent) =>
              for (RecordType.Field(fName, fOrigName, fTpe, fMutable) <- fields)
                yield VarRef(makeRecordFieldIdent(rhsIdent, fName, fOrigName))(fTpe)
          }
          js.Block(for {
            (RecordType.Field(fName, fOrigName, fTpe, fMutable),
                fRhs) <- fields zip elems
          } yield {
            doAssign(VarRef(makeRecordFieldIdent(ident, fName, fOrigName))(fTpe), fRhs)
          })

        case _ =>
          js.Assign(transformExpr(lhs), transformExpr(rhs))
      }
    }

    /** Push an lhs into a (potentially complex) rhs */
    def pushLhsInto(lhs: Lhs, rhs: Tree, tailPosLabels: Set[String])(
        implicit env: Env): js.Tree = {
      implicit val pos = rhs.pos

      /** Push the current lhs further into a deeper rhs. */
      @inline def redo(newRhs: Tree)(implicit env: Env) =
        pushLhsInto(lhs, newRhs, tailPosLabels)

      /** Extract a definition of the lhs if it is a VarDef, to avoid changing
       *  its scope.
       *  This only matters in ECMAScript 6, because we emit Lets.
       */
      def extractLet(inner: Lhs => js.Tree): js.Tree = {
        outputMode match {
          case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
            inner(lhs)
          case OutputMode.ECMAScript6 =>
            lhs match {
              case Lhs.VarDef(name, tpe, mutable) =>
                js.Block(
                    doEmptyVarDef(name, tpe),
                    inner(Lhs.Assign(VarRef(name)(tpe))))
              case _ =>
                inner(lhs)
            }
        }
      }

      def doReturnToLabel(l: Ident): js.Tree = {
        val newLhs = env.lhsForLabeledExpr(l)
        val body = pushLhsInto(newLhs, rhs, Set.empty)
        if (newLhs.hasNothingType) {
          /* A touch of peephole dead code elimination.
           * This is actually necessary to avoid dangling breaks to eliminated
           * labels, as in issue #2307.
           */
          body
        } else if (tailPosLabels.contains(l.name)) {
          body
        } else if (env.isDefaultBreakTarget(l.name)) {
          js.Block(body, js.Break(None))
        } else {
          usedLabels += l.name
          js.Block(body, js.Break(Some(transformIdent(l))))
        }
      }

      if (rhs.tpe == NothingType && lhs != Lhs.Discard) {
        /* A touch of peephole dead code elimination.
         * Actually necessary to handle pushing an lhs into an infinite loop,
         * for example.
         */
        val transformedRhs = pushLhsInto(Lhs.Discard, rhs, tailPosLabels)
        lhs match {
          case Lhs.VarDef(name, tpe, _) =>
            /* We still need to declare the var, in case it is used somewhere
             * else in the function, where we can't dce it.
             */
            js.Block(doEmptyVarDef(name, tpe), transformedRhs)

          case _ =>
            transformedRhs
        }
      } else (rhs match {
        // Handle the Block before testing whether it is an expression

        case Block(stats0 :+ expr0) =>
          val (stats1, env0) = transformBlockStats(stats0)
          val expr1 = redo(expr0)(env0)
          js.Block(stats1 :+ expr1)

        // Base case, rhs is already a regular JS expression

        case _ if isExpression(rhs) =>
          lhs match {
            case Lhs.Discard =>
              if (isSideEffectFreeExpression(rhs)) js.Skip()
              else transformExpr(rhs)
            case Lhs.VarDef(name, tpe, mutable) =>
              doVarDef(name, tpe, mutable, rhs)
            case Lhs.Assign(lhs) =>
              doAssign(lhs, rhs)
            case Lhs.Return(None) =>
              js.Return(transformExpr(rhs))
            case Lhs.Return(Some(l)) =>
              doReturnToLabel(l)
          }

        // Almost base case with RecordValue

        case RecordValue(recTpe, elems) =>
          lhs match {
            case Lhs.Discard =>
              val (newStat, _) = transformBlockStats(elems)
              js.Block(newStat)

            case Lhs.VarDef(name, tpe, mutable) =>
              unnest(elems) { (newElems, env) =>
                doVarDef(name, tpe, mutable, RecordValue(recTpe, newElems))(env)
              }

            case Lhs.Assign(lhs) =>
              unnest(elems) { (newElems, env0) =>
                implicit val env = env0
                val temp = newSyntheticVar()
                js.Block(
                    doVarDef(temp, recTpe, mutable = false,
                        RecordValue(recTpe, newElems)),
                    doAssign(lhs, VarRef(temp)(recTpe)))
              }

            case Lhs.Return(None) =>
              throw new AssertionError("Cannot return a record value.")

            case Lhs.Return(Some(l)) =>
              doReturnToLabel(l)
          }

        // Control flow constructs

        case Labeled(label, tpe, body) =>
          extractLet { newLhs =>
            val bodyEnv = env.withLabeledExprLHS(label, newLhs)
            val newBody =
              pushLhsInto(newLhs, body, tailPosLabels + label.name)(bodyEnv)
            if (usedLabels.contains(label.name))
              js.Labeled(label, newBody)
            else
              newBody
          }

        case Return(expr, label) =>
          pushLhsInto(Lhs.Return(label), expr, tailPosLabels)

        case Continue(label) =>
          js.Continue(label.map(transformIdent))

        case If(cond, thenp, elsep) =>
          unnest(cond) { (newCond, env0) =>
            implicit val env = env0
            extractLet { newLhs =>
              js.If(transformExpr(newCond),
                  pushLhsInto(newLhs, thenp, tailPosLabels),
                  pushLhsInto(newLhs, elsep, tailPosLabels))
            }
          }

        case TryCatch(block, errVar, handler) =>
          extractLet { newLhs =>
            val newBlock = pushLhsInto(newLhs, block, tailPosLabels)
            val newHandler = pushLhsInto(newLhs, handler, tailPosLabels)
            js.TryCatch(newBlock, errVar, newHandler)
          }

        case TryFinally(block, finalizer) =>
          extractLet { newLhs =>
            val newBlock = pushLhsInto(newLhs, block, tailPosLabels)
            val newFinalizer = transformStat(finalizer, Set.empty)
            js.TryFinally(newBlock, newFinalizer)
          }

        // TODO Treat throw as an LHS?
        case Throw(expr) =>
          unnest(expr) { (newExpr, env) =>
            js.Throw(transformExpr(newExpr)(env))
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
          unnest(selector) { (newSelector, env0) =>
            implicit val env = env0.withDefaultBreakTargets(tailPosLabels)
            extractLet { newLhs =>
              val newCases = {
                for {
                  (values, body) <- cases
                  newValues = (values map transformExpr)
                  // add the break statement
                  newBody = js.Block(
                      pushLhsInto(newLhs, body, tailPosLabels),
                      js.Break())
                  // desugar alternatives into several cases falling through
                  caze <- (newValues.init map (v => (v, js.Skip()))) :+ (newValues.last, newBody)
                } yield {
                  caze
                }
              }
              val newDefault = pushLhsInto(newLhs, default, tailPosLabels)
              js.Switch(transformExpr(newSelector), newCases, newDefault)
            }
          }

        // Scala expressions (if we reach here their arguments are not expressions)

        case New(cls, ctor, args) =>
          unnest(args) { (newArgs, env) =>
            redo(New(cls, ctor, newArgs))(env)
          }

        case Select(qualifier, item) =>
          unnest(qualifier) { (newQualifier, env) =>
            redo(Select(newQualifier, item)(rhs.tpe))(env)
          }

        case Apply(receiver, method, args) =>
          unnest(receiver, args) { (newReceiver, newArgs, env) =>
            redo(Apply(newReceiver, method, newArgs)(rhs.tpe))(env)
          }

        case ApplyStatically(receiver, cls, method, args) =>
          unnest(receiver, args) { (newReceiver, newArgs, env) =>
            redo(ApplyStatically(newReceiver, cls, method, newArgs)(rhs.tpe))(env)
          }

        case ApplyStatic(cls, method, args) =>
          unnest(args) { (newArgs, env) =>
            redo(ApplyStatic(cls, method, newArgs)(rhs.tpe))(env)
          }

        case UnaryOp(op, lhs) =>
          unnest(lhs) { (newLhs, env) =>
            redo(UnaryOp(op, newLhs))(env)
          }

        case BinaryOp(op, lhs, rhs) =>
          unnest(lhs, rhs) { (newLhs, newRhs, env) =>
            redo(BinaryOp(op, newLhs, newRhs))(env)
          }

        case NewArray(tpe, lengths) =>
          unnest(lengths) { (newLengths, env) =>
            redo(NewArray(tpe, newLengths))(env)
          }

        case ArrayValue(tpe, elems) =>
          unnest(elems) { (newElems, env) =>
            redo(ArrayValue(tpe, newElems))(env)
          }

        case ArrayLength(array) =>
          unnest(array) { (newArray, env) =>
            redo(ArrayLength(newArray))(env)
          }

        case ArraySelect(array, index) =>
          unnest(array, index) { (newArray, newIndex, env) =>
            redo(ArraySelect(newArray, newIndex)(rhs.tpe))(env)
          }

        case IsInstanceOf(expr, cls) =>
          unnest(expr) { (newExpr, env) =>
            redo(IsInstanceOf(newExpr, cls))(env)
          }

        case AsInstanceOf(expr, cls) =>
          if (semantics.asInstanceOfs == Unchecked) {
            redo(expr)
          } else {
            unnest(expr) { (newExpr, env) =>
              redo(AsInstanceOf(newExpr, cls))(env)
            }
          }

        case Unbox(expr, charCode) =>
          unnest(expr) { (newExpr, env) =>
            redo(Unbox(newExpr, charCode))(env)
          }

        case GetClass(expr) =>
          unnest(expr) { (newExpr, env) =>
            redo(GetClass(newExpr))(env)
          }

        case CallHelper(helper, args) =>
          unnest(args) { (newArgs, env) =>
            redo(CallHelper(helper, newArgs)(rhs.tpe))(env)
          }

        // JavaScript expressions (if we reach here their arguments are not expressions)

        case JSNew(ctor, args) =>
          if (containsAnySpread(args)) {
            redo {
              CallHelper("newJSObjectWithVarargs",
                  List(ctor, spreadToArgArray(args)))(AnyType)
            }
          } else {
            unnest(ctor :: args) { (newCtorAndArgs, env) =>
              val newCtor :: newArgs = newCtorAndArgs
              redo(JSNew(newCtor, newArgs))(env)
            }
          }

        case JSFunctionApply(fun, args) =>
          if (containsAnySpread(args)) {
            redo {
              JSBracketMethodApply(fun, StringLiteral("apply"),
                  List(Undefined(), spreadToArgArray(args)))
            }
          } else {
            unnest(fun :: args) { (newFunAndArgs, env) =>
              val newFun :: newArgs = newFunAndArgs
              redo(JSFunctionApply(newFun, newArgs))(env)
            }
          }

        case JSDotMethodApply(receiver, method, args) =>
          if (containsAnySpread(args)) {
            withTempVar(receiver) { (newReceiver, env0) =>
              implicit val env = env0
              redo {
                JSBracketMethodApply(
                    JSDotSelect(newReceiver, method),
                    StringLiteral("apply"),
                    List(newReceiver, spreadToArgArray(args)))
              }
            }
          } else {
            unnest(receiver :: args) { (newReceiverAndArgs, env) =>
              val newReceiver :: newArgs = newReceiverAndArgs
              redo(JSDotMethodApply(newReceiver, method, newArgs))(env)
            }
          }

        case JSBracketMethodApply(receiver, method, args) =>
          if (containsAnySpread(args)) {
            withTempVar(receiver) { (newReceiver, env0) =>
              implicit val env = env0
              redo {
                JSBracketMethodApply(
                    JSBracketSelect(newReceiver, method),
                    StringLiteral("apply"),
                    List(newReceiver, spreadToArgArray(args)))
              }
            }
          } else {
            unnest(receiver :: method :: args) { (newReceiverAndArgs, env) =>
              val newReceiver :: newMethod :: newArgs = newReceiverAndArgs
              redo(JSBracketMethodApply(newReceiver, newMethod, newArgs))(env)
            }
          }

        case JSSuperBracketSelect(cls, qualifier, item) =>
          unnest(qualifier, item) { (newQualifier, newItem, env) =>
            redo(JSSuperBracketSelect(cls, newQualifier, newItem))(env)
          }

        case JSSuperBracketCall(cls, receiver, method, args) =>
          val superClass = globalKnowledge.getSuperClassOfJSClass(cls.className)
          val superCtor = LoadJSConstructor(ClassType(superClass))

          redo {
            JSBracketMethodApply(
                JSBracketSelect(
                    JSBracketSelect(superCtor, StringLiteral("prototype")),
                    method),
                StringLiteral("call"),
                receiver :: args)
          }

        case JSDotSelect(qualifier, item) =>
          unnest(qualifier) { (newQualifier, env) =>
            redo(JSDotSelect(newQualifier, item))(env)
          }

        case JSBracketSelect(qualifier, item) =>
          unnest(qualifier, item) { (newQualifier, newItem, env) =>
            redo(JSBracketSelect(newQualifier, newItem))(env)
          }

        case JSUnaryOp(op, lhs) =>
          unnest(lhs) { (newLhs, env) =>
            redo(JSUnaryOp(op, newLhs))(env)
          }

        case JSBinaryOp(JSBinaryOp.&&, lhs, rhs) =>
          if (lhs.tpe == BooleanType) {
            redo(If(lhs, rhs, BooleanLiteral(false))(AnyType))
          } else {
            unnest(lhs) { (newLhs, env) =>
              redo(If(newLhs, rhs, newLhs)(AnyType))(env)
            }
          }

        case JSBinaryOp(JSBinaryOp.||, lhs, rhs) =>
          if (lhs.tpe == BooleanType) {
            redo(If(lhs, BooleanLiteral(true), rhs)(AnyType))
          } else {
            unnest(lhs) { (newLhs, env) =>
              redo(If(newLhs, newLhs, rhs)(AnyType))(env)
            }
          }

        case JSBinaryOp(op, lhs, rhs) =>
          unnest(lhs, rhs) { (newLhs, newRhs, env) =>
            redo(JSBinaryOp(op, newLhs, newRhs))(env)
          }

        case JSArrayConstr(items) =>
          if (containsAnySpread(items)) {
            redo {
              spreadToArgArray(items)
            }
          } else {
            unnest(items) { (newItems, env) =>
              redo(JSArrayConstr(newItems))(env)
            }
          }

        case rhs @ JSObjectConstr(fields) =>
          if (doesObjectConstrRequireDesugaring(rhs)) {
            val objVarDef = VarDef(newSyntheticVar(), AnyType, mutable = false,
                JSObjectConstr(Nil))
            val assignFields = fields.foldRight((Set.empty[String], List.empty[Tree])) {
              case ((prop, value), (namesSeen, statsAcc)) =>
                implicit val pos = value.pos
                val nameForDupes = prop match {
                  case _:StringLiteral | _:Ident => Some(prop.encodedName)
                  case _: ComputedName           => None
                }
                val stat = prop match {
                  case _ if nameForDupes.exists(namesSeen) =>
                    /* Important: do not emit the assignment, otherwise
                     * Closure recreates a literal with the duplicate field!
                     */
                    value
                  case prop: Ident =>
                    Assign(JSDotSelect(objVarDef.ref, prop), value)
                  case prop: StringLiteral =>
                    Assign(JSBracketSelect(objVarDef.ref, prop), value)
                  case ComputedName(tree, _) =>
                    Assign(JSBracketSelect(objVarDef.ref, tree), value)
                }
                (namesSeen ++ nameForDupes, stat :: statsAcc)
            }._2
            redo {
              Block(objVarDef :: assignFields ::: objVarDef.ref :: Nil)
            }
          } else {
            unnestJSObjectConstrFields(fields) { (newFields, env) =>
              redo(JSObjectConstr(newFields))(env)
            }
          }

        // Closures

        case Closure(captureParams, params, body, captureValues) =>
          unnest(captureValues) { (newCaptureValues, env) =>
            redo(Closure(captureParams, params, body, newCaptureValues))(env)
          }

        case _ =>
          if (lhs == Lhs.Discard) {
            /* Go "back" to transformStat() after having dived into
             * expression statements. Remember that Lhs.Discard is a trick that
             * we use to "add" all the code of pushLhsInto() to transformStat().
             */
            rhs match {
              case _:Skip | _:VarDef | _:Assign | _:While | _:DoWhile |
                  _:Debugger | _:JSSuperConstructorCall | _:JSDelete |
                  _:StoreModule | _:ClassDef =>
                transformStat(rhs, tailPosLabels)
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

    private def containsAnySpread(args: List[Tree]): Boolean =
      args.exists(_.isInstanceOf[JSSpread])

    private def spreadToArgArray(args: List[Tree])(
        implicit env: Env, pos: Position): Tree = {
      var reversedParts: List[Tree] = Nil
      var reversedPartUnderConstruction: List[Tree] = Nil

      def closeReversedPartUnderConstruction() = {
        if (!reversedPartUnderConstruction.isEmpty) {
          val part = reversedPartUnderConstruction.reverse
          reversedParts ::= JSArrayConstr(part)(part.head.pos)
          reversedPartUnderConstruction = Nil
        }
      }

      for (arg <- args) {
        arg match {
          case JSSpread(spreadArray) =>
            closeReversedPartUnderConstruction()
            reversedParts ::= spreadArray
          case _ =>
            reversedPartUnderConstruction ::= arg
        }
      }
      closeReversedPartUnderConstruction()

      reversedParts match {
        case Nil        => JSArrayConstr(Nil)
        case List(part) => part
        case _          =>
          val partHead :: partTail = reversedParts.reverse
          JSBracketMethodApply(
              partHead, StringLiteral("concat"), partTail)
      }
    }

    /** Tests whether a [[JSObjectConstr]] must be desugared. */
    private def doesObjectConstrRequireDesugaring(
        tree: JSObjectConstr): Boolean = {
      def computedNamesAllowed: Boolean =
        outputMode == OutputMode.ECMAScript6

      def hasComputedName: Boolean =
        tree.fields.exists(_._1.isInstanceOf[ComputedName])

      def hasDuplicateNonComputedProp: Boolean = {
        val names = tree.fields.collect {
          case (StringLiteral(name), _) => name
          case (Ident(name, _), _)      => name
        }
        names.toSet.size != names.size
      }

      (!computedNamesAllowed && hasComputedName) || hasDuplicateNonComputedProp
    }

    /** Evaluates `expr` and stores the result in a temp, then evaluates the
     *  result of `makeTree(temp)`.
     */
    private def withTempVar(expr: Tree)(makeTree: (Tree, Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      expr match {
        case VarRef(ident) if !env.isLocalMutable(ident) =>
          makeTree(expr, env)
        case _ =>
          implicit val pos = expr.pos
          val temp = newSyntheticVar()
          val newEnv = env.withDef(temp, expr.tpe, false)
          val computeTemp = pushLhsInto(
              Lhs.VarDef(temp, expr.tpe, mutable = false), expr,
              Set.empty)
          js.Block(computeTemp, makeTree(VarRef(temp)(expr.tpe), newEnv))
      }
    }

    // Desugar Scala operations to JavaScript operations -----------------------

    /** Desugar an expression of the IR into ES5 JS */
    def transformExpr(tree: Tree)(implicit env: Env): js.Tree = {
      import TreeDSL._

      implicit val pos = tree.pos

      def or0(tree: js.Tree): js.Tree =
        js.BinaryOp(JSBinaryOp.|, tree, js.IntLiteral(0))

      tree match {
        // Control flow constructs

        case Block(stats :+ expr) =>
          val (newStats, newEnv) = transformBlockStats(stats)
          js.Block(newStats :+ transformExpr(expr)(newEnv))

        // Note that these work even if thenp/elsep is not a BooleanType
        case If(cond, BooleanLiteral(true), elsep) =>
          js.BinaryOp(JSBinaryOp.||, transformExpr(cond), transformExpr(elsep))
        case If(cond, thenp, BooleanLiteral(false)) =>
          js.BinaryOp(JSBinaryOp.&&, transformExpr(cond), transformExpr(thenp))

        case If(cond, thenp, elsep) =>
          js.If(transformExpr(cond), transformExpr(thenp), transformExpr(elsep))

        // Scala expressions

        case New(cls, ctor, args) =>
          js.Apply(js.New(encodeClassVar(cls.className), Nil) DOT ctor,
              args map transformExpr)

        case LoadModule(cls) =>
          genLoadModule(cls.className)

        case RecordFieldVarRef(VarRef(name)) =>
          js.VarRef(name)

        case Select(qualifier, item) =>
          transformExpr(qualifier) DOT item

        case SelectStatic(cls, item) =>
          genSelectStatic(cls.className, item)

        case Apply(receiver, method, args) =>
          val newReceiver = transformExpr(receiver)
          val newArgs = args map transformExpr

          /* If the receiver is maybe a hijacked class instance, and there
           * exists a hijacked method helper for the method, use it. Methods
           * that do not have helpers are
           * - Reflective proxies
           * - Methods of RuntimeLong that are not also in java.lang.Long
           */
          if (isMaybeHijackedClass(receiver.tpe) &&
              hijackedClassMethodToHelperName.contains(method.name)) {
            val helperName = hijackedClassMethodToHelperName(method.name)
            genCallHelper(helperName, newReceiver :: newArgs: _*)
          } else {
            js.Apply(newReceiver DOT method, newArgs)
          }

        case ApplyStatically(receiver, cls, method, args) =>
          val className = cls.className
          val transformedArgs = (receiver :: args) map transformExpr

          if (globalKnowledge.isInterface(className)) {
            val Ident(methodName, origName) = method
            val fullName = className + "__" + methodName
            js.Apply(envField("f", fullName, origName),
                transformedArgs)
          } else {
            val fun = encodeClassVar(className).prototype DOT method
            js.Apply(fun DOT "call", transformedArgs)
          }

        case ApplyStatic(cls, method, args) =>
          val Ident(methodName, origName) = method
          val fullName = cls.className + "__" + methodName
          js.Apply(envField("s", fullName, origName), args map transformExpr)

        case UnaryOp(op, lhs) =>
          import UnaryOp._
          val newLhs = transformExpr(lhs)
          (op: @switch) match {
            case Boolean_!   => js.UnaryOp(JSUnaryOp.!, newLhs)
            case DoubleToInt => genCallHelper("doubleToInt", newLhs)

            case LongToInt    => genLongMethodApply(newLhs, LongImpl.toInt)
            case LongToDouble => genLongMethodApply(newLhs, LongImpl.toDouble)

            case DoubleToFloat => genFround(newLhs)

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
            case === | Num_== | Boolean_== =>
              js.BinaryOp(JSBinaryOp.===, newLhs, newRhs)
            case !== | Num_!= | Boolean_!= =>
              js.BinaryOp(JSBinaryOp.!==, newLhs, newRhs)

            case String_+ =>
              if (lhs.tpe == StringType || rhs.tpe == StringType) {
                js.BinaryOp(JSBinaryOp.+, newLhs, newRhs)
              } else {
                js.BinaryOp(JSBinaryOp.+, js.BinaryOp(JSBinaryOp.+,
                    js.StringLiteral(""), newLhs), newRhs)
              }

            case Int_+ => or0(js.BinaryOp(JSBinaryOp.+, newLhs, newRhs))
            case Int_- =>
              lhs match {
                case IntLiteral(0) => or0(js.UnaryOp(JSUnaryOp.-, newRhs))
                case _             => or0(js.BinaryOp(JSBinaryOp.-, newLhs, newRhs))
              }
            case Int_* => genCallHelper("imul", newLhs, newRhs)
            case Int_/ => or0(js.BinaryOp(JSBinaryOp./, newLhs, newRhs))
            case Int_% => or0(js.BinaryOp(JSBinaryOp.%, newLhs, newRhs))

            case Int_|   => js.BinaryOp(JSBinaryOp.|, newLhs, newRhs)
            case Int_&   => js.BinaryOp(JSBinaryOp.&, newLhs, newRhs)
            case Int_^   =>
              lhs match {
                case IntLiteral(-1) => js.UnaryOp(JSUnaryOp.~, newRhs)
                case _              => js.BinaryOp(JSBinaryOp.^, newLhs, newRhs)
              }
            case Int_<<  => js.BinaryOp(JSBinaryOp.<<, newLhs, newRhs)
            case Int_>>> => or0(js.BinaryOp(JSBinaryOp.>>>, newLhs, newRhs))
            case Int_>>  => js.BinaryOp(JSBinaryOp.>>, newLhs, newRhs)

            case Float_+ => genFround(js.BinaryOp(JSBinaryOp.+, newLhs, newRhs))
            case Float_- =>
              genFround(lhs match {
                case DoubleLiteral(0.0) => js.UnaryOp(JSUnaryOp.-, newRhs)
                case _                  => js.BinaryOp(JSBinaryOp.-, newLhs, newRhs)
              })
            case Float_* => genFround(js.BinaryOp(JSBinaryOp.*, newLhs, newRhs))
            case Float_/ => genFround(js.BinaryOp(JSBinaryOp./, newLhs, newRhs))
            case Float_% => genFround(js.BinaryOp(JSBinaryOp.%, newLhs, newRhs))

            case Double_+ => js.BinaryOp(JSBinaryOp.+, newLhs, newRhs)
            case Double_- =>
              lhs match {
                case DoubleLiteral(0.0) => js.UnaryOp(JSUnaryOp.-, newRhs)
                case _                  => js.BinaryOp(JSBinaryOp.-, newLhs, newRhs)
              }
            case Double_* => js.BinaryOp(JSBinaryOp.*, newLhs, newRhs)
            case Double_/ => js.BinaryOp(JSBinaryOp./, newLhs, newRhs)
            case Double_% => js.BinaryOp(JSBinaryOp.%, newLhs, newRhs)

            case Num_<  => js.BinaryOp(JSBinaryOp.< , newLhs, newRhs)
            case Num_<= => js.BinaryOp(JSBinaryOp.<=, newLhs, newRhs)
            case Num_>  => js.BinaryOp(JSBinaryOp.> , newLhs, newRhs)
            case Num_>= => js.BinaryOp(JSBinaryOp.>=, newLhs, newRhs)

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

            case Boolean_| => !(!js.BinaryOp(JSBinaryOp.|, newLhs, newRhs))
            case Boolean_& => !(!js.BinaryOp(JSBinaryOp.&, newLhs, newRhs))
          }

        case NewArray(tpe, lengths) =>
          genCallHelper("newArrayObject",
              genClassDataOf(tpe), js.ArrayConstr(lengths map transformExpr))

        case ArrayValue(tpe, elems) =>
          genCallHelper("makeNativeArrayWrapper",
              genClassDataOf(tpe), js.ArrayConstr(elems map transformExpr))

        case ArrayLength(array) =>
          genIdentBracketSelect(js.DotSelect(transformExpr(array),
              Ident("u")), "length")

        case ArraySelect(array, index) =>
          val newArray = transformExpr(array)
          val newIndex = transformExpr(index)
          semantics.arrayIndexOutOfBounds match {
            case CheckedBehavior.Compliant | CheckedBehavior.Fatal =>
              js.Apply(js.DotSelect(newArray, js.Ident("get")), List(newIndex))
            case CheckedBehavior.Unchecked =>
              js.BracketSelect(js.DotSelect(newArray, js.Ident("u")), newIndex)
          }

        case IsInstanceOf(expr, cls) =>
          genIsInstanceOf(transformExpr(expr), cls)

        case AsInstanceOf(expr, cls) =>
          val newExpr = transformExpr(expr)
          if (semantics.asInstanceOfs == Unchecked) newExpr
          else genAsInstanceOf(newExpr, cls)

        case Unbox(expr, charCode) =>
          val newExpr = transformExpr(expr)

          if (semantics.asInstanceOfs == Unchecked) {
            (charCode: @switch) match {
              case 'Z'             => !(!newExpr)
              case 'B' | 'S' | 'I' => or0(newExpr)
              case 'J'             => genCallHelper("uJ", newExpr)
              case 'D'             => js.UnaryOp(JSUnaryOp.+, newExpr)

              case 'F' =>
                if (semantics.strictFloats)
                  genFround(newExpr)
                else
                  js.UnaryOp(JSUnaryOp.+, newExpr)
            }
          } else {
            genCallHelper("u"+charCode, newExpr)
          }

        case GetClass(expr) =>
          genCallHelper("objectGetClass", transformExpr(expr))

        case CallHelper(helper, args) =>
          helper match {
            case "classDataOf" =>
              args.head match {
                case ClassOf(tpe) =>
                  genClassDataOf(tpe)
                case jlClass =>
                  js.DotSelect(transformExpr(jlClass), js.Ident("data$1"))
              }
            case "arrayDataOf" =>
              js.Apply(js.DotSelect(transformExpr(args.head),
                  js.Ident("getArrayOf")), Nil)
            case "zeroOf" =>
              js.DotSelect(
                  js.DotSelect(transformExpr(args.head), js.Ident("data$1")),
                  js.Ident("zero"))
            case _ =>
              genCallHelper(helper, args map transformExpr: _*)
          }

        // JavaScript expressions

        case JSBracketSelect(
            JSBracketSelect(JSLinkingInfo(), StringLiteral("envInfo")),
            StringLiteral("global")) =>
          // Shortcut for this field which is heavily used
          envField("g")

        case JSBracketSelect(JSLinkingInfo(), StringLiteral("envInfo")) =>
          /* environmentInfo is not used that often, but it makes sense to
           * short-cut it too anyway.
           */
          envField("env")

        case JSNew(constr, args) =>
          js.New(transformExpr(constr), args map transformExpr)

        case JSDotSelect(qualifier, item) =>
          js.DotSelect(transformExpr(qualifier), item)

        case JSBracketSelect(qualifier, item) =>
          genBracketSelect(transformExpr(qualifier), transformExpr(item))

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
          js.Apply(genBracketSelect(transformExpr(receiver),
              transformExpr(method)), args map transformExpr)

        case JSSuperBracketSelect(cls, qualifier, item) =>
          val ctor = genRawJSClassConstructor(cls.className)
          genCallHelper("superGet", ctor DOT "prototype",
              transformExpr(qualifier), transformExpr(item))

        case LoadJSConstructor(cls) =>
          genRawJSClassConstructor(cls.className)

        case LoadJSModule(cls) =>
          val className = cls.className
          globalKnowledge.getJSNativeLoadSpec(className) match {
            case None =>
              // this is a Scala.js-defined JS module class
              genLoadModule(className)

            case Some(spec) =>
              genLoadJSFromSpec(spec)
          }

        case JSSpread(items) =>
          assert(outputMode == OutputMode.ECMAScript6)
          js.Spread(transformExpr(items))

        case JSUnaryOp(op, lhs) =>
          js.UnaryOp(op, transformExpr(lhs))

        case JSBinaryOp(op, lhs, rhs) =>
          js.BinaryOp(op, transformExpr(lhs), transformExpr(rhs))

        case JSArrayConstr(items) =>
          js.ArrayConstr(items map transformExpr)

        case JSObjectConstr(fields) =>
          js.ObjectConstr(fields map { case (name, value) =>
            (transformPropertyName(name), transformExpr(value))
          })

        case JSLinkingInfo() =>
          envField("linkingInfo")

        // Literals

        case Undefined()            => js.Undefined()
        case Null()                 => js.Null()
        case BooleanLiteral(value)  => js.BooleanLiteral(value)
        case IntLiteral(value)      => js.IntLiteral(value)
        case FloatLiteral(value)    => js.DoubleLiteral(value.toDouble)
        case DoubleLiteral(value)   => js.DoubleLiteral(value)
        case StringLiteral(value)   => js.StringLiteral(value)

        case LongLiteral(0L) =>
          genLongZero()
        case LongLiteral(value) =>
          if (globalKnowledge.hasNewRuntimeLong) {
            val (lo, hi) = LongImpl.extractParts(value)
            genNewLong(LongImpl.initFromParts,
                js.IntLiteral(lo), js.IntLiteral(hi))
          } else {
            val (l, m, h) = LongImpl.extractPartsOld(value)
            genNewLong(LongImpl.initFromPartsOld,
                js.IntLiteral(l), js.IntLiteral(m), js.IntLiteral(h))
          }

        case ClassOf(cls) =>
          js.Apply(js.DotSelect(genClassDataOf(cls), Ident("getClassOf")), Nil)

        // Atomic expressions

        case VarRef(name) =>
          js.VarRef(name)

        case This() =>
          env.thisIdent.fold[js.Tree] {
            js.This()
          } { ident =>
            js.VarRef(ident)
          }

        case Closure(captureParams, params, body, captureValues) =>
          val innerFunction =
            desugarToFunction(params, body, isStat = false,
                Env.empty.withParams(captureParams ++ params))

          if (captureParams.isEmpty) {
            innerFunction
          } else {
            js.Apply(
                js.Function(captureParams.map(transformParamDef), {
                  js.Return(innerFunction)
                }),
                captureValues.map(transformExpr))
          }

        // Invalid trees

        case _ =>
          sys.error("Invalid tree in JSDesugar.transformExpr() "+
              s"of class ${tree.getClass}")
      }
    }

    def isMaybeHijackedClass(tpe: Type): Boolean = tpe match {
      case ClassType(cls) =>
        Definitions.HijackedClasses.contains(cls) ||
        Definitions.AncestorsOfHijackedClasses.contains(cls)
      case AnyType | UndefType | BooleanType | IntType | LongType |
          FloatType | DoubleType | StringType =>
        true
      case _ =>
        false
    }

    val hijackedClassMethodToHelperName: Map[String, String] = Map(
        "getClass__jl_Class" -> "objectGetClass",
        "toString__T"        -> "objectToString",
        "clone__O"           -> "objectClone",
        "finalize__V"        -> "objectFinalize",
        "notify__V"          -> "objectNotify",
        "notifyAll__V"       -> "objectNotifyAll",
        "equals__O__Z"       -> "objectEquals",
        "hashCode__I"        -> "objectHashCode",

        "length__I"                          -> "charSequenceLength",
        "charAt__I__C"                       -> "charSequenceCharAt",
        "subSequence__I__I__jl_CharSequence" -> "charSequenceSubSequence",

        "compareTo__O__I"          -> "comparableCompareTo",
        "compareTo__jl_Boolean__I" -> "comparableCompareTo",
        "compareTo__jl_Byte__I"    -> "comparableCompareTo",
        "compareTo__jl_Short__I"   -> "comparableCompareTo",
        "compareTo__jl_Integer__I" -> "comparableCompareTo",
        "compareTo__jl_Long__I"    -> "comparableCompareTo",
        "compareTo__jl_Float__I"   -> "comparableCompareTo",
        "compareTo__jl_Double__I"  -> "comparableCompareTo",
        "compareTo__jl_String__I"  -> "comparableCompareTo",

        "booleanValue__Z" -> "booleanBooleanValue",

        "byteValue__B"   -> "numberByteValue",
        "shortValue__S"  -> "numberShortValue",
        "intValue__I"    -> "numberIntValue",
        "longValue__J"   -> "numberLongValue",
        "floatValue__F"  -> "numberFloatValue",
        "doubleValue__D" -> "numberDoubleValue",

        "isNaN__Z"      -> "isNaN",
        "isInfinite__Z" -> "isInfinite"
    )

    def transformPropertyName(pName: PropertyName)(
        implicit env: Env): js.PropertyName = {
      implicit val pos = pName.pos
      pName match {
        case name: Ident           => transformIdent(name)
        case StringLiteral(s)      => js.StringLiteral(s)
        case ComputedName(tree, _) => js.ComputedName(transformExpr(tree))
      }
    }

    def genClassDataOf(cls: ReferenceType)(implicit pos: Position): js.Tree = {
      cls match {
        case ClassType(className) =>
          envField("d", className)
        case ArrayType(base, dims) =>
          (1 to dims).foldLeft(envField("d", base)) { (prev, _) =>
            js.Apply(js.DotSelect(prev, js.Ident("getArrayOf")), Nil)
          }
      }
    }

    private def genFround(arg: js.Tree)(implicit pos: Position): js.Tree = {
      genCallHelper("fround", arg)
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

    private implicit class RecordAwareEnv(env: Env) {
      def withDef(ident: Ident, tpe: Type, mutable: Boolean): Env = tpe match {
        case RecordType(fields) =>
          withRecordDefs(ident, fields, mutable)
        case _ =>
          env.withDef(ident, mutable)
      }

      private def withRecordDefs(recIdent: Ident,
          fields: List[RecordType.Field], recMutable: Boolean): Env = {
        fields.foldLeft(env) { (env, fld) =>
          val ident = makeRecordFieldIdent(recIdent, fld.name,
              fld.originalName)(recIdent.pos)
          env.withDef(ident, fld.tpe, recMutable || fld.mutable)
        }
      }
    }
  }

  // Helpers

  private[emitter] def genZeroOf(tpe: Type)(implicit pos: Position): js.Tree = {
    tpe match {
      case BooleanType => js.BooleanLiteral(false)
      case IntType     => js.IntLiteral(0)
      case LongType    => genLongZero()
      case FloatType   => js.DoubleLiteral(0.0)
      case DoubleType  => js.DoubleLiteral(0.0)
      case StringType  => js.StringLiteral("")
      case UndefType   => js.Undefined()
      case _           => js.Null()
    }
  }

  private def genLongZero()(implicit pos: Position): js.Tree = {
    genLongModuleApply(LongImpl.Zero)
  }

  private def genLongModuleApply(methodName: String, args: js.Tree*)(
      implicit pos: Position): js.Tree = {
    import TreeDSL._
    js.Apply(
        genLoadModule(LongImpl.RuntimeLongModuleClass) DOT methodName,
        args.toList)
  }

  private[emitter] def genLet(name: js.Ident, mutable: Boolean, rhs: js.Tree)(
      implicit pos: Position): js.LocalDef = {
    outputMode match {
      case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
        js.VarDef(name, Some(rhs))
      case OutputMode.ECMAScript6 =>
        js.Let(name, mutable, Some(rhs))
    }
  }

  private[emitter] def genEmptyMutableLet(name: js.Ident)(
      implicit pos: Position): js.LocalDef = {
    outputMode match {
      case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
        js.VarDef(name, rhs = None)
      case OutputMode.ECMAScript6 =>
        js.Let(name, mutable = true, rhs = None)
    }
  }

  private[emitter] def genSelectStatic(className: String, item: Ident)(
      implicit pos: Position): js.Tree = {
    envField("t", className + "__" + item.name)
  }

  private[emitter] def genIsInstanceOf(expr: js.Tree, cls: ReferenceType)(
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
              case BoxedFloatClass   => genCallHelper("isFloat", expr)
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
              envField(if (test) "is" else "as", className),
              List(expr))
        }

      case ArrayType(base, depth) =>
        js.Apply(
            envField(if (test) "isArrayOf" else "asArrayOf", base),
            List(expr, js.IntLiteral(depth)))
    }
  }

  private[emitter] def genCallHelper(helperName: String, args: js.Tree*)(
      implicit pos: Position): js.Tree = {
    js.Apply(envField(helperName), args.toList)
  }

  private[emitter] def encodeClassVar(className: String)(
      implicit pos: Position): js.Tree =
    envField("c", className)

  private[emitter] def genLoadModule(moduleClass: String)(
      implicit pos: Position): js.Tree = {
    import TreeDSL._
    js.Apply(envField("m", moduleClass), Nil)
  }

  private[emitter] def genRawJSClassConstructor(className: String)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): js.Tree = {

    genRawJSClassConstructor(className,
        globalKnowledge.getJSNativeLoadSpec(className))
  }

  private[emitter] def genRawJSClassConstructor(className: String,
      spec: Option[JSNativeLoadSpec])(
      implicit pos: Position): js.Tree = {
    spec match {
      case None =>
        // This is a Scala.js-defined JS class, call its class value accessor
        js.Apply(envField("a", className), Nil)

      case Some(spec) =>
        genLoadJSFromSpec(spec)
    }
  }

  private[emitter] def genLoadJSFromSpec(spec: JSNativeLoadSpec)(
      implicit pos: Position): js.Tree = {

    def pathSelection(from: js.Tree, path: List[String]): js.Tree = {
      path.foldLeft(from) {
        (prev, part) => genBracketSelect(prev, js.StringLiteral(part))
      }
    }

    spec match {
      case JSNativeLoadSpec.Global(path) =>
        pathSelection(envField("g"), path)

      case JSNativeLoadSpec.Import(module, path) =>
        val moduleValue = envModuleField(module)
        path match {
          case DefaultExportName :: rest =>
            val defaultField = genCallHelper("moduleDefault", moduleValue)
            pathSelection(defaultField, rest)
          case _ =>
            pathSelection(moduleValue, path)
        }
    }
  }

  private final val DefaultExportName = "default"

  private[emitter] def envModuleField(module: String)(
      implicit pos: Position): js.VarRef = {

    /* This is written so that the happy path, when `module` contains only
     * valid characters, is fast.
     */

    def isValidChar(c: Char): Boolean =
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

    def containsOnlyValidChars(): Boolean = {
      val len = module.length
      var i = 0
      while (i != len) {
        if (!isValidChar(module.charAt(i)))
          return false
        i += 1
      }
      true
    }

    def buildValidName(): String = {
      val result = new java.lang.StringBuilder("$i_")
      val len = module.length
      var i = 0
      while (i != len) {
        val c = module.charAt(i)
        if (isValidChar(c))
          result.append(c)
        else
          result.append("$%04x".format(c.toInt))
        i += 1
      }
      result.toString()
    }

    val varName =
      if (containsOnlyValidChars()) "$i_" + module
      else buildValidName()

    js.VarRef(js.Ident(varName, Some(module)))
  }

  private[emitter] def envField(field: String, subField: String,
      origName: Option[String] = None)(
      implicit pos: Position): js.Tree = {
    import TreeDSL._

    outputMode match {
      case OutputMode.ECMAScript51Global =>
        envField(field) DOT js.Ident(subField, origName)

      case OutputMode.ECMAScript51Isolated | OutputMode.ECMAScript6 =>
        js.VarRef(js.Ident("$" + field + "_" + subField, origName))
    }
  }

  private[emitter] def envField(field: String)(
      implicit pos: Position): js.Tree = {
    import TreeDSL._

    outputMode match {
      case OutputMode.ECMAScript51Global =>
        js.VarRef(js.Ident(ScalaJSEnvironmentName)) DOT field

      case OutputMode.ECMAScript51Isolated | OutputMode.ECMAScript6 =>
        js.VarRef(js.Ident("$" + field))
    }
  }

  private[emitter] def envFieldDef(field: String, subField: String,
      value: js.Tree)(
      implicit pos: Position): js.Tree = {
    envFieldDef(field, subField, value, mutable = false)
  }

  private[emitter] def envFieldDef(field: String, subField: String,
      value: js.Tree, mutable: Boolean)(
      implicit pos: Position): js.Tree = {
    envFieldDef(field, subField, origName = None, value, mutable)
  }

  private[emitter] def envFieldDef(field: String, subField: String,
      origName: Option[String], value: js.Tree)(
      implicit pos: Position): js.Tree = {
    envFieldDef(field, subField, origName, value, mutable = false)
  }

  private[emitter] def envFieldDef(field: String, subField: String,
      origName: Option[String], value: js.Tree, mutable: Boolean)(
      implicit pos: Position): js.Tree = {
    envFieldDef(field, subField, origName, value, mutable,
        keepFunctionExpression = false)
  }

  private[emitter] def envFieldDef(field: String, subField: String,
      origName: Option[String], value: js.Tree, mutable: Boolean,
      keepFunctionExpression: Boolean)(
      implicit pos: Position): js.Tree = {
    val globalVar = envField(field, subField, origName)
    def globalVarIdent = globalVar.asInstanceOf[js.VarRef].ident

    outputMode match {
      case OutputMode.ECMAScript51Global =>
        js.Assign(globalVar, value)

      case OutputMode.ECMAScript51Isolated =>
        value match {
          case js.Function(args, body) =>
            // Make sure the function has a meaningful `name` property
            val functionExpr = js.FunctionDef(globalVarIdent, args, body)
            if (keepFunctionExpression)
              js.VarDef(globalVarIdent, Some(functionExpr))
            else
              functionExpr
          case _ =>
            js.VarDef(globalVarIdent, Some(value))
        }

      case OutputMode.ECMAScript6 =>
        genLet(globalVarIdent, mutable, value)
    }
  }

  private[emitter] def genPropSelect(qual: js.Tree, item: js.PropertyName)(
      implicit pos: Position): js.Tree = {
    item match {
      case item: js.Ident         => js.DotSelect(qual, item)
      case item: js.StringLiteral => genBracketSelect(qual, item)
      case js.ComputedName(tree)  => genBracketSelect(qual, tree)
    }
  }

  private[emitter] def genBracketSelect(qual: js.Tree, item: js.Tree)(
      implicit pos: Position): js.Tree = {
    item match {
      case js.StringLiteral(name) if internalOptions.optimizeBracketSelects &&
          isValidIdentifier(name) && name != "eval" =>
        /* We exclude "eval" because Rhino does not respect the strict mode
         * specificities of eval().
         */
        js.DotSelect(qual, js.Ident(name))
      case _ =>
        js.BracketSelect(qual, item)
    }
  }

  private[emitter] def genIdentBracketSelect(qual: js.Tree, item: String)(
      implicit pos: Position): js.Tree = {
    require(item != "eval")
    if (internalOptions.optimizeBracketSelects)
      js.DotSelect(qual, js.Ident(item))
    else
      js.BracketSelect(qual, js.StringLiteral(item))
  }

  private[emitter] implicit class MyTreeOps(val self: js.Tree) {
    def prototype(implicit pos: Position): js.Tree =
      js.DotSelect(self, js.Ident("prototype"))
  }

  class DesugarException(tree: Tree,
      cause: Throwable) extends Exception(exceptionMsg(tree), cause)

  private def exceptionMsg(tree: Tree): String = {
    val writer = new StringWriter
    val printer = new ir.Printers.IRTreePrinter(writer)
    printer.printTopLevelTree(tree)
    "Exception while desugaring: " + writer.toString
  }
}

private object JSDesugaring {
  /** A left hand side that can be pushed into a right hand side tree. */
  sealed abstract class Lhs {
    def hasNothingType: Boolean = false
  }

  object Lhs {
    case class Assign(lhs: Tree) extends Lhs
    case class VarDef(name: Ident, tpe: Type, mutable: Boolean) extends Lhs

    case class Return(label: Option[Ident]) extends Lhs {
      override def hasNothingType: Boolean = true
    }

    /** Discard the value of rhs (but retain side effects). */
    case object Discard extends Lhs
  }

  // Environment

  final class Env private (
      val thisIdent: Option[js.Ident],
      val enclosingClassName: Option[String],
      vars: Map[String, Boolean],
      labeledExprLHSes: Map[String, Lhs],
      defaultBreakTargets: Set[String]
  ) {
    def isLocalMutable(ident: Ident): Boolean = vars(ident.name)

    def lhsForLabeledExpr(label: Ident): Lhs = labeledExprLHSes(label.name)

    def isDefaultBreakTarget(label: String): Boolean =
      defaultBreakTargets.contains(label)

    def withEnclosingClassName(enclosingClassName: Option[String]): Env =
      copy(enclosingClassName = enclosingClassName)

    def withThisIdent(thisIdent: Option[js.Ident]): Env =
      copy(thisIdent = thisIdent)

    def withParams(params: List[ParamDef]): Env = {
      params.foldLeft(this) {
        case (env, ParamDef(name, tpe, mutable, _)) =>
          // ParamDefs may not contain record types
          env.withDef(name, mutable)
      }
    }

    def withDef(ident: Ident, mutable: Boolean): Env =
      copy(vars = vars + (ident.name -> mutable))

    def withLabeledExprLHS(label: Ident, lhs: Lhs): Env =
      copy(labeledExprLHSes = labeledExprLHSes + (label.name -> lhs))

    def withDefaultBreakTargets(targets: Set[String]): Env =
      copy(defaultBreakTargets = targets)

    private def copy(
        thisIdent: Option[js.Ident] = this.thisIdent,
        enclosingClassName: Option[String] = this.enclosingClassName,
        vars: Map[String, Boolean] = this.vars,
        labeledExprLHSes: Map[String, Lhs] = this.labeledExprLHSes,
        defaultBreakTargets: Set[String] = this.defaultBreakTargets): Env = {
      new Env(thisIdent, enclosingClassName, vars, labeledExprLHSes,
          defaultBreakTargets)
    }
  }

  object Env {
    def empty: Env = new Env(None, None, Map.empty, Map.empty, Set.empty)
  }
}
