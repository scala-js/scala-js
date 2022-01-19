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

package org.scalajs.linker.backend.emitter

import scala.annotation.{switch, tailrec}

import scala.collection.mutable

import org.scalajs.ir._
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Position._
import org.scalajs.ir.Printers.IRTreePrinter
import org.scalajs.ir.Transformers._
import org.scalajs.ir.Traversers._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.CheckedBehavior._
import org.scalajs.linker.backend.javascript.{Trees => js}

import java.io.StringWriter

import EmitterNames._
import Transients._

/** Desugaring of the IR to JavaScript functions.
 *
 *  The general shape and compliance to standards is chosen with
 *  [[ESFeatures]].
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
 *     * Throw, i.e., `throw`
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
 *  FunctionEmitter does all this in a single pass, but it helps to think that:
 *  * Rule 1) is implemented by unnest(), and used most notably in
 *    * transformStat() for statement-only constructs
 *    * pushLhsInto() for statement-or-expression constructs
 *  * Rule 2) is implemented by pushLhsInto()
 *  * Emitting the class structure is delegated to [[ScalaJSClassEmitter]].
 *
 *  There are a few other things that FunctionEmitter takes care of:
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
 *  Finally, we also recover JavaScript `continue` statements out of labeled
 *  blocks that are immediately nested in the body of `while` loops. When we
 *  have:
 *
 *  {{{
 *  while (cond) {
 *    lab: {
 *      ...
 *      if (c)
 *        return(lab) expr;
 *      ...
 *    }
 *  }
 *  }}}
 *
 *  we make the `while` loop "acquire" the label as its own, and instruct the
 *  environment that, should an instruction want to return to that label, it
 *  should use `continue` instead of `break`. This produces:
 *
 *  {{{
 *  lab: while (cond) {
 *    ...
 *    if (c)
 *      { expr; continue lab }
 *    ...
 *  }
 *  }}}
 *
 *  Note that the result of `expr` is always discarded, in that case, since the
 *  labeled block was in statement position (by construction).
 *
 *  Similarly to the treatment with `defaultBreakTargets`, we keep track of
 *  `defaultContinueTargets`, the set of labels for which we can simply write
 *  `continue` instead of `continue lab`. This allows the above code to finally
 *  become:
 *
 *  {{{
 *  while (cond) {
 *    ...
 *    if (c)
 *      { expr; continue }
 *    ...
 *  }
 *  }}}
 *
 *  @author Sébastien Doeraene
 */
private[emitter] class FunctionEmitter(sjsGen: SJSGen) {
  import FunctionEmitter._
  import sjsGen._
  import jsGen._
  import config._
  import nameGen._
  import varGen._

  /** Desugars parameters and body to a JS function.
   */
  def desugarToFunction(enclosingClassName: ClassName, params: List[ParamDef],
      body: Tree, resultType: Type)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[js.Function] = {
    desugarToFunction(enclosingClassName, params, restParam = None, body,
        resultType)
  }

  /** Desugars parameters and body to a JS function.
   */
  def desugarToFunction(enclosingClassName: ClassName, params: List[ParamDef],
      restParam: Option[ParamDef], body: Tree, resultType: Type)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[js.Function] = {
    new JSDesugar().desugarToFunction(params, restParam, body,
        isStat = resultType == NoType,
        Env.empty(resultType).withEnclosingClassName(Some(enclosingClassName)))
  }

  /** Desugars parameters and body to a JS function where `this` is given as
   *  an explicit normal parameter.
   */
  def desugarToFunctionWithExplicitThis(enclosingClassName: ClassName,
      params: List[ParamDef], body: Tree, resultType: Type)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[js.Function] = {
    new JSDesugar().desugarToFunctionWithExplicitThis(params, body,
        isStat = resultType == NoType,
        Env.empty(resultType).withEnclosingClassName(Some(enclosingClassName)))
  }

  /** Desugars parameters and body to a JS function.
   */
  def desugarToFunction(params: List[ParamDef], restParam: Option[ParamDef],
      body: Tree, resultType: Type)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[js.Function] = {
    new JSDesugar().desugarToFunction(params, restParam, body,
        isStat = resultType == NoType, Env.empty(resultType))
  }

  /** Desugars a class-level expression. */
  def desugarExpr(expr: Tree, resultType: Type)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    implicit val pos = expr.pos

    for (fun <- desugarToFunction(Nil, None, expr, resultType)) yield {
      fun match {
        case js.Function(_, Nil, None, js.Return(newExpr)) =>
          // no need for an IIFE, we can just use `newExpr` directly
          newExpr
        case _ =>
          js.Apply(fun, Nil)
      }
    }
  }

  private class JSDesugar()(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge) {

    // For convenience
    private val es2015 = esFeatures.esVersion >= ESVersion.ES2015

    // Name management

    /** Whether we are running in the "optimistic naming" run.
     *
     *  In theory, `JSDesugar` works in two passes: the optimistic run,
     *  followed by the pessimistic run should the first one fail.
     *
     *  The optimistic run assumes that there is no clash between a local
     *  variable name and a global variable name (i.e., a `JSGlobalRef`). This
     *  allows it to straightforwardly translate IR identifiers to JS
     *  identifiers. While it does that, it records all the local variable and
     *  global variable names that are used in the method.
     *
     *  At the end of the translation, we check whether there was a clash by
     *  testing if the two sets intersect. If they do not, we are lucky, and
     *  can completely by-pass the pessimistic run. If there is a clash, then
     *  we need to restart everything in pessimistic mode.
     *
     *  In the pessimistic run, we use the set of global variable names that
     *  was collected during the optimistic run to *prevent* clashes from
     *  happening. This requires that we maintain a map of IR identifiers to
     *  allocated JS identifiers, as some IR identifiers need to be renamed.
     */
    private var isOptimisticNamingRun: Boolean = true

    private val globalVarNames = mutable.Set.empty[String]
    private val localVarNames = mutable.Set.empty[String]

    private lazy val localVarAllocs = mutable.Map.empty[LocalName, String]

    private def referenceGlobalName(name: String): Unit =
      globalVarNames += name

    private def extractWithGlobals[A](withGlobals: WithGlobals[A]): A = {
      for (globalRef <- withGlobals.globalVarNames)
        referenceGlobalName(globalRef)
      withGlobals.value
    }

    private def transformLocalName(name: LocalName): String = {
      if (isOptimisticNamingRun) {
        val jsName = genName(name)
        localVarNames += jsName
        jsName
      } else {
        // Slow path in a different `def` to keep it out of the JIT's way
        def slowPath(): String = {
          localVarAllocs.getOrElseUpdate(name, {
            var suffix = 0
            val baseJSName = genName(name)
            var result: String = baseJSName
            while (globalVarNames.contains(result) ||
                localVarNames.contains(result)) {
              suffix += 1
              result = baseJSName + "$" + suffix
            }
            localVarNames += result
            result
          })
        }
        slowPath()
      }
    }

    private var syntheticVarCounter: Int = 0

    private def newSyntheticVar()(implicit pos: Position): js.Ident = {
      syntheticVarCounter += 1
      fileLevelVarIdent("$x" + syntheticVarCounter)
    }

    @inline
    @tailrec
    private def performOptimisticThenPessimisticRuns[A](
        body: => A): WithGlobals[A] = {
      val result = body
      if (!isOptimisticNamingRun || !globalVarNames.exists(localVarNames)) {
        /* At this point, filter out the global refs that do not need to be
         * tracked across functions and classes.
         *
         * By default, only dangerous global refs need to be tracked outside of
         * functions, to power `mentionedDangerousGlobalRefs` In that case, the
         * set is hopefully already emptied at this point for the large majority
         * of methods, if not all.
         *
         * However, when integrating with GCC, we must tell it a list of all the
         * global variables that are accessed in an externs file. In that case, we
         * need to track all global variables across functions and classes. This is
         * slower, but running GCC will take most of the time anyway in that case.
         */
        val globalRefs =
          if (trackAllGlobalRefs) globalVarNames.toSet
          else GlobalRefUtils.keepOnlyDangerousGlobalRefs(globalVarNames.toSet)

        WithGlobals(result, globalRefs)
      } else {
        /* Clear the local var names, but *not* the global var names.
         * In the pessimistic run, we will use the knowledge gathered during
         * the optimistic run about the set of global variable names that are
         * used.
         */
        localVarNames.clear()
        isOptimisticNamingRun = false
        performOptimisticThenPessimisticRuns(body)
      }
    }

    // Record names

    def makeRecordFieldIdentForVarRef(tree: RecordSelect)(
        implicit pos: Position): js.Ident = {

      val recIdent = (tree.record: @unchecked) match {
        case VarRef(ident)                 => transformLocalVarIdent(ident)
        case Transient(JSVarRef(ident, _)) => ident
        case record: RecordSelect          => makeRecordFieldIdentForVarRef(record)
      }

      // Since this is only used for VarRefs, we never need an original name
      makeRecordFieldIdent(recIdent, tree.field.name, NoOriginalName)
    }

    def makeRecordFieldIdent(recIdent: js.Ident,
        fieldName: FieldName, fieldOrigName: OriginalName)(
        implicit pos: Position): js.Ident = {

      /* "__" is a safe separator for generated names because JSGen avoids it
       * when generating `LocalName`s and `FieldName`s.
       */
      val name = recIdent.name + "__" + genName(fieldName)
      val originalName = OriginalName(
          recIdent.originalName.getOrElse(recIdent.name) ++ UTF8Period ++
          fieldOrigName.getOrElse(fieldName))
      js.Ident(name, originalName)
    }

    // LHS'es for labeled expressions

    val usedLabels = mutable.Set.empty[LabelName]

    // Now the work

    /** Desugars parameters and body to a JS function where `this` is given as
     *  a normal parameter.
     */
    def desugarToFunctionWithExplicitThis(
        params: List[ParamDef], body: Tree, isStat: Boolean, env0: Env)(
        implicit pos: Position): WithGlobals[js.Function] = {

      performOptimisticThenPessimisticRuns {
        val thisIdent = fileLevelVarIdent("thiz", thisOriginalName)
        val env = env0.withThisIdent(Some(thisIdent))
        val js.Function(jsArrow, jsParams, restParam, jsBody) =
          desugarToFunctionInternal(arrow = false, params, None, body, isStat, env)
        js.Function(jsArrow, js.ParamDef(thisIdent) :: jsParams, restParam, jsBody)
      }
    }

    /** Desugars parameters and body to a JS function.
     */
    def desugarToFunction(params: List[ParamDef], restParam: Option[ParamDef],
        body: Tree, isStat: Boolean, env0: Env)(
        implicit pos: Position): WithGlobals[js.Function] = {
      performOptimisticThenPessimisticRuns {
        desugarToFunctionInternal(arrow = false, params, restParam, body, isStat, env0)
      }
    }

    /** Desugars parameters and body to a JS function.
     */
    private def desugarToFunctionInternal(arrow: Boolean,
        params: List[ParamDef], restParam: Option[ParamDef], body: Tree,
        isStat: Boolean, env0: Env)(
        implicit pos: Position): js.Function = {

      val env = env0.withParams(params ++ restParam)

      val newBody = if (isStat) {
        body match {
          // Necessary to optimize away top-level _return: {} blocks
          case Labeled(label, _, body) =>
            transformStat(body, Set.empty)(
                env.withLabeledExprLHS(label, Lhs.ReturnFromFunction))
          case _ =>
            transformStat(body, Set.empty)(env)
        }
      } else {
        pushLhsInto(Lhs.ReturnFromFunction, body, Set.empty)(env)
      }

      val cleanedNewBody = newBody match {
        case js.Block(stats :+ js.Return(js.Undefined())) => js.Block(stats)
        case other                                        => other
      }

      val actualArrowFun = arrow && esFeatures.useECMAScript2015Semantics
      val jsParams = params.map(transformParamDef(_))

      if (es2015) {
        val jsRestParam = restParam.map(transformParamDef(_))
        js.Function(actualArrowFun, jsParams, jsRestParam, cleanedNewBody)
      } else {
        val patchedBody = restParam.fold {
          cleanedNewBody
        } { restParam =>
          js.Block(makeExtractRestParam(restParam, jsParams.size), cleanedNewBody)
        }

        js.Function(actualArrowFun, jsParams, None, patchedBody)
      }
    }

    private def makeExtractRestParam(restParamDef: ParamDef, offset: Int)(
        implicit pos: Position): js.Tree = {
      val lenIdent = newSyntheticVar()
      val len = js.VarRef(lenIdent)

      val counterIdent = newSyntheticVar()
      val counter = js.VarRef(counterIdent)

      val restParamIdent = transformLocalVarIdent(restParamDef.name,
          restParamDef.originalName)
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
    def transformStat(tree: Tree, tailPosLabels: Set[LabelName])(
        implicit env: Env): js.Tree = {
      import TreeDSL._

      implicit val pos = tree.pos

      tree match {
        // VarDefs at the end of block. Normal VarDefs are handled in
        // transformBlockStats

        case VarDef(_, _, _, _, rhs) =>
          pushLhsInto(Lhs.Discard, rhs, tailPosLabels)

        // Statement-only language constructs

        case Skip() =>
          js.Skip()

        case Assign(lhs, rhs) =>
          lhs match {
            case Select(qualifier, className, field) =>
              unnest(qualifier, rhs) { (newQualifier, newRhs, env0) =>
                implicit val env = env0
                js.Assign(
                    genSelect(transformExprNoChar(newQualifier), className, field)(lhs.pos),
                    transformExpr(newRhs, lhs.tpe))
              }

            case ArraySelect(array, index) =>
              unnest(array, index, rhs) { (newArray, newIndex, newRhs, env0) =>
                implicit val env = env0
                val genArray = transformExprNoChar(newArray)
                val genIndex = transformExprNoChar(newIndex)
                val genRhs = transformExpr(newRhs, lhs.tpe)
                semantics.arrayIndexOutOfBounds match {
                  case CheckedBehavior.Compliant | CheckedBehavior.Fatal =>
                    js.Apply(js.DotSelect(genArray, js.Ident("set")),
                        List(genIndex, genRhs))
                  case CheckedBehavior.Unchecked =>
                    js.Assign(
                        js.BracketSelect(
                            js.DotSelect(genArray, js.Ident("u"))(lhs.pos),
                            genIndex)(lhs.pos),
                        genRhs)
                }
              }

            case lhs: RecordSelect =>
              val newLhs = Transient(JSVarRef(makeRecordFieldIdentForVarRef(lhs),
                  mutable = true)(lhs.tpe))
              pushLhsInto(Lhs.Assign(newLhs), rhs, tailPosLabels)

            case JSPrivateSelect(qualifier, className, field) =>
              unnest(qualifier, rhs) { (newQualifier, newRhs, env0) =>
                implicit val env = env0
                js.Assign(
                    genJSPrivateSelect(transformExprNoChar(newQualifier),
                        className, field)(moduleContext, globalKnowledge, lhs.pos),
                    transformExprNoChar(newRhs))
              }

            case JSSelect(qualifier, item) =>
              unnest(qualifier, item, rhs) {
                (newQualifier, newItem, newRhs, env0) =>
                  implicit val env = env0
                  js.Assign(
                      genBracketSelect(transformExprNoChar(newQualifier),
                          transformExprNoChar(newItem))(lhs.pos),
                      transformExprNoChar(newRhs))
              }

            case JSSuperSelect(superClass, qualifier, item) =>
              unnest(superClass, qualifier, item, rhs) {
                (newSuperClass, newQualifier, newItem, newRhs, env0) =>
                  implicit val env = env0
                  genCallHelper("superSet", transformExprNoChar(newSuperClass),
                      transformExprNoChar(newQualifier), transformExprNoChar(item),
                      transformExprNoChar(rhs))
              }

            case SelectStatic(className, item) =>
              val scope = (className, item.name)

              if (needToUseGloballyMutableVarSetter(scope)) {
                unnest(rhs) { (rhs, env0) =>
                  implicit val env = env0
                  js.Apply(globalVar("u", scope), transformExpr(rhs, lhs.tpe) :: Nil)
                }
              } else {
                // Assign normally.
                pushLhsInto(Lhs.Assign(lhs), rhs, tailPosLabels)
              }

            case _:VarRef | _:JSGlobalRef =>
              pushLhsInto(Lhs.Assign(lhs), rhs, tailPosLabels)
          }

        case StoreModule(className, value) =>
          unnest(value) { (newValue, env0) =>
            implicit val env = env0
            js.Assign(globalVar("n", className), transformExprNoChar(newValue))
          }

        case While(cond, body) =>
          /* If there is a Labeled block immediately enclosed within the body
           * of this while loop, acquire its label and use it as the while's
           * label, turning it into a `continue` label.
           */
          val (optLabel, newBody) = {
            body match {
              case Labeled(label, _, innerBody) =>
                val innerBodyEnv = env
                  .withLabeledExprLHS(label, Lhs.Discard)
                  .withTurnLabelIntoContinue(label.name)
                  .withDefaultBreakTargets(tailPosLabels)
                  .withDefaultContinueTargets(Set(label.name))
                val newBody =
                  pushLhsInto(Lhs.Discard, innerBody, Set(label.name))(innerBodyEnv)
                val optLabel = if (usedLabels.contains(label.name))
                  Some(transformLabelIdent(label))
                else
                  None
                (optLabel, newBody)

              case _ =>
                val bodyEnv = env
                  .withDefaultBreakTargets(tailPosLabels)
                  .withDefaultContinueTargets(Set.empty)
                val newBody = transformStat(body, Set.empty)(bodyEnv)
                (None, newBody)
            }
          }

          /* We cannot simply unnest(cond) here, because that would eject the
           * evaluation of the condition out of the loop.
           */
          if (isExpression(cond)) {
            js.While(transformExprNoChar(cond), newBody, optLabel)
          } else {
            js.While(js.BooleanLiteral(true), {
              unnest(cond) { (newCond, env0) =>
                implicit val env = env0
                js.If(transformExprNoChar(newCond), newBody, js.Break())
              }
            }, optLabel)
          }

        case DoWhile(body, cond) =>
          /* We cannot simply unnest(cond) here, because that would eject the
           * evaluation of the condition out of the loop.
           */
          val bodyEnv = env
            .withDefaultBreakTargets(tailPosLabels)
            .withDefaultContinueTargets(Set.empty)
          val newBody = transformStat(body, Set.empty)(bodyEnv)
          if (isExpression(cond)) {
            /* Here, we could do the same optimization with `continue` as in
             * `While` loops (see above), but no Scala source code produces
             * patterns where this happens. Therefore, we do not bother.
             */
            js.DoWhile(newBody, transformExprNoChar(cond))
          } else {
            /* Since in this rewriting, the old body is not in tail position of
             * the emitted do..while body, we cannot optimize an inner Labeled
             * block into using `continue` statements.
             */
            js.While(js.BooleanLiteral(true), {
              js.Block(
                  newBody,
                  unnest(cond) { (newCond, env0) =>
                    implicit val env = env0
                    js.If(transformExprNoChar(newCond), js.Skip(), js.Break())
                  })
            })
          }

        case ForIn(obj, keyVar, keyVarOriginalName, body) =>
          unnest(obj) { (newObj, env0) =>
            implicit val env = env0

            val lhs = genEmptyImmutableLet(
                transformLocalVarIdent(keyVar, keyVarOriginalName))
            js.ForIn(lhs, transformExprNoChar(newObj), {
              transformStat(body, Set.empty)(
                  env.withDef(keyVar, mutable = false))
            })
          }

        case Debugger() =>
          js.Debugger()

        case JSSuperConstructorCall(args) =>
          unnestOrSpread(args) { (newArgs, env0) =>
            implicit val env = env0

            val enclosingClassName = env.enclosingClassName.getOrElse {
              throw new AssertionError(
                  "Need enclosing class for super constructor call.")
            }

            val superCtorCall = if (useClassesForJSClassesAndThrowables) {
              js.Apply(js.Super(), newArgs.map(transformJSArg))
            } else {
              val superCtor = {
                if (globalKnowledge.hasStoredSuperClass(enclosingClassName)) {
                  fileLevelVar("superClass")
                } else {
                  val superClass =
                    globalKnowledge.getSuperClassOfJSClass(enclosingClassName)
                  extractWithGlobals(genJSClassConstructor(superClass))
                }
              }

              if (needsToTranslateAnySpread(newArgs)) {
                val argArray = spreadToArgArray(newArgs)
                js.Apply(
                    genIdentBracketSelect(superCtor, "apply"),
                    List(js.This(), transformExprNoChar(argArray)))
              } else {
                js.Apply(
                    genIdentBracketSelect(superCtor, "call"),
                    js.This() :: newArgs.map(transformJSArg))
              }
            }

            val enclosingClassFieldDefs =
              globalKnowledge.getJSClassFieldDefs(enclosingClassName)

            val fieldDefs = for {
              field <- enclosingClassFieldDefs
              if !field.flags.namespace.isStatic
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
               * For private fields, we can use a normal assignment, since they
               * cannot clash with anything else in the prototype chain anyway.
               */

              val zero = genBoxedZeroOf(field.ftpe)

              field match {
                case FieldDef(_, name, _, _) =>
                  js.Assign(
                      genJSPrivateSelect(js.This(), enclosingClassName, name),
                      zero)

                case JSFieldDef(_, name, _) =>
                  unnest(name) { (newName, env0) =>
                    implicit val env = env0

                    val descriptor = List(
                        "configurable" -> js.BooleanLiteral(true),
                        "enumerable" -> js.BooleanLiteral(true),
                        "writable" -> js.BooleanLiteral(true),
                        "value" -> zero
                    )

                    extractWithGlobals(
                        genDefineProperty(js.This(), transformExprNoChar(newName), descriptor))
                  }
              }
            }

            js.Block(superCtorCall :: fieldDefs)
          }

        case JSDelete(qualifier, item) =>
          unnest(qualifier, item) { (newQual, newItem, env0) =>
            implicit val env = env0
            js.Delete(genBracketSelect(
                transformExprNoChar(newQual), transformExprNoChar(newItem)))
          }

        // Treat 'return' as an LHS

        case Return(expr, label) =>
          pushLhsInto(Lhs.Return(label), expr, tailPosLabels)

        case Transient(SystemArrayCopy(src, srcPos, dest, destPos, length)) =>
          unnest(List(src, srcPos, dest, destPos, length)) { (newArgs, env0) =>
            implicit val env = env0
            val jsArgs = newArgs.map(transformExprNoChar(_))

            if (es2015)
              js.Apply(js.DotSelect(jsArgs.head, js.Ident("copyTo")), jsArgs.tail)
            else
              genCallHelper("systemArraycopy", jsArgs: _*)
          }

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

    def transformBlockStats(trees: List[Tree])(
        implicit env: Env): (List[js.Tree], Env) = {

      @tailrec
      def transformLoop(trees: List[Tree], env: Env,
          acc: List[js.Tree]): (List[js.Tree], Env) = trees match {
        case VarDef(ident, originalName, tpe, mutable, rhs) :: ts =>
          val newEnv = env.withDef(ident, mutable)
          val lhs = Lhs.VarDef(transformLocalVarIdent(ident, originalName),
              tpe, mutable)
          val newTree = pushLhsInto(lhs, rhs, Set.empty)(env)
          transformLoop(ts, newEnv, newTree :: acc)

        case tree :: ts =>
          transformLoop(ts, env, transformStat(tree, Set.empty)(env) :: acc)

        case Nil =>
          (acc.reverse, env)
      }

      transformLoop(trees, env, Nil)
    }

    /** Same as `unnest`, but allows (and preserves) [[JSSpread]]s at the
     *  top-level.
     */
    def unnestOrSpread(args: List[TreeOrJSSpread])(
        makeStat: (List[TreeOrJSSpread], Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      unnestOrSpread(Nil, args) { (newNil, newArgs, env) =>
        assert(newNil.isEmpty)
        makeStat(newArgs, env)
      }
    }

    /** Same as `unnest`, but allows (and preserves) [[JSSpread]]s at the
     *  top-level.
     */
    def unnestOrSpread(nonSpreadArgs: List[Tree], args: List[TreeOrJSSpread])(
        makeStat: (List[Tree], List[TreeOrJSSpread], Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      val (argsNoSpread, argsWereSpread) = args.map {
        case JSSpread(items) => (items, true)
        case arg: Tree       => (arg, false)
      }.unzip

      unnest(nonSpreadArgs ::: argsNoSpread) { (newAllArgs, env) =>
        val (newNonSpreadArgs, newArgsNoSpread) =
          newAllArgs.splitAt(nonSpreadArgs.size)
        val newArgs = newArgsNoSpread.zip(argsWereSpread).map {
          case (newItems, true) => JSSpread(newItems)(newItems.pos)
          case (newArg, false)  => newArg
        }
        makeStat(newNonSpreadArgs, newArgs, env)
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
        makeStat: (List[Tree], Env) => js.Tree)(
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

          val keepAsIs =
            if (noExtractYet) isExpression(arg)
            else isPureExpression(arg)

          if (keepAsIs) {
            arg
          } else {
            implicit val pos = arg.pos
            arg match {
              case Block(stats :+ expr) =>
                val (jsStats, newEnv) = transformBlockStats(stats)
                val result = rec(expr)(newEnv)
                // Put the stats in a Block because ++=: is not smart
                js.Block(jsStats) +=: extractedStatements
                innerEnv = stats.foldLeft(innerEnv) { (prev, stat) =>
                  stat match {
                    case VarDef(name, _, _, mutable, _) =>
                      prev.withDef(name, mutable)
                    case _ =>
                      prev
                  }
                }
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
              case IsInstanceOf(expr, testType) =>
                IsInstanceOf(rec(expr), testType)

              case AsInstanceOf(expr, tpe)
                  if noExtractYet || semantics.asInstanceOfs == Unchecked =>
                AsInstanceOf(rec(expr), tpe)

              case NewArray(tpe, lengths) =>
                NewArray(tpe, recs(lengths))
              case ArrayValue(tpe, elems) =>
                ArrayValue(tpe, recs(elems))
              case JSArrayConstr(items) if !needsToTranslateAnySpread(items) =>
                JSArrayConstr(recsOrSpread(items))

              case arg @ JSObjectConstr(items)
                  if !doesObjectConstrRequireDesugaring(arg) =>
                // We need to properly interleave keys and values here
                val newItems = items.foldRight[List[(Tree, Tree)]](Nil) {
                  case ((key, value), acc) =>
                    val newValue = rec(value) // value first!
                    val newKey = rec(key)
                    (newKey, newValue) :: acc
                }
                JSObjectConstr(newItems)

              case Closure(arrow, captureParams, params, restParam, body, captureValues) =>
                Closure(arrow, captureParams, params, restParam, body, recs(captureValues))

              case New(className, constr, args) if noExtractYet =>
                New(className, constr, recs(args))
              case Select(qualifier, className, item) if noExtractYet =>
                Select(rec(qualifier), className, item)(arg.tpe)
              case Apply(flags, receiver, method, args) if noExtractYet =>
                val newArgs = recs(args)
                Apply(flags, rec(receiver), method, newArgs)(arg.tpe)
              case ApplyStatically(flags, receiver, className, method, args) if noExtractYet =>
                val newArgs = recs(args)
                ApplyStatically(flags, rec(receiver), className, method, newArgs)(arg.tpe)
              case ApplyStatic(flags, className, method, args) if noExtractYet =>
                ApplyStatic(flags, className, method, recs(args))(arg.tpe)
              case ApplyDynamicImport(flags, className, method, args) if noExtractYet =>
                ApplyDynamicImport(flags, className, method, recs(args))
              case ArrayLength(array) if noExtractYet =>
                ArrayLength(rec(array))
              case ArraySelect(array, index) if noExtractYet =>
                val newIndex = rec(index)
                ArraySelect(rec(array), newIndex)(arg.tpe)
              case RecordSelect(record, field) if noExtractYet =>
                RecordSelect(rec(record), field)(arg.tpe)

              case Transient(ZeroOf(runtimeClass)) =>
                Transient(ZeroOf(rec(runtimeClass)))
              case Transient(NumberOfLeadingZeroes(num)) =>
                Transient(NumberOfLeadingZeroes(rec(num)))
              case Transient(ObjectClassName(obj)) =>
                Transient(ObjectClassName(rec(obj)))

              case Transient(NativeArrayWrapper(elemClass, nativeArray)) if noExtractYet =>
                val newNativeArray = rec(nativeArray)
                val newElemClass = rec(elemClass)
                Transient(NativeArrayWrapper(newElemClass, newNativeArray)(arg.tpe))
              case Transient(ArrayToTypedArray(expr, primRef)) if noExtractYet =>
                Transient(ArrayToTypedArray(rec(expr), primRef))
              case Transient(TypedArrayToArray(expr, primRef)) if noExtractYet =>
                Transient(TypedArrayToArray(rec(expr), primRef))

              case If(cond, thenp, elsep)
                  if noExtractYet && isExpression(thenp) && isExpression(elsep) =>
                If(rec(cond), thenp, elsep)(arg.tpe)

              case _ =>
                val temp = newSyntheticVar()
                val computeTemp = pushLhsInto(
                    Lhs.VarDef(temp, arg.tpe, mutable = false), arg,
                    Set.empty)
                computeTemp +=: extractedStatements
                Transient(JSVarRef(temp, mutable = false)(arg.tpe))
            }
          }
        }

        def recs(args: List[Tree])(implicit env: Env): List[Tree] = {
          // This is a right-to-left map
          args.foldRight[List[Tree]](Nil) { (arg, acc) =>
            rec(arg) :: acc
          }
        }

        def recsOrSpread(args: List[TreeOrJSSpread])(
            implicit env: Env): List[TreeOrJSSpread] = {
          args.foldRight[List[TreeOrJSSpread]](Nil) { (arg, acc) =>
            val newArg = arg match {
              case JSSpread(items) => JSSpread(rec(items))(arg.pos)
              case arg: Tree       => rec(arg)
            }
            newArg :: acc
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
      unnest(List(arg)) { (newArgs, env) =>
        val newArg :: Nil = newArgs
        makeStat(newArg, env)
      }
    }

    /** Same as above, for two arguments */
    def unnest(arg1: Tree, arg2: Tree)(
        makeStat: (Tree, Tree, Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      unnest(List(arg1, arg2)) { (newArgs, env) =>
        val newArg1 :: newArg2 :: Nil = newArgs
        makeStat(newArg1, newArg2, env)
      }
    }

    /** Same as above, for 3 arguments */
    def unnest(arg1: Tree, arg2: Tree, arg3: Tree)(
        makeStat: (Tree, Tree, Tree, Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      unnest(List(arg1, arg2, arg3)) { (newArgs, env) =>
        val newArg1 :: newArg2 :: newArg3 :: Nil = newArgs
        makeStat(newArg1, newArg2, newArg3, env)
      }
    }

    /** Same as above, for 4 arguments */
    def unnest(arg1: Tree, arg2: Tree, arg3: Tree, arg4: Tree)(
        makeStat: (Tree, Tree, Tree, Tree, Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      unnest(List(arg1, arg2, arg3, arg4)) { (newArgs, env) =>
        val newArg1 :: newArg2 :: newArg3 :: newArg4 :: Nil = newArgs
        makeStat(newArg1, newArg2, newArg3, newArg4, env)
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
    def unnestJSObjectConstrFields(fields: List[(Tree, Tree)])(
        makeStat: (List[(Tree, Tree)], Env) => js.Tree)(
        implicit env: Env): js.Tree = {

      // Collect all the trees that need unnesting, in evaluation order
      val trees = fields.flatMap { field =>
        List(field._1, field._2)
      }

      unnest(trees) { (newTrees, env) =>
        // Re-decompose all the trees into pairs of (key, value)
        val newTreesIterator = newTrees.iterator
        val newFields = List.newBuilder[(Tree, Tree)]
        while (newTreesIterator.hasNext)
          newFields += ((newTreesIterator.next(), newTreesIterator.next()))

        makeStat(newFields.result(), env)
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

      def testJSArg(tree: TreeOrJSSpread): Boolean = tree match {
        case JSSpread(items) => es2015 && test(items)
        case tree: Tree      => test(tree)
      }

      def test(tree: Tree): Boolean = tree match {
        // Atomic expressions
        case _: Literal       => true
        case _: This          => true
        case _: JSNewTarget   => true
        case _: JSLinkingInfo => true

        // Vars (side-effect free, pure if immutable)
        case VarRef(name) =>
          allowUnpure || !env.isLocalMutable(name)
        case Transient(JSVarRef(_, mutable)) =>
          allowUnpure || !mutable

        // Fields may throw if qualifier is null but that is UB.
        case Select(qualifier, _, _) =>
          allowUnpure && test(qualifier)

        // Static fields are side-effect free
        case SelectStatic(_, _) =>
          allowUnpure

        // Division and modulo, preserve pureness unless they can divide by 0
        case BinaryOp(BinaryOp.Int_/ | BinaryOp.Int_%, lhs, rhs) if !allowSideEffects =>
          rhs match {
            case IntLiteral(r) if r != 0 => test(lhs)
            case _                       => false
          }
        case BinaryOp(BinaryOp.Long_/ | BinaryOp.Long_%, lhs, rhs) if !allowSideEffects =>
          rhs match {
            case LongLiteral(r) if r != 0L => test(lhs)
            case _                         => false
          }

        // Expressions preserving pureness
        case Block(trees)            => trees forall test
        case If(cond, thenp, elsep)  => test(cond) && test(thenp) && test(elsep)
        case BinaryOp(_, lhs, rhs)   => test(lhs) && test(rhs)
        case UnaryOp(_, lhs)         => test(lhs)
        case ArrayLength(array)      => test(array)
        case RecordSelect(record, _) => test(record)
        case IsInstanceOf(expr, _)   => test(expr)
        case IdentityHashCode(expr)  => test(expr)
        case GetClass(arg)           => test(arg) // may NPE but that is UB.

        // Transients preserving pureness
        case Transient(ZeroOf(runtimeClass)) =>
          test(runtimeClass) // may NPE but that is UB.
        case Transient(NumberOfLeadingZeroes(num)) =>
          test(num)
        case Transient(ObjectClassName(obj)) =>
          test(obj) // may NPE but that is UB.

        // Expressions preserving side-effect freedom
        case NewArray(tpe, lengths) =>
          allowUnpure && (lengths forall test)
        case ArrayValue(tpe, elems) =>
          allowUnpure && (elems forall test)
        case Clone(arg) =>
          allowUnpure && test(arg) // may NPE but that is UB.
        case JSArrayConstr(items) =>
          allowUnpure && (items.forall(testJSArg))
        case tree @ JSObjectConstr(items) =>
          allowUnpure &&
          !doesObjectConstrRequireDesugaring(tree) &&
          items.forall { item =>
            test(item._1) && test(item._2)
          }
        case Closure(arrow, captureParams, params, restParam, body, captureValues) =>
          allowUnpure && (captureValues forall test)

        // Transients preserving side-effect freedom
        case Transient(NativeArrayWrapper(elemClass, nativeArray)) =>
          allowUnpure && test(elemClass) && test(nativeArray) // may NPE but that is UB.
        case Transient(ArrayToTypedArray(expr, primRef)) =>
          allowUnpure && test(expr) // may NPE but that is UB.

        // Scala expressions that can always have side-effects
        case New(className, constr, args) =>
          allowSideEffects && (args forall test)
        case LoadModule(className) => // unfortunately
          allowSideEffects
        case Apply(_, receiver, method, args) =>
          allowSideEffects && test(receiver) && (args forall test)
        case ApplyStatically(_, receiver, className, method, args) =>
          allowSideEffects && test(receiver) && (args forall test)
        case ApplyStatic(_, className, method, args) =>
          allowSideEffects && (args forall test)
        case ApplyDynamicImport(_, _, _, args) =>
          allowSideEffects && args.forall(test)

        // Transients with side effects.
        case Transient(TypedArrayToArray(expr, primRef)) =>
          allowSideEffects && test(expr) // may TypeError

        // Array access can throw ArrayIndexOutOfBounds exception
        case ArraySelect(array, index) =>
          (allowSideEffects || semantics.arrayIndexOutOfBounds == Unchecked && allowUnpure) &&
          test(array) && test(index)

        // Casts
        case AsInstanceOf(expr, _) =>
          (allowSideEffects || semantics.asInstanceOfs == Unchecked) && test(expr)

        // JavaScript expressions that can always have side-effects
        case SelectJSNativeMember(_, _) =>
          allowSideEffects
        case JSNew(fun, args) =>
          allowSideEffects && test(fun) && (args.forall(testJSArg))
        case Transient(JSNewVararg(ctor, argArray)) =>
          allowSideEffects && test(ctor) && test(argArray)
        case JSPrivateSelect(qualifier, _, _) =>
          allowSideEffects && test(qualifier)
        case JSSelect(qualifier, item) =>
          allowSideEffects && test(qualifier) && test(item)
        case JSFunctionApply(fun, args) =>
          allowSideEffects && test(fun) && (args.forall(testJSArg))
        case JSMethodApply(receiver, method, args) =>
          allowSideEffects && test(receiver) && test(method) && (args.forall(testJSArg))
        case JSSuperSelect(superClass, qualifier, item) =>
          allowSideEffects && test(superClass) && test(qualifier) && test(item)
        case JSImportCall(arg) =>
          allowSideEffects && test(arg)
        case JSImportMeta() =>
          allowSideEffects
        case LoadJSModule(_) =>
          allowSideEffects
        case JSBinaryOp(_, lhs, rhs) =>
          allowSideEffects && test(lhs) && test(rhs)
        case JSUnaryOp(_, lhs) =>
          allowSideEffects && test(lhs)
        case JSGlobalRef(_) =>
          allowSideEffects
        case JSTypeOfGlobalRef(_) =>
          allowSideEffects
        case CreateJSClass(_, captureValues) =>
          allowSideEffects && captureValues.forall(test)

        /* LoadJSConstructor is pure only for non-native JS classes,
         * which do not have a native load spec. Note that this test makes
         * sense per se, as the actual desugaring of `LoadJSConstructor` is
         * based on the jsNativeLoadSpec of the class.
         */
        case LoadJSConstructor(className) =>
          allowUnpure || globalKnowledge.getJSNativeLoadSpec(className).isEmpty

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

    def doVarDef(ident: js.Ident, tpe: Type, mutable: Boolean, rhs: Tree)(
        implicit env: Env): js.Tree = {
      implicit val pos = rhs.pos
      tpe match {
        case RecordType(fields) =>
          val elems = extractRecordElems(rhs)
          js.Block(for {
            (RecordType.Field(fName, fOrigName, fTpe, fMutable),
                fRhs) <- fields zip elems
          } yield {
            doVarDef(makeRecordFieldIdent(ident, fName, fOrigName), fTpe,
                mutable || fMutable, fRhs)
          })

        case _ =>
          genLet(ident, mutable, transformExpr(rhs, tpe))
      }
    }

    def doEmptyVarDef(ident: js.Ident, tpe: Type)(
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
          val ident = (lhs: @unchecked) match {
            case VarRef(ident)                 => transformLocalVarIdent(ident)
            case Transient(JSVarRef(ident, _)) => ident
          }
          val elems = extractRecordElems(rhs)
          js.Block(for {
            (RecordType.Field(fName, fOrigName, fTpe, fMutable),
                fRhs) <- fields zip elems
          } yield {
            doAssign(
                Transient(JSVarRef(makeRecordFieldIdent(ident, fName, fOrigName),
                    mutable = true)(fTpe)),
                fRhs)
          })

        case _ =>
          val base = js.Assign(transformExpr(lhs, preserveChar = true),
              transformExpr(rhs, lhs.tpe))
          lhs match {
            case SelectStatic(className, FieldIdent(field))
                if moduleKind == ModuleKind.NoModule =>
              val mirrors =
                globalKnowledge.getStaticFieldMirrors(className, field)
              mirrors.foldLeft(base) { (prev, mirror) =>
                js.Assign(genGlobalVarRef(mirror), prev)
              }
            case _ =>
              base
          }
      }
    }

    private def extractRecordElems(recordTree: Tree)(
        implicit pos: Position, env: Env): List[Tree] = {

      val recordType = recordTree.tpe.asInstanceOf[RecordType]

      (recordTree: @unchecked) match {
        case RecordValue(_, elems) =>
          elems

        case VarRef(ident) =>
          val jsIdent = transformLocalVarIdent(ident)
          val mutable = env.isLocalMutable(ident)
          for (RecordType.Field(fName, fOrigName, fTpe, _) <- recordType.fields)
            yield Transient(JSVarRef(makeRecordFieldIdent(jsIdent, fName, fOrigName), mutable)(fTpe))

        case Transient(JSVarRef(ident, mutable)) =>
          for (RecordType.Field(fName, fOrigName, fTpe, _) <- recordType.fields)
            yield Transient(JSVarRef(makeRecordFieldIdent(ident, fName, fOrigName), mutable)(fTpe))
      }
    }

    /** Push an lhs into a (potentially complex) rhs */
    def pushLhsInto(lhs: Lhs, rhs: Tree, tailPosLabels: Set[LabelName])(
        implicit env: Env): js.Tree = {
      implicit val pos = rhs.pos

      /** Push the current lhs further into a deeper rhs. */
      @inline def redo(newRhs: Tree)(implicit env: Env) =
        pushLhsInto(lhs, newRhs, tailPosLabels)

      /** Extract a definition of the lhs if it is a VarDef, to avoid changing
       *  its scope.
       *  This only matters when we emit lets and consts.
       */
      def extractLet(inner: Lhs => js.Tree)(
          implicit env: Env): js.Tree = {
        if (useLets) {
          lhs match {
            case Lhs.VarDef(name, tpe, mutable) =>
              js.Block(
                  doEmptyVarDef(name, tpe),
                  inner(Lhs.Assign(Transient(JSVarRef(name, mutable)(tpe)))))
            case _ =>
              inner(lhs)
          }
        } else {
          inner(lhs)
        }
      }

      def doReturnToLabel(l: LabelIdent): js.Tree = {
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
        } else if (env.isDefaultContinueTarget(l.name)) {
          js.Block(body, js.Continue(None))
        } else {
          usedLabels += l.name
          val transformedLabel = Some(transformLabelIdent(l))
          val jump =
            if (env.isLabelTurnedIntoContinue(l.name)) js.Continue(transformedLabel)
            else js.Break(transformedLabel)
          js.Block(body, jump)
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
              else transformExpr(rhs, preserveChar = true)
            case Lhs.VarDef(name, tpe, mutable) =>
              doVarDef(name, tpe, mutable, rhs)
            case Lhs.Assign(lhs) =>
              doAssign(lhs, rhs)
            case Lhs.ReturnFromFunction =>
              js.Return(transformExpr(rhs, env.expectedReturnType))
            case Lhs.Return(l) =>
              doReturnToLabel(l)
            case Lhs.Throw =>
              js.Throw(transformExprNoChar(rhs))
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
                val varDef = doVarDef(temp, recTpe, mutable = false,
                    RecordValue(recTpe, newElems))
                val assign = doAssign(lhs,
                    Transient(JSVarRef(temp, mutable = false)(recTpe)))
                js.Block(varDef, assign)
              }

            case Lhs.Return(l) =>
              doReturnToLabel(l)

            case Lhs.ReturnFromFunction | Lhs.Throw =>
              throw new AssertionError("Cannot return or throw a record value.")
          }

        // Control flow constructs

        case Labeled(label, tpe, body) =>
          extractLet { newLhs =>
            val bodyEnv = env.withLabeledExprLHS(label, newLhs)
            val newBody =
              pushLhsInto(newLhs, body, tailPosLabels + label.name)(bodyEnv)
            if (usedLabels.contains(label.name))
              js.Labeled(transformLabelIdent(label), newBody)
            else
              newBody
          }

        case Return(expr, label) =>
          pushLhsInto(Lhs.Return(label), expr, tailPosLabels)

        case If(cond, thenp, elsep) =>
          unnest(cond) { (newCond, env0) =>
            implicit val env = env0
            extractLet { newLhs =>
              js.If(transformExprNoChar(newCond),
                  pushLhsInto(newLhs, thenp, tailPosLabels),
                  pushLhsInto(newLhs, elsep, tailPosLabels))
            }
          }

        case TryCatch(block, errVar, errVarOriginalName, handler) =>
          extractLet { newLhs =>
            val newBlock = pushLhsInto(newLhs, block, tailPosLabels)(env)
            val newHandler = pushLhsInto(newLhs, handler, tailPosLabels)(
                env.withDef(errVar, mutable = false))
            js.TryCatch(newBlock,
                transformLocalVarIdent(errVar, errVarOriginalName), newHandler)
          }

        case TryFinally(block, finalizer) =>
          extractLet { newLhs =>
            val newBlock = pushLhsInto(newLhs, block, tailPosLabels)
            val newFinalizer = transformStat(finalizer, Set.empty)
            js.TryFinally(newBlock, newFinalizer)
          }

        case Throw(expr) =>
          pushLhsInto(Lhs.Throw, expr, tailPosLabels)

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
                  newValues = values.map(transformExprNoChar(_))
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
              js.Switch(transformExpr(newSelector, preserveChar = true),
                  newCases, newDefault)
            }
          }

        // Scala expressions (if we reach here their arguments are not expressions)

        case New(className, ctor, args) =>
          unnest(args) { (newArgs, env) =>
            redo(New(className, ctor, newArgs))(env)
          }

        case Select(qualifier, className, item) =>
          unnest(qualifier) { (newQualifier, env) =>
            redo(Select(newQualifier, className, item)(rhs.tpe))(env)
          }

        case Apply(flags, receiver, method, args) =>
          unnest(receiver, args) { (newReceiver, newArgs, env) =>
            redo(Apply(flags, newReceiver, method, newArgs)(rhs.tpe))(env)
          }

        case ApplyStatically(flags, receiver, className, method, args) =>
          unnest(receiver, args) { (newReceiver, newArgs, env) =>
            redo(ApplyStatically(flags, newReceiver, className, method,
                newArgs)(rhs.tpe))(env)
          }

        case ApplyStatic(flags, className, method, args) =>
          unnest(args) { (newArgs, env) =>
            redo(ApplyStatic(flags, className, method, newArgs)(rhs.tpe))(env)
          }

        case ApplyDynamicImport(flags, className, method, args) =>
          unnest(args) { (newArgs, env) =>
            redo(ApplyDynamicImport(flags, className, method, newArgs))(env)
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

        case RecordSelect(record, field) =>
          unnest(record) { (newRecord, env) =>
            redo(RecordSelect(newRecord, field)(rhs.tpe))(env)
          }

        case IsInstanceOf(expr, testType) =>
          unnest(expr) { (newExpr, env) =>
            redo(IsInstanceOf(newExpr, testType))(env)
          }

        case AsInstanceOf(expr, tpe) =>
          unnest(expr) { (newExpr, env) =>
            redo(AsInstanceOf(newExpr, tpe))(env)
          }

        case GetClass(expr) =>
          unnest(expr) { (newExpr, env) =>
            redo(GetClass(newExpr))(env)
          }

        case Clone(expr) =>
          unnest(expr) { (newExpr, env) =>
            redo(Clone(newExpr))(env)
          }

        case IdentityHashCode(expr) =>
          unnest(expr) { (newExpr, env) =>
            redo(IdentityHashCode(newExpr))(env)
          }

        case Transient(ZeroOf(runtimeClass)) =>
          unnest(runtimeClass) { (newRuntimeClass, env) =>
            redo(Transient(ZeroOf(newRuntimeClass)))(env)
          }

        case Transient(NativeArrayWrapper(elemClass, nativeArray)) =>
          unnest(elemClass, nativeArray) { (newElemClass, newNativeArray, env) =>
            redo(Transient(NativeArrayWrapper(newElemClass, newNativeArray)(rhs.tpe)))(env)
          }

        case Transient(NumberOfLeadingZeroes(num)) =>
          unnest(num) { (newNum, env) =>
            redo(Transient(NumberOfLeadingZeroes(newNum)))(env)
          }

        case Transient(ObjectClassName(obj)) =>
          unnest(obj) { (newObj, env) =>
            redo(Transient(ObjectClassName(newObj)))(env)
          }

        case Transient(ArrayToTypedArray(expr, primRef)) =>
          unnest(expr) { (newExpr, env) =>
            redo(Transient(ArrayToTypedArray(newExpr, primRef)))(env)
          }

        case Transient(TypedArrayToArray(expr, primRef)) =>
          unnest(expr) { (newExpr, env) =>
            redo(Transient(TypedArrayToArray(newExpr, primRef)))(env)
          }

        // JavaScript expressions (if we reach here their arguments are not expressions)

        case JSNew(ctor, args) =>
          if (needsToTranslateAnySpread(args)) {
            redo {
              Transient(JSNewVararg(ctor, spreadToArgArray(args)))
            }
          } else {
            unnestOrSpread(ctor :: Nil, args) { (newCtor0, newArgs, env) =>
              val newCtor :: Nil = newCtor0
              redo(JSNew(newCtor, newArgs))(env)
            }
          }

        case Transient(JSNewVararg(ctor, argArray)) =>
          unnest(ctor, argArray) { (newCtor, newArgArray, env) =>
            redo(Transient(JSNewVararg(newCtor, newArgArray)))(env)
          }

        case JSFunctionApply(fun, args) =>
          if (needsToTranslateAnySpread(args)) {
            redo {
              JSMethodApply(fun, StringLiteral("apply"),
                  List(Undefined(), spreadToArgArray(args)))
            }
          } else {
            unnestOrSpread(fun :: Nil, args) { (newFun0, newArgs, env) =>
              val newFun :: Nil = newFun0
              redo(JSFunctionApply(newFun, newArgs))(env)
            }
          }

        case JSMethodApply(receiver, method, args) =>
          if (needsToTranslateAnySpread(args)) {
            withTempVar(receiver) { newReceiver =>
              redo {
                JSMethodApply(
                    JSSelect(newReceiver, method),
                    StringLiteral("apply"),
                    List(newReceiver, spreadToArgArray(args)))
              }
            }
          } else {
            unnestOrSpread(receiver :: method :: Nil, args) {
              (newReceiverAndMethod, newArgs, env) =>
                val newReceiver :: newMethod :: Nil = newReceiverAndMethod
                redo(JSMethodApply(newReceiver, newMethod, newArgs))(env)
            }
          }

        case JSSuperSelect(superClass, qualifier, item) =>
          unnest(superClass, qualifier, item) {
            (newSuperClass, newQualifier, newItem, env) =>
              redo(JSSuperSelect(newSuperClass, newQualifier, newItem))(env)
          }

        case JSSuperMethodCall(superClass, receiver, method, args) =>
          redo {
            JSMethodApply(
                JSSelect(
                    JSSelect(superClass, StringLiteral("prototype")),
                    method),
                StringLiteral("call"),
                receiver :: args)
          }

        case JSImportCall(arg) =>
          unnest(arg) { (newArg, env) =>
            redo(JSImportCall(newArg))(env)
          }

        case JSPrivateSelect(qualifier, className, field) =>
          unnest(qualifier) { (newQualifier, env) =>
            redo(JSPrivateSelect(newQualifier, className, field))(env)
          }

        case JSSelect(qualifier, item) =>
          unnest(qualifier, item) { (newQualifier, newItem, env) =>
            redo(JSSelect(newQualifier, newItem))(env)
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
          if (needsToTranslateAnySpread(items)) {
            redo {
              spreadToArgArray(items)
            }
          } else {
            unnestOrSpread(items) { (newItems, env) =>
              redo(JSArrayConstr(newItems))(env)
            }
          }

        case rhs @ JSObjectConstr(fields) =>
          if (doesObjectConstrRequireDesugaring(rhs)) {
            val objVarIdent = newSyntheticVar()

            def objVarRef(implicit pos: Position): Tree =
              Transient(JSVarRef(objVarIdent, mutable = false)(AnyType))

            val objVarDef =
              genLet(objVarIdent, mutable = false, js.ObjectConstr(Nil))

            val assignFields = for {
              (key, value) <- fields
            } yield {
              implicit val pos = value.pos
              Assign(JSSelect(objVarRef, key), value)
            }

            js.Block(
                objVarDef,
                redo {
                  Block(assignFields ::: objVarRef :: Nil)
                }
            )
          } else {
            unnestJSObjectConstrFields(fields) { (newFields, env) =>
              redo(JSObjectConstr(newFields))(env)
            }
          }

        // Closures

        case Closure(arrow, captureParams, params, restParam, body, captureValues) =>
          unnest(captureValues) { (newCaptureValues, env) =>
            redo(Closure(arrow, captureParams, params, restParam, body, newCaptureValues))(
                env)
          }

        case CreateJSClass(className, captureValues) =>
          unnest(captureValues) { (newCaptureValues, env) =>
            redo(CreateJSClass(className, newCaptureValues))(env)
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
                  _:StoreModule | Transient(_:SystemArrayCopy) =>
                transformStat(rhs, tailPosLabels)
              case _ =>
                throw new IllegalArgumentException(
                    "Illegal tree in JSDesugar.pushLhsInto():\n" +
                    "lhs = " + lhs + "\n" +
                    "rhs = " + rhs + " of class " + rhs.getClass)
            }
          } else {
            throw new IllegalArgumentException(
                "Illegal tree in JSDesugar.pushLhsInto():\n" +
                "lhs = " + lhs + "\n" +
                "rhs = " + rhs + " of class " + rhs.getClass)
          }
      })
    }

    private def needsToTranslateAnySpread(args: List[TreeOrJSSpread]): Boolean =
      !es2015 && args.exists(_.isInstanceOf[JSSpread])

    private def spreadToArgArray(args: List[TreeOrJSSpread])(
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
          case arg: Tree =>
            reversedPartUnderConstruction ::= arg
        }
      }
      closeReversedPartUnderConstruction()

      reversedParts match {
        case Nil        => JSArrayConstr(Nil)
        case List(part) => part
        case _          =>
          val partHead :: partTail = reversedParts.reverse
          JSMethodApply(partHead, StringLiteral("concat"), partTail)
      }
    }

    /** Tests whether a [[JSObjectConstr]] must be desugared. */
    private def doesObjectConstrRequireDesugaring(
        tree: JSObjectConstr): Boolean = {
      def computedNamesAllowed: Boolean =
        es2015

      def hasComputedName: Boolean =
        tree.fields.exists(!_._1.isInstanceOf[StringLiteral])

      def hasDuplicateNonComputedProp: Boolean = {
        val names = tree.fields.collect {
          case (StringLiteral(name), _) => name
        }
        names.toSet.size != names.size
      }

      (!computedNamesAllowed && hasComputedName) || hasDuplicateNonComputedProp
    }

    /** Evaluates `expr` and stores the result in a temp, then evaluates the
     *  result of `makeTree(temp)`.
     */
    private def withTempVar(expr: Tree)(makeTree: Tree => js.Tree)(
        implicit env: Env): js.Tree = {
      expr match {
        case VarRef(ident) if !env.isLocalMutable(ident) =>
          makeTree(expr)
        case Transient(JSVarRef(_, false)) =>
          makeTree(expr)
        case _ =>
          implicit val pos = expr.pos
          val temp = newSyntheticVar()
          val computeTemp = pushLhsInto(
              Lhs.VarDef(temp, expr.tpe, mutable = false), expr, Set.empty)
          js.Block(computeTemp,
              makeTree(Transient(JSVarRef(temp, mutable = false)(expr.tpe))))
      }
    }

    def transformJSArg(tree: TreeOrJSSpread)(implicit env: Env): js.Tree = {
      tree match {
        case JSSpread(items) =>
          assert(es2015)
          js.Spread(transformExprNoChar(items))(tree.pos)
        case tree: Tree =>
          transformExprNoChar(tree)
      }
    }

    def transformExprNoChar(tree: Tree)(implicit env: Env): js.Tree =
      transformExpr(tree, preserveChar = false)

    def transformExpr(tree: Tree, expectedType: Type)(
        implicit env: Env): js.Tree = {
      transformExpr(tree, preserveChar = expectedType == CharType)
    }

    def transformTypedArgs(methodName: MethodName, args: List[Tree])(
        implicit env: Env): List[js.Tree] = {
      if (args.forall(_.tpe != CharType)) {
        // Fast path
        args.map(transformExpr(_, preserveChar = true))
      } else {
        args.zip(methodName.paramTypeRefs).map {
          case (arg, CharRef) => transformExpr(arg, preserveChar = true)
          case (arg, _)       => transformExpr(arg, preserveChar = false)
        }
      }
    }

    /** Desugar an expression of the IR into JavaScript. */
    def transformExpr(tree: Tree, preserveChar: Boolean)(
        implicit env: Env): js.Tree = {

      import TreeDSL._

      implicit val pos = tree.pos

      def or0(tree: js.Tree): js.Tree =
        js.BinaryOp(JSBinaryOp.|, tree, js.IntLiteral(0))

      def bigIntShiftRhs(tree: js.Tree): js.Tree = {
        tree match {
          case js.IntLiteral(v) =>
            js.BigIntLiteral(v & 63)
          case _ =>
            js.Apply(genGlobalVarRef("BigInt"),
                List(js.BinaryOp(JSBinaryOp.&, tree, js.IntLiteral(63))))
        }
      }

      val baseResult: js.Tree = tree match {
        // Control flow constructs

        case Block(stats :+ expr) =>
          val (newStats, newEnv) = transformBlockStats(stats)
          js.Block(newStats :+ transformExpr(expr, tree.tpe)(newEnv))

        // Note that these work even if thenp/elsep is not a BooleanType
        case If(cond, BooleanLiteral(true), elsep) =>
          js.BinaryOp(JSBinaryOp.||, transformExprNoChar(cond),
              transformExpr(elsep, tree.tpe))
        case If(cond, thenp, BooleanLiteral(false)) =>
          js.BinaryOp(JSBinaryOp.&&, transformExprNoChar(cond),
              transformExpr(thenp, tree.tpe))

        case If(cond, thenp, elsep) =>
          js.If(transformExprNoChar(cond), transformExpr(thenp, tree.tpe),
              transformExpr(elsep, tree.tpe))

        // Scala expressions

        case New(className, ctor, args) =>
          val newArgs = transformTypedArgs(ctor.name, args)
          genScalaClassNew(className, ctor.name, newArgs: _*)

        case LoadModule(className) =>
          genLoadModule(className)

        case Select(qualifier, className, field) =>
          genSelect(transformExprNoChar(qualifier), className, field)

        case SelectStatic(className, item) =>
          globalVar("t", (className, item.name))

        case SelectJSNativeMember(className, member) =>
          val jsNativeLoadSpec =
            globalKnowledge.getJSNativeLoadSpec(className, member.name)
          extractWithGlobals(genLoadJSFromSpec(
              jsNativeLoadSpec, keepOnlyDangerousVarNames = false))

        case Apply(_, receiver, method, args) =>
          val methodName = method.name
          val newReceiver = transformExprNoChar(receiver)
          val newArgs = transformTypedArgs(method.name, args)

          def genNormalApply(): js.Tree =
            js.Apply(newReceiver DOT transformMethodIdent(method), newArgs)

          def genDispatchApply(): js.Tree =
            genCallHelper("dp_" + genName(methodName), newReceiver :: newArgs: _*)

          def genHijackedMethodApply(className: ClassName): js.Tree =
            genApplyStaticLike("f", className, method, newReceiver :: newArgs)

          if (isMaybeHijackedClass(receiver.tpe) &&
              !methodName.isReflectiveProxy) {
            receiver.tpe match {
              case AnyType =>
                genDispatchApply()

              case LongType | ClassType(BoxedLongClass) if !useBigIntForLongs =>
                // All methods of java.lang.Long are also in RuntimeLong
                genNormalApply()

              case _ if hijackedMethodsInheritedFromObject.contains(methodName) =>
                /* Methods inherited from j.l.Object do not have a dedicated
                 * hijacked method that we can call, even when we know the
                 * precise type of the receiver. Therefore, we always have to
                 * go through the dispatcher in those cases.
                 *
                 * Note that when the optimizer is enabled, if the receiver
                 * had a precise type, the method would have been inlined
                 * anyway (because all the affected methods are @inline).
                 * Therefore this roundabout of dealing with this does not
                 * prevent any optimization.
                 */
                genDispatchApply()

              case ClassType(className) if !HijackedClasses.contains(className) =>
                /* This is a strict ancestor of a hijacked class. We need to
                 * use the dispatcher available in the helper method.
                 */
                genDispatchApply()

              case tpe =>
                /* This is a concrete hijacked class or its corresponding
                 * primitive type. Directly call the hijacked method. Note that
                 * there might not even be a dispatcher for this method, so
                 * this is important.
                 */
                genHijackedMethodApply(typeToBoxedHijackedClass(tpe))
            }
          } else {
            genNormalApply()
          }

        case ApplyStatically(flags, receiver, className, method, args) =>
          val newReceiver = transformExprNoChar(receiver)
          val newArgs = transformTypedArgs(method.name, args)
          val transformedArgs = newReceiver :: newArgs

          if (flags.isConstructor) {
            genApplyStaticLike("ct", className, method, transformedArgs)
          } else if (flags.isPrivate) {
            genApplyStaticLike("p", className, method, transformedArgs)
          } else if (globalKnowledge.isInterface(className)) {
            genApplyStaticLike("f", className, method, transformedArgs)
          } else {
            val fun =
              globalVar("c", className).prototype DOT transformMethodIdent(method)
            js.Apply(fun DOT "call", transformedArgs)
          }

        case ApplyStatic(flags, className, method, args) =>
          genApplyStaticLike(
              if (flags.isPrivate) "ps" else "s",
              className,
              method,
              transformTypedArgs(method.name, args))

        case ApplyDynamicImport(flags, className, method, args) =>
          // Protect the args by an IIFE to avoid bad loop captures (see #4385).
          val captureParams = args.map(_ => js.ParamDef(newSyntheticVar()))

          val innerCall = extractWithGlobals {
            withDynamicGlobalVar("s", (className, method.name)) { v =>
              js.Apply(v, captureParams.map(_.ref))
            }
          }

          if (captureParams.isEmpty) {
            innerCall
          } else {
            val captures =
              captureParams.zip(transformTypedArgs(method.name, args))

            genIIFE(captures, js.Return(innerCall))
          }

        case UnaryOp(op, lhs) =>
          import UnaryOp._
          val newLhs = transformExpr(lhs, preserveChar = op == CharToInt)
          (op: @switch) match {
            case Boolean_! => js.UnaryOp(JSUnaryOp.!, newLhs)

            // Widening conversions
            case CharToInt | ByteToInt | ShortToInt | IntToDouble |
                FloatToDouble =>
              newLhs
            case IntToLong =>
              if (useBigIntForLongs)
                js.Apply(genGlobalVarRef("BigInt"), List(newLhs))
              else
                genLongModuleApply(LongImpl.fromInt, newLhs)

            // Narrowing conversions
            case IntToChar =>
              js.BinaryOp(JSBinaryOp.&, js.IntLiteral(0xffff), newLhs)
            case IntToByte =>
              js.BinaryOp(JSBinaryOp.>>,
                  js.BinaryOp(JSBinaryOp.<<, newLhs, js.IntLiteral(24)),
                  js.IntLiteral(24))
            case IntToShort =>
              js.BinaryOp(JSBinaryOp.>>,
                  js.BinaryOp(JSBinaryOp.<<, newLhs, js.IntLiteral(16)),
                  js.IntLiteral(16))
            case LongToInt =>
              if (useBigIntForLongs)
                js.Apply(genGlobalVarRef("Number"), List(wrapBigInt32(newLhs)))
              else
                genLongMethodApply(newLhs, LongImpl.toInt)
            case DoubleToInt =>
              genCallHelper("doubleToInt", newLhs)
            case DoubleToFloat =>
              genFround(newLhs)

            // Long <-> Double (neither widening nor narrowing)
            case LongToDouble =>
              if (useBigIntForLongs)
                js.Apply(genGlobalVarRef("Number"), List(newLhs))
              else
                genLongMethodApply(newLhs, LongImpl.toDouble)
            case DoubleToLong =>
              if (useBigIntForLongs)
                genCallHelper("doubleToLong", newLhs)
              else
                genLongModuleApply(LongImpl.fromDouble, newLhs)

            // Long -> Float (neither widening nor narrowing)
            case LongToFloat =>
              if (useBigIntForLongs)
                genCallHelper("longToFloat", newLhs)
              else
                genLongMethodApply(newLhs, LongImpl.toFloat)
          }

        case BinaryOp(op, lhs, rhs) =>
          import BinaryOp._
          val newLhs = transformExprNoChar(lhs)
          val newRhs = transformExprNoChar(rhs)

          (op: @switch) match {
            case === | !== =>
              /** Tests whether an operand receives exemption from the
               *  `Object.is` treatment.
               *
               *  If either operand receives an exemption, we use `===`
               *  instead.
               */
              def receivesExemption(tree: js.Tree): Boolean = tree match {
                case _:js.Undefined | _:js.Null =>
                  /* An `undefined` operand happens a lot for default
                   * parameters in exported methods. Because exported methods
                   * are not optimized, they survive until here.
                   *
                   * A `null` operand often happens in the constructor of inner
                   * JS classes, which are not optimized either.
                   */
                  true
                case _:js.This =>
                  /* Due to how hijacked classes are encoded in JS, we know
                   * that in `java.lang.Object` itself, `this` can never be a
                   * primitive. It will always be a proper Scala.js object.
                   *
                   * Exempting `this` in `java.lang.Object` is important so
                   * that the body of `Object.equals__O__Z` can be compiled as
                   * `this === that` instead of `Object.is(this, that)`.
                   *
                   * This is something that the optimizer is not supposed to be
                   * able to do, since it doesn't know how hijacked classes are
                   * encoded.
                   */
                  env.enclosingClassName.exists(_ == ObjectClass)
                case _ =>
                  false
              }
              if (receivesExemption(newLhs) || receivesExemption(newRhs)) {
                js.BinaryOp(if (op == ===) JSBinaryOp.=== else JSBinaryOp.!==,
                    newLhs, newRhs)
              } else {
                val objectIs =
                  if (!es2015) globalVar("is", CoreVar)
                  else genIdentBracketSelect(genGlobalVarRef("Object"), "is")
                val objectIsCall = js.Apply(objectIs, newLhs :: newRhs :: Nil)
                if (op == ===) objectIsCall
                else js.UnaryOp(JSUnaryOp.!, objectIsCall)
              }

            case Int_== | Double_== | Boolean_== =>
              js.BinaryOp(JSBinaryOp.===, newLhs, newRhs)
            case Int_!= | Double_!= | Boolean_!= =>
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
            case Int_/ =>
              rhs match {
                case IntLiteral(r) if r != 0 =>
                  or0(js.BinaryOp(JSBinaryOp./, newLhs, newRhs))
                case _ =>
                  genCallHelper("intDiv", newLhs, newRhs)
              }
            case Int_% =>
              rhs match {
                case IntLiteral(r) if r != 0 =>
                  or0(js.BinaryOp(JSBinaryOp.%, newLhs, newRhs))
                case _ =>
                  genCallHelper("intMod", newLhs, newRhs)
              }

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

            case Int_<  => js.BinaryOp(JSBinaryOp.<, newLhs, newRhs)
            case Int_<= => js.BinaryOp(JSBinaryOp.<=, newLhs, newRhs)
            case Int_>  => js.BinaryOp(JSBinaryOp.>, newLhs, newRhs)
            case Int_>= => js.BinaryOp(JSBinaryOp.>=, newLhs, newRhs)

            case Long_+ =>
              if (useBigIntForLongs)
                wrapBigInt64(js.BinaryOp(JSBinaryOp.+, newLhs, newRhs))
              else
                genLongMethodApply(newLhs, LongImpl.+, newRhs)
            case Long_- =>
              lhs match {
                case LongLiteral(0L) =>
                  if (useBigIntForLongs)
                    wrapBigInt64(js.UnaryOp(JSUnaryOp.-, newRhs))
                  else
                    genLongMethodApply(newRhs, LongImpl.UNARY_-)
                case _ =>
                  if (useBigIntForLongs)
                    wrapBigInt64(js.BinaryOp(JSBinaryOp.-, newLhs, newRhs))
                  else
                    genLongMethodApply(newLhs, LongImpl.-, newRhs)
              }
            case Long_* =>
              if (useBigIntForLongs)
                wrapBigInt64(js.BinaryOp(JSBinaryOp.*, newLhs, newRhs))
              else
                genLongMethodApply(newLhs, LongImpl.*, newRhs)
            case Long_/ =>
              if (useBigIntForLongs) {
                rhs match {
                  case LongLiteral(r) if r != 0L =>
                    wrapBigInt64(js.BinaryOp(JSBinaryOp./, newLhs, newRhs))
                  case _ =>
                    genCallHelper("longDiv", newLhs, newRhs)
                }
              } else {
                genLongMethodApply(newLhs, LongImpl./, newRhs)
              }
            case Long_% =>
              if (useBigIntForLongs) {
                rhs match {
                  case LongLiteral(r) if r != 0L =>
                    wrapBigInt64(js.BinaryOp(JSBinaryOp.%, newLhs, newRhs))
                  case _ =>
                    genCallHelper("longMod", newLhs, newRhs)
                }
              } else {
                genLongMethodApply(newLhs, LongImpl.%, newRhs)
              }

            case Long_| =>
              if (useBigIntForLongs)
                wrapBigInt64(js.BinaryOp(JSBinaryOp.|, newLhs, newRhs))
              else
                genLongMethodApply(newLhs, LongImpl.|, newRhs)
            case Long_& =>
              if (useBigIntForLongs)
                wrapBigInt64(js.BinaryOp(JSBinaryOp.&, newLhs, newRhs))
              else
                genLongMethodApply(newLhs, LongImpl.&, newRhs)
            case Long_^ =>
              lhs match {
                case LongLiteral(-1L) =>
                  if (useBigIntForLongs)
                    wrapBigInt64(js.UnaryOp(JSUnaryOp.~, newRhs))
                  else
                    genLongMethodApply(newRhs, LongImpl.UNARY_~)
                case _ =>
                  if (useBigIntForLongs)
                    wrapBigInt64(js.BinaryOp(JSBinaryOp.^, newLhs, newRhs))
                  else
                    genLongMethodApply(newLhs, LongImpl.^, newRhs)
              }
            case Long_<< =>
              if (useBigIntForLongs)
                wrapBigInt64(js.BinaryOp(JSBinaryOp.<<, newLhs, bigIntShiftRhs(newRhs)))
              else
                genLongMethodApply(newLhs, LongImpl.<<, newRhs)
            case Long_>>> =>
              if (useBigIntForLongs)
                wrapBigInt64(js.BinaryOp(JSBinaryOp.>>, wrapBigIntU64(newLhs), bigIntShiftRhs(newRhs)))
              else
                genLongMethodApply(newLhs, LongImpl.>>>, newRhs)
            case Long_>> =>
              if (useBigIntForLongs)
                wrapBigInt64(js.BinaryOp(JSBinaryOp.>>, newLhs, bigIntShiftRhs(newRhs)))
              else
                genLongMethodApply(newLhs, LongImpl.>>, newRhs)

            case Long_== =>
              if (useBigIntForLongs)
                js.BinaryOp(JSBinaryOp.===, newLhs, newRhs)
              else
                genLongMethodApply(newLhs, LongImpl.===, newRhs)
            case Long_!= =>
              if (useBigIntForLongs)
                js.BinaryOp(JSBinaryOp.!==, newLhs, newRhs)
              else
                genLongMethodApply(newLhs, LongImpl.!==, newRhs)
            case Long_< =>
              if (useBigIntForLongs)
                js.BinaryOp(JSBinaryOp.<, newLhs, newRhs)
              else
                genLongMethodApply(newLhs, LongImpl.<, newRhs)
            case Long_<= =>
              if (useBigIntForLongs)
                js.BinaryOp(JSBinaryOp.<=, newLhs, newRhs)
              else
                genLongMethodApply(newLhs, LongImpl.<=, newRhs)
            case Long_> =>
              if (useBigIntForLongs)
                js.BinaryOp(JSBinaryOp.>, newLhs, newRhs)
              else
                genLongMethodApply(newLhs, LongImpl.>, newRhs)
            case Long_>= =>
              if (useBigIntForLongs)
                js.BinaryOp(JSBinaryOp.>=, newLhs, newRhs)
              else
                genLongMethodApply(newLhs, LongImpl.>=, newRhs)

            case Float_+ => genFround(js.BinaryOp(JSBinaryOp.+, newLhs, newRhs))
            case Float_- => genFround(js.BinaryOp(JSBinaryOp.-, newLhs, newRhs))
            case Float_* =>
              genFround(lhs match {
                case FloatLiteral(-1.0f) => js.UnaryOp(JSUnaryOp.-, newRhs)
                case _                   => js.BinaryOp(JSBinaryOp.*, newLhs, newRhs)
              })
            case Float_/ => genFround(js.BinaryOp(JSBinaryOp./, newLhs, newRhs))
            case Float_% => genFround(js.BinaryOp(JSBinaryOp.%, newLhs, newRhs))

            case Double_+ => js.BinaryOp(JSBinaryOp.+, newLhs, newRhs)
            case Double_- => js.BinaryOp(JSBinaryOp.-, newLhs, newRhs)
            case Double_* =>
              lhs match {
                case DoubleLiteral(-1.0) => js.UnaryOp(JSUnaryOp.-, newRhs)
                case _                   => js.BinaryOp(JSBinaryOp.*, newLhs, newRhs)
              }
            case Double_/ => js.BinaryOp(JSBinaryOp./, newLhs, newRhs)
            case Double_% => js.BinaryOp(JSBinaryOp.%, newLhs, newRhs)

            case Double_<  => js.BinaryOp(JSBinaryOp.<, newLhs, newRhs)
            case Double_<= => js.BinaryOp(JSBinaryOp.<=, newLhs, newRhs)
            case Double_>  => js.BinaryOp(JSBinaryOp.>, newLhs, newRhs)
            case Double_>= => js.BinaryOp(JSBinaryOp.>=, newLhs, newRhs)

            case Boolean_| => !(!js.BinaryOp(JSBinaryOp.|, newLhs, newRhs))
            case Boolean_& => !(!js.BinaryOp(JSBinaryOp.&, newLhs, newRhs))
          }

        case NewArray(typeRef, lengths) =>
          genNewArray(typeRef, lengths.map(transformExprNoChar))

        case ArrayValue(typeRef, elems) =>
          val preserveChar = typeRef match {
            case ArrayTypeRef(CharRef, 1) => true
            case _                        => false
          }
          extractWithGlobals(
              genArrayValue(typeRef, elems.map(transformExpr(_, preserveChar))))

        case ArrayLength(array) =>
          genIdentBracketSelect(js.DotSelect(transformExprNoChar(array),
              js.Ident("u")), "length")

        case ArraySelect(array, index) =>
          val newArray = transformExprNoChar(array)
          val newIndex = transformExprNoChar(index)
          semantics.arrayIndexOutOfBounds match {
            case CheckedBehavior.Compliant | CheckedBehavior.Fatal =>
              js.Apply(js.DotSelect(newArray, js.Ident("get")), List(newIndex))
            case CheckedBehavior.Unchecked =>
              js.BracketSelect(js.DotSelect(newArray, js.Ident("u")), newIndex)
          }

        case tree: RecordSelect =>
          js.VarRef(makeRecordFieldIdentForVarRef(tree))

        case IsInstanceOf(expr, testType) =>
          genIsInstanceOf(transformExprNoChar(expr), testType)

        case AsInstanceOf(expr, tpe) =>
          genAsInstanceOf(transformExprNoChar(expr), tpe)

        case GetClass(expr) =>
          genCallHelper("objectGetClass", transformExprNoChar(expr))

        case Clone(expr) =>
          val newExpr = transformExprNoChar(expr)
          expr.tpe match {
            /* If the argument is known to be an array, directly call its
             * `clone__O` method.
             * This happens all the time when calling `clone()` on an array,
             * since the optimizer will inline `java.lang.Object.clone()` in
             * those cases, leaving a `Clone()` node an array.
             */
            case _: ArrayType =>
              js.Apply(newExpr DOT genName(cloneMethodName), Nil)

            /* Otherwise, if it might be an array, use the full dispatcher.
             * In theory, only the `CloneableClass` case is required, since
             * `Clone` only accepts values of type `Cloneable`. However, since
             * the inliner does not always refine the type of receivers, we
             * also account for other supertypes of array types. There is a
             * similar issue for CharSequenceClass in `Apply` nodes.
             *
             * In practice, this only happens in the (non-inlined) definition
             * of `java.lang.Object.clone()` itself, since everywhere else it
             * is inlined in contexts where the receiver has a more precise
             * type.
             */
            case ClassType(CloneableClass) | ClassType(SerializableClass) |
                ClassType(ObjectClass) | AnyType =>
              genCallHelper("objectOrArrayClone", newExpr)

            // Otherwise, it is known not to be an array.
            case _ =>
              genCallHelper("objectClone", newExpr)
          }

        case IdentityHashCode(expr) =>
          genCallHelper("systemIdentityHashCode", transformExprNoChar(expr))

        // Transients

        case Transient(ZeroOf(runtimeClass)) =>
          js.DotSelect(
              js.DotSelect(transformExprNoChar(runtimeClass), js.Ident("jl_Class__f_data")),
              js.Ident("zero"))

        case Transient(NativeArrayWrapper(elemClass, nativeArray)) =>
          val newNativeArray = transformExprNoChar(nativeArray)
          elemClass match {
            case ClassOf(elemTypeRef) =>
              val arrayTypeRef = ArrayTypeRef.of(elemTypeRef)
              extractWithGlobals(
                  genNativeArrayWrapper(arrayTypeRef, newNativeArray))
            case _ =>
              val elemClassData = js.DotSelect(
                  transformExprNoChar(elemClass),
                  js.Ident("jl_Class__f_data"))
              val arrayClassData = js.Apply(
                  js.DotSelect(elemClassData, js.Ident("getArrayOf")), Nil)
              js.Apply(arrayClassData DOT "wrapArray", newNativeArray :: Nil)
          }

        case Transient(NumberOfLeadingZeroes(num)) =>
          genCallHelper("clz32", transformExprNoChar(num))

        case Transient(ObjectClassName(obj)) =>
          genCallHelper("objectClassName", transformExprNoChar(obj))

        case Transient(ArrayToTypedArray(expr, primRef)) =>
          val value = transformExprNoChar(expr)

          if (es2015) {
            js.Apply(genIdentBracketSelect(value.u, "slice"), Nil)
          } else {
            val typedArrayClass = extractWithGlobals(typedArrayRef(primRef).get)
            js.New(typedArrayClass, value.u :: Nil)
          }

        case Transient(TypedArrayToArray(expr, primRef)) =>
          val value = transformExprNoChar(expr)

          val arrayValue = if (es2015) {
            js.Apply(genIdentBracketSelect(value, "slice"), Nil)
          } else {
            /* Array.prototype.slice.call(value)
             *
             * This works because:
             * - If the `this` value of `slice` is not a proper `Array`, the
             *   result will be created through `ArrayCreate` without explicit
             *   prototype, which creates a new proper `Array`.
             * - To know what elements to copy, `slice` does not check that its
             *   `this` value is a proper `Array`. Instead, it simply assumes
             *   that it is an "Array-like", and reads the `"length"` property
             *   as well as indexed properties. Both of those work on a typed
             *   array.
             *
             * Reference:
             * http://www.ecma-international.org/ecma-262/6.0/#sec-array.prototype.slice
             * (also follow the link for `ArraySpeciesCreate`)
             */
            js.Apply(genIdentBracketSelect(
                genIdentBracketSelect(genGlobalVarRef("Array").prototype, "slice"), "call"),
                value :: Nil)
          }
          js.New(genArrayConstrOf(ArrayTypeRef(primRef, 1)), arrayValue :: Nil)

        // JavaScript expressions

        case JSNew(constr, args) =>
          js.New(transformExprNoChar(constr), args.map(transformJSArg))

        case Transient(JSNewVararg(constr, argsArray)) =>
          assert(!es2015, s"generated a JSNewVargs with ES 2015+ at ${tree.pos}")
          genCallHelper("newJSObjectWithVarargs",
              transformExprNoChar(constr), transformExprNoChar(argsArray))

        case JSPrivateSelect(qualifier, className, field) =>
          genJSPrivateSelect(transformExprNoChar(qualifier), className, field)

        case JSSelect(qualifier, item) =>
          genBracketSelect(transformExprNoChar(qualifier),
              transformExprNoChar(item))

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
           *
           * A bare identifier `eval` also need to be protected in the same
           * way, because calling a bare `eval` executes the code in the
           * current lexical scope, as opposed to the global scope.
           */
          val transformedFun = transformExprNoChar(fun)
          val protectedFun = transformedFun match {
            case _:js.DotSelect | _:js.BracketSelect |
                js.VarRef(js.Ident("eval", _)) =>
              js.Block(js.IntLiteral(0), transformedFun)
            case _ =>
              transformedFun
          }
          js.Apply(protectedFun, args.map(transformJSArg))

        case JSMethodApply(receiver, method, args) =>
          js.Apply(genBracketSelect(transformExprNoChar(receiver),
              transformExprNoChar(method)), args.map(transformJSArg))

        case JSSuperSelect(superClass, qualifier, item) =>
          genCallHelper("superGet", transformExprNoChar(superClass),
              transformExprNoChar(qualifier), transformExprNoChar(item))

        case JSImportCall(arg) =>
          js.ImportCall(transformExprNoChar(arg))

        case JSNewTarget() =>
          js.NewTarget()

        case JSImportMeta() =>
          js.ImportMeta()

        case LoadJSConstructor(className) =>
          extractWithGlobals(genJSClassConstructor(className))

        case LoadJSModule(className) =>
          globalKnowledge.getJSNativeLoadSpec(className) match {
            case None =>
              // this is a non-native JS module class
              genLoadModule(className)

            case Some(spec) =>
              extractWithGlobals(
                  genLoadJSFromSpec(spec, keepOnlyDangerousVarNames = false))
          }

        case JSUnaryOp(op, lhs) =>
          val transformedLhs = transformExprNoChar(lhs)
          val protectedLhs = if (op == JSUnaryOp.typeof && lhs.isInstanceOf[JSGlobalRef]) {
            /* #3822 We protect the argument so that it throws a ReferenceError
             * if the global variable is not defined at all, as specified.
             */
            js.Block(js.IntLiteral(0), transformedLhs)
          } else {
            transformedLhs
          }
          js.UnaryOp(op, protectedLhs)

        case JSBinaryOp(op, lhs, rhs) =>
          js.BinaryOp(op, transformExprNoChar(lhs), transformExprNoChar(rhs))

        case JSArrayConstr(items) =>
          js.ArrayConstr(items.map(transformJSArg))

        case JSObjectConstr(fields) =>
          js.ObjectConstr(fields.map { field =>
            val key = field._1
            implicit val pos = key.pos
            val newKey = key match {
              case StringLiteral(s) => js.StringLiteral(s)
              case _                => js.ComputedName(transformExprNoChar(key))
            }
            (newKey, transformExprNoChar(field._2))
          })

        case JSGlobalRef(name) =>
          js.VarRef(transformGlobalVarIdent(name))

        case JSTypeOfGlobalRef(globalRef) =>
          js.UnaryOp(JSUnaryOp.typeof, transformExprNoChar(globalRef))

        case JSLinkingInfo() =>
          globalVar("linkingInfo", CoreVar)

        // Literals

        case Undefined()            => js.Undefined()
        case Null()                 => js.Null()
        case BooleanLiteral(value)  => js.BooleanLiteral(value)
        case CharLiteral(value)     => js.IntLiteral(value.toInt)
        case ByteLiteral(value)     => js.IntLiteral(value.toInt)
        case ShortLiteral(value)    => js.IntLiteral(value.toInt)
        case IntLiteral(value)      => js.IntLiteral(value)
        case FloatLiteral(value)    => js.DoubleLiteral(value.toDouble)
        case DoubleLiteral(value)   => js.DoubleLiteral(value)
        case StringLiteral(value)   => js.StringLiteral(value)

        case LongLiteral(0L) =>
          genLongZero()
        case LongLiteral(value) =>
          if (useBigIntForLongs) {
            js.BigIntLiteral(value)
          } else {
            val (lo, hi) = LongImpl.extractParts(value)
            genScalaClassNew(
                LongImpl.RuntimeLongClass, LongImpl.initFromParts,
                js.IntLiteral(lo), js.IntLiteral(hi))
          }

        case ClassOf(typeRef) =>
          genClassOf(typeRef)

        // Atomic expressions

        case VarRef(name) =>
          if (env.isLocalVar(name))
            js.VarRef(transformLocalVarIdent(name))
          else
            fileLevelVar("cc", genName(name.name))

        case Transient(JSVarRef(name, _)) =>
          js.VarRef(name)

        case This() =>
          env.thisIdent.fold[js.Tree] {
            js.This()
          } { ident =>
            js.VarRef(ident)
          }

        case Closure(arrow, captureParams, params, restParam, body, captureValues) =>
          val innerFunction = {
            desugarToFunctionInternal(arrow, params, restParam, body, isStat = false,
                Env.empty(AnyType).withParams(captureParams ++ params ++ restParam))
          }

          val captures = for {
            (param, value) <- captureParams.zip(captureValues)
          } yield {
            (transformParamDef(param), transformExpr(value, param.ptpe))
          }

          if (captures.isEmpty) {
            innerFunction
          } else {
            genIIFE(captures, js.Return(innerFunction))
          }

        case CreateJSClass(className, captureValues) =>
          val transformedArgs = if (captureValues.forall(_.tpe != CharType)) {
            // Fast path
            captureValues.map(transformExpr(_, preserveChar = true))
          } else {
            val expectedTypes =
              globalKnowledge.getJSClassCaptureTypes(className).get
            for ((value, expectedType) <- captureValues.zip(expectedTypes))
              yield transformExpr(value, expectedType)
          }
          js.Apply(globalVar("a", className), transformedArgs)

        // Invalid trees

        case _ =>
          throw new IllegalArgumentException(
              "Invalid tree in JSDesugar.transformExpr() of class " +
              tree.getClass)
      }

      if (preserveChar || tree.tpe != CharType)
        baseResult
      else
        genCallHelper("bC", baseResult)
    }

    def isMaybeHijackedClass(tpe: Type): Boolean = tpe match {
      case ClassType(className) =>
        HijackedClasses.contains(className) ||
        className != ObjectClass && globalKnowledge.isAncestorOfHijackedClass(className)

      case AnyType | UndefType | BooleanType | CharType | ByteType | ShortType |
          IntType | LongType | FloatType | DoubleType | StringType =>
        true
      case _ =>
        false
    }

    def typeToBoxedHijackedClass(tpe: Type): ClassName = (tpe: @unchecked) match {
      case ClassType(className) => className
      case AnyType              => ObjectClass
      case UndefType            => BoxedUnitClass
      case BooleanType          => BoxedBooleanClass
      case CharType             => BoxedCharacterClass
      case ByteType             => BoxedByteClass
      case ShortType            => BoxedShortClass
      case IntType              => BoxedIntegerClass
      case LongType             => BoxedLongClass
      case FloatType            => BoxedFloatClass
      case DoubleType           => BoxedDoubleClass
      case StringType           => BoxedStringClass
    }

    /* Ideally, we should dynamically figure out this set. We should test
     * whether a particular method is defined in the receiver's hijacked
     * class: if not, it must be inherited. However, this would require
     * additional global knowledge for no practical reason.
     */
    val hijackedMethodsInheritedFromObject: Set[MethodName] = Set(
        getClassMethodName,
        cloneMethodName,
        MethodName("finalize", Nil, VoidRef),
        MethodName("notify", Nil, VoidRef),
        MethodName("notifyAll", Nil, VoidRef)
    )

    private def transformParamDef(paramDef: ParamDef): js.ParamDef =
      js.ParamDef(transformLocalVarIdent(paramDef.name, paramDef.originalName))(paramDef.pos)

    private def transformLabelIdent(ident: LabelIdent): js.Ident =
      js.Ident(genName(ident.name))(ident.pos)

    private def transformMethodIdent(ident: MethodIdent): js.Ident =
      js.Ident(genName(ident.name))(ident.pos)

    private def transformLocalVarIdent(ident: LocalIdent): js.Ident =
      js.Ident(transformLocalName(ident.name))(ident.pos)

    private def transformLocalVarIdent(ident: LocalIdent,
        originalName: OriginalName): js.Ident = {
      val jsName = transformLocalName(ident.name)
      js.Ident(jsName, genOriginalName(ident.name, originalName, jsName))(
          ident.pos)
    }

    private def transformGlobalVarIdent(name: String)(
        implicit pos: Position): js.Ident = {
      referenceGlobalName(name)
      js.Ident(name)
    }

    private def genGlobalVarRef(name: String)(
        implicit pos: Position): js.VarRef = {
      js.VarRef(transformGlobalVarIdent(name))
    }

    /* In FunctionEmitter, we must always keep all global var names, not only
     * dangerous ones. This helper makes it less annoying.
     */
    private def genJSClassConstructor(className: ClassName)(
        implicit pos: Position): WithGlobals[js.Tree] = {
      sjsGen.genJSClassConstructor(className,
          keepOnlyDangerousVarNames = false)
    }

    private def genApplyStaticLike(field: String, className: ClassName,
        method: MethodIdent, args: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      js.Apply(globalVar(field, (className, method.name)), args)
    }

    private def genFround(arg: js.Tree)(implicit pos: Position): js.Tree = {
      genCallHelper("fround", arg)
    }

    private def wrapBigInt32(tree: js.Tree)(implicit pos: Position): js.Tree =
      wrapBigIntN(32, tree)

    private def wrapBigInt64(tree: js.Tree)(implicit pos: Position): js.Tree =
      wrapBigIntN(64, tree)

    private def wrapBigIntN(n: Int, tree: js.Tree)(
        implicit pos: Position): js.Tree = {
      js.Apply(genIdentBracketSelect(genGlobalVarRef("BigInt"), "asIntN"),
          List(js.IntLiteral(n), tree))
    }

    private def wrapBigIntU64(tree: js.Tree)(implicit pos: Position): js.Tree =
      wrapBigIntUN(64, tree)

    private def wrapBigIntUN(n: Int, tree: js.Tree)(
        implicit pos: Position): js.Tree = {
      js.Apply(genIdentBracketSelect(genGlobalVarRef("BigInt"), "asUintN"),
          List(js.IntLiteral(n), tree))
    }

    private def genLongMethodApply(receiver: js.Tree, methodName: MethodName,
        args: js.Tree*)(implicit pos: Position): js.Tree = {
      import TreeDSL._
      js.Apply(receiver DOT genName(methodName), args.toList)
    }
  }
}

private object FunctionEmitter {
  private val UTF8Period: UTF8String = UTF8String(".")

  private val thisOriginalName: OriginalName = OriginalName("this")

  private final case class JSVarRef(ident: js.Ident, mutable: Boolean)(val tpe: Type)
      extends Transient.Value {

    def traverse(traverser: Traverser): Unit = ()

    def transform(transformer: Transformer, isStat: Boolean)(
        implicit pos: Position): Tree = {
      Transient(this)
    }

    def printIR(out: org.scalajs.ir.Printers.IRTreePrinter): Unit =
      out.print(ident.name)
  }

  private final case class JSNewVararg(ctor: Tree, argArray: Tree)
      extends Transient.Value {
    val tpe: Type = AnyType

    def traverse(traverser: Traverser): Unit = {
      traverser.traverse(ctor)
      traverser.traverse(argArray)
    }

    def transform(transformer: Transformer, isStat: Boolean)(
        implicit pos: Position): Tree = {
      Transient(JSNewVararg(transformer.transformExpr(ctor),
          transformer.transformExpr(argArray)))
    }

    def printIR(out: IRTreePrinter): Unit = {
      out.print("new (")
      out.print(ctor)
      out.print(")(...")
      out.print(argArray)
      out.print(')')
    }
  }

  /** A left hand side that can be pushed into a right hand side tree. */
  sealed abstract class Lhs {
    def hasNothingType: Boolean = false
  }

  object Lhs {
    final case class Assign(lhs: Tree) extends Lhs
    final case class VarDef(name: js.Ident, tpe: Type, mutable: Boolean) extends Lhs

    case object ReturnFromFunction extends Lhs {
      override def hasNothingType: Boolean = true
    }

    final case class Return(label: LabelIdent) extends Lhs {
      override def hasNothingType: Boolean = true
    }

    case object Throw extends Lhs {
      override def hasNothingType: Boolean = true
    }

    /** Discard the value of rhs (but retain side effects). */
    case object Discard extends Lhs
  }

  // Environment

  final class Env private (
      val thisIdent: Option[js.Ident],
      val expectedReturnType: Type,
      val enclosingClassName: Option[ClassName],
      vars: Map[LocalName, Boolean],
      labeledExprLHSes: Map[LabelName, Lhs],
      labelsTurnedIntoContinue: Set[LabelName],
      defaultBreakTargets: Set[LabelName],
      defaultContinueTargets: Set[LabelName]
  ) {
    def isLocalVar(ident: LocalIdent): Boolean = vars.contains(ident.name)

    def isLocalMutable(ident: LocalIdent): Boolean = {
      /* If we do not know the var, it must be a JS class capture, which must
       * be immutable.
       */
      vars.getOrElse(ident.name, false)
    }

    def lhsForLabeledExpr(label: LabelIdent): Lhs = labeledExprLHSes(label.name)

    def isLabelTurnedIntoContinue(label: LabelName): Boolean =
      labelsTurnedIntoContinue.contains(label)

    def isDefaultBreakTarget(label: LabelName): Boolean =
      defaultBreakTargets.contains(label)

    def isDefaultContinueTarget(label: LabelName): Boolean =
      defaultContinueTargets.contains(label)

    def withEnclosingClassName(enclosingClassName: Option[ClassName]): Env =
      copy(enclosingClassName = enclosingClassName)

    def withThisIdent(thisIdent: Option[js.Ident]): Env =
      copy(thisIdent = thisIdent)

    def withParams(params: List[ParamDef]): Env = {
      params.foldLeft(this) {
        case (env, ParamDef(name, _, _, mutable)) =>
          env.withDef(name, mutable)
      }
    }

    def withDef(ident: LocalIdent, mutable: Boolean): Env =
      copy(vars = vars + (ident.name -> mutable))

    def withLabeledExprLHS(label: LabelIdent, lhs: Lhs): Env =
      copy(labeledExprLHSes = labeledExprLHSes + (label.name -> lhs))

    def withTurnLabelIntoContinue(label: LabelName): Env =
      copy(labelsTurnedIntoContinue = labelsTurnedIntoContinue + label)

    def withDefaultBreakTargets(targets: Set[LabelName]): Env =
      copy(defaultBreakTargets = targets)

    def withDefaultContinueTargets(targets: Set[LabelName]): Env =
      copy(defaultContinueTargets = targets)

    private def copy(
        thisIdent: Option[js.Ident] = this.thisIdent,
        expectedReturnType: Type = this.expectedReturnType,
        enclosingClassName: Option[ClassName] = this.enclosingClassName,
        vars: Map[LocalName, Boolean] = this.vars,
        labeledExprLHSes: Map[LabelName, Lhs] = this.labeledExprLHSes,
        labelsTurnedIntoContinue: Set[LabelName] = this.labelsTurnedIntoContinue,
        defaultBreakTargets: Set[LabelName] = this.defaultBreakTargets,
        defaultContinueTargets: Set[LabelName] = this.defaultContinueTargets): Env = {
      new Env(thisIdent, expectedReturnType, enclosingClassName, vars,
          labeledExprLHSes, labelsTurnedIntoContinue, defaultBreakTargets,
          defaultContinueTargets)
    }
  }

  object Env {
    def empty(expectedReturnType: Type): Env = {
      new Env(None, expectedReturnType, None, Map.empty, Map.empty, Set.empty,
          Set.empty, Set.empty)
    }
  }
}
