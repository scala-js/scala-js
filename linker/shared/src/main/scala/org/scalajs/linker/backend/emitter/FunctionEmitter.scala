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

import org.scalajs.ir
import org.scalajs.ir._
import org.scalajs.ir.Definitions._
import org.scalajs.ir.Position._
import org.scalajs.ir.Transformers._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.CheckedBehavior._
import org.scalajs.linker.backend.javascript.{Trees => js}

import java.io.StringWriter

import EmitterDefinitions._
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
private[emitter] class FunctionEmitter(jsGen: JSGen) {
  import FunctionEmitter._
  import jsGen._

  /** Desugars parameters and body to a JS function.
   */
  def desugarToFunction(enclosingClassName: ClassName, params: List[ParamDef],
      body: Tree, resultType: Type)(
      implicit globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[js.Function] = {
    new JSDesugar().desugarToFunction(params, body,
        isStat = resultType == NoType,
        Env.empty(resultType).withEnclosingClassName(Some(enclosingClassName)))
  }

  /** Desugars parameters and body to a JS function where `this` is given as
   *  an explicit normal parameter.
   */
  def desugarToFunctionWithExplicitThis(enclosingClassName: ClassName,
      params: List[ParamDef], body: Tree, resultType: Type)(
      implicit globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[js.Function] = {
    new JSDesugar().desugarToFunctionWithExplicitThis(params, body,
        isStat = resultType == NoType,
        Env.empty(resultType).withEnclosingClassName(Some(enclosingClassName)))
  }

  /** Desugars parameters and body to a JS function.
   */
  def desugarToFunction(params: List[ParamDef], body: Tree, resultType: Type)(
      implicit globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[js.Function] = {
    new JSDesugar().desugarToFunction(params, body,
        isStat = resultType == NoType, Env.empty(resultType))
  }

  /** Desugars a class-level expression. */
  def desugarExpr(expr: Tree, resultType: Type)(
      implicit globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[js.Tree] = {
    for (fun <- desugarToFunction(Nil, expr, resultType)) yield {
      fun match {
        case js.Function(_, Nil, js.Return(newExpr)) =>
          // no need for an IIFE, we can just use `newExpr` directly
          newExpr
        case _ =>
          js.Apply(fun, Nil)
      }
    }
  }

  private class JSDesugar()(implicit globalKnowledge: GlobalKnowledge) {

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

    var syntheticVarCounter: Int = 0

    def newSyntheticVar()(implicit pos: Position): LocalIdent = {
      /* TODO Integrate this with proper name management.
       * This is filed as #2971.
       */
      syntheticVarCounter += 1
      LocalIdent(LocalName("jsx$" + syntheticVarCounter), None)
    }

    def resetSyntheticVarCounterIn[A](f: => A): A = {
      val savedCounter = syntheticVarCounter
      syntheticVarCounter = 0
      try f
      finally syntheticVarCounter = savedCounter
    }

    @inline
    @tailrec
    private def performOptimisticThenPessimisticRuns[A](
        body: => A): WithGlobals[A] = {
      val result = body
      if (!isOptimisticNamingRun || !globalVarNames.exists(localVarNames)) {
        /* At this point, filter out the global refs that do not need to be
         * tracked across functions and classes.
         */
        WithGlobals(result, keepOnlyTrackedGlobalRefs(globalVarNames.toSet))
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

    def makeRecordFieldVarRef(tree: RecordSelect): VarRef =
      VarRef(makeRecordFieldIdent(tree)(tree.pos))(tree.tpe)(tree.pos)

    def makeRecordFieldIdent(tree: RecordSelect)(
        implicit pos: Position): LocalIdent = {
      val recIdent = (tree.record: @unchecked) match {
        case VarRef(ident)        => ident
        case record: RecordSelect => makeRecordFieldIdent(record)
      }
      makeRecordFieldIdent(recIdent, tree.field)
    }

    def makeRecordFieldIdent(recIdent: LocalIdent, fieldIdent: FieldIdent)(
        implicit pos: Position): LocalIdent =
      makeRecordFieldIdent(genName(recIdent.name), recIdent.originalName,
          fieldIdent.name, fieldIdent.originalName)

    def makeRecordFieldIdent(recIdent: LocalIdent,
        fieldName: FieldName, fieldOrigiName: Option[String])(
        implicit pos: Position): LocalIdent =
      makeRecordFieldIdent(genName(recIdent.name), recIdent.originalName,
          fieldName, fieldOrigiName)

    def makeRecordFieldIdent(recName: String, recOrigName: Option[String],
        fieldName: FieldName, fieldOrigName: Option[String])(
        implicit pos: Position): LocalIdent = {
      val name = fieldName.toLocalName.withPrefix(recName + "_$_")
      val originalName = Some(recOrigName.getOrElse(recName) + "." +
          fieldOrigName.getOrElse(fieldName))
      LocalIdent(name, originalName)
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
        /* TODO The identifier `$thiz` can clash with a potential access to a
         * JSGlobalRef named `$thiz`. This is filed as #2972.
         */
        val thisIdent = js.Ident("$thiz", Some("this"))
        val env = env0.withThisIdent(Some(thisIdent))
        val js.Function(jsArrow, jsParams, jsBody) =
          desugarToFunctionInternal(arrow = false, params, body, isStat, env)
        js.Function(jsArrow, js.ParamDef(thisIdent, rest = false) :: jsParams,
            jsBody)
      }
    }

    /** Desugars parameters and body to a JS function.
     */
    def desugarToFunction(
        params: List[ParamDef], body: Tree, isStat: Boolean, env0: Env)(
        implicit pos: Position): WithGlobals[js.Function] = {
      performOptimisticThenPessimisticRuns {
        desugarToFunctionInternal(arrow = false, params, body, isStat, env0)
      }
    }

    /** Desugars parameters and body to a JS function.
     */
    private def desugarToFunctionInternal(arrow: Boolean,
        params: List[ParamDef], body: Tree, isStat: Boolean, env0: Env)(
        implicit pos: Position): js.Function = {

      val env = env0.withParams(params)

      val translateRestParam =
        if (esFeatures.useECMAScript2015) false
        else params.nonEmpty && params.last.rest

      val extractRestParam =
        if (translateRestParam) makeExtractRestParam(params)
        else js.Skip()

      val newParams =
        (if (translateRestParam) params.init else params).map(transformParamDef)

      val newBody =
        if (isStat) transformStat(body, Set.empty)(env)
        else pushLhsInto(Lhs.ReturnFromFunction, body, Set.empty)(env)

      val cleanedNewBody = newBody match {
        case js.Block(stats :+ js.Return(js.Undefined())) => js.Block(stats)
        case other                                        => other
      }

      js.Function(arrow && useArrowFunctions, newParams,
          js.Block(extractRestParam, cleanedNewBody))
    }

    private def makeExtractRestParam(params: List[ParamDef])(
        implicit pos: Position): js.Tree = {
      val offset = params.size - 1
      val restParamDef = params.last

      val lenIdent = transformLocalVarIdent(newSyntheticVar())
      val len = js.VarRef(lenIdent)

      val counterIdent = transformLocalVarIdent(newSyntheticVar())
      val counter = js.VarRef(counterIdent)

      val restParamIdent = transformLocalVarIdent(restParamDef.name)
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

        case VarDef(_, _, _, rhs) =>
          pushLhsInto(Lhs.Discard, rhs, tailPosLabels)

        // Statement-only language constructs

        case Skip() =>
          js.Skip()

        case Assign(select @ Select(qualifier, cls, field), rhs) =>
          unnest(qualifier, rhs) { (newQualifier, newRhs, env0) =>
            implicit val env = env0
            js.Assign(
                genSelect(transformExprNoChar(newQualifier), cls, field)(select.pos),
                transformExpr(newRhs, select.tpe))
          }

        case Assign(select @ ArraySelect(array, index), rhs) =>
          unnest(List(array, index, rhs)) {
            case (List(newArray, newIndex, newRhs), env0) =>
              implicit val env = env0
              val genArray = transformExprNoChar(newArray)
              val genIndex = transformExprNoChar(newIndex)
              val genRhs = transformExpr(newRhs, select.tpe)
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

        case Assign(lhs: RecordSelect, rhs) =>
          pushLhsInto(Lhs.Assign(makeRecordFieldVarRef(lhs)), rhs, tailPosLabels)

        case Assign(select @ JSPrivateSelect(qualifier, cls, field), rhs) =>
          unnest(qualifier, rhs) { (newQualifier, newRhs, env0) =>
            implicit val env = env0
            js.Assign(
                genJSPrivateSelect(transformExprNoChar(newQualifier),
                    cls, field)(select.pos),
                transformExprNoChar(newRhs))
          }

        case Assign(select @ JSSelect(qualifier, item), rhs) =>
          unnest(List(qualifier, item, rhs)) {
            case (List(newQualifier, newItem, newRhs), env0) =>
              implicit val env = env0
              js.Assign(
                  genBracketSelect(transformExprNoChar(newQualifier),
                      transformExprNoChar(newItem))(select.pos),
                  transformExprNoChar(newRhs))
          }

        case Assign(select @ JSSuperSelect(superClass, qualifier, item), rhs) =>
          unnest(List(superClass, qualifier, item, rhs)) {
            case (List(newSuperClass, newQualifier, newItem, newRhs), env0) =>
              implicit val env = env0
              genCallHelper("superSet", transformExprNoChar(newSuperClass),
                  transformExprNoChar(newQualifier), transformExprNoChar(item),
                  transformExprNoChar(rhs))
          }

        case Assign(lhs @ (_:VarRef | _:SelectStatic | _:JSGlobalRef), rhs) =>
          pushLhsInto(Lhs.Assign(lhs), rhs, tailPosLabels)

        case Assign(_, _) =>
          throw new IllegalArgumentException(
              s"Illegal Assign in transformStat: $tree")

        case StoreModule(cls, value) =>
          unnest(value) { (newValue, env0) =>
            implicit val env = env0
            js.Assign(
                envField("n", cls.className),
                transformExprNoChar(newValue))
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

        case ForIn(obj, keyVar, body) =>
          unnest(obj) { (newObj, env0) =>
            implicit val env = env0

            val lhs = genEmptyImmutableLet(transformLocalVarIdent(keyVar))
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

            val superCtorCall = if (useClasses) {
              js.Apply(js.Super(), newArgs.map(transformJSArg))
            } else {
              val superCtor = {
                if (globalKnowledge.hasStoredSuperClass(enclosingClassName)) {
                  envField("superClass")
                } else {
                  val superClass =
                    globalKnowledge.getSuperClassOfJSClass(enclosingClassName)
                  extractWithGlobals(genJSClassConstructor(superClass))
                }
              }

              if (containsAnySpread(newArgs)) {
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
                referenceGlobalName("Object")
                js.Apply(
                  genIdentBracketSelect(
                      js.VarRef(js.Ident("Object", Some("Object"))),
                      methodName),
                  args)
              }

              val zero =
                if (field.ftpe == CharType) js.VarRef(js.Ident("$bC0"))
                else genZeroOf(field.ftpe)

              val descriptor = js.ObjectConstr(List(
                  js.StringLiteral("configurable") -> js.BooleanLiteral(true),
                  js.StringLiteral("enumerable") -> js.BooleanLiteral(true),
                  js.StringLiteral("writable") -> js.BooleanLiteral(true),
                  js.StringLiteral("value") -> zero
              ))

              field match {
                case FieldDef(_, name, _) =>
                  val fieldIdent =
                    genJSPrivateFieldIdent(enclosingClassName, name)
                  val descriptors =
                    js.ObjectConstr(List(fieldIdent -> descriptor))
                  makeObjectMethodApply("defineProperties",
                      List(js.This(), descriptors))

                case JSFieldDef(_, name, _) =>
                  unnest(name) { (newName, env0) =>
                    implicit val env = env0
                    makeObjectMethodApply("defineProperty",
                        List(js.This(), transformExprNoChar(newName), descriptor))
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

    /** Same as `unnest`, but allows (and preserves) [[JSSpread]]s at the
     *  top-level.
     */
    def unnestOrSpread(args: List[TreeOrJSSpread])(
        makeStat: (List[TreeOrJSSpread], Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      val (argsNoSpread, argsWereSpread) = args.map {
        case JSSpread(items) => (items, true)
        case arg: Tree       => (arg, false)
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
                val result = rec(expr)(newEnv)
                // Put the stats in a Block because ++=: is not smart
                js.Block(jsStats) +=: extractedStatements
                innerEnv = stats.foldLeft(innerEnv) { (prev, stat) =>
                  stat match {
                    case VarDef(name, tpe, mutable, _) =>
                      prev.withDef(name, tpe, mutable)
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
              case JSArrayConstr(items) if !containsAnySpread(items) =>
                JSArrayConstr(recs(castNoSpread(items)))

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

              case Closure(arrow, captureParams, params, body, captureValues) =>
                Closure(arrow, captureParams, params, body, recs(captureValues))

              case New(cls, constr, args) if noExtractYet =>
                New(cls, constr, recs(args))
              case Select(qualifier, cls, item) if noExtractYet =>
                Select(rec(qualifier), cls, item)(arg.tpe)
              case Apply(flags, receiver, method, args) if noExtractYet =>
                val newArgs = recs(args)
                Apply(flags, rec(receiver), method, newArgs)(arg.tpe)
              case ApplyStatically(flags, receiver, cls, method, args) if noExtractYet =>
                val newArgs = recs(args)
                ApplyStatically(flags, rec(receiver), cls, method, newArgs)(arg.tpe)
              case ApplyStatic(flags, cls, method, args) if noExtractYet =>
                ApplyStatic(flags, cls, method, recs(args))(arg.tpe)
              case ArrayLength(array) if noExtractYet =>
                ArrayLength(rec(array))
              case ArraySelect(array, index) if noExtractYet =>
                val newIndex = rec(index)
                ArraySelect(rec(array), newIndex)(arg.tpe)
              case RecordSelect(record, field) if noExtractYet =>
                RecordSelect(rec(record), field)(arg.tpe)
              case Transient(CallHelper(helper, args)) if noExtractYet =>
                Transient(CallHelper(helper, recs(args)))(arg.tpe)

              case If(cond, thenp, elsep)
                  if noExtractYet && isExpression(thenp) && isExpression(elsep) =>
                If(rec(cond), thenp, elsep)(arg.tpe)

              case _ =>
                val temp = newSyntheticVar()
                val computeTemp = pushLhsInto(
                    Lhs.VarDef(temp, arg.tpe, mutable = false), arg,
                    Set.empty)
                computeTemp +=: extractedStatements
                innerEnv = innerEnv.withDef(temp, arg.tpe, false)
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
        case JSSpread(_) => false
        case tree: Tree  => test(tree)
      }

      def test(tree: Tree): Boolean = tree match {
        // Atomic expressions
        case _: Literal       => true
        case _: This          => true
        case _: JSLinkingInfo => true

        // Vars (side-effect free, pure if immutable)
        case VarRef(name) =>
          allowUnpure || !env.isLocalMutable(name)

        // Fields may throw if qualifier is null
        case Select(qualifier, _, _) =>
          allowSideEffects && test(qualifier)

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
        case JSBinaryOp(_, lhs, rhs) => test(lhs) && test(rhs)
        case JSUnaryOp(_, lhs)       => test(lhs)
        case ArrayLength(array)      => test(array)
        case RecordSelect(record, _) => test(record)
        case IsInstanceOf(expr, _)   => test(expr)

        // Expressions preserving side-effect freedom
        case NewArray(tpe, lengths) =>
          allowUnpure && (lengths forall test)
        case ArrayValue(tpe, elems) =>
          allowUnpure && (elems forall test)
        case ArraySelect(array, index) =>
          allowUnpure && test(array) && test(index)
        case JSArrayConstr(items) =>
          allowUnpure && (items.forall(testJSArg))
        case tree @ JSObjectConstr(items) =>
          allowUnpure &&
          !doesObjectConstrRequireDesugaring(tree) &&
          items.forall { item =>
            test(item._1) && test(item._2)
          }
        case Closure(arrow, captureParams, params, body, captureValues) =>
          allowUnpure && (captureValues forall test)

        // Scala expressions that can always have side-effects
        case New(cls, constr, args) =>
          allowSideEffects && (args forall test)
        case LoadModule(cls) => // unfortunately
          allowSideEffects
        case Apply(_, receiver, method, args) =>
          allowSideEffects && test(receiver) && (args forall test)
        case ApplyStatically(_, receiver, cls, method, args) =>
          allowSideEffects && test(receiver) && (args forall test)
        case ApplyStatic(_, cls, method, args) =>
          allowSideEffects && (args forall test)
        case GetClass(arg) =>
          allowSideEffects && test(arg)
        case Transient(CallHelper(helper, args)) =>
          allowSideEffects && (args forall test)

        // Casts
        case AsInstanceOf(expr, _) =>
          (allowSideEffects || semantics.asInstanceOfs == Unchecked) && test(expr)

        // JavaScript expressions that can always have side-effects
        case JSNew(fun, args) =>
          allowSideEffects && test(fun) && (args.forall(testJSArg))
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
        case LoadJSModule(_) =>
          allowSideEffects
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

    def doVarDef(ident: LocalIdent, tpe: Type, mutable: Boolean, rhs: Tree)(
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
          genLet(transformLocalVarIdent(ident), mutable,
              transformExpr(rhs, tpe))
      }
    }

    def doEmptyVarDef(ident: LocalIdent, tpe: Type)(
        implicit pos: Position, env: Env): js.Tree = {
      tpe match {
        case RecordType(fields) =>
          js.Block(for {
            RecordType.Field(fName, fOrigName, fTpe, fMutable) <- fields
          } yield {
            doEmptyVarDef(makeRecordFieldIdent(ident, fName, fOrigName), fTpe)
          })

        case _ =>
          genEmptyMutableLet(transformLocalVarIdent(ident))
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
          val base = js.Assign(transformExpr(lhs, preserveChar = true),
              transformExpr(rhs, lhs.tpe))
          lhs match {
            case SelectStatic(ClassRef(className), FieldIdent(field, _))
                if moduleKind == ModuleKind.NoModule =>
              val mirrors =
                globalKnowledge.getStaticFieldMirrors(className, field)
              mirrors.foldLeft(base) { (prev, mirror) =>
                referenceGlobalName(mirror)
                js.Assign(js.VarRef(js.Ident(mirror)), prev)
              }
            case _ =>
              base
          }
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
       *  This only matters in ECMAScript 6, because we emit Lets.
       */
      def extractLet(inner: (Lhs, Env) => js.Tree)(
          implicit env: Env): js.Tree = {
        if (esFeatures.useECMAScript2015) {
          lhs match {
            case Lhs.VarDef(name, tpe, mutable) =>
              val innerEnv = env.withDef(name, tpe, true)
              js.Block(
                  doEmptyVarDef(name, tpe),
                  inner(Lhs.Assign(VarRef(name)(tpe)), innerEnv))
            case _ =>
              inner(lhs, env)
          }
        } else {
          inner(lhs, env)
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
                val assign = doAssign(lhs, VarRef(temp)(recTpe))(
                    env.withDef(temp, recTpe, false))
                js.Block(varDef, assign)
              }

            case Lhs.ReturnFromFunction =>
              throw new AssertionError("Cannot return a record value.")

            case Lhs.Return(l) =>
              doReturnToLabel(l)
          }

        // Control flow constructs

        case Labeled(label, tpe, body) =>
          extractLet { (newLhs, env) =>
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
            extractLet { (newLhs, branchesEnv) =>
              js.If(transformExprNoChar(newCond),
                  pushLhsInto(newLhs, thenp, tailPosLabels)(branchesEnv),
                  pushLhsInto(newLhs, elsep, tailPosLabels)(branchesEnv))
            }
          }

        case TryCatch(block, errVar, handler) =>
          extractLet { (newLhs, env) =>
            val newBlock = pushLhsInto(newLhs, block, tailPosLabels)(env)
            val newHandler = pushLhsInto(newLhs, handler, tailPosLabels)(
                env.withDef(errVar, mutable = false))
            js.TryCatch(newBlock, transformLocalVarIdent(errVar), newHandler)
          }

        case TryFinally(block, finalizer) =>
          extractLet { (newLhs, blockEnv) =>
            val newBlock = pushLhsInto(newLhs, block, tailPosLabels)(blockEnv)
            val newFinalizer = transformStat(finalizer, Set.empty)
            js.TryFinally(newBlock, newFinalizer)
          }

        // TODO Treat throw as an LHS?
        case Throw(expr) =>
          unnest(expr) { (newExpr, env) =>
            js.Throw(transformExprNoChar(newExpr)(env))
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
            extractLet { (newLhs, branchesEnv) =>
              val newCases = {
                for {
                  (values, body) <- cases
                  newValues = values.map(v => js.IntLiteral(v.value)(v.pos))
                  // add the break statement
                  newBody = js.Block(
                      pushLhsInto(newLhs, body, tailPosLabels)(branchesEnv),
                      js.Break())
                  // desugar alternatives into several cases falling through
                  caze <- (newValues.init map (v => (v, js.Skip()))) :+ (newValues.last, newBody)
                } yield {
                  caze
                }
              }
              val newDefault = pushLhsInto(newLhs, default, tailPosLabels)(
                  branchesEnv)
              js.Switch(transformExpr(newSelector, preserveChar = true),
                  newCases, newDefault)
            }
          }

        // Scala expressions (if we reach here their arguments are not expressions)

        case New(cls, ctor, args) =>
          unnest(args) { (newArgs, env) =>
            redo(New(cls, ctor, newArgs))(env)
          }

        case Select(qualifier, cls, item) =>
          unnest(qualifier) { (newQualifier, env) =>
            redo(Select(newQualifier, cls, item)(rhs.tpe))(env)
          }

        case Apply(flags, receiver, method, args) =>
          unnest(receiver, args) { (newReceiver, newArgs, env) =>
            redo(Apply(flags, newReceiver, method, newArgs)(rhs.tpe))(env)
          }

        case ApplyStatically(flags, receiver, cls, method, args) =>
          unnest(receiver, args) { (newReceiver, newArgs, env) =>
            redo(ApplyStatically(flags, newReceiver, cls, method, newArgs)(rhs.tpe))(env)
          }

        case ApplyStatic(flags, cls, method, args) =>
          unnest(args) { (newArgs, env) =>
            redo(ApplyStatic(flags, cls, method, newArgs)(rhs.tpe))(env)
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

        case Transient(CallHelper(helper, args)) =>
          unnest(args) { (newArgs, env) =>
            redo(Transient(CallHelper(helper, newArgs))(rhs.tpe))(env)
          }

        // JavaScript expressions (if we reach here their arguments are not expressions)

        case JSNew(ctor, args) =>
          if (containsAnySpread(args)) {
            redo {
              Transient(CallHelper("newJSObjectWithVarargs",
                  List(ctor, spreadToArgArray(args))))(AnyType)
            }
          } else {
            unnest(ctor :: castNoSpread(args)) { (newCtorAndArgs, env) =>
              val newCtor :: newArgs = newCtorAndArgs
              redo(JSNew(newCtor, newArgs))(env)
            }
          }

        case JSFunctionApply(fun, args) =>
          if (containsAnySpread(args)) {
            redo {
              JSMethodApply(fun, StringLiteral("apply"),
                  List(Undefined(), spreadToArgArray(args)))
            }
          } else {
            unnest(fun :: castNoSpread(args)) { (newFunAndArgs, env) =>
              val newFun :: newArgs = newFunAndArgs
              redo(JSFunctionApply(newFun, newArgs))(env)
            }
          }

        case JSMethodApply(receiver, method, args) =>
          if (containsAnySpread(args)) {
            withTempVar(receiver) { (newReceiver, env0) =>
              implicit val env = env0
              redo {
                JSMethodApply(
                    JSSelect(newReceiver, method),
                    StringLiteral("apply"),
                    List(newReceiver, spreadToArgArray(args)))
              }
            }
          } else {
            unnest(receiver :: method :: castNoSpread(args)) { (newReceiverAndArgs, env) =>
              val newReceiver :: newMethod :: newArgs = newReceiverAndArgs
              redo(JSMethodApply(newReceiver, newMethod, newArgs))(env)
            }
          }

        case JSSuperSelect(superClass, qualifier, item) =>
          unnest(List(superClass, qualifier, item)) {
            case (List(newSuperClass, newQualifier, newItem), env) =>
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

        case JSPrivateSelect(qualifier, cls, field) =>
          unnest(qualifier) { (newQualifier, env) =>
            redo(JSPrivateSelect(newQualifier, cls, field))(env)
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
          if (containsAnySpread(items)) {
            redo {
              spreadToArgArray(items)
            }
          } else {
            unnest(castNoSpread(items)) { (newItems, env) =>
              redo(JSArrayConstr(newItems))(env)
            }
          }

        case rhs @ JSObjectConstr(fields) =>
          if (doesObjectConstrRequireDesugaring(rhs)) {
            val objVarDef = VarDef(newSyntheticVar(), AnyType, mutable = false,
                JSObjectConstr(Nil))
            val assignFields = fields.foldRight((Set.empty[String], List.empty[Tree])) {
              case ((key, value), (namesSeen, statsAcc)) =>
                implicit val pos = value.pos
                val nameForDupes = key match {
                  case StringLiteral(s) => Some(s)
                  case _                => None
                }
                val stat = if (nameForDupes.exists(namesSeen)) {
                  /* Important: do not emit the assignment, otherwise
                   * Closure recreates a literal with the duplicate field!
                   */
                  value
                } else {
                  Assign(JSSelect(objVarDef.ref, key), value)
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

        case Closure(arrow, captureParams, params, body, captureValues) =>
          unnest(captureValues) { (newCaptureValues, env) =>
            redo(Closure(arrow, captureParams, params, body, newCaptureValues))(
                env)
          }

        case CreateJSClass(cls, captureValues) =>
          unnest(captureValues) { (newCaptureValues, env) =>
            redo(CreateJSClass(cls, newCaptureValues))(env)
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
                  _:StoreModule =>
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

    private def containsAnySpread(args: List[TreeOrJSSpread]): Boolean =
      args.exists(_.isInstanceOf[JSSpread])

    /** Precondition: `!containsAnySpread(args)`. */
    private def castNoSpread(args: List[TreeOrJSSpread]): List[Tree] =
      args.asInstanceOf[List[Tree]]

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
        esFeatures.useECMAScript2015

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

    def transformJSArg(tree: TreeOrJSSpread)(implicit env: Env): js.Tree = {
      tree match {
        case JSSpread(items) =>
          assert(esFeatures.useECMAScript2015)
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

      val baseResult = tree match {
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

        case New(cls, ctor, args) =>
          val className = cls.className
          val encodedClassVar = encodeClassVar(className)
          val newArgs = transformTypedArgs(ctor.name, args)
          if (globalKnowledge.hasInlineableInit(className)) {
            js.New(encodedClassVar, newArgs)
          } else {
            genApplyStaticLike("ct", className, ctor,
                js.New(encodedClassVar, Nil) :: newArgs)
          }

        case LoadModule(cls) =>
          genLoadModule(cls.className)

        case Select(qualifier, cls, field) =>
          genSelect(transformExprNoChar(qualifier), cls, field)

        case SelectStatic(cls, item) =>
          genSelectStatic(cls.className, item)

        case Apply(_, receiver, method, args) =>
          import Definitions._

          val methodName = method.name
          val newReceiver = transformExprNoChar(receiver)
          val newArgs = transformTypedArgs(method.name, args)

          def genNormalApply(): js.Tree =
            js.Apply(newReceiver DOT transformMethodIdent(method), newArgs)

          def genDispatchApply(): js.Tree =
            genCallHelper("dp_" + genName(methodName), newReceiver :: newArgs: _*)

          def genHijackedMethodApply(className: ClassName): js.Tree = {
            js.Apply(envField("f", className, methodName, method.originalName),
                newReceiver :: newArgs)
          }

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

              case ClassType(CharSequenceClass)
                  if !hijackedMethodsOfStringWithDispatcher.contains(methodName) =>
                /* This case is required as a hack around a peculiar behavior
                 * of the optimizer. In theory, it should never happen, because
                 * we should always have a dispatcher when the receiver is not
                 * a concrete hijacked class. However, if the optimizer inlines
                 * a method of CharSequence from String (because there is no
                 * other CharSequence in the whole program), we can end up with
                 * the inlined code calling another method of String although
                 * its receiver is still declared as a CharSequence.
                 *
                 * TODO The proper fix for this would be to improve how the
                 * optimizer handles inlinings such as those: it should refine
                 * the type of `this` within the inlined body.
                 *
                 * This cannot happen with other ancestors of hijacked classes
                 * because all the other ones have several hijacked classes
                 * implementing them, which prevents that form of inlining from
                 * happening.
                 */
                genHijackedMethodApply(BoxedStringClass)

              case ClassType(cls) if !HijackedClasses.contains(cls) =>
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

        case ApplyStatically(flags, receiver, cls, method, args) =>
          val className = cls.className
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
              encodeClassVar(className).prototype DOT transformMethodIdent(method)
            js.Apply(fun DOT "call", transformedArgs)
          }

        case ApplyStatic(flags, cls, method, args) =>
          genApplyStaticLike(
              if (flags.isPrivate) "ps" else "s",
              cls.className,
              method,
              transformTypedArgs(method.name, args))

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
                  env.enclosingClassName.exists(_ == Definitions.ObjectClass)
                case _ =>
                  false
              }
              if (receivesExemption(newLhs) || receivesExemption(newRhs)) {
                js.BinaryOp(if (op == ===) JSBinaryOp.=== else JSBinaryOp.!==,
                    newLhs, newRhs)
              } else {
                val objectIs =
                  if (!esFeatures.useECMAScript2015) envField("is")
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

            case Double_<  => js.BinaryOp(JSBinaryOp.<, newLhs, newRhs)
            case Double_<= => js.BinaryOp(JSBinaryOp.<=, newLhs, newRhs)
            case Double_>  => js.BinaryOp(JSBinaryOp.>, newLhs, newRhs)
            case Double_>= => js.BinaryOp(JSBinaryOp.>=, newLhs, newRhs)

            case Boolean_| => !(!js.BinaryOp(JSBinaryOp.|, newLhs, newRhs))
            case Boolean_& => !(!js.BinaryOp(JSBinaryOp.&, newLhs, newRhs))
          }

        case NewArray(typeRef, lengths) =>
          genCallHelper("newArrayObject",
              genClassDataOf(typeRef),
              js.ArrayConstr(lengths.map(transformExprNoChar)))

        case ArrayValue(typeRef, elems) =>
          val preserveChar = typeRef match {
            case ArrayTypeRef(CharRef, 1) => true
            case _                        => false
          }
          genArrayValue(typeRef, elems.map(transformExpr(_, preserveChar)))

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
          val name = makeRecordFieldIdent(tree)
          assert(env.isLocalVar(name), name.name)
          js.VarRef(transformLocalVarIdent(name))

        case IsInstanceOf(expr, testType) =>
          genIsInstanceOf(transformExprNoChar(expr), testType)

        case AsInstanceOf(expr, tpe) =>
          genAsInstanceOf(transformExprNoChar(expr), tpe)

        case GetClass(expr) =>
          genCallHelper("objectGetClass", transformExprNoChar(expr))

        case Transient(CallHelper(helper, args)) =>
          helper match {
            case "classDataOf" =>
              args.head match {
                case ClassOf(tpe) =>
                  genClassDataOf(tpe)
                case jlClass =>
                  js.DotSelect(transformExprNoChar(jlClass), js.Ident("jl_Class__f_data"))
              }
            case "arrayDataOf" =>
              js.Apply(js.DotSelect(transformExprNoChar(args.head),
                  js.Ident("getArrayOf")), Nil)
            case "zeroOf" =>
              js.DotSelect(
                  js.DotSelect(transformExprNoChar(args.head), js.Ident("jl_Class__f_data")),
                  js.Ident("zero"))
            case _ =>
              genCallHelper(helper,
                  args.map(transformExpr(_, preserveChar = true)): _*)
          }

        // JavaScript expressions

        case JSNew(constr, args) =>
          js.New(transformExprNoChar(constr), args.map(transformJSArg))

        case JSPrivateSelect(qualifier, cls, field) =>
          genJSPrivateSelect(transformExprNoChar(qualifier), cls, field)

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

        case LoadJSConstructor(cls) =>
          extractWithGlobals(genJSClassConstructor(cls.className))

        case LoadJSModule(cls) =>
          val className = cls.className
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
          envField("linkingInfo")

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
            js.New(encodeClassVar(LongImpl.RuntimeLongClass),
                List(js.IntLiteral(lo), js.IntLiteral(hi)))
          }

        case ClassOf(cls) =>
          genClassOf(cls)

        // Atomic expressions

        case VarRef(name) =>
          if (env.isLocalVar(name))
            js.VarRef(transformLocalVarIdent(name))
          else
            envField("cc", genName(name.name), name.originalName)

        case This() =>
          env.thisIdent.fold[js.Tree] {
            js.This()
          } { ident =>
            js.VarRef(ident)
          }

        case Closure(arrow, captureParams, params, body, captureValues) =>
          val innerFunction = {
            desugarToFunctionInternal(arrow, params, body, isStat = false,
                Env.empty(AnyType).withParams(captureParams ++ params))
          }

          if (captureParams.isEmpty) {
            innerFunction
          } else {
            js.Apply(
                genArrowFunction(captureParams.map(transformParamDef), {
                  js.Return(innerFunction)
                }),
                captureValues.zip(captureParams).map {
                  case (value, param) => transformExpr(value, param.ptpe)
                })
          }

        case CreateJSClass(cls, captureValues) =>
          val transformedArgs = if (captureValues.forall(_.tpe != CharType)) {
            // Fast path
            captureValues.map(transformExpr(_, preserveChar = true))
          } else {
            val expectedTypes =
              globalKnowledge.getJSClassCaptureTypes(cls.className).get
            for ((value, expectedType) <- captureValues.zip(expectedTypes))
              yield transformExpr(value, expectedType)
          }
          js.Apply(envField("a", cls.className), transformedArgs)

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
      case ClassType(cls) =>
        MaybeHijackedClasses.contains(cls)
      case AnyType | UndefType | BooleanType | CharType | ByteType | ShortType |
          IntType | LongType | FloatType | DoubleType | StringType =>
        true
      case _ =>
        false
    }

    def typeToBoxedHijackedClass(tpe: Type): ClassName = (tpe: @unchecked) match {
      case ClassType(cls) => cls
      case AnyType        => Definitions.ObjectClass
      case UndefType      => Definitions.BoxedUnitClass
      case BooleanType    => Definitions.BoxedBooleanClass
      case CharType       => Definitions.BoxedCharacterClass
      case ByteType       => Definitions.BoxedByteClass
      case ShortType      => Definitions.BoxedShortClass
      case IntType        => Definitions.BoxedIntegerClass
      case LongType       => Definitions.BoxedLongClass
      case FloatType      => Definitions.BoxedFloatClass
      case DoubleType     => Definitions.BoxedDoubleClass
      case StringType     => Definitions.BoxedStringClass
    }

    /* Ideally, we should dynamically figure out this set. We should test
     * whether a particular method is defined in the receiver's hijacked
     * class: if not, it must be inherited. However, this would require
     * additional global knowledge for no practical reason.
     */
    val hijackedMethodsInheritedFromObject: Set[MethodName] = Set(
        getClassMethodName, cloneMethodName, finalizeMethodName,
        notifyMethodName, notifyAllMethodName
    )

    val hijackedMethodsOfStringWithDispatcher: Set[MethodName] = Set(
        getClassMethodName,
        cloneMethodName,
        finalizeMethodName,
        notifyMethodName,
        notifyAllMethodName,
        toStringMethodName,
        equalsMethodName,
        hashCodeMethodName,
        compareToMethodName,
        lengthMethodName,
        charAtMethodName,
        subSequenceMethodName
    )

    private def transformParamDef(paramDef: ParamDef): js.ParamDef = {
      js.ParamDef(transformLocalVarIdent(paramDef.name), paramDef.rest)(
          paramDef.pos)
    }

    private def transformLabelIdent(ident: LabelIdent): js.Ident =
      js.Ident(genName(ident.name))(ident.pos)

    private def transformMethodIdent(ident: MethodIdent): js.Ident =
      js.Ident(genName(ident.name), ident.originalName)(ident.pos)

    private def transformLocalVarIdent(ident: LocalIdent): js.Ident =
      js.Ident(transformLocalName(ident.name), ident.originalName)(ident.pos)

    private def transformGlobalVarIdent(name: String)(
        implicit pos: Position): js.Ident = {
      referenceGlobalName(name)
      js.Ident(name)
    }

    private def genGlobalVarRef(name: String)(
        implicit pos: Position): js.VarRef = {
      referenceGlobalName(name)
      js.VarRef(js.Ident(name))
    }

    /* In FunctionEmitter, we must always keep all global var names, not only
     * dangerous ones. This helper makes it less annoying.
     */
    private def genJSClassConstructor(className: ClassName)(
        implicit pos: Position): WithGlobals[js.Tree] = {
      jsGen.genJSClassConstructor(className,
          keepOnlyDangerousVarNames = false)
    }

    private def genApplyStaticLike(field: String, className: ClassName,
        method: MethodIdent, args: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      js.Apply(envField(field, className, method.name, method.originalName),
          args)
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

    private implicit class RecordAwareEnv(env: Env) {
      def withDef(ident: LocalIdent, tpe: Type, mutable: Boolean): Env = tpe match {
        case RecordType(fields) =>
          withRecordDefs(ident, fields, mutable)
        case _ =>
          env.withDef(ident, mutable)
      }

      private def withRecordDefs(recIdent: LocalIdent,
          fields: List[RecordType.Field], recMutable: Boolean): Env = {
        fields.foldLeft(env) { (env, fld) =>
          val ident = makeRecordFieldIdent(recIdent, fld.name,
              fld.originalName)(recIdent.pos)
          env.withDef(ident, fld.tpe, recMutable || fld.mutable)
        }
      }
    }
  }
}

private object FunctionEmitter {
  private val MaybeHijackedClasses = {
    (Definitions.HijackedClasses ++ EmitterDefinitions.AncestorsOfHijackedClasses) -
    Definitions.ObjectClass
  }

  /** A left hand side that can be pushed into a right hand side tree. */
  sealed abstract class Lhs {
    def hasNothingType: Boolean = false
  }

  object Lhs {
    case class Assign(lhs: Tree) extends Lhs
    case class VarDef(name: LocalIdent, tpe: Type, mutable: Boolean) extends Lhs

    case object ReturnFromFunction extends Lhs {
      override def hasNothingType: Boolean = true
    }

    case class Return(label: LabelIdent) extends Lhs {
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
        case (env, ParamDef(name, tpe, mutable, _)) =>
          // ParamDefs may not contain record types
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
