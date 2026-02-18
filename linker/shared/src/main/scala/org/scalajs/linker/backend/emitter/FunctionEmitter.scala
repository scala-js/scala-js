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
import org.scalajs.ir.WellKnownNames._

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.CheckedBehavior._
import org.scalajs.linker.backend.javascript.{Trees => js}

import java.io.StringWriter

import EmitterNames._
import PolyfillableBuiltin._
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
  import coreSpec._
  import nameGen._
  import varGen._

  /** Desugars parameters and body to a JS function. */
  def desugarToFunction(enclosingClassName: ClassName, params: List[ParamDef],
      body: Tree, resultType: Type)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      globalRefTracking: GlobalRefTracking, pos: Position): WithGlobals[js.Function] = {
    desugarToFunction(enclosingClassName, params, restParam = None, body,
        resultType)
  }

  /** Desugars parameters and body to a JS function (JS constructor variant). */
  def desugarToFunction(enclosingClassName: ClassName, params: List[ParamDef],
      restParam: Option[ParamDef], body: JSConstructorBody)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      globalRefTracking: GlobalRefTracking, pos: Position): WithGlobals[js.Function] = {
    val bodyBlock = Block(body.allStats)(body.pos)
    new JSDesugar(globalRefTracking).desugarToFunction(
        params, restParam, bodyBlock, isStat = false,
        Env.empty(AnyType).withEnclosingClassName(Some(enclosingClassName)))
  }

  /** Desugars parameters and body to a JS function. */
  def desugarToFunction(enclosingClassName: ClassName, params: List[ParamDef],
      restParam: Option[ParamDef], body: Tree, resultType: Type)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      globalRefTracking: GlobalRefTracking, pos: Position): WithGlobals[js.Function] = {
    new JSDesugar(globalRefTracking).desugarToFunction(
        params, restParam, body, isStat = resultType == VoidType,
        Env.empty(resultType).withEnclosingClassName(Some(enclosingClassName)))
  }

  /** Desugars parameters and body to a JS function where `this` is given as
   *  an explicit normal parameter.
   */
  def desugarToFunctionWithExplicitThis(enclosingClassName: ClassName,
      params: List[ParamDef], body: Tree, resultType: Type)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      globalRefTracking: GlobalRefTracking, pos: Position): WithGlobals[js.Function] = {
    new JSDesugar(globalRefTracking).desugarToFunctionWithExplicitThis(
        params, body, isStat = resultType == VoidType,
        Env.empty(resultType).withEnclosingClassName(Some(enclosingClassName)))
  }

  /** Desugars parameters and body to a JS function. */
  def desugarToFunction(params: List[ParamDef], restParam: Option[ParamDef],
      body: Tree, resultType: Type)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      globalRefTracking: GlobalRefTracking, pos: Position): WithGlobals[js.Function] = {
    new JSDesugar(globalRefTracking).desugarToFunction(
        params, restParam, body, isStat = resultType == VoidType,
        Env.empty(resultType))
  }

  /** Desugars a class-level expression. */
  def desugarExpr(expr: Tree, resultType: Type)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      globalRefTracking: GlobalRefTracking): WithGlobals[js.Tree] = {
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

  private class JSDesugar(outerGlobalRefTracking: GlobalRefTracking)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge) {

    // Inside JSDesugar, we always track everything
    private implicit val globalRefTracking: GlobalRefTracking =
      GlobalRefTracking.All

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
      fileLevelVarIdent(VarField.x, syntheticVarCounter.toString())
    }

    @inline
    @tailrec
    private def performOptimisticThenPessimisticRuns[A](
        body: => A): WithGlobals[A] = {
      val result = body
      if (!isOptimisticNamingRun || !globalVarNames.exists(localVarNames)) {
        /* At this point, filter out the global refs that do not need to be
         * tracked in the outer context.
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
        val outerGlobalRefs =
          outerGlobalRefTracking.refineFrom(globalRefTracking, globalVarNames.toSet)

        WithGlobals(result, outerGlobalRefs)
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
        case record: VarRef                => transformLocalVarRefIdent(record)
        case Transient(JSVarRef(ident, _)) => ident
        case record: RecordSelect          => makeRecordFieldIdentForVarRef(record)
      }

      // Since this is only used for VarRefs, we never need an original name
      makeRecordFieldIdent(recIdent, tree.field.name, NoOriginalName)
    }

    def makeRecordFieldIdent(recIdent: js.Ident,
        fieldName: SimpleFieldName, fieldOrigName: OriginalName)(
        implicit pos: Position): js.Ident = {

      /* "__" is a safe separator for generated names because JSGen avoids it
       * when generating `LocalName`s and `SimpleFieldName`s.
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
        val thisParams = if (env0.enclosingClassName.contains(BoxedLongClass) && !useBigIntForLongs) {
          List(
            js.ParamDef(fileLevelVarIdent(VarField.thiz, thisOriginalName)),
            js.ParamDef(fileLevelVarIdent(VarField.thizhi, thisOriginalName))
          )
        } else {
          List(
            js.ParamDef(fileLevelVarIdent(VarField.thiz, thisOriginalName))
          )
        }
        val env = env0.withExplicitThis()
        val js.Function(jsFlags, jsParams, restParam, jsBody) =
          desugarToFunctionInternal(ClosureFlags.function, params, None, body, isStat, env)
        js.Function(jsFlags, thisParams ::: jsParams, restParam, jsBody)
      }
    }

    /** Desugars parameters and body to a JS function. */
    def desugarToFunction(params: List[ParamDef], restParam: Option[ParamDef],
        body: Tree, isStat: Boolean, env0: Env)(
        implicit pos: Position): WithGlobals[js.Function] = {
      performOptimisticThenPessimisticRuns {
        desugarToFunctionInternal(ClosureFlags.function, params, restParam, body, isStat, env0)
      }
    }

    /** Desugars parameters and body to a JS function. */
    private def desugarToFunctionInternal(flags: ClosureFlags,
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

      val jsFlags =
        if (esFeatures.useECMAScript2015Semantics) flags
        else flags.withArrow(false)
      val jsParams =
        if (useBigIntForLongs) params.map(transformParamDef(_))
        else params.flatMap(transformParamDefExpanded(_))

      if (es2015) {
        val jsRestParam = restParam.map(transformParamDef(_))
        js.Function(jsFlags, jsParams, jsRestParam, cleanedNewBody)
      } else {
        val patchedBody = restParam.fold {
          cleanedNewBody
        } { restParam =>
          js.Block(makeExtractRestParam(restParam, jsParams.size), cleanedNewBody)
        }

        js.Function(jsFlags, jsParams, None, patchedBody)
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
        js.While(js.BinaryOp(JSBinaryOp.<, counter, len),
            js.Block(
              // restParam.push(arguments[i]);
              js.Apply(
                  genIdentBracketSelect(restParam, "push"),
                  List(
                      js.BracketSelect(arguments, counter))),
              // i = (i + 1) | 0
              js.Assign(counter,
                  or0(js.BinaryOp(JSBinaryOp.+,
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
            case Select(qualifier, field) =>
              unnest(checkNotNull(qualifier), rhs) { (newQualifier, newRhs, env0) =>
                implicit val env = env0
                val transformedQual = transformExprNoChar(newQualifier)
                if (isSplitLongType(lhs.tpe)) {
                  val (qualDef, qualRef) = if (isDuplicatable(newQualifier)) {
                    // almost always true, since fields are accessed through `this`
                    (js.Skip(), transformedQual)
                  } else {
                    val tempQual = newSyntheticVar()
                    (genConst(tempQual, transformedQual), js.VarRef(tempQual))
                  }
                  val (lhsLo, lhsHi) = genSelectLong(qualRef, field)(lhs.pos)
                  val (rhsLo, rhsHi) = transformLongExpr(newRhs)
                  js.Block(
                    qualDef,
                    js.Assign(lhsLo, rhsLo),
                    js.Assign(lhsHi, rhsHi)
                  )
                } else {
                  js.Assign(
                      genSelect(transformedQual, field)(lhs.pos),
                      transformExpr(newRhs, lhs.tpe))
                }
              }

            case ArraySelect(array, index) =>
              unnest(checkNotNull(array), index, rhs) { (newArray, newIndex, newRhs, env0) =>
                implicit val env = env0
                val genArray = transformExprNoChar(newArray)
                val genIndex = transformExprNoChar(newIndex)

                /* We need to use a checked 'set' if at least one of the following applies:
                 * - Array index out of bounds are checked, or
                 * - Array stores are checked and the array is an array of reference types.
                 *
                 * We could avoid the arrayStore checks if the array type is exact
                 * and if the rhs has an IR subtype of the array elem type.
                 * It is probably not worth the complexity, though. We currently do
                 * not have infrastructure for subtyping tests in the backend.
                 */
                val checked = {
                  (semantics.arrayIndexOutOfBounds != CheckedBehavior.Unchecked) ||
                  ((semantics.arrayStores != CheckedBehavior.Unchecked) && RefArray.is(array.tpe))
                }

                if (isSplitLongType(lhs.tpe)) {
                  val (rhsLo, rhsHi) = transformLongExpr(newRhs)

                  if (checked) {
                    genSyntheticPropApply(genArray, SyntheticProperty.set, genIndex, rhsLo, rhsHi)
                  } else {
                    withTempJSVar(genSyntheticPropSelect(genArray, SyntheticProperty.u)(lhs.pos)) {
                      uRef =>
                        genIndex match {
                          case js.IntLiteral(genIndexValue) =>
                            val scaledIdx = genIndexValue << 1
                            js.Block(
                              js.Assign(
                                  js.BracketSelect(uRef, js.IntLiteral(scaledIdx)(lhs.pos))(lhs.pos),
                                  rhsLo),
                              js.Assign(
                                  js.BracketSelect(uRef, js.IntLiteral(scaledIdx + 1)(lhs.pos))(
                                      lhs.pos),
                                  rhsHi)
                            )
                          case _ =>
                            withTempJSVar(genIndex << 1) { scaledIndex =>
                              js.Block(
                                js.Assign(js.BracketSelect(uRef, scaledIndex)(lhs.pos), rhsLo),
                                js.Assign(
                                    js.BracketSelect(uRef, (scaledIndex + 1) | 0)(lhs.pos), rhsHi)
                              )
                            }
                        }
                    }
                  }
                } else {
                  val genRhs = transformExpr(newRhs, lhs.tpe)

                  if (checked) {
                    genSyntheticPropApply(genArray, SyntheticProperty.set, genIndex, genRhs)
                  } else {
                    js.Assign(
                        js.BracketSelect(
                            genSyntheticPropSelect(genArray, SyntheticProperty.u)(lhs.pos),
                            genIndex)(lhs.pos),
                        genRhs)
                  }
                }
              }

            case lhs: RecordSelect =>
              val newLhs = Transient(JSVarRef(makeRecordFieldIdentForVarRef(lhs),
                  mutable = true)(lhs.tpe))
              pushLhsInto(Lhs.Assign(newLhs), rhs, tailPosLabels)

            case JSPrivateSelect(qualifier, field) =>
              unnest(qualifier, rhs) { (newQualifier, newRhs, env0) =>
                implicit val env = env0
                js.Assign(
                    genJSPrivateSelect(transformExprNoChar(newQualifier), field)(
                        moduleContext, globalKnowledge, lhs.pos),
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
                  genCallHelper(VarField.superSet, transformExprNoChar(newSuperClass),
                      transformExprNoChar(newQualifier), transformExprNoChar(item),
                      transformExprNoChar(rhs))
              }

            case SelectStatic(item) =>
              if (needToUseGloballyMutableVarSetter(item.name)) {
                unnest(rhs) { (rhs, env0) =>
                  implicit val env = env0
                  js.Apply(globalVar(VarField.u, item.name), transformExpr(rhs, lhs.tpe) :: Nil)
                }
              } else {
                // Assign normally.
                pushLhsInto(Lhs.Assign(lhs), rhs, tailPosLabels)
              }

            case _:VarRef | _:JSGlobalRef =>
              pushLhsInto(Lhs.Assign(lhs), rhs, tailPosLabels)
          }

        case StoreModule() =>
          val enclosingClassName = env.enclosingClassName.getOrElse {
            throw new AssertionError(
                "Need enclosing class for StoreModule().")
          }
          js.Assign(globalVar(VarField.n, enclosingClassName), js.This())

        case While(cond, body) =>
          val loopEnv = env.withInLoopForVarCapture(true)

          /* If there is a Labeled block immediately enclosed within the body
           * of this while loop, acquire its label and use it as the while's
           * label, turning it into a `continue` label.
           */
          val (optLabel, newBody) = {
            body match {
              case Labeled(label, _, innerBody) =>
                val innerBodyEnv = loopEnv
                  .withLabeledExprLHS(label, Lhs.Discard)
                  .withTurnLabelIntoContinue(label)
                  .withDefaultBreakTargets(tailPosLabels)
                  .withDefaultContinueTargets(Set(label))
                val newBody =
                  pushLhsInto(Lhs.Discard, innerBody, Set(label))(innerBodyEnv)
                val optLabel = if (usedLabels.contains(label))
                  Some(transformLabelIdent(label))
                else
                  None
                (optLabel, newBody)

              case _ =>
                val bodyEnv = loopEnv
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
            js.While(transformExprNoChar(cond)(loopEnv), newBody, optLabel)
          } else {
            js.While(js.BooleanLiteral(true), {
              unnest(cond) { (newCond, env0) =>
                implicit val env = env0
                js.If(transformExprNoChar(newCond), newBody, js.Break())
              }(loopEnv)
            }, optLabel)
          }

        case ForIn(obj, keyVar, keyVarOriginalName, body) =>
          unnest(obj) { (newObj, env0) =>
            implicit val env = env0

            val lhs = genEmptyImmutableLet(
                transformLocalVarIdent(keyVar, keyVarOriginalName))
            val bodyEnv = env
              .withDef(keyVar, mutable = false)
              .withInLoopForVarCapture(true)

            js.ForIn(lhs, transformExprNoChar(newObj), {
              transformStat(body, Set.empty)(bodyEnv)
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
                  fileLevelVar(VarField.superClass)
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
              globalKnowledge.getFieldDefs(enclosingClassName)

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
                      genJSPrivateSelect(js.This(), name),
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

            def genUnchecked(): js.Tree = {
              if (esFeatures.esVersion >= ESVersion.ES2015 && semantics.nullPointers == CheckedBehavior.Unchecked)
                genSyntheticPropApply(jsArgs.head, SyntheticProperty.copyTo, jsArgs.tail)
              else
                genCallHelper(VarField.systemArraycopy, jsArgs: _*)
            }

            if (semantics.arrayStores == Unchecked) {
              genUnchecked()
            } else {
              (src.tpe, dest.tpe) match {
                case (PrimArray(srcPrimRef), PrimArray(destPrimRef)) if srcPrimRef == destPrimRef =>
                  genUnchecked()
                case (RefArray(), RefArray()) =>
                  genCallHelper(VarField.systemArraycopyRefs, jsArgs: _*)
                case _ =>
                  genCallHelper(VarField.systemArraycopyFull, jsArgs: _*)
              }
            }
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

        def extractInSyntheticVar(arg: Tree)(implicit env: Env): Tree = {
          implicit val pos = arg.pos

          /* The duplication in the two branches is unfortunate, but any
           * attempt at factorization makes it worse.
           */
          if (isSplitLongType(arg.tpe) && !isRTLongBoxingAvoidable(arg)) {
            // Extract into a jl.Long!, then re-export as a JSBoxedRTLongVarRef
            val temp = newSyntheticVar()
            val computeTemp = pushLhsInto(
                Lhs.VarDef(temp, BoxedRTLongType, mutable = false), arg, Set.empty)
            computeTemp +=: extractedStatements
            Transient(JSBoxedRTLongVarRef(temp))
          } else {
            // Regular extraction
            val temp = newSyntheticVar()
            val computeTemp = pushLhsInto(
                Lhs.VarDef(temp, arg.tpe, mutable = false), arg, Set.empty)
            computeTemp +=: extractedStatements
            Transient(JSVarRef(temp, mutable = false)(arg.tpe))
          }
        }

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

              /* Handle PackLong, Select and RecordSelect first.
               *
               * These are the only trees that both
               * a) have subtrees and
               * b) can be split.
               *
               * All other trees that can be split are atomic. If they cause
               * !keepAsIs, they need to be extracted themselves, rather than
               * extracting their parts.
               */
              case Transient(PackLong(lo, hi)) =>
                val newHi = rec(hi)
                Transient(PackLong(rec(lo), newHi))
              case Select(qualifier, item) if noExtractYet =>
                val newQualifier =
                  if (isSplitLongType(arg.tpe)) extractInSyntheticVar(qualifier)
                  else rec(qualifier)
                Select(newQualifier, item)(arg.tpe)
              case RecordSelect(record, field) if noExtractYet =>
                RecordSelect(rec(record), field)(arg.tpe)

              /* Extract any remaining arguments of type `long`.
               *
               * When we have an argument of type `long`, we will almost always
               * have to split it at the end of the day. As explained in the
               * previous comment on PackLong/Select/RecordSelect, if we get
               * here, we have exhausted all the trees that *could* become
               * splittable if we extracted their parts.
               *
               * It is easier to deal with all of them once and for all at this
               * point. Otherwise, we would need exceptions for longs in many
               * different kinds of trees below, which complicates the logic.
               *
               * If it turns out we didn't need to split it after all, we'll
               * reuse the `JSBoxedRTLongVarRef` as is, without unboxing+boxing
               * it. In that case, we may unnest a little bit too much than
               * necessary, but it won't cause more boxing than necessary.
               */
              case _ if isSplitLongType(arg.tpe) =>
                extractInSyntheticVar(arg)

              case arg @ UnaryOp(op, lhs)
                  if canUnaryOpBeExpression(arg) && (UnaryOp.isPureOp(op) || noExtractYet) =>
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

              case NewArray(tpe, length) =>
                NewArray(tpe, rec(length))
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

              case Closure(
                      flags, captureParams, params, restParam, resultType, body, captureValues) =>
                Closure(
                    flags, captureParams, params, restParam, resultType, body, recs(captureValues))

              case New(className, constr, args) if noExtractYet =>
                New(className, constr, recs(args))
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
              case ApplyTypedClosure(flags, fun, args) if noExtractYet =>
                val newArgs = recs(args)
                ApplyTypedClosure(flags, rec(fun), newArgs)
              case ArraySelect(array, index) if noExtractYet =>
                val newIndex = rec(index)
                ArraySelect(rec(array), newIndex)(arg.tpe)

              case Transient(ExtractLongHi(longValue)) =>
                Transient(ExtractLongHi(rec(longValue)))
              case Transient(Cast(expr, tpe)) =>
                Transient(Cast(rec(expr), tpe))
              case Transient(ZeroOf(runtimeClass)) =>
                Transient(ZeroOf(rec(runtimeClass)))
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
                extractInSyntheticVar(arg)
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

    private def canUnaryOpBeExpression(tree: UnaryOp): Boolean = {
      import UnaryOp._

      tree.op match {
        case Throw =>
          false
        case WrapAsThrowable | UnwrapFromThrowable =>
          isDuplicatable(tree.lhs)
        case _ =>
          true
      }
    }

    /** Common implementation for the functions below.
     *
     *  A pure expression can be moved around or executed twice, because it
     *  will always produce the same result and never have side-effects.
     *  A side-effect free expression can be elided if its result is not used.
     *
     *  By default, trees of type `long` can be considered as expressions only
     *  if they are splittable. With `allowUnsplittableLongs`, that is relaxed,
     *  but only at the top-level of the tree (subtrees of type `long` must
     *  still be splittable).
     */
    private def isExpressionInternal(tree: Tree, allowUnpure: Boolean,
        allowSideEffects: Boolean, allowUnsplittableLongs: Boolean)(
        implicit env: Env): Boolean = {

      require(!allowSideEffects || allowUnpure)

      def allowBehavior(behavior: CheckedBehavior): Boolean =
        allowSideEffects || behavior == Unchecked

      def testJSArg(tree: TreeOrJSSpread): Boolean = tree match {
        case JSSpread(items) => es2015 && test(items)
        case tree: Tree      => test(tree)
      }

      def testNPE(tree: Tree): Boolean = {
        val npeOK = allowBehavior(semantics.nullPointers) || !tree.tpe.isNullable
        npeOK && test(tree)
      }

      def testAll(trees: List[Tree]): Boolean =
        trees.forall(test(_))

      /* allowUnsplittableLongs is only ever relevant at the top-level.
       * In every recursive call of `test`, it must be reset to `false`, hence
       * the default parameter value.
       */
      def test(tree: Tree, allowUnsplittableLongs: Boolean = false): Boolean = tree match {
        // Atomic expressions
        case _: Literal                   => true
        case _: JSNewTarget               => true
        case Transient(GetFPBitsDataView) => true

        // Vars (side-effect free, pure if immutable)
        case VarRef(name) =>
          allowUnpure || !env.isLocalMutable(name)
        case Transient(JSVarRef(_, mutable)) =>
          allowUnpure || !mutable
        case Transient(JSBoxedRTLongVarRef(_)) =>
          true
        case Transient(JSLongArraySelect(_, _)) =>
          allowUnpure

        // Other expressions that can be split if they are longs
        case Transient(PackLong(lo, hi)) =>
          test(lo) && test(hi)
        case Select(qualifier, _) =>
          allowUnpure && testNPE(qualifier) && {
            !isSplitLongType(tree.tpe) || isDuplicatable(qualifier)
          }
        case SelectStatic(_) =>
          allowUnpure
        case RecordSelect(record, _) =>
          test(record)

        // Other trees of type long cannot be split
        case _ if !allowUnsplittableLongs && isSplitLongType(tree.tpe) =>
          false

        case tree @ UnaryOp(op, lhs) if canUnaryOpBeExpression(tree) =>
          if (op == UnaryOp.CheckNotNull)
            testNPE(lhs)
          else if (UnaryOp.isPureOp(op))
            test(lhs)
          else if (UnaryOp.isSideEffectFreeOp(op))
            allowUnpure && test(lhs)
          else
            allowSideEffects && test(lhs)

        // Division and modulo, preserve pureness unless they can divide by 0
        case BinaryOp(
                BinaryOp.Int_/ | BinaryOp.Int_% | BinaryOp.Int_unsigned_/ | BinaryOp.Int_unsigned_%,
                lhs, rhs)
            if !allowSideEffects =>
          rhs match {
            case IntLiteral(r) if r != 0 => test(lhs)
            case _                       => false
          }
        case BinaryOp(
                BinaryOp.Long_/ | BinaryOp.Long_% | BinaryOp.Long_unsigned_/ | BinaryOp.Long_unsigned_%,
                lhs, rhs)
            if !allowSideEffects =>
          rhs match {
            case LongLiteral(r) if r != 0L => test(lhs)
            case _                         => false
          }

        // String_charAt preserves pureness iff the semantics for stringIndexOutOfBounds are unchecked
        case BinaryOp(BinaryOp.String_charAt, lhs, rhs) =>
          allowBehavior(semantics.stringIndexOutOfBounds) && test(lhs) && test(rhs)

        // Binary Class_x operations that can have side effects
        case BinaryOp(BinaryOp.Class_cast, lhs, rhs) =>
          allowBehavior(semantics.asInstanceOfs) && test(lhs) && test(rhs)
        case BinaryOp(BinaryOp.Class_newArray, lhs, rhs) =>
          allowSideEffects && test(lhs) && test(rhs)

        // Expressions preserving pureness (modulo NPE)
        case Block(trees) =>
          testAll(trees)
        case If(cond, thenp, elsep) =>
          /* In theory we could push allowUnsplittableLongs into the branches,
           * as an exception to the only-top-level rule. However, that would
           * muddy the waters and complicate other parts of the codegen.
           */
          test(cond) && test(thenp) && test(elsep) && !isSplitLongType(tree.tpe)
        case BinaryOp(_, lhs, rhs) =>
          test(lhs) && test(rhs)
        case IsInstanceOf(expr, _) =>
          test(expr)

        // Transients preserving pureness (modulo NPE)
        case Transient(ExtractLongHi(longValue)) =>
          test(longValue)
        case Transient(Cast(expr, _)) =>
          test(expr)
        case Transient(ZeroOf(runtimeClass)) =>
          test(runtimeClass) // ZeroOf *assumes* that `runtimeClass ne null`
        case Transient(ObjectClassName(obj)) =>
          test(obj)

        // Expressions preserving side-effect freedom (modulo NPE)
        case ArrayValue(tpe, elems) =>
          allowUnpure && testAll(elems)
        case JSArrayConstr(items) =>
          allowUnpure && (items.forall(testJSArg))
        case tree @ JSObjectConstr(items) =>
          allowUnpure &&
          !doesObjectConstrRequireDesugaring(tree) &&
          items.forall { item =>
            test(item._1) && test(item._2)
          }
        case Closure(flags, captureParams, params, restParam, resultType, body, captureValues) =>
          allowUnpure && testAll(captureValues)

        // Transients preserving side-effect freedom (modulo NPE)
        case Transient(NativeArrayWrapper(elemClass, nativeArray)) =>
          allowUnpure && testNPE(elemClass) && test(nativeArray)
        case Transient(ArrayToTypedArray(expr, primRef)) =>
          allowUnpure && testNPE(expr)

        // Scala expressions that can always have side-effects
        case New(className, constr, args) =>
          allowSideEffects && testAll(args)
        case LoadModule(className) => // unfortunately
          allowSideEffects
        case Apply(_, receiver, method, args) =>
          allowSideEffects && test(receiver) && testAll(args)
        case ApplyStatically(_, receiver, className, method, args) =>
          allowSideEffects && test(receiver) && testAll(args)
        case ApplyStatic(_, className, method, args) =>
          allowSideEffects && testAll(args)
        case ApplyDynamicImport(_, _, _, args) =>
          allowSideEffects && testAll(args)
        case ApplyTypedClosure(_, fun, args) =>
          allowSideEffects && test(fun) && testAll(args)

        // Transients with side effects.
        case Transient(TypedArrayToArray(expr, primRef)) =>
          allowSideEffects && test(expr) // may TypeError

        // Array operations with conditional exceptions
        case NewArray(tpe, length) =>
          allowBehavior(semantics.negativeArraySizes) && allowUnpure && test(length)
        case ArraySelect(array, index) =>
          allowBehavior(semantics.arrayIndexOutOfBounds) && allowUnpure &&
          testNPE(array) && test(index) &&
          !isSplitLongType(tree.tpe) // long ArraySelect is never directly usable; see JSLongArraySelect for details

        // Casts
        case AsInstanceOf(expr, _) =>
          allowBehavior(semantics.asInstanceOfs) && test(expr)

        // JavaScript expressions that can always have side-effects
        case JSAwait(arg) =>
          allowSideEffects && test(arg)
        case SelectJSNativeMember(_, _) =>
          allowSideEffects
        case JSNew(fun, args) =>
          allowSideEffects && test(fun) && (args.forall(testJSArg))
        case Transient(JSNewVararg(ctor, argArray)) =>
          allowSideEffects && test(ctor) && test(argArray)
        case JSPrivateSelect(qualifier, _) =>
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
          allowSideEffects && testAll(captureValues)

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

      test(tree, allowUnsplittableLongs)
    }

    /** Can the given tree be freely duplicated without extra computation?
     *
     *  In practice, this tests whether the tree is a variable reference or a
     *  literal.
     */
    private def isDuplicatable(tree: Tree): Boolean = tree match {
      case VarRef(_) | Transient(JSVarRef(_, _)) | _:Literal => true
      case _                                                 => false
    }

    /** Test whether the given tree is a standard JS expression. */
    def isExpression(tree: Tree)(implicit env: Env): Boolean = {
      isExpressionInternal(
          tree, allowUnpure = true, allowSideEffects = true, allowUnsplittableLongs = false)
    }

    /** Test whether the given tree is a side-effect-free standard JS expression. */
    def isSideEffectFreeExpression(tree: Tree)(implicit env: Env): Boolean = {
      isExpressionInternal(
          tree, allowUnpure = true, allowSideEffects = false, allowUnsplittableLongs = false)
    }

    /** Test whether the given tree is a pure standard JS expression. */
    def isPureExpression(tree: Tree)(implicit env: Env): Boolean = {
      isExpressionInternal(
          tree, allowUnpure = false, allowSideEffects = false, allowUnsplittableLongs = false)
    }

    /** Test whether the given tree is an expression, or an RTLong computation
     *  whose arguments are real expressions.
     *
     *  These can be directly assigned to an `Lhs`, but cannot otherwise be
     *  used as expressions.
     */
    def isExpressionOrLongOpOfExpressions(tree: Tree)(implicit env: Env): Boolean = {
      isExpressionInternal(
          tree, allowUnpure = true, allowSideEffects = true, allowUnsplittableLongs = true)
    }

    /** Test whether, at the top level, the given tree (assumed of type `Long`)
     *  is splittable.
     */
    def isSplittableLongAtTopLevel(tree: Tree)(implicit env: Env): Boolean = {
      tree match {
        case LongLiteral(_)                     => true
        case VarRef(_)                          => true
        case Transient(JSVarRef(_, _))          => true
        case Transient(JSBoxedRTLongVarRef(_))  => true
        case Transient(JSLongArraySelect(_, _)) => true
        case Transient(PackLong(_, _))          => true
        case Select(_, _)                       => true
        case SelectStatic(_)                    => true
        case RecordSelect(_, _)                 => true
        case _                                  => false
      }
    }

    /** Test whether, at the top level, we can avoid boxing the result of the
     *  given `tree`.
     */
    def isRTLongBoxingAvoidable(tree: Tree)(implicit env: Env): Boolean = {
      tree match {
        case _:Labeled | _:If | _:TryCatch | _:TryFinally | _:Match | _:ArraySelect =>
          /* Trees resulting in JS statements we can push the LHS into.
           * See the comment on `JSLongArraySelect` why `ArraySelect` is here.
           */
          true
        case _ =>
          isSplittableLongAtTopLevel(tree)
      }
    }

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

        case LongType if !useBigIntForLongs =>
          val (lo, hi) = transformLongExpr(rhs)
          js.Block(
            genLet(identLongLo(ident), mutable, lo),
            genLet(identLongHi(ident), mutable, hi)
          )

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

        case LongType if !useBigIntForLongs =>
          js.Block(
            genEmptyMutableLet(identLongLo(ident)),
            genEmptyMutableLet(identLongHi(ident))
          )

        case _ =>
          genEmptyMutableLet(ident)
      }
    }

    def doAssign(lhs: Tree, rhs: Tree)(implicit env: Env): js.Tree = {
      implicit val pos = rhs.pos
      lhs.tpe match {
        case RecordType(fields) =>
          val ident = (lhs: @unchecked) match {
            case lhs: VarRef                   => transformLocalVarRefIdent(lhs)
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

        case LongType if !useBigIntForLongs =>
          /* There cannot be any static mirrors here.
           * Only static variables of type `any` can be exported.
           */
          val (lhsLo, lhsHi) = transformLongExpr(lhs)
          val (rhsLo, rhsHi) = transformLongExpr(rhs)
          js.Block(
            js.Assign(lhsLo, rhsLo),
            js.Assign(lhsHi, rhsHi)
          )

        case _ =>
          val base = js.Assign(transformExpr(lhs, preserveChar = true),
              transformExpr(rhs, lhs.tpe))
          lhs match {
            case SelectStatic(FieldIdent(field))
                if moduleKind == ModuleKind.NoModule =>
              val mirrors = globalKnowledge.getStaticFieldMirrors(field)
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

        case recordTree @ VarRef(name) =>
          val jsIdent = transformLocalVarRefIdent(recordTree)
          val mutable = env.isLocalMutable(name)
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

      def doReturnToLabel(label: LabelName): js.Tree = {
        val newLhs = env.lhsForLabeledExpr(label)
        if (newLhs.hasNothingType) {
          /* A touch of peephole dead code elimination.
           * This is actually necessary to avoid dangling breaks to eliminated
           * labels, as in issue #2307.
           */
          pushLhsInto(newLhs, rhs, tailPosLabels)
        } else if (tailPosLabels.contains(label)) {
          pushLhsInto(newLhs, rhs, tailPosLabels)
        } else {
          val body = pushLhsInto(newLhs, rhs, Set.empty)

          val jump = if (env.isDefaultBreakTarget(label)) {
            js.Break(None)
          } else if (env.isDefaultContinueTarget(label)) {
            js.Continue(None)
          } else {
            usedLabels += label
            val transformedLabel = Some(transformLabelIdent(label))
            if (env.isLabelTurnedIntoContinue(label))
              js.Continue(transformedLabel)
            else
              js.Break(transformedLabel)
          }

          js.Block(body, jump)
        }
      }

      def lhsAcceptsUnsplittableRTLong(lhs: Lhs): Boolean = lhs match {
        case Lhs.Discard | Lhs.ReturnFromFunction | Lhs.Throw =>
          true
        case Lhs.VarDef(_, tpe, _) =>
          // Explicitly check the type to allow BoxedRTLongType
          tpe != LongType
        case Lhs.Assign(_) =>
          false
        case Lhs.Return(l) =>
          lhsAcceptsUnsplittableRTLong(env.lhsForLabeledExpr(l))
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

        // Extract a VarDef for unsplittable longs, if the lhs cannot handle them

        case _
            if isSplitLongType(rhs.tpe) && !lhsAcceptsUnsplittableRTLong(lhs) &&
              !isRTLongBoxingAvoidable(rhs) =>
          val temp = newSyntheticVar()
          val computeTemp = pushLhsInto(
              Lhs.VarDef(temp, BoxedRTLongType, mutable = false), rhs, Set.empty)
          js.Block(
            computeTemp,
            redo(Transient(JSBoxedRTLongVarRef(temp)))
          )

        // Base case, rhs is already a regular JS expression

        case _ if isExpressionOrLongOpOfExpressions(rhs) =>
          lhs match {
            case Lhs.Discard =>
              if (isSideEffectFreeExpression(rhs)) js.Skip()
              else transformExpr(rhs, preserveChar = true)
            case Lhs.VarDef(name, tpe, mutable) =>
              doVarDef(name, tpe, mutable, rhs)
            case Lhs.Assign(lhs) =>
              doAssign(lhs, rhs)
            case Lhs.ReturnFromFunction =>
              env.expectedReturnType match {
                case VoidType =>
                  js.Block(transformStat(rhs, tailPosLabels = Set.empty), js.Return(js.Undefined()))
                case LongType if !useBigIntForLongs =>
                  // An RTLong must be boxed
                  js.Return(transformExpr(rhs, BoxedRTLongType))
                case expectedType =>
                  js.Return(transformExpr(rhs, expectedType))
              }
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

            case Lhs.ReturnFromFunction =>
              assert(env.expectedReturnType == VoidType,
                  "Cannot return a record value from a non-void function")
              val (newStats, _) = transformBlockStats(elems)
              js.Block(newStats :+ js.Return(js.Undefined()))

            case Lhs.Throw =>
              throw new AssertionError("Cannot throw a record value.")
          }

        // Control flow constructs

        case Labeled(label, tpe, body) =>
          extractLet { newLhs =>
            val bodyEnv = env.withLabeledExprLHS(label, newLhs)
            val newBody =
              pushLhsInto(newLhs, body, tailPosLabels + label)(bodyEnv)
            if (usedLabels.contains(label))
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

        case JSAwait(arg) =>
          unnest(arg) { (newArg, env) =>
            redo(JSAwait(newArg))(env)
          }

        // Scala expressions (if we reach here their arguments are not expressions)

        case New(className, ctor, args) =>
          unnest(args) { (newArgs, env) =>
            redo(New(className, ctor, newArgs))(env)
          }

        case Select(qualifier, item) =>
          unnest(qualifier) { (newQualifier, newEnv) =>
            implicit val env = newEnv
            if (isSplitLongType(rhs.tpe) && !isDuplicatable(newQualifier)) {
              withTempJSVar(newQualifier) { varRef =>
                redo(Select(varRef, item)(rhs.tpe))
              }
            } else {
              redo(Select(newQualifier, item)(rhs.tpe))
            }
          }

        case Apply(flags, receiver, method, args) =>
          unnest(checkNotNull(receiver), args) { (newReceiver, newArgs, env) =>
            redo(Apply(flags, newReceiver, method, newArgs)(rhs.tpe))(env)
          }

        case ApplyStatically(flags, receiver, className, method, args) =>
          unnest(checkNotNull(receiver), args) { (newReceiver, newArgs, env) =>
            redo(ApplyStatically(flags, newReceiver, className, method,
                newArgs)(rhs.tpe))(
                env)
          }

        case ApplyStatic(flags, className, method, args) =>
          unnest(args) { (newArgs, env) =>
            redo(ApplyStatic(flags, className, method, newArgs)(rhs.tpe))(env)
          }

        case ApplyDynamicImport(flags, className, method, args) =>
          unnest(args) { (newArgs, env) =>
            redo(ApplyDynamicImport(flags, className, method, newArgs))(env)
          }

        case ApplyTypedClosure(flags, fun, args) =>
          unnest(checkNotNull(fun), args) { (newFun, newArgs, env) =>
            redo(ApplyTypedClosure(flags, newFun, newArgs))(env)
          }

        case UnaryOp(op, lhs) =>
          op match {
            case UnaryOp.Throw =>
              pushLhsInto(Lhs.Throw, lhs, tailPosLabels)

            case UnaryOp.WrapAsThrowable | UnaryOp.UnwrapFromThrowable =>
              unnest(lhs) { (newLhs, newEnv) =>
                implicit val env = newEnv
                withTempJSVar(newLhs) { varRef =>
                  redo(UnaryOp(op, varRef))
                }
              }

            case _ =>
              unnest(lhs) { (newLhs, env) =>
                redo(UnaryOp(op, newLhs))(env)
              }
          }

        case BinaryOp(op, lhs, rhs) =>
          unnest(lhs, rhs) { (newLhs, newRhs, env) =>
            redo(BinaryOp(op, newLhs, newRhs))(env)
          }

        case NewArray(tpe, length) =>
          unnest(length) { (newLength, env) =>
            redo(NewArray(tpe, newLength))(env)
          }

        case ArrayValue(tpe, elems) =>
          unnest(elems) { (newElems, env) =>
            redo(ArrayValue(tpe, newElems))(env)
          }

        case rhs @ ArraySelect(array, index) =>
          unnest(checkNotNull(array), index) { (newArray, newIndex, newEnv) =>
            implicit val env = newEnv

            if (!isSplitLongType(rhs.tpe)) {
              redo(ArraySelect(newArray, newIndex)(rhs.tpe))(env)
            } else {
              import TreeDSL._

              val genArray = transformExprNoChar(newArray)
              val genIndex = transformExprNoChar(newIndex)

              withTempJSVar(genSyntheticPropSelect(genArray, SyntheticProperty.u)) { uRef =>
                def redoWithScaledIndex(scaledIndex: js.Tree): js.Tree =
                  redo(Transient(JSLongArraySelect(uRef, scaledIndex)))

                def checkAndScaleIndex: js.Tree =
                  if (semantics.arrayIndexOutOfBounds == CheckedBehavior.Unchecked) genIndex << 1
                  else genCallHelper(VarField.aJCheckGet, uRef, genIndex)

                genIndex match {
                  case js.IntLiteral(genIndexValue) =>
                    // Check index if required, then "constant-fold" it
                    val checkStatement =
                      if (semantics.arrayIndexOutOfBounds == CheckedBehavior.Unchecked) js.Skip()
                      else checkAndScaleIndex
                    js.Block(
                      checkStatement,
                      redoWithScaledIndex(js.IntLiteral(genIndexValue << 1))
                    )
                  case _ =>
                    withTempJSVar(checkAndScaleIndex) { scaledIndex =>
                      redoWithScaledIndex(scaledIndex)
                    }
                }
              }
            }
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

        case Transient(PackLong(lo, hi)) =>
          unnest(lo, hi) { (newLo, newHi, env) =>
            redo(Transient(PackLong(newLo, newHi)))(env)
          }

        case Transient(ExtractLongHi(longValue)) =>
          unnest(longValue) { (newLongValue, env) =>
            redo(Transient(ExtractLongHi(newLongValue)))(env)
          }

        case Transient(Cast(expr, tpe)) =>
          unnest(expr) { (newExpr, env) =>
            redo(Transient(Cast(newExpr, tpe)))(env)
          }

        case Transient(ZeroOf(runtimeClass)) =>
          unnest(runtimeClass) { (newRuntimeClass, env) =>
            redo(Transient(ZeroOf(newRuntimeClass)))(env)
          }

        case Transient(NativeArrayWrapper(elemClass, nativeArray)) =>
          unnest(elemClass, nativeArray) { (newElemClass, newNativeArray, env) =>
            redo(Transient(NativeArrayWrapper(newElemClass, newNativeArray)(rhs.tpe)))(env)
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

        case JSPrivateSelect(qualifier, field) =>
          unnest(qualifier) { (newQualifier, env) =>
            redo(JSPrivateSelect(newQualifier, field))(env)
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

        case Closure(flags, captureParams, params, restParam, resultType, body, captureValues) =>
          unnest(captureValues) { (newCaptureValues, env) =>
            redo(Closure(flags, captureParams, params, restParam, resultType,
                body, newCaptureValues))(
                env)
          }

        case CreateJSClass(className, captureValues) =>
          unnest(captureValues) { (newCaptureValues, env) =>
            redo(CreateJSClass(className, newCaptureValues))(env)
          }

        // Statement-only trees

        case _:Skip | _:VarDef | _:Assign | _:While | _:Debugger |
            _:JSSuperConstructorCall | _:JSDelete | _:StoreModule |
            Transient(_: SystemArrayCopy) =>
          /* Go "back" to transformStat() after having dived into
           * expression statements. This can only happen for Lhs.Discard and
           * for Lhs.Return's whose target is a statement.
           */
          lhs match {
            case Lhs.Discard =>
              transformStat(rhs, tailPosLabels)
            case Lhs.ReturnFromFunction =>
              /* If we get here, it is because desugarToFunctionInternal()
               * found a top-level Labeled and eliminated it. Therefore, unless
               * we're mistaken, by construction we cannot be in tail position
               * of the whole function (otherwise doReturnToLabel would have
               * eliminated the lhs). That means there is no point trying to
               * avoid the `js.Return(js.Undefined())`.
               */
              js.Block(
                  transformStat(rhs, tailPosLabels = Set.empty),
                  js.Return(js.Undefined()))
            case Lhs.Return(l) =>
              doReturnToLabel(l)

            case _:Lhs.VarDef | _:Lhs.Assign | Lhs.Throw =>
              throw new IllegalArgumentException(
                  "Illegal tree in FunctionEmitter.pushLhsInto():\n" +
                  "lhs = " + lhs + "\n" +
                  "rhs = " + rhs + " of class " + rhs.getClass)
          }

        case _ =>
          throw new IllegalArgumentException(
              "Illegal tree in FunctionEmitter.pushLhsInto():\n" +
              "lhs = " + lhs + "\n" +
              "rhs = " + rhs + " of class " + rhs.getClass)
      })
    }

    private def withTempJSVar(value: Tree)(makeBody: Transient => js.Tree)(
        implicit env: Env, pos: Position): js.Tree = {
      val varIdent = newSyntheticVar()
      val varDef = genLet(varIdent, mutable = false, transformExpr(value, value.tpe))
      val body = makeBody(Transient(JSVarRef(varIdent, mutable = false)(value.tpe)))
      js.Block(varDef, body)
    }

    private def withTempJSVar(value: js.Tree)(makeBody: js.VarRef => js.Tree)(
        implicit pos: Position): js.Tree = {
      val varIdent = newSyntheticVar()
      val varDef = genLet(varIdent, mutable = false, value)
      val body = makeBody(js.VarRef(varIdent))
      js.Block(varDef, body)
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
      if (args.forall(a => a.tpe != CharType && !isSplitLongType(a.tpe))) {
        // Fast path
        args.map(transformExpr(_, preserveChar = true))
      } else {
        args.zip(methodName.paramTypeRefs).flatMap {
          case (arg, CharRef) =>
            transformExpr(arg, preserveChar = true) :: Nil
          case (arg, LongRef) if !useBigIntForLongs =>
            val (lo, hi) = transformLongExpr(arg)
            List(lo, hi)
          case (arg, _) =>
            transformExpr(arg, preserveChar = false) :: Nil
        }
      }
    }

    def transformTypedArgs(paramTypes: List[Type], args: List[Tree])(
        implicit env: Env): List[js.Tree] = {
      args.zip(paramTypes).flatMap {
        case (arg, LongType) if !useBigIntForLongs =>
          val (lo, hi) = transformLongExpr(arg)
          List(lo, hi)
        case (arg, paramType) =>
          transformExpr(arg, paramType) :: Nil
      }
    }

    /** Desugar an expression of the IR into JavaScript.
     *
     *  With RuntimeLong, expressions of type `long` will be emitted in their
     *  boxed form by this method. So it is only valid if the expected type is
     *  a supertype of `jl.Long!`.
     */
    def transformExpr(tree: Tree, preserveChar: Boolean)(
        implicit env: Env): js.Tree = {

      import TreeDSL._

      implicit val pos = tree.pos

      def or0(tree: js.Tree): js.Tree =
        js.BinaryOp(JSBinaryOp.|, tree, js.IntLiteral(0))

      def shr0(tree: js.Tree): js.Tree = tree match {
        case js.IntLiteral(value) =>
          js.UintLiteral(value)
        case _ =>
          js.BinaryOp(JSBinaryOp.>>>, tree, js.IntLiteral(0))
      }

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
        // For splittable Long expressions, reuse the codegen in transformLongExpr

        case _ if isSplitLongType(tree.tpe) && isSplittableLongAtTopLevel(tree) =>
          tree match {
            case Transient(JSBoxedRTLongVarRef(name)) =>
              js.VarRef(name)
            case _ =>
              val (lo, hi) = transformLongExpr(tree)
              genCallHelper(VarField.bL, lo, hi)
          }

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

        case JSAwait(arg) =>
          js.Await(transformExprNoChar(arg))

        // Scala expressions

        case New(className, ctor, args) =>
          val newArgs = transformTypedArgs(ctor.name, args)
          genScalaClassNew(className, ctor.name, newArgs: _*)

        case LoadModule(className) =>
          genLoadModule(className)

        case Select(qualifier, field) =>
          genSelect(transformExprNoChar(checkNotNull(qualifier)), field)

        case SelectStatic(item) =>
          globalVar(VarField.t, item.name)

        case SelectJSNativeMember(className, member) =>
          val jsNativeLoadSpec =
            globalKnowledge.getJSNativeLoadSpec(className, member.name)
          extractWithGlobals(genLoadJSFromSpec(jsNativeLoadSpec))

        case Apply(_, receiver, method, args) =>
          val methodName = method.name

          def newNormalReceiver: js.Tree =
            transformExprNoChar(checkNotNull(receiver))

          val newArgs = transformTypedArgs(method.name, args)

          def genNormalApply(): js.Tree =
            js.Apply(newNormalReceiver DOT genMethodIdent(method), newArgs)

          def genDispatchApply(): js.Tree =
            js.Apply(globalVar(VarField.dp, methodName), newNormalReceiver :: newArgs)

          def genHijackedMethodApply(className: ClassName): js.Tree = {
            className match {
              case BoxedLongClass if !useBigIntForLongs =>
                if (receiver.tpe == LongType) {
                  val (lo, hi) = transformLongExpr(receiver)
                  genApplyStaticLike(VarField.f, className, method, lo :: hi :: newArgs)
                } else {
                  /* Abuse the dispatch method to extract the lo and hi fields
                   * while preserving evaluation order. This is not efficient,
                   * but it only happens when we do not use the optimizer.
                   */
                  genDispatchApply()
                }
              case BoxedCharacterClass =>
                /* When statically calling a (hijacked) method of j.l.Character,
                 * the receiver must be passed as a primitive CharType. If it is
                 * not already a CharType, we must introduce a cast to unbox the
                 * value.
                 */
                val charReceiver =
                  if (receiver.tpe == CharType) receiver
                  else Transient(Cast(checkNotNull(receiver), CharType))
                val newRec = transformExpr(charReceiver, preserveChar = true)
                genApplyStaticLike(VarField.f, className, method, newRec :: newArgs)
              case _ =>
                /* For other primitive types, unboxes/casts are not necessary,
                 * because they would only convert `null` to the zero value of
                 * the type. However, `null` is ruled out by `checkNotNull` (or
                 * because it is UB).
                 */
                genApplyStaticLike(VarField.f, className, method, newNormalReceiver :: newArgs)
            }
          }

          if (isMaybePrimitive(receiver.tpe) && !methodName.isReflectiveProxy) {
            receiver.tpe match {
              case AnyType | AnyNotNullType =>
                genDispatchApply()

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

              case ClassType(className, _, _) if !HijackedClasses.contains(className) =>
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
          val newReceiver = transformExprNoChar(checkNotNull(receiver))
          val newArgs = transformTypedArgs(method.name, args)
          val transformedArgs = newReceiver :: newArgs

          if (flags.isConstructor) {
            genApplyStaticLike(VarField.ct, className, method, transformedArgs)
          } else if (flags.isPrivate) {
            genApplyStaticLike(VarField.p, className, method, transformedArgs)
          } else if (globalKnowledge.isInterface(className)) {
            genApplyStaticLike(VarField.f, className, method, transformedArgs)
          } else {
            val fun =
              globalVar(VarField.c, className).prototype DOT genMethodIdent(method)
            js.Apply(fun DOT "call", transformedArgs)
          }

        case ApplyStatic(flags, className, method, args) =>
          genApplyStaticLike(
              if (flags.isPrivate) VarField.ps else VarField.s,
              className,
              method,
              transformTypedArgs(method.name, args))

        case tree: ApplyDynamicImport =>
          transformApplyDynamicImport(tree)

        case ApplyTypedClosure(_, fun, args) =>
          val newFun = transformExprNoChar(checkNotNull(fun))
          val newArgs = fun.tpe match {
            case ClosureType(paramTypes, _, _) =>
              transformTypedArgs(paramTypes, args)
            case NothingType | NullType =>
              args.map(transformExpr(_, preserveChar = true))
            case _ =>
              throw new AssertionError(
                  s"Unexpected type for the fun of ApplyTypedClosure: ${fun.tpe}")
          }
          js.Apply.makeProtected(newFun, newArgs)

        case UnaryOp(op, lhs) =>
          import UnaryOp._

          def newLhs: js.Tree =
            transformExpr(lhs, preserveChar = (op == CharToInt || op == CheckNotNull))

          def requireDuplicatableNewLhs(): js.Tree = {
            val result = newLhs // see `def newLhs` above; it is non-trivial
            assert(isDuplicatable(lhs), s"$result is not duplicatable at $pos")
            result
          }

          def rtLongOp(rtLongMethodName: MethodName): js.Tree = {
            val (lo, hi) = transformLongExpr(lhs)
            genLongApplyStatic(rtLongMethodName, lo, hi)
          }

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
                genLongApplyStatic(LongImpl.fromInt, newLhs)

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
              if (useBigIntForLongs) {
                js.Apply(genGlobalVarRef("Number"), List(wrapBigInt32(newLhs)))
              } else {
                val (lo, hi) = transformLongExpr(lhs)
                hi match {
                  case _:js.VarRef | _:js.IntLiteral =>
                    // we can safely drop the hi word
                    lo
                  case _ =>
                    genLongApplyStatic(LongImpl.toInt, lo, hi)
                }
              }
            case DoubleToInt =>
              genCallHelper(VarField.doubleToInt, newLhs)
            case DoubleToFloat =>
              genFround(newLhs)

            // Long <-> Double (neither widening nor narrowing)
            case LongToDouble =>
              if (useBigIntForLongs)
                js.Apply(genGlobalVarRef("Number"), List(newLhs))
              else
                rtLongOp(LongImpl.toDouble)
            case DoubleToLong =>
              if (useBigIntForLongs)
                genCallHelper(VarField.doubleToLong, newLhs)
              else
                genLongApplyStatic(LongImpl.fromDouble, newLhs)

            // Long -> Float (neither widening nor narrowing)
            case LongToFloat =>
              if (useBigIntForLongs)
                genCallHelper(VarField.longToFloat, newLhs)
              else
                rtLongOp(LongImpl.toFloat)

            // String.length
            case String_length =>
              genIdentBracketSelect(newLhs, "length")

            // Null check
            case CheckNotNull =>
              if (semantics.nullPointers == CheckedBehavior.Unchecked)
                newLhs
              else
                genCallHelper(VarField.n, newLhs)

            // Class operations
            case Class_name =>
              genGetDataOf(newLhs) DOT cpn.name
            case Class_isPrimitive =>
              genGetDataOf(newLhs) DOT cpn.isPrimitive
            case Class_isInterface =>
              genGetDataOf(newLhs) DOT cpn.isInterface
            case Class_isArray =>
              genGetDataOf(newLhs) DOT cpn.isArrayClass
            case Class_componentType =>
              js.Apply(genGetDataOf(newLhs) DOT cpn.getComponentType, Nil)
            case Class_superClass =>
              js.Apply(genGetDataOf(newLhs) DOT cpn.getSuperclass, Nil)

            case Array_length =>
              val rawLength = genIdentBracketSelect(
                  genSyntheticPropSelect(newLhs, SyntheticProperty.u),
                  "length")
              lhs.tpe match {
                case ArrayType(ArrayTypeRef(LongRef, 1), _, _) if !useBigIntForLongs =>
                  or0(rawLength >>> js.IntLiteral(1))
                case _ =>
                  rawLength
              }

            case GetClass =>
              genCallHelper(VarField.objectGetClass, newLhs)

            case Clone =>
              lhs.tpe match {
                /* If the argument is known to be an array, directly call its
                 * `clone__O` method.
                 * This happens all the time when calling `clone()` on an array,
                 * since the optimizer will inline `java.lang.Object.clone()` in
                 * those cases, leaving a `Clone()` node an array.
                 */
                case _: ArrayType =>
                  genApply(newLhs, cloneMethodName, Nil)

                /* Otherwise, if it might be an array, use the full dispatcher.
                 * In theory, only the `CloneableClass` case is required, since
                 * `Clone` only accepts values of type `Cloneable`. However, since
                 * the inliner does not always refine the type of receivers, we
                 * also account for other supertypes of array types. There is a
                 * similar issue for CharSequenceClass in `Apply` nodes.
                 *
                 * TODO Is the above comment still relevant now that the optimizer
                 * is type-preserving?
                 *
                 * In practice, this only happens in the (non-inlined) definition
                 * of `java.lang.Object.clone()` itself, since everywhere else it
                 * is inlined in contexts where the receiver has a more precise
                 * type.
                 */
                case ClassType(CloneableClass, _, false) | ClassType(SerializableClass, _, false) |
                    ClassType(ObjectClass, _, false) | AnyType | AnyNotNullType =>
                  genCallHelper(VarField.objectOrArrayClone, newLhs)

                // Otherwise, it is known not to be an array.
                case _ =>
                  genCallHelper(VarField.objectClone, newLhs)
              }

            case IdentityHashCode =>
              genCallHelper(VarField.systemIdentityHashCode, newLhs)

            case WrapAsThrowable =>
              val newLhs = requireDuplicatableNewLhs()
              js.If(
                  genIsInstanceOfClass(newLhs, ThrowableClass),
                  newLhs,
                  genScalaClassNew(JavaScriptExceptionClass, AnyArgConstructorName, newLhs))

            case UnwrapFromThrowable =>
              val newLhs = requireDuplicatableNewLhs()
              js.If(
                  genIsInstanceOfClass(newLhs, JavaScriptExceptionClass),
                  genSelect(newLhs, FieldIdent(exceptionFieldName)),
                  newLhs)

            // Floating point bit manipulation
            case Float_toBits =>
              genCallHelper(VarField.floatToBits, newLhs)
            case Float_fromBits =>
              genCallHelper(VarField.floatFromBits, newLhs)
            case Double_toBits =>
              genCallHelper(VarField.doubleToBits, newLhs)
            case Double_fromBits =>
              /* TODO Ideally we should avoid boxing into a Long pair.
               * However, when the optimizer is enabled, this only happens when
               * targeting ES 5.1. It is probably not worth the trouble.
               */
              genCallHelper(VarField.doubleFromBits, newLhs)

            // clz
            case Int_clz =>
              genCallPolyfillableBuiltin(PolyfillableBuiltin.Clz32Builtin, newLhs)
            case Long_clz =>
              if (useBigIntForLongs)
                genCallHelper(VarField.longClz, newLhs)
              else
                rtLongOp(LongImpl.clz)

            case UnsignedIntToLong =>
              if (useBigIntForLongs)
                js.Apply(genGlobalVarRef("BigInt"), List(shr0(newLhs)))
              else
                genLongApplyStatic(LongImpl.fromUnsignedInt, newLhs)
          }

        case BinaryOp(op, lhs, rhs) =>
          import BinaryOp._

          def newLhs: js.Tree = transformExprNoChar(lhs)
          def newRhs: js.Tree = transformExprNoChar(rhs)

          def extractClassData(origTree: Tree, jsTree: js.Tree): js.Tree = origTree match {
            case ClassOf(typeRef) => genClassDataOf(typeRef)(implicitly, implicitly, origTree.pos)
            case _                => genGetDataOf(jsTree)
          }

          def rtLongLongOp(rtLongMethodName: MethodName): js.Tree = {
            val (lhsLo, lhsHi) = transformLongExpr(lhs)
            val (rhsLo, rhsHi) = transformLongExpr(rhs)
            genLongApplyStatic(rtLongMethodName, lhsLo, lhsHi, rhsLo, rhsHi)
          }

          def rtLongIntOp(rtLongMethodName: MethodName): js.Tree = {
            val (lhsLo, lhsHi) = transformLongExpr(lhs)
            genLongApplyStatic(rtLongMethodName, lhsLo, lhsHi, newRhs)
          }

          def longComparisonOp(bigIntBinaryOp: JSBinaryOp.Code,
              rtLongMethodName: MethodName): js.Tree = {
            if (useBigIntForLongs)
              js.BinaryOp(bigIntBinaryOp, newLhs, newRhs)
            else
              rtLongLongOp(rtLongMethodName)
          }

          def unsignedLongComparisonOp(bigIntBinaryOp: JSBinaryOp.Code,
              rtLongMethodName: MethodName): js.Tree = {
            if (useBigIntForLongs)
              js.BinaryOp(bigIntBinaryOp, wrapBigIntU64(newLhs), wrapBigIntU64(newRhs))
            else
              rtLongLongOp(rtLongMethodName)
          }

          (op: @switch) match {
            case === | !== =>
              /* Semantically, this is an `Object.is` test in JS. However, we
               * optimize it as a primitive JS strict equality (`===`) when
               * possible.
               *
               * The optimizer does not do this optimization because:
               *
               * - it partly relies on the specifics of how hijacked classes are encoded in JS
               *   (see the `ClassType(ObjectClass)` case in `canBePrimitiveNum`),
               * - if handled in the optimizer, `JSBinaryOp`s become frequent
               *   and require additional infrastructure to optimize common patterns, and
               * - it is very specific to *JavaScript*, and is actually detrimental in Wasm.
               */

              def canBePrimitiveNum(tree: Tree): Boolean = tree.tpe match {
                case AnyType | ByteType | ShortType | IntType | FloatType | DoubleType =>
                  true
                case ClassType(_, _, true) =>
                  /* Per subtyping rules, primitives upcast to their hijacked
                   * classes are never exact. Therefore, exact class types
                   * cannot be primitive numbers.
                   */
                  false
                case ClassType(ObjectClass, _, false) =>
                  /* Due to how hijacked classes are encoded in JS, we know
                   * that in `java.lang.Object` itself, `this` can never be a
                   * primitive. It will always be a proper Scala.js object.
                   *
                   * Exempting `this` in `java.lang.Object` is important so
                   * that the body of `Object.equals__O__Z` can be compiled as
                   * `this === that` instead of `Object.is(this, that)`.
                   */
                  tree match {
                    case This() => false
                    case _      => true
                  }
                case ClassType(
                        BoxedByteClass | BoxedShortClass |
                        BoxedIntegerClass | BoxedFloatClass | BoxedDoubleClass,
                        _, false) =>
                  true
                case ClassType(className, _, false) =>
                  globalKnowledge.isAncestorOfHijackedClass(BoxedDoubleClass)
                case _ =>
                  false
              }

              def isWhole(tree: Tree): Boolean = tree.tpe match {
                case ByteType | ShortType | IntType =>
                  true
                case ClassType(className, _, _) =>
                  className == BoxedByteClass ||
                  className == BoxedShortClass ||
                  className == BoxedIntegerClass
                case _ =>
                  false
              }

              val canOptimizeAsJSStrictEq = {
                !canBePrimitiveNum(lhs) ||
                !canBePrimitiveNum(rhs) ||
                (isWhole(lhs) && isWhole(rhs))
              }

              if (canOptimizeAsJSStrictEq) {
                js.BinaryOp(if (op == ===) JSBinaryOp.=== else JSBinaryOp.!==,
                    newLhs, newRhs)
              } else {
                val objectIsCall =
                  genCallPolyfillableBuiltin(ObjectIsBuiltin, newLhs, newRhs)
                if (op == ===) objectIsCall
                else js.UnaryOp(JSUnaryOp.!, objectIsCall)
              }

            case Int_== | Double_== | Boolean_== =>
              js.BinaryOp(JSBinaryOp.===, newLhs, newRhs)
            case Int_!= | Double_!= | Boolean_!= =>
              js.BinaryOp(JSBinaryOp.!==, newLhs, newRhs)

            case String_+ =>
              def transformToString(arg: Tree): js.Tree = arg.tpe match {
                case CharType =>
                  genCallHelper(VarField.charToString, transformExpr(arg, preserveChar = true))
                case LongType if !useBigIntForLongs =>
                  val (lo, hi) = transformLongExpr(arg)
                  genLongApplyStatic(LongImpl.toString_, lo, hi)
                case _ =>
                  transformExprNoChar(arg)
              }

              def knownString(tpe: Type): Boolean = tpe match {
                case StringType | CharType | LongType => true
                case _                                => false
              }

              val lhsString = transformToString(lhs)
              val rhsString = transformToString(rhs)
              if (knownString(lhs.tpe) || knownString(rhs.tpe))
                lhsString + rhsString
              else
                (js.StringLiteral("") + lhsString) + rhsString

            case Int_+ =>
              lhs match {
                case IntLiteral(l) if l < 0 && l != Int.MinValue =>
                  // Print `(b - a) | 0` instead of `((-a) + b) | 0` when `a` is a literal
                  or0(js.BinaryOp(JSBinaryOp.-, newRhs, js.IntLiteral(-l)(lhs.pos)))
                case _ =>
                  or0(js.BinaryOp(JSBinaryOp.+, newLhs, newRhs))
              }
            case Int_- =>
              lhs match {
                case IntLiteral(0) => or0(js.UnaryOp(JSUnaryOp.-, newRhs))
                case _             => or0(js.BinaryOp(JSBinaryOp.-, newLhs, newRhs))
              }
            case Int_* =>
              genCallPolyfillableBuiltin(ImulBuiltin, newLhs, newRhs)
            case Int_/ | Int_% | Int_unsigned_/ | Int_unsigned_% =>
              val newRhs1 = rhs match {
                case IntLiteral(r) if r != 0 => newRhs
                case _                       => genCallHelper(VarField.checkIntDivisor, newRhs)
              }
              or0((op: @switch) match {
                case Int_/          => js.BinaryOp(JSBinaryOp./, newLhs, newRhs1)
                case Int_%          => js.BinaryOp(JSBinaryOp.%, newLhs, newRhs1)
                case Int_unsigned_/ => js.BinaryOp(JSBinaryOp./, shr0(newLhs), shr0(newRhs1))
                case Int_unsigned_% => js.BinaryOp(JSBinaryOp.%, shr0(newLhs), shr0(newRhs1))
              })

            case Int_| => js.BinaryOp(JSBinaryOp.|, newLhs, newRhs)
            case Int_& => js.BinaryOp(JSBinaryOp.&, newLhs, newRhs)
            case Int_^ =>
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
              if (useBigIntForLongs) {
                lhs match {
                  case LongLiteral(l) if l < 0L && l != Long.MinValue =>
                    // Print `asIntN(64, b - a)` instead of `asIntN(64, (-a) + b))` when `a` is a literal
                    wrapBigInt64(js.BinaryOp(JSBinaryOp.-, newRhs, js.BigIntLiteral(-l)(lhs.pos)))
                  case _ =>
                    wrapBigInt64(js.BinaryOp(JSBinaryOp.+, newLhs, newRhs))
                }
              } else {
                rtLongLongOp(LongImpl.add)
              }
            case Long_- =>
              if (useBigIntForLongs) {
                lhs match {
                  case LongLiteral(0L) =>
                    wrapBigInt64(js.UnaryOp(JSUnaryOp.-, newRhs))
                  case _ =>
                    wrapBigInt64(js.BinaryOp(JSBinaryOp.-, newLhs, newRhs))
                }
              } else {
                /* RuntimeLong does not have a dedicated method for 0L - b.
                 * The regular expansion done by the optimizer for the binary
                 * form is already optimal.
                 * So we don't special-case it here either.
                 */
                rtLongLongOp(LongImpl.sub)
              }
            case Long_* =>
              if (useBigIntForLongs)
                wrapBigInt64(js.BinaryOp(JSBinaryOp.*, newLhs, newRhs))
              else
                rtLongLongOp(LongImpl.mul)
            case Long_/ | Long_% | Long_unsigned_/ | Long_unsigned_% =>
              if (useBigIntForLongs) {
                val newRhs1 = rhs match {
                  case LongLiteral(r) if r != 0L => newRhs
                  case _                         => genCallHelper(VarField.checkLongDivisor, newRhs)
                }
                wrapBigInt64((op: @switch) match {
                  case Long_/          => js.BinaryOp(JSBinaryOp./, newLhs, newRhs1)
                  case Long_%          => js.BinaryOp(JSBinaryOp.%, newLhs, newRhs1)
                  case Long_unsigned_/ =>
                    js.BinaryOp(JSBinaryOp./, wrapBigIntU64(newLhs), wrapBigIntU64(newRhs1))
                  case Long_unsigned_% =>
                    js.BinaryOp(JSBinaryOp.%, wrapBigIntU64(newLhs), wrapBigIntU64(newRhs1))
                })
              } else {
                // The zero divisor check is performed by the implementation methods
                val implMethodName = (op: @switch) match {
                  case Long_/          => LongImpl.divide
                  case Long_%          => LongImpl.remainder
                  case Long_unsigned_/ => LongImpl.divideUnsigned
                  case Long_unsigned_% => LongImpl.remainderUnsigned
                }
                rtLongLongOp(implMethodName)
              }

            case Long_| =>
              if (useBigIntForLongs)
                wrapBigInt64(js.BinaryOp(JSBinaryOp.|, newLhs, newRhs))
              else
                rtLongLongOp(LongImpl.or)
            case Long_& =>
              if (useBigIntForLongs)
                wrapBigInt64(js.BinaryOp(JSBinaryOp.&, newLhs, newRhs))
              else
                rtLongLongOp(LongImpl.and)
            case Long_^ =>
              if (useBigIntForLongs) {
                lhs match {
                  case LongLiteral(-1L) =>
                    wrapBigInt64(js.UnaryOp(JSUnaryOp.~, newRhs))
                  case _ =>
                    wrapBigInt64(js.BinaryOp(JSBinaryOp.^, newLhs, newRhs))
                }
              } else {
                /* RuntimeLong does not have a dedicated method for -1L ^ b.
                 * The regular expansion done by the optimizer for the binary
                 * form is already optimal.
                 * So we don't special-case it here either.
                 */
                rtLongLongOp(LongImpl.xor)
              }
            case Long_<< =>
              if (useBigIntForLongs)
                wrapBigInt64(js.BinaryOp(JSBinaryOp.<<, newLhs, bigIntShiftRhs(newRhs)))
              else
                rtLongIntOp(LongImpl.shl)
            case Long_>>> =>
              if (useBigIntForLongs)
                wrapBigInt64(js.BinaryOp(JSBinaryOp.>>, wrapBigIntU64(newLhs), bigIntShiftRhs(newRhs)))
              else
                rtLongIntOp(LongImpl.shr)
            case Long_>> =>
              if (useBigIntForLongs)
                wrapBigInt64(js.BinaryOp(JSBinaryOp.>>, newLhs, bigIntShiftRhs(newRhs)))
              else
                rtLongIntOp(LongImpl.sar)

            case Long_== =>
              longComparisonOp(JSBinaryOp.===, LongImpl.equals_)
            case Long_!= =>
              longComparisonOp(JSBinaryOp.!==, LongImpl.notEquals)
            case Long_< =>
              longComparisonOp(JSBinaryOp.<, LongImpl.lt)
            case Long_<= =>
              longComparisonOp(JSBinaryOp.<=, LongImpl.le)
            case Long_> =>
              longComparisonOp(JSBinaryOp.>, LongImpl.gt)
            case Long_>= =>
              longComparisonOp(JSBinaryOp.>=, LongImpl.ge)

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

            case String_charAt =>
              semantics.stringIndexOutOfBounds match {
                case CheckedBehavior.Compliant | CheckedBehavior.Fatal =>
                  genCallHelper(VarField.charAt, newLhs, newRhs)
                case CheckedBehavior.Unchecked =>
                  js.Apply(genIdentBracketSelect(newLhs, "charCodeAt"), List(newRhs))
              }

            case Class_isInstance =>
              js.Apply(extractClassData(lhs, newLhs) DOT cpn.isInstance, newRhs :: Nil)
            case Class_isAssignableFrom =>
              js.Apply(extractClassData(lhs, newLhs) DOT cpn.isAssignableFrom,
                  extractClassData(rhs, newRhs) :: Nil)
            case Class_cast =>
              if (semantics.asInstanceOfs == CheckedBehavior.Unchecked)
                js.Block(newLhs, newRhs)
              else
                js.Apply(extractClassData(lhs, newLhs) DOT cpn.cast, newRhs :: Nil)
            case Class_newArray =>
              js.Apply(extractClassData(lhs, newLhs) DOT cpn.newArray, newRhs :: Nil)

            case Int_unsigned_<  => js.BinaryOp(JSBinaryOp.<, shr0(newLhs), shr0(newRhs))
            case Int_unsigned_<= => js.BinaryOp(JSBinaryOp.<=, shr0(newLhs), shr0(newRhs))
            case Int_unsigned_>  => js.BinaryOp(JSBinaryOp.>, shr0(newLhs), shr0(newRhs))
            case Int_unsigned_>= => js.BinaryOp(JSBinaryOp.>=, shr0(newLhs), shr0(newRhs))

            case Long_unsigned_< =>
              unsignedLongComparisonOp(JSBinaryOp.<, LongImpl.ltu)
            case Long_unsigned_<= =>
              unsignedLongComparisonOp(JSBinaryOp.<=, LongImpl.leu)
            case Long_unsigned_> =>
              unsignedLongComparisonOp(JSBinaryOp.>, LongImpl.gtu)
            case Long_unsigned_>= =>
              unsignedLongComparisonOp(JSBinaryOp.>=, LongImpl.geu)
          }

        case NewArray(typeRef, length) =>
          js.New(genArrayConstrOf(typeRef), transformExprNoChar(length) :: Nil)

        case ArrayValue(ArrayTypeRef(primRef: PrimRef, 1), elems)
            if shouldGenerateAsConstantArray(primRef, elems) =>
          genConstantArray(primRef, elems)

        case ArrayValue(typeRef, elems) =>
          val newElems = typeRef match {
            case ArrayTypeRef(CharRef, 1) =>
              elems.map(transformExpr(_, preserveChar = true))
            case ArrayTypeRef(LongRef, 1) if !useBigIntForLongs =>
              elems.flatMap { elem =>
                val (elemLo, elemHi) = transformLongExpr(elem)
                List(elemLo, elemHi)
              }
            case _ =>
              elems.map(transformExprNoChar(_))
          }
          extractWithGlobals(genArrayValue(typeRef, newElems))

        case ArraySelect(array, index) =>
          val newArray = transformExprNoChar(checkNotNull(array))
          val newIndex = transformExprNoChar(index)
          semantics.arrayIndexOutOfBounds match {
            case CheckedBehavior.Compliant | CheckedBehavior.Fatal =>
              genSyntheticPropApply(newArray, SyntheticProperty.get, newIndex)
            case CheckedBehavior.Unchecked =>
              js.BracketSelect(genSyntheticPropSelect(newArray, SyntheticProperty.u), newIndex)
          }

        case tree: RecordSelect =>
          js.VarRef(makeRecordFieldIdentForVarRef(tree))

        case IsInstanceOf(expr, testType) =>
          genIsInstanceOf(transformExprNoChar(expr), testType)

        case AsInstanceOf(expr, tpe) =>
          extractWithGlobals(genAsInstanceOf(transformExprNoChar(expr), tpe))

        // Transients

        case Transient(ExtractLongHi(longValue)) =>
          assert(!useBigIntForLongs, "RuntimeLong only")
          val (lo, hi) = transformLongExpr(longValue)
          lo match {
            case _:js.VarRef | _:js.IntLiteral =>
              // we can safely drop the lo word
              hi
            case _ =>
              js.Block(lo, hi)
          }

        case Transient(Cast(expr, tpe)) =>
          val newExpr = transformExpr(expr, preserveChar = true)
          if (tpe == CharType && expr.tpe != CharType)
            newExpr DOT cpn.c
          else
            newExpr

        case Transient(ZeroOf(runtimeClass)) =>
          js.DotSelect(
              genGetDataOf(transformExprNoChar(checkNotNull(runtimeClass))),
              js.Ident(cpn.zero))

        case Transient(NativeArrayWrapper(elemClass, nativeArray)) =>
          val newNativeArray = transformExprNoChar(nativeArray)
          elemClass match {
            case ClassOf(elemTypeRef) if (elemTypeRef != LongRef) || useBigIntForLongs =>
              val arrayTypeRef = ArrayTypeRef.of(elemTypeRef)
              extractWithGlobals(
                  genNativeArrayWrapper(arrayTypeRef, newNativeArray))
            case _ =>
              val elemClassData =
                genGetDataOf(transformExprNoChar(checkNotNull(elemClass)))
              val arrayClassData = js.Apply(
                  js.DotSelect(elemClassData, js.Ident(cpn.getArrayOf)), Nil)
              js.Apply(arrayClassData DOT cpn.wrapArray, newNativeArray :: Nil)
          }

        case Transient(ObjectClassName(obj)) =>
          genCallHelper(VarField.objectClassName, transformExprNoChar(obj))

        case Transient(GetFPBitsDataView) =>
          globalVar(VarField.fpBitsDataView, CoreVar)

        case Transient(ArrayToTypedArray(expr, primRef)) =>
          val value = transformExprNoChar(checkNotNull(expr))
          val valueUnderlying = genSyntheticPropSelect(value, SyntheticProperty.u)

          if (es2015) {
            js.Apply(genIdentBracketSelect(valueUnderlying, "slice"), Nil)
          } else {
            val typedArrayClass = extractWithGlobals(typedArrayRef(primRef).get)
            js.New(typedArrayClass, valueUnderlying :: Nil)
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
            js.Apply(
                genIdentBracketSelect(
                    genIdentBracketSelect(genGlobalVarRef("Array").prototype, "slice"), "call"),
                value :: Nil)
          }
          js.New(genArrayConstrOf(ArrayTypeRef(primRef, 1)), arrayValue :: Nil)

        // JavaScript expressions

        case JSNew(constr, args) =>
          js.New(transformExprNoChar(constr), args.map(transformJSArg))

        case Transient(JSNewVararg(constr, argsArray)) =>
          assert(!es2015, s"generated a JSNewVargs with ES 2015+ at ${tree.pos}")
          genCallHelper(VarField.newJSObjectWithVarargs,
              transformExprNoChar(constr), transformExprNoChar(argsArray))

        case JSPrivateSelect(qualifier, field) =>
          genJSPrivateSelect(transformExprNoChar(qualifier), field)

        case JSSelect(qualifier, item) =>
          genBracketSelect(transformExprNoChar(qualifier),
              transformExprNoChar(item))

        case JSFunctionApply(fun, args) =>
          js.Apply.makeProtected(transformExprNoChar(fun), args.map(transformJSArg))

        case JSMethodApply(receiver, method, args) =>
          js.Apply(
              genBracketSelect(transformExprNoChar(receiver),
                  transformExprNoChar(method)),
              args.map(transformJSArg))

        case JSSuperSelect(superClass, qualifier, item) =>
          genCallHelper(VarField.superGet, transformExprNoChar(superClass),
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
              extractWithGlobals(genLoadJSFromSpec(spec))
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
          if (name == JSGlobalRef.FileLevelThis)
            globalVar(VarField.fileLevelThis, CoreVar)
          else
            js.VarRef(transformGlobalVarIdent(name))

        case JSTypeOfGlobalRef(globalRef) =>
          js.UnaryOp(JSUnaryOp.typeof, transformExprNoChar(globalRef))

        // Literals

        case Undefined()           => js.Undefined()
        case Null()                => js.Null()
        case BooleanLiteral(value) => js.BooleanLiteral(value)
        case CharLiteral(value)    => js.IntLiteral(value.toInt)
        case ByteLiteral(value)    => js.IntLiteral(value.toInt)
        case ShortLiteral(value)   => js.IntLiteral(value.toInt)
        case IntLiteral(value)     => js.IntLiteral(value)
        case FloatLiteral(value)   => js.DoubleLiteral(value.toDouble)
        case DoubleLiteral(value)  => js.DoubleLiteral(value)
        case StringLiteral(value)  => js.StringLiteral(value)

        case LongLiteral(value) =>
          assert(useBigIntForLongs, "useBigIntForLongs only")
          js.BigIntLiteral(value)

        case ClassOf(typeRef) =>
          genClassOf(typeRef)

        // Atomic expressions

        case tree @ VarRef(name) =>
          env.varKind(name) match {
            case VarKind.Mutable | VarKind.Immutable =>
              js.VarRef(transformLocalVarRefIdent(tree))

            case VarKind.ThisAlias =>
              js.This()

            case VarKind.ExplicitThisAlias =>
              fileLevelVar(VarField.thiz)

            case VarKind.ClassCapture =>
              fileLevelVar(VarField.cc, genName(name))
          }

        case Transient(JSVarRef(name, _)) =>
          js.VarRef(name)

        case tree: Closure =>
          transformClosure(tree)

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
          js.Apply(globalVar(VarField.a, className), transformedArgs)

        // Invalid trees

        case _ =>
          throw new IllegalArgumentException(
              "Invalid tree in JSDesugar.transformExpr() of class " +
              tree.getClass)
      }

      if (preserveChar || tree.tpe != CharType)
        baseResult
      else
        genCallHelper(VarField.bC, baseResult)
    }

    /** Minimum amount of elements in a constant array to use the encoded strategy.
     *
     *  This is a trade-off. For small arrays, it is probably more important
     *  that they are created quickly, rather than saving some bytes.
     *
     *  Must not be 0. The constant array decoder functions assume that the
     *  resulting array is not empty.
     */
    private final val ConstantArrayThreshold = 8

    /** Should we a use a constant array strategy for the given array elems.
     *
     *  This true when all of the following apply:
     *
     *  - We emit for ES2015+ (because we assume that arrays use TypedArrays),
     *  - The elem type is an integer type,
     *  - There are at least `ConstantArrayThreshold` elements, and
     *  - All the elements are Literals.
     *
     *  In the future, we could add floating point types and booleans.
     */
    private def shouldGenerateAsConstantArray(primRef: PrimRef, elems: List[Tree]): Boolean = {
      primRef match {
        case _ if !es2015 =>
          false
        case CharRef | ByteRef | ShortRef | IntRef | LongRef =>
          elems.lengthCompare(ConstantArrayThreshold) >= 0 &&
          elems.forall(_.isInstanceOf[Literal])
        case _ =>
          false
      }
    }

    // Ordered by decreasing run-time performance
    private final val CAStrategyRaw = 0
    private final val CAStrategyUVals = 1
    private final val CAStrategyUDiffs = 2
    private final val CAStrategySVals = 3
    private final val CAStrategySDiffs = 4

    /** Generates a constant array using a base-64-encoded string.
     *
     *  See CoreJSLib.defineConstantArrayMakers() for more information on the
     *  encoding.
     */
    private def genConstantArray(primRef: PrimRef, elems: List[Tree])(
        implicit pos: Position): js.Tree = {

      val elemCount = elems.size
      val elemByteSize = primRef match {
        case ByteRef            => 1
        case CharRef | ShortRef => 2
        case IntRef             => 4
        case LongRef            => 8
      }

      val rawBuffer = java.nio.ByteBuffer.allocate(elemCount * elemByteSize)
        .order(java.nio.ByteOrder.LITTLE_ENDIAN)

      for (elem <- elems) {
        (elem: @unchecked) match {
          case CharLiteral(v)  => rawBuffer.putChar(v)
          case ByteLiteral(v)  => rawBuffer.put(v)
          case ShortLiteral(v) => rawBuffer.putShort(v)
          case IntLiteral(v)   => rawBuffer.putInt(v)
          case LongLiteral(v)  => rawBuffer.putLong(v)
        }
      }
      rawBuffer.position(0)

      // Currently, only ints and longs have the non-raw strategies
      val strategy = primRef match {
        case IntRef | LongRef =>
          pickConstantInt32ArrayStrategy(rawBuffer)
        case _ =>
          CAStrategyRaw
      }
      rawBuffer.position(0)

      val encoded =
        if (strategy == CAStrategyRaw) encodeConstantArrayRaw(rawBuffer)
        else encodeConstantInt32ArrayVarLen(rawBuffer, strategy)

      val helperVarField = strategy match {
        case CAStrategyRaw    => VarField.constArrRaw
        case CAStrategyUVals  => VarField.constArrUVals
        case CAStrategyUDiffs => VarField.constArrUDiffs
        case CAStrategySVals  => VarField.constArrSVals
        case CAStrategySDiffs => VarField.constArrSDiffs
      }

      js.Apply(
        globalVar(helperVarField, primRef),
        List(
          js.IntLiteral(elemCount),
          js.StringLiteral(encoded)
        )
      )
    }

    private def ceilDiv(x: Int, y: Int): Int =
      Integer.divideUnsigned(x + y - 1, y)

    /* The number of bits required to encode an unsigned int.
     * The | 1 ensures that we return 1 for x == 0.
     */
    private def uBitSize(x: Int): Int = 32 - Integer.numberOfLeadingZeros(x | 1)

    /* The number of bits required to encode a signed int.
     * 1 for the sign bit, plus the number of bits required to encode the
     * "bit-absolute" value of x (i.e., ~x if x < 0).
     */
    private def sBitSize(x: Int): Int = 1 + uBitSize(x ^ (x >> 31))

    /* The number of base-64 chars required to encode an unsigned int. */
    private def uCharSize(x: Int): Int = ceilDiv(uBitSize(x), 5)

    /* The number of base-64 chars required to encode a signed int. */
    private def sCharSize(x: Int): Int = ceilDiv(sBitSize(x), 5)

    /** Pick an encoding strategy for an Int32Array-based array.
     *
     *  This is used for longs as well, since we store their lo and hi fields
     *  separately. This is true even when using bigints for longs: we decode
     *  as pairs of ints, then reinterpret as a BigInt64Array.
     */
    private def pickConstantInt32ArrayStrategy(buffer: java.nio.ByteBuffer): Int = {
      val sizeRaw = ceilDiv(buffer.remaining() * 4, 3)

      var sizeUVals = 0
      var sizeUDiffs = 0
      var sizeSVals = 0
      var sizeSDiffs = 0

      var prevElem = 0

      while (buffer.hasRemaining()) {
        val elem = buffer.getInt()
        val diff = elem - prevElem
        prevElem = elem

        sizeUVals += uCharSize(elem)
        sizeUDiffs += uCharSize(diff)
        sizeSVals += sCharSize(elem)
        sizeSDiffs += sCharSize(diff)
      }

      // Indexed by CAStrategyX constants; the order matters
      val sizes = Array(
        sizeRaw,
        sizeUVals,
        sizeUDiffs,
        sizeSVals,
        sizeSDiffs
      )

      var best = 0
      for (i <- 1 until sizes.length) {
        if (sizes(i) < sizes(best))
          best = i
      }
      best
    }

    private def encodeConstantArrayRaw(buffer: java.nio.ByteBuffer): String = {
      val len = buffer.remaining()

      val encoded = new java.lang.StringBuilder(ceilDiv(len * 4, 3))

      while (buffer.hasRemaining()) {
        val a = buffer.get() & 0xff
        val b = if (buffer.hasRemaining()) buffer.get() & 0xff else 0
        val c = if (buffer.hasRemaining()) buffer.get() & 0xff else 0

        def encode6Bits(x: Int): Char = {
          if (x < 8) (0x34 + x).toChar
          else if (x < 36) (0x40 + (x - 8)).toChar
          else (0x60 + (x - 36)).toChar
        }

        encoded.append(encode6Bits(a & 0x3f))
        encoded.append(encode6Bits((a >>> 6) | ((b & 0x0f) << 2)))
        encoded.append(encode6Bits((b >>> 4) | ((c & 0x03) << 4)))
        encoded.append(encode6Bits(c >>> 2))
      }

      encoded.toString()
    }

    private def encodeConstantInt32ArrayVarLen(buffer: java.nio.ByteBuffer,
        strategy: Int): String = {

      val LowRangeStart = 0x30
      val HighRangeStart = 0x5d

      val isSigned = strategy == CAStrategySVals || strategy == CAStrategySDiffs
      val isDiffs = strategy == CAStrategyUDiffs || strategy == CAStrategySDiffs

      val encoded = new java.lang.StringBuilder()
      var prev = 0

      while (buffer.hasRemaining()) {
        val elemValue = buffer.getInt()
        var valueToEncode = if (isDiffs) elemValue - prev else elemValue
        prev = elemValue

        /* We must encode the chars from most significant to least significant.
         * Therefore, we must first compute how many chars we need, then
         * encode backwards.
         * That order is expensive to encode but cheap to decode.
         */

        val charCount =
          if (isSigned) sCharSize(valueToEncode)
          else uCharSize(valueToEncode)

        for (i <- (charCount - 1) to 1 by -1) {
          val shifted =
            if (isSigned) valueToEncode >> (5 * i)
            else valueToEncode >>> (5 * i)
          encoded.append((LowRangeStart + (shifted & 0x1f)).toChar)
        }
        encoded.append((HighRangeStart + (valueToEncode & 0x1f)).toChar)
      }

      encoded.toString()
    }

    /** Desugar a Long expression of the IR into a pair `(lo, hi)` of JavaScript expressions.
     *
     *  This is only valid for splittable longs, i.e., those that pass the
     *  `isExpression` test.
     */
    def transformLongExpr(tree: Tree)(implicit env: Env): (js.Tree, js.Tree) = {
      import TreeDSL._

      implicit val pos = tree.pos

      assert(!useBigIntForLongs,
          s"transformLongExpr must not be called with bigIntForLongs, at $pos with tree\n$tree")

      tree match {
        case LongLiteral(value) =>
          val (lo, hi) = LongImpl.extractParts(value)
          (js.IntLiteral(lo), js.IntLiteral(hi))

        case tree @ VarRef(name) =>
          env.varKind(name) match {
            case VarKind.Mutable | VarKind.Immutable =>
              val jsIdent = transformLocalVarRefIdent(tree)
              (js.VarRef(identLongLo(jsIdent)), js.VarRef(identLongHi(jsIdent)))

            case VarKind.ThisAlias =>
              throw new AssertionError("ThisAlias cannot be a `long`")

            case VarKind.ExplicitThisAlias =>
              (fileLevelVar(VarField.thiz), fileLevelVar(VarField.thizhi))

            case VarKind.ClassCapture =>
              val newName = genName(name)
              (fileLevelVar(VarField.cc, newName), fileLevelVar(VarField.cchi, newName))
          }

        case Transient(JSVarRef(name, _)) =>
          (js.VarRef(identLongLo(name)), js.VarRef(identLongHi(name)))

        case Transient(JSBoxedRTLongVarRef(name)) =>
          val varRef = js.VarRef(name)
          (varRef DOT cpn.lo, varRef DOT cpn.hi)

        case Transient(JSLongArraySelect(jsArray, scaledIndex)) =>
          val scaledIndexPlusOne = scaledIndex match {
            case js.IntLiteral(scaledIndexValue) => js.IntLiteral(scaledIndexValue + 1)
            case _                               => (scaledIndex + 1) | 0
          }
          val newLo = js.BracketSelect(jsArray, scaledIndex)
          val newHi = js.BracketSelect(jsArray, scaledIndexPlusOne)
          (newLo, newHi)

        case Transient(PackLong(lo, hi)) =>
          (transformExprNoChar(lo), transformExprNoChar(hi))

        case Select(qualifier, field) =>
          assert(isDuplicatable(qualifier),
              s"trying to make a long selection ${tree.show} from a non-duplicatable qualifier at $pos")
          genSelectLong(transformExprNoChar(checkNotNull(qualifier)), field)

        case SelectStatic(item) =>
          (globalVar(VarField.t, item.name), globalVar(VarField.thi, item.name))

        case tree: RecordSelect =>
          val jsIdent = makeRecordFieldIdentForVarRef(tree)
          (js.VarRef(identLongLo(jsIdent)), js.VarRef(identLongHi(jsIdent)))

        case _ =>
          throw new IllegalArgumentException(
              "Invalid tree in FunctionEmitter.transformLongExpr() of class " +
              tree.getClass)
      }
    }

    private def transformApplyDynamicImport(tree: ApplyDynamicImport)(
        implicit env: Env): js.Tree = {
      implicit val pos = tree.pos

      val ApplyDynamicImport(flags, className, method, args) = tree
      // Protect non-elidable args by an IIFE to avoid bad loop captures (see #4385).
      val targs = transformTypedArgs(method.name, args)

      val capturesBuilder = List.newBuilder[(js.ParamDef, js.Tree)]

      val newArgs = for {
        (arg, targ) <- args.zip(targs)
      } yield {
        var newArg: js.Tree = targ

        prepareCapture(arg, forceName = None, arrow = true) { () =>
          val v = newSyntheticVar()
          capturesBuilder += js.ParamDef(v) -> targ
          newArg = js.VarRef(v)
        }

        newArg
      }

      val innerCall = extractWithGlobals {
        withDynamicGlobalVar(VarField.s, (className, method.name)) { v =>
          js.Apply(v, newArgs)
        }
      }

      val captures = capturesBuilder.result()

      if (captures.isEmpty)
        innerCall
      else
        genIIFE(captures, js.Return(innerCall))
    }

    private def transformClosure(tree: Closure)(implicit env: Env): js.Tree = {
      val Closure(flags, captureParams, params, restParam, resultType, body, captureValues) = tree

      implicit val pos = tree.pos

      val capturesBuilder = List.newBuilder[(js.ParamDef, js.Tree)]

      val envVarsForCaptures = (for {
        (param, value) <- captureParams.zip(captureValues)
      } yield {
        assert(!param.mutable, f"Found mutable capture at ${param.pos}")

        val captureName = param.name.name

        val varKind = prepareCapture(value, Some(captureName), flags.arrow) { () =>
          if (!isSplitLongType(param.ptpe)) {
            capturesBuilder += transformParamDef(param) -> transformExpr(value, param.ptpe)
          } else {
            val List(loParam, hiParam) = transformParamDefExpanded(param)
            val (loValue, hiValue) = transformLongExpr(value)
            capturesBuilder += loParam -> loValue
            capturesBuilder += hiParam -> hiValue
          }
        }

        captureName -> varKind
      }).toMap

      val innerFunction = {
        val bodyEnv = Env.empty(resultType)
          .withParams(params ++ restParam)
          .withVars(envVarsForCaptures)

        desugarToFunctionInternal(flags, params, restParam, body,
            isStat = resultType == VoidType, bodyEnv)
      }

      val captures = capturesBuilder.result()

      if (captures.isEmpty) {
        innerFunction
      } else {
        genIIFE(captures, js.Return(innerFunction))
      }
    }

    private def prepareCapture(value: Tree, forceName: Option[LocalName], arrow: Boolean)(
        explicitCapture: () => Unit)(implicit env: Env): VarKind = {
      def permitImplicitJSThisCapture =
        esFeatures.useECMAScript2015Semantics && arrow

      value match {
        case VarRef(name) =>
          /* forceName is needed when capturing for Closure trees:
           *
           * If the name we want to capture implicitly isn't the same as the
           * capture param name, we cannot capture implicitly: we'd have to
           * ensure that inside the closure body there is no other var
           * named like the one we want to capture.
           * However, that would need a full rename pass.
           *
           * Note that with the optimizer enabled, this is unlikely to
           * ever happen, because the optimizer tries to give the same
           * names in this case.
           *
           * For other usages (notably ApplyDynamicImport), we generate the
           * body, so it's easy to ensure no collision.
           */
          def permitImplicitNameCapture = forceName.forall(_ == name)

          env.varKind(name) match {
            case VarKind.Immutable if !env.inLoopForVarCapture && permitImplicitNameCapture =>
              VarKind.Immutable

            case VarKind.ClassCapture if permitImplicitNameCapture =>
              VarKind.ClassCapture

            /* Generated trees for (Explicit)ThisAlias do not depend on the name
             * of the VarRef. Therefore, we can still implicitly capture them,
             * even if the capture param name and the capture value var name are
             * not the same.
             */

            case VarKind.ThisAlias if permitImplicitJSThisCapture =>
              VarKind.ThisAlias

            case VarKind.ExplicitThisAlias =>
              VarKind.ExplicitThisAlias

            case _ =>
              explicitCapture()
              VarKind.Immutable
          }

        case _ =>
          explicitCapture()
          VarKind.Immutable
      }
    }

    /** Is the given type the `long` type subject to splitting?
     *
     *  This is false when we use bigints for longs.
     */
    def isSplitLongType(tpe: Type): Boolean =
      tpe == LongType && !useBigIntForLongs

    def isMaybePrimitive(tpe: Type): Boolean = tpe match {
      /* Primitives upcast to their hijacked classes are never exact, per the
       * subtyping rules. Therefore, if we have an exact class type, it is
       * never a primitive.
       */
      case ClassType(className, _, false) =>
        HijackedClasses.contains(className) ||
        className != ObjectClass && globalKnowledge.isAncestorOfHijackedClass(className)

      case AnyType | AnyNotNullType | UndefType | BooleanType | CharType | ByteType |
          ShortType | IntType | LongType | FloatType | DoubleType | StringType =>
        true
      case _ =>
        false
    }

    def typeToBoxedHijackedClass(tpe: Type): ClassName = (tpe: @unchecked) match {
      case ClassType(className, _, _) => className
      case UndefType                  => BoxedUnitClass
      case BooleanType                => BoxedBooleanClass
      case CharType                   => BoxedCharacterClass
      case ByteType                   => BoxedByteClass
      case ShortType                  => BoxedShortClass
      case IntType                    => BoxedIntegerClass
      case LongType                   => BoxedLongClass
      case FloatType                  => BoxedFloatClass
      case DoubleType                 => BoxedDoubleClass
      case StringType                 => BoxedStringClass
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

    private def checkNotNull(tree: Tree)(implicit pos: Position): Tree = {
      if (semantics.nullPointers == CheckedBehavior.Unchecked || !tree.tpe.isNullable)
        tree
      else
        UnaryOp(UnaryOp.CheckNotNull, tree)
    }

    private def transformParamDef(paramDef: ParamDef): js.ParamDef =
      js.ParamDef(transformLocalVarIdent(paramDef.name, paramDef.originalName))(paramDef.pos)

    private def transformParamDefExpanded(paramDef: ParamDef): List[js.ParamDef] = {
      assert(!useBigIntForLongs,
          s"transformParamDefExpanded must not be called with bigIntForLongs at ${paramDef.pos}")
      val ident = transformLocalVarIdent(paramDef.name, paramDef.originalName)
      if (paramDef.ptpe == LongType) {
        List(js.ParamDef(identLongLo(ident))(paramDef.pos),
            js.ParamDef(identLongHi(ident))(paramDef.pos))
      } else {
        js.ParamDef(ident)(paramDef.pos) :: Nil
      }
    }

    private def identLongLo(ident: js.Ident): js.Ident =
      js.Ident(ident.name + "_$_lo")(ident.pos)

    private def identLongHi(ident: js.Ident): js.Ident =
      js.Ident(ident.name + "_$_hi")(ident.pos)

    private def transformLabelIdent(label: LabelName)(implicit pos: Position): js.Ident =
      js.Ident(genName(label))

    private def transformLocalVarRefIdent(varRef: VarRef): js.Ident =
      js.Ident(transformLocalName(varRef.name))(varRef.pos)

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
      sjsGen.genJSClassConstructor(className)
    }

    private def genApplyStaticLike(field: VarField, className: ClassName,
        method: MethodIdent, args: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      js.Apply(globalVar(field, (className, method.name)), args)
    }

    private def genGetDataOf(jlClassValue: js.Tree)(implicit pos: Position): js.Tree =
      genSyntheticPropSelect(jlClassValue, SyntheticProperty.data)

    private def genCallPolyfillableBuiltin(
        builtin: PolyfillableBuiltin, args: js.Tree*)(
        implicit pos: Position): js.Tree = {
      extractWithGlobals(sjsGen.genCallPolyfillableBuiltin(builtin, args: _*))
    }

    private def genFround(arg: js.Tree)(implicit pos: Position): js.Tree =
      genCallPolyfillableBuiltin(FroundBuiltin, arg)

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
  }
}

private object FunctionEmitter {
  private val UTF8Period: UTF8String = UTF8String(".")

  private val thisOriginalName: OriginalName = OriginalName("this")

  /** In their boxed form, RTLongs are typed as `jl.Long!`. */
  private val BoxedRTLongType: ClassType =
    ClassType(BoxedLongClass, nullable = false, exact = false)

  private object PrimArray {
    def unapply(tpe: ArrayType): Option[PrimRef] = tpe.arrayTypeRef match {
      case ArrayTypeRef(primRef: PrimRef, 1) => Some(primRef)
      case _                                 => None
    }
  }

  private object RefArray {
    def unapply(tpe: ArrayType): Boolean = tpe.arrayTypeRef match {
      case ArrayTypeRef(_, n) if n > 1  => true
      case ArrayTypeRef(_: ClassRef, _) => true
      case _                            => false
    }

    def is(tpe: Type): Boolean = tpe match {
      case RefArray() => true
      case _          => false
    }
  }

  private final case class JSVarRef(ident: js.Ident, mutable: Boolean)(val tpe: Type)
      extends Transient.Value {

    def traverse(traverser: Traverser): Unit = ()

    def transform(transformer: Transformer)(implicit pos: Position): Tree =
      Transient(this)

    def printIR(out: org.scalajs.ir.Printers.IRTreePrinter): Unit =
      out.print(ident.name)
  }

  /** A temp JS var ref that contains a `Long` in its boxed form.
   *
   *  Splitting it consists in accessing its `cpn.lo` and `cpn.hi` fields.
   *  That is different from normal `Long` var refs, which are represented in
   *  record form.
   *
   *  They are always synthetic temporaries, and therefore immutable.
   */
  private final case class JSBoxedRTLongVarRef(ident: js.Ident) extends Transient.Value {

    val tpe = LongType

    def traverse(traverser: Traverser): Unit = ()

    def transform(transformer: Transformer)(implicit pos: Position): Tree =
      Transient(this)

    def printIR(out: org.scalajs.ir.Printers.IRTreePrinter): Unit = {
      out.print(ident.name)
      out.print("^")
    }
  }

  /** A selection from a Long array where the underlying JS array and scaled
   *  index have already been extracted in immutable JS vars.
   *
   *  The scaled index may be a `js.IntLiteral` as well.
   *
   *  More generally, the actual requirement is that `jsArray` and
   *  `scaledIndex` be duplicatable. In practice, though, only `js.VarRef`s and
   *  `js.IntLiteral`s are used when producing `JSLongArraySelect`. Call sites
   *  don't need the more general case.
   */
  private final case class JSLongArraySelect(jsArray: js.VarRef, scaledIndex: js.Tree)
      extends Transient.Value {

    val tpe = LongType

    def traverse(traverser: Traverser): Unit = ()

    def transform(transformer: Transformer)(implicit pos: Position): Tree =
      Transient(this)

    def printIR(out: org.scalajs.ir.Printers.IRTreePrinter): Unit = {
      out.print("<jsLongArraySelect>(")
      out.print(jsArray.show)
      out.print(", ")
      out.print(scaledIndex.show)
      out.print(")")
    }
  }

  private final case class JSNewVararg(ctor: Tree, argArray: Tree) extends Transient.Value {
    val tpe: Type = AnyType

    def traverse(traverser: Traverser): Unit = {
      traverser.traverse(ctor)
      traverser.traverse(argArray)
    }

    def transform(transformer: Transformer)(implicit pos: Position): Tree = {
      Transient(JSNewVararg(transformer.transform(ctor),
          transformer.transform(argArray)))
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

    final case class Return(label: LabelName) extends Lhs {
      override def hasNothingType: Boolean = true
    }

    case object Throw extends Lhs {
      override def hasNothingType: Boolean = true
    }

    /** Discard the value of rhs (but retain side effects). */
    case object Discard extends Lhs
  }

  sealed abstract class VarKind

  object VarKind {
    case object Mutable extends VarKind
    case object Immutable extends VarKind
    case object ThisAlias extends VarKind
    case object ExplicitThisAlias extends VarKind
    case object ClassCapture extends VarKind
  }

  // Environment

  final class Env private (
      val expectedReturnType: Type,
      val enclosingClassName: Option[ClassName],
      vars: Map[LocalName, VarKind],
      labeledExprLHSes: Map[LabelName, Lhs],
      labelsTurnedIntoContinue: Set[LabelName],
      defaultBreakTargets: Set[LabelName],
      defaultContinueTargets: Set[LabelName],
      val inLoopForVarCapture: Boolean
  ) {
    def varKind(name: LocalName): VarKind = {
      // If we do not know the var, it must be a JS class capture.
      vars.getOrElse(name, VarKind.ClassCapture)
    }

    def isLocalMutable(name: LocalName): Boolean =
      VarKind.Mutable == varKind(name)

    def lhsForLabeledExpr(label: LabelName): Lhs = labeledExprLHSes(label)

    def isLabelTurnedIntoContinue(label: LabelName): Boolean =
      labelsTurnedIntoContinue.contains(label)

    def isDefaultBreakTarget(label: LabelName): Boolean =
      defaultBreakTargets.contains(label)

    def isDefaultContinueTarget(label: LabelName): Boolean =
      defaultContinueTargets.contains(label)

    def withEnclosingClassName(enclosingClassName: Option[ClassName]): Env =
      copy(enclosingClassName = enclosingClassName)

    def withExplicitThis(): Env =
      copy(vars = vars + (LocalName.This -> VarKind.ExplicitThisAlias))

    def withVars(newVars: Map[LocalName, VarKind]): Env =
      copy(vars = vars ++ newVars)

    def withParams(params: List[ParamDef]): Env = {
      params.foldLeft(this) {
        case (env, ParamDef(name, _, _, mutable)) =>
          env.withDef(name, mutable)
      }
    }

    def withDef(ident: LocalIdent, mutable: Boolean): Env = {
      val kind =
        if (mutable) VarKind.Mutable
        else VarKind.Immutable
      copy(vars = vars + (ident.name -> kind))
    }

    def withLabeledExprLHS(label: LabelName, lhs: Lhs): Env =
      copy(labeledExprLHSes = labeledExprLHSes + (label -> lhs))

    def withTurnLabelIntoContinue(label: LabelName): Env =
      copy(labelsTurnedIntoContinue = labelsTurnedIntoContinue + label)

    def withDefaultBreakTargets(targets: Set[LabelName]): Env =
      copy(defaultBreakTargets = targets)

    def withDefaultContinueTargets(targets: Set[LabelName]): Env =
      copy(defaultContinueTargets = targets)

    def withInLoopForVarCapture(inLoopForVarCapture: Boolean): Env =
      copy(inLoopForVarCapture = inLoopForVarCapture)

    private def copy(
        expectedReturnType: Type = this.expectedReturnType,
        enclosingClassName: Option[ClassName] = this.enclosingClassName,
        vars: Map[LocalName, VarKind] = this.vars,
        labeledExprLHSes: Map[LabelName, Lhs] = this.labeledExprLHSes,
        labelsTurnedIntoContinue: Set[LabelName] = this.labelsTurnedIntoContinue,
        defaultBreakTargets: Set[LabelName] = this.defaultBreakTargets,
        defaultContinueTargets: Set[LabelName] = this.defaultContinueTargets,
        inLoopForVarCapture: Boolean = this.inLoopForVarCapture): Env = {
      new Env(expectedReturnType, enclosingClassName, vars,
          labeledExprLHSes, labelsTurnedIntoContinue, defaultBreakTargets,
          defaultContinueTargets, inLoopForVarCapture)
    }
  }

  object Env {
    private val InitVars: Map[LocalName, VarKind] =
      Map(LocalName.This -> VarKind.ThisAlias)

    def empty(expectedReturnType: Type): Env = {
      new Env(expectedReturnType, None, InitVars, Map.empty, Set.empty,
          Set.empty, Set.empty, false)
    }
  }
}
