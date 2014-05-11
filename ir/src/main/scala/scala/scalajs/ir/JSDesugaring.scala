/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

import Position._
import Transformers._
import Trees._
import Types._

/** Desugaring of the IR (herein called Extended-JS) to regular ES5 JavaScript
 *
 *  Extended-JS is a non-existent language that is a superset of JavaScript
 *  with Scala-esque constructs. Most notably, most constructs can be used in
 *  expression position. Extended-JS also features ES6-like classes.
 *
 *  GenJSCode emits Extended-JS because it is *much* easier not to deal with
 *  the expression position issue in there. But of course, in the end, we need
 *  to output genuine JavaScript code.
 *
 *  JSDesugaring desugars a statement of Extended-JS into genuine ES5
 *  JavaScript code.
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
 *  @author Sébastien Doeraene
 */
object JSDesugaring {

  private final val ScalaJSEnvironmentName = "ScalaJS"

  /** Desugar a statement of Extended-JS into genuine ES5 JavaScript */
  def desugarJavaScript(tree: Tree): Tree = {
    new JSDesugar().transformStat(tree)
  }

  class JSDesugar extends Transformer {
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

    // LHS'es for labeled expressions

    var labeledExprLHSes: Map[Ident, Tree] = Map.empty

    // Now the work

    /** Desugar a statement of Extended-JS into ES5 JS */
    override def transformStat(tree: Tree): Tree = {
      implicit val pos = tree.pos

      tree match {
        // Comments

        case DocComment(text) =>
          tree

        // Statement-only (I mean, even in Extended-JS) language constructs

        case Skip() =>
          tree

        case VarDef(name, _, _, EmptyTree) =>
          VarDef(name, DynType, mutable = true, EmptyTree)

        case VarDef(_, _, _, rhs) =>
          pushLhsInto(tree, rhs)

        case Assign(select @ Select(qualifier, item, mutable), rhs) =>
          unnest(qualifier, rhs) { (newQualifier, newRhs) =>
            Assign(
                JSDotSelect(transformExpr(newQualifier), item)(select.pos),
                transformExpr(newRhs))
          }

        case Assign(select @ ArraySelect(array, index), rhs) =>
          unnest(List(array, index, rhs)) {
            case List(newArray, newIndex, newRhs) =>
              Assign(
                  JSBracketSelect(JSDotSelect(transformExpr(newArray),
                      Ident("u"))(select.pos),
                      transformExpr(newIndex))(select.pos),
                  transformExpr(newRhs))
          }

        case Assign(select @ JSDotSelect(qualifier, item), rhs) =>
          unnest(qualifier, rhs) { (newQualifier, newRhs) =>
            Assign(
                JSDotSelect(transformExpr(newQualifier), item)(select.pos),
                transformExpr(newRhs))
          }

        case Assign(select @ JSBracketSelect(qualifier, item), rhs) =>
          unnest(List(qualifier, item, rhs)) {
            case List(newQualifier, newItem, newRhs) =>
              Assign(
                  JSBracketSelect(transformExpr(newQualifier),
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
            Assign(
                JSDotSelect(envField("n"), Ident(moduleName)),
                newValue)
          }

        case While(cond, body, label) =>
          /* We cannot simply unnest(cond) here, because that would eject the
           * evaluation of the condition out of the loop.
           */
          if (isExpression(cond)) super.transformStat(tree)
          else {
            While(BooleanLiteral(true), {
              transformStat {
                If(cond, body, Break())(UndefType)
              }
            }, label)
          }

        case DoWhile(body, cond, label) =>
          /* We cannot simply unnest(cond) here, because that would eject the
           * evaluation of the condition out of the loop.
           */
          if (isExpression(cond)) super.transformStat(tree)
          else {
            /* This breaks 'continue' statements for this loop, but we don't
             * care because we never emit continue statements for do..while
             * loops.
             */
            While(BooleanLiteral(true), {
              transformStat {
                Block(body, If(cond, Skip(), Break())(UndefType))
              }
            }, label)
          }

        case Switch(selector, cases, body) =>
          unnest(selector) { newSelector =>
            super.transformStat(Switch(newSelector, cases, body))
          }

        case Debugger() =>
          tree

        // Treat 'return' as an LHS

        case Return(expr, label) =>
          pushLhsInto(tree, expr)

        // Classes - that's another story

        case classDef : ClassDef =>
          transformStat(ScalaJSClassEmitter.genClassDef(classDef))

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
        makeStat: List[Tree] => Tree): Tree = {
      if (args forall isExpression) makeStat(args)
      else {
        var computeTemps: List[Tree] = Nil
        var newArgs: List[Tree] = Nil

        val (safeArgsRev, unsafeArgsRev) = args.reverse.span(isExpression)

        for (arg <- safeArgsRev)
          newArgs = arg :: newArgs

        for (arg <- unsafeArgsRev) {
          if (isPureExpression(arg)) {
            newArgs = arg :: newArgs
          } else {
            implicit val pos = arg.pos
            val temp = newSyntheticVar()
            val computeTemp =
              pushLhsInto(VarDef(temp, arg.tpe, mutable = false, EmptyTree), arg)
            computeTemps = computeTemp :: computeTemps
            newArgs = VarRef(temp, mutable = false)(arg.tpe) :: newArgs
          }
        }

        assert(computeTemps.nonEmpty,
            "Reached computeTemps with no temp to compute")

        val newStatement = makeStat(newArgs)
        Block(computeTemps :+ newStatement)(newStatement.pos)
      }
    }

    /** Same as above, for a single argument */
    def unnest(arg: Tree)(
        makeStat: Tree => Tree): Tree = {
      if (isExpression(arg)) makeStat(arg)
      else {
        val temp = newSyntheticVar()(arg.pos)
        val computeTemp =
          pushLhsInto(VarDef(temp, arg.tpe, mutable = false, EmptyTree)(arg.pos), arg)
        val newStatement =
          makeStat(VarRef(temp, mutable = false)(arg.tpe)(arg.pos))
        Block(computeTemp, newStatement)(newStatement.pos)
      }
    }

    /** Same as above, for two arguments */
    def unnest(lhs: Tree, rhs: Tree)(
        makeStat: (Tree, Tree) => Tree): Tree = {
      unnest(List(lhs, rhs)) {
        case List(newLhs, newRhs) => makeStat(newLhs, newRhs)
      }
    }

    /** Same as above, for one head argument and a list of arguments */
    def unnest(arg0: Tree, args: List[Tree])(
        makeStat: (Tree, List[Tree]) => Tree): Tree = {
      unnest(arg0 :: args) { newArgs =>
        makeStat(newArgs.head, newArgs.tail)
      }
    }

    private val isHelperThatPreservesPureness: Set[String] = Set(
        "protect",
        "isByte", "isShort", "isInt",
        "asUnit", "asBoolean", "asByte", "asShort", "asInt",
        "asFloat", "asDouble",
        "bC", "uV", "uZ", "uC", "uB", "uS", "uI", "uJ", "uF", "uD",
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
        case _: ClassOf  => true
        case _: Function => true
        case _: JSGlobal => true

        // Vars and fields (side-effect free, pure if immutable)
        case VarRef(_, mutable) =>
          allowUnpure || !mutable
        case Select(qualifier, item, mutable) =>
          (allowUnpure || !mutable) && test(qualifier)

        // Expressions preserving pureness
        case BinaryOp(_, lhs, rhs, _) => test(lhs) && test(rhs)
        case UnaryOp(_, lhs, _)       => test(lhs)
        case JSBinaryOp(_, lhs, rhs)  => test(lhs) && test(rhs)
        case JSUnaryOp(_, lhs)        => test(lhs)
        case ArrayLength(array)       => test(array)
        case IsInstanceOf(expr, _)    => test(expr)
        case AsInstanceOf(expr, _)    => test(expr)
        case Cast(expr, _)            => test(expr)

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

        // JavaScript expressions that can always have side-effects
        case JSNew(fun, args) =>
          allowSideEffects && test(fun) && (args forall test)
        case JSDotSelect(qualifier, item) =>
          allowSideEffects && test(qualifier)
        case JSBracketSelect(qualifier, item) =>
          allowSideEffects && test(qualifier) && test(item)
        case JSApply(fun, args) =>
          allowSideEffects && test(fun) && (args forall test)
        case JSDelete(obj, prop) =>
          allowSideEffects && test(obj) && test(prop)

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

    /** Push an lhs into a (potentially complex) rhs
     *  lhs can be either a EmptyTree, a VarDef, a Assign or a
     *  Return
     */
    def pushLhsInto(lhs: Tree, rhs: Tree): Tree = {
      implicit val rhsPos = rhs.pos

      /** Push the current lhs further into a deeper rhs */
      @inline def redo(newRhs: Tree) = pushLhsInto(lhs, newRhs)

      rhs match {
        // Base case, rhs is already a regular JS expression

        case _ if isExpression(rhs) =>
          val newRhs = transformExpr(rhs)
          (lhs: @unchecked) match {
            case EmptyTree =>
              if (isSideEffectFreeExpression(newRhs)) Skip()
              else newRhs
            case VarDef(name, _, _, _) =>
              VarDef(name, DynType, mutable = true, newRhs)
            case Assign(lhs, _) => Assign(lhs, newRhs)
            case Return(_, None) => Return(newRhs, None)
            case Return(_, label @ Some(l)) =>
              labeledExprLHSes(l) match {
                case newLhs @ Return(_, _) =>
                  pushLhsInto(newLhs, rhs) // no need to break here
                case newLhs =>
                  Block(pushLhsInto(newLhs, rhs), Break(label))
              }
          }

        // Control flow constructs

        case Block(stats :+ expr) =>
          Block((stats map transformStat) :+ redo(expr))

        case Labeled(label, tpe, body) =>
          val savedMap = labeledExprLHSes
          labeledExprLHSes = labeledExprLHSes + (label -> lhs)
          try {
            lhs match {
              case Return(_, _) => redo(body)
              case _ => Labeled(label, UndefType, redo(body))
            }
          } finally {
            labeledExprLHSes = savedMap
          }

        case Return(expr, _) =>
          pushLhsInto(rhs, expr)

        case Break(_) | Continue(_) =>
          rhs

        case If(cond, thenp, elsep) =>
          unnest(cond) { newCond =>
            If(transformExpr(newCond), redo(thenp), redo(elsep))(UndefType)
          }

        case Try(block, errVar, handler, finalizer) =>
          val newHandler =
            if (handler == EmptyTree) handler else redo(handler)
          val newFinalizer =
            if (finalizer == EmptyTree) finalizer else transformStat(finalizer)
          Try(redo(block), errVar, newHandler, newFinalizer)(UndefType)

        // TODO Treat throw as an LHS?
        case Throw(expr) =>
          unnest(expr) { newExpr =>
            Throw(transformExpr(newExpr))
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
                newBody = Block(redo(body), Break())
                // desugar alternatives into several cases falling through
                caze <- (newValues.init map (v => (v, Skip()))) :+ (newValues.last, newBody)
              } yield {
                caze
              }
            }
            val newDefault =
              if (default == EmptyTree) default else redo(default)
            Switch(newSelector, newCases, newDefault)
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

        case UnaryOp(op, lhs, tpe) =>
          unnest(lhs) { newLhs =>
            redo(UnaryOp(op, newLhs, tpe))
          }

        case BinaryOp("&&", lhs, rhs, BooleanType) =>
          redo(If(lhs, rhs, BooleanLiteral(false))(BooleanType))

        case BinaryOp("||", lhs, rhs, BooleanType) =>
          redo(If(lhs, BooleanLiteral(true), rhs)(BooleanType))

        case BinaryOp(op, lhs, rhs, tpe) =>
          unnest(lhs, rhs) { (newLhs, newRhs) =>
            redo(BinaryOp(op, newLhs, newRhs, tpe))
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
          unnest(expr) { newExpr =>
            redo(AsInstanceOf(newExpr, cls))
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

        case JSApply(JSDotSelect(receiver, methodName), args) =>
          /* We must special-case this because the top-level select must not
           * be deconstructed. Thanks to JavaScript having different semantics
           * for
           * var f = x.m; f(y)
           * than for
           * x.m(y)
           * Namely, in the former case, `this` is not bound to `x`
           */
          unnest(receiver :: args) { newReceiverAndArgs =>
            val newReceiver :: newArgs = newReceiverAndArgs
            redo(JSApply(JSDotSelect(newReceiver, methodName), newArgs))
          }

        case JSApply(JSBracketSelect(receiver, methodExpr), args) =>
          // Same as above
          unnest(receiver :: args) { newReceiverAndArgs =>
            val newReceiver :: newArgs = newReceiverAndArgs
            redo(JSApply(JSBracketSelect(newReceiver, methodExpr), newArgs))
          }

        case JSApply(fun, args) =>
          unnest(fun :: args) { newFunAndArgs =>
            val newFun :: newArgs = newFunAndArgs
            redo(JSApply(newFun, newArgs))
          }

        case JSDelete(obj, prop) =>
          unnest(obj, prop) { (newObj, newProp) =>
            redo(JSDelete(newObj, newProp))
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
            redo(If(lhs, rhs, BooleanLiteral(false))(DynType))
          } else {
            unnest(lhs) { newLhs =>
              redo(If(newLhs, rhs, newLhs)(DynType))
            }
          }

        case JSBinaryOp("||", lhs, rhs) =>
          if (lhs.tpe == BooleanType) {
            redo(If(lhs, BooleanLiteral(true), rhs)(DynType))
          } else {
            unnest(lhs) { newLhs =>
              redo(If(newLhs, newLhs, rhs)(DynType))
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

        // Type-related

        case Cast(expr, _) =>
          redo(expr)

        // Classes

        case _ =>
          if (lhs == EmptyTree) {
            /* Go "back" to transformStat() after having dived into
             * expression statements. Remember that (lhs == EmptyTree)
             * is a trick that we use to "add" all the code of pushLhsInto()
             * to transformStat().
             */
            rhs match {
              case _:Skip | _:VarDef | _:Assign | _:While | _:DoWhile |
                  _:Switch | _:Debugger | _:DocComment | _:StoreModule |
                  _:ClassDef =>
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
      }
    }

    // Desugar Scala operations to JavaScript operations -----------------------

    /** Desugar an expression of Extended-JS into ES5 JS */
    override def transformExpr(tree: Tree): Tree = {
      import TreeDSL._

      implicit val pos = tree.pos

      tree match {
        case New(cls, ctor, args) =>
          JSApply(JSNew(encodeClassVar(cls), Nil) DOT ctor,
              args map transformExpr)

        case LoadModule(cls) =>
          assert(cls.className.endsWith("$"),
              s"Trying to load module for non-module class $cls")
          val moduleName = cls.className.dropRight(1)
          JSApply(envField("m") DOT moduleName, Nil)

        case Select(qualifier, item, _) =>
          transformExpr(qualifier) DOT item

        case Apply(receiver, method, args) =>
          JSApply(transformExpr(receiver) DOT method, args map transformExpr)

        case StaticApply(receiver, cls, method, args) =>
          val fun = encodeClassVar(cls).prototype DOT method
          JSApply(fun DOT "call", (receiver :: args) map transformExpr)

        case TraitImplApply(impl, method, args) =>
          JSApply(envField("i") DOT method, args map transformExpr)

        case UnaryOp(op, lhs, tpe) =>
          val newLhs = transformExpr(lhs)
          op match {
            case "(int)" => JSBinaryOp("|", newLhs, IntLiteral(0))
            case _       => JSUnaryOp(op, newLhs)
          }

        case BinaryOp(op, lhs, rhs, tpe) =>
          val lhs1 = lhs match {
            case UnaryOp("(int)", inner, _) if op == "&" || op == "<<" =>
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
          val default = JSBinaryOp(op, newLhs, newRhs)

          op match {
            case "+" if tpe == StringType =>
              if (lhs.tpe == StringType || rhs.tpe == StringType) default
              else JSBinaryOp("+", JSBinaryOp("+", StringLiteral(""), newLhs), newRhs)
            case "+" | "-" | "/" | ">>>" if tpe == IntType =>
              JSBinaryOp("|", default, IntLiteral(0))
            case "*" if tpe == IntType =>
              genCallHelper("imul", newLhs, newRhs)
            case "|" | "&" | "^" if tpe == BooleanType =>
              !(!default)
            case _ =>
              default
          }

        case NewArray(tpe, lengths) =>
          genCallHelper("newArrayObject",
              genClassDataOf(tpe), JSArrayConstr(lengths map transformExpr))

        case ArrayValue(tpe, elems) =>
          genCallHelper("makeNativeArrayWrapper",
              genClassDataOf(tpe), JSArrayConstr(elems map transformExpr))

        case ArrayLength(array) =>
          JSBracketSelect(JSDotSelect(transformExpr(array),
              Ident("u")), StringLiteral("length"))

        case ArraySelect(array, index) =>
          JSBracketSelect(JSDotSelect(transformExpr(array),
              Ident("u")), transformExpr(index))

        case IsInstanceOf(expr, cls) =>
          genIsInstanceOf(transformExpr(expr), cls)

        case AsInstanceOf(expr, cls) =>
          genAsInstanceOf(transformExpr(expr), cls)

        case ClassOf(cls) =>
          JSApply(JSDotSelect(genClassDataOf(cls), Ident("getClassOf")), Nil)

        case CallHelper(helper, args) =>
          genCallHelper(helper, args map transformExpr: _*)

        case JSGlobal() =>
          envField("g")

        // Remove types

        case Cast(expr, _) =>
          transformExpr(expr)

        case Function(params, resultType, body) =>
          Function(params.map(eraseParamType), DynType, transformStat(body))

        case _ =>
          super.transformExpr(tree)
      }
    }

    def genClassDataOf(cls: ReferenceType)(implicit pos: Position): Tree = {
      cls match {
        case ClassType(className) =>
          encodeClassField("d", className)
        case ArrayType(base, dims) =>
          (1 to dims).foldLeft(encodeClassField("d", base)) { (prev, _) =>
            JSApply(JSDotSelect(prev, Ident("getArrayOf")), Nil)
          }
      }
    }

    def genIsInstanceOf(expr: Tree, cls: ReferenceType)(implicit pos: Position): Tree =
      genIsAsInstanceOf(expr, cls, test = true)

    def genAsInstanceOf(expr: Tree, cls: ReferenceType)(implicit pos: Position): Tree =
      genIsAsInstanceOf(expr, cls, test = false)

    def genIsAsInstanceOf(expr: Tree, cls: ReferenceType, test: Boolean)(
        implicit pos: Position): Tree = {
      import Definitions._
      import TreeDSL._

      cls match {
        case ClassType(className0) =>
          val className =
            if (className0 == BoxedLongClass) RuntimeLongClass
            else className0

          if (HijackedBoxedClasses.contains(className)) {
            if (test) {
              className match {
                case BoxedUnitClass    => expr === Undefined()
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
            JSApply(
                envField(if (test) "is" else "as") DOT Ident(className),
                List(expr))
          }

        case ArrayType(base, depth) =>
          JSApply(
              envField(if (test) "isArrayOf" else "asArrayOf") DOT Ident(base),
              List(expr, IntLiteral(depth)))
      }
    }

    def eraseParamType(param: ParamDef): ParamDef =
      ParamDef(param.name, DynType)(param.pos)

  }

  // Helpers

  private[ir] def genCallHelper(helperName: String, args: Tree*)(
      implicit pos: Position): Tree =
    JSApply(envField(helperName), args.toList)

  private[ir] def encodeClassVar(cls: ClassType)(implicit pos: Position): Tree =
    encodeClassVar(cls.className)

  private[ir] def encodeClassVar(classIdent: Ident)(implicit pos: Position): Tree =
    encodeClassVar(classIdent.name)

  private[ir] def encodeClassVar(className: String)(
      implicit pos: Position): Tree =
    encodeClassField("c", className)

  private[ir] def encodeClassField(field: String, className: String)(
      implicit pos: Position): Tree =
    JSDotSelect(envField(field), Ident(className))

  private[ir] def envField(field: String)(implicit pos: Position): Tree =
    JSDotSelect(VarRef(Ident(ScalaJSEnvironmentName), false)(DynType),
        Ident(field))

  private[ir] implicit class MyTreeOps(val self: Tree) {
    def prototype(implicit pos: Position): Tree =
      JSDotSelect(self, Ident("prototype"))
  }
}
