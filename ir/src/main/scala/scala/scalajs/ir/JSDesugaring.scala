/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

import scala.annotation.switch

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

        case VarDef(varIdent, RecordType(fields), recMutable, EmptyTree) =>
          Block(for {
            RecordType.Field(fieldName, fieldOrigName, tpe, fieldMutable) <- fields
          } yield {
            transformStat {
              VarDef(makeRecordFieldIdent(varIdent, fieldName, fieldOrigName),
                  tpe, recMutable || fieldMutable, EmptyTree)
            }
          })

        case VarDef(_, _, _, EmptyTree) =>
          tree

        case VarDef(_, _, _, rhs) =>
          pushLhsInto(tree, rhs)

        case Assign(RecordFieldVarRef(lhs), rhs) =>
          pushLhsInto(Assign(lhs, EmptyTree), rhs)

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
                If(cond, body, Break())(NoType)
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
                Block(body, If(cond, Skip(), Break())(NoType))
              }
            }, label)
          }

        case Switch(selector, cases, body) =>
          unnest(selector) { newSelector =>
            super.transformStat(Switch(newSelector, cases, body))
          }

        case Debugger() =>
          tree

        case JSDelete(obj, prop) =>
          unnest(obj, prop) { (newObj, newProp) =>
            JSDelete(newObj, newProp)
          }

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
        makeStat: List[Tree] => Tree): Tree = {
      if (args forall isExpression) makeStat(args)
      else {
        val extractedStatements = new scala.collection.mutable.ListBuffer[Tree]

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
                Block(stats.map(transformStat)) +=: extractedStatements
                result

              case BinaryOp(op, lhs, rhs)
                  if (op != BinaryOp.Boolean_&& && op != BinaryOp.Boolean_||) =>
                val newRhs = rec(rhs)
                BinaryOp(op, rec(lhs), newRhs)

              case BinaryOp(op, lhs, rhs) if noExtractYet && isExpression(rhs) =>
                assert(op == BinaryOp.Boolean_&& || op == BinaryOp.Boolean_||)
                BinaryOp(op, rec(lhs), rhs)

              case UnaryOp(op, lhs) =>
                UnaryOp(op, rec(lhs))
              case JSBinaryOp(op, lhs, rhs) =>
                val newRhs = rec(rhs)
                JSBinaryOp(op, rec(lhs), newRhs)
              case JSUnaryOp(op, lhs) =>
                JSUnaryOp(op, rec(lhs))
              case IsInstanceOf(expr, tpe) =>
                IsInstanceOf(rec(expr), tpe)
              case AsInstanceOf(expr, tpe) =>
                AsInstanceOf(rec(expr), tpe)
              case Cast(expr, tpe) =>
                Cast(rec(expr), tpe)

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
        Block(extractedStatements.result() ::: List(newStatement))(newStatement.pos)
      }
    }

    /** Same as above, for a single argument */
    def unnest(arg: Tree)(
        makeStat: Tree => Tree): Tree = {
      unnest(List(arg)) {
        case List(newArg) => makeStat(newArg)
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
        "bC", "uZ", "uC", "uB", "uS", "uI", "uJ", "uF", "uD",
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
        case Block(trees)            => trees forall test
        case If(cond, thenp, elsep)  => test(cond) && test(thenp) && test(elsep)
        case BinaryOp(_, lhs, rhs)   => test(lhs) && test(rhs)
        case UnaryOp(_, lhs)         => test(lhs)
        case JSBinaryOp(_, lhs, rhs) => test(lhs) && test(rhs)
        case JSUnaryOp(_, lhs)       => test(lhs)
        case ArrayLength(array)      => test(array)
        case IsInstanceOf(expr, _)   => test(expr)
        case AsInstanceOf(expr, _)   => test(expr)
        case Cast(expr, _)           => test(expr)

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
        case JSApply(fun, args) =>
          allowSideEffects && test(fun) && (args forall test)

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

    def doVarDef(ident: Ident, tpe: Type, mutable: Boolean, rhs: Tree): Tree = {
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
          Block(for {
            (RecordType.Field(fName, fOrigName, fTpe, fMutable),
                fRhs) <- fields zip elems
          } yield {
            doVarDef(makeRecordFieldIdent(ident, fName, fOrigName),
                fTpe, mutable || fMutable, fRhs)
          })

        case _ =>
          VarDef(ident, tpe, mutable, rhs)
      }
    }

    def doAssign(lhs: Tree, rhs: Tree): Tree = {
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
          Block(for {
            (RecordType.Field(fName, fOrigName, fTpe, fMutable),
                fRhs) <- fields zip elems
          } yield {
            doAssign(VarRef(makeRecordFieldIdent(ident, fName, fOrigName),
                mutable || fMutable)(fTpe), fRhs)
          })

        case _ =>
          Assign(lhs, rhs)
      }
    }

    /** Push an lhs into a (potentially complex) rhs
     *  lhs can be either a EmptyTree, a VarDef, a Assign or a
     *  Return
     */
    def pushLhsInto(lhs: Tree, rhs: Tree): Tree = {
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
          case VarDef(name, tpe, mutable, _) =>
            /* We still need to declare the var, in case it is used somewhere
             * else in the function, where we can't dce it.
             */
            Block(
                transformStat(VarDef(name, tpe, true, EmptyTree)),
                transformedRhs)
          case _ =>
            transformedRhs
        }
      } else (rhs match {
        // Base case, rhs is already a regular JS expression

        case _ if isExpression(rhs) =>
          val newRhs = transformExpr(rhs)
          (lhs: @unchecked) match {
            case EmptyTree =>
              if (isSideEffectFreeExpression(newRhs)) Skip()
              else newRhs
            case VarDef(name, tpe, mutable, _) =>
              doVarDef(name, tpe, mutable, newRhs)
            case Assign(lhs, _) =>
              doAssign(lhs, newRhs)
            case Return(_, None) => Return(newRhs, None)
            case Return(_, label @ Some(l)) =>
              labeledExprLHSes(l) match {
                case newLhs @ Return(_, _) =>
                  pushLhsInto(newLhs, rhs) // no need to break here
                case newLhs =>
                  Block(pushLhsInto(newLhs, rhs), Break(label))
              }
          }

        // Almost base case with RecordValue

        case RecordValue(recTpe, elems) =>
          (lhs: @unchecked) match {
            case EmptyTree =>
              Block(elems map transformStat)
            case VarDef(name, tpe, mutable, _) =>
              unnest(elems) { newElems0 =>
                val newElems = newElems0 map transformExpr
                doVarDef(name, tpe, mutable, RecordValue(recTpe, newElems))
              }
            case Assign(lhs, _) =>
              unnest(elems) { newElems0 =>
                val newElems = newElems0 map transformExpr
                val temp = newSyntheticVar()
                Block(
                    doVarDef(temp, recTpe, false, RecordValue(recTpe, newElems)),
                    doAssign(lhs, VarRef(temp, false)(recTpe)))
              }
            case Return(_, label @ Some(l)) =>
              val newLhs = labeledExprLHSes(l)
              Block(pushLhsInto(newLhs, rhs), Break(label))
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
              case _            => Labeled(label, NoType, redo(body))
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
            If(transformExpr(newCond), redo(thenp), redo(elsep))(NoType)
          }

        case Try(block, errVar, handler, finalizer) =>
          val newHandler =
            if (handler == EmptyTree) handler else redo(handler)
          val newFinalizer =
            if (finalizer == EmptyTree) finalizer else transformStat(finalizer)

          if (newHandler != EmptyTree && newFinalizer != EmptyTree) {
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
            Try(Try(redo(block), errVar, newHandler, EmptyTree)(NoType),
                errVar, EmptyTree, newFinalizer)(NoType)
          } else
            Try(redo(block), errVar, newHandler, newFinalizer)(NoType)

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

        case UnaryOp(op, lhs) =>
          unnest(lhs) { newLhs =>
            redo(UnaryOp(op, newLhs))
          }

        case BinaryOp(BinaryOp.Boolean_&&, lhs, rhs) =>
          redo(If(lhs, rhs, BooleanLiteral(false))(BooleanType))

        case BinaryOp(BinaryOp.Boolean_||, lhs, rhs) =>
          redo(If(lhs, BooleanLiteral(true), rhs)(BooleanType))

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

        // Closures

        case Closure(thisType, args, resultType, body, captures) =>
          unnest(captures) { newCaptures =>
            redo(Closure(thisType, args, resultType, body, newCaptures))
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
                  _:Switch | _:Debugger | _:JSDelete | _:DocComment |
                  _:StoreModule | _:ClassDef =>
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

    /** Desugar an expression of Extended-JS into ES5 JS */
    override def transformExpr(tree: Tree): Tree = {
      import TreeDSL._

      implicit val pos = tree.pos

      tree match {
        case New(cls, ctor, args) =>
          JSApply(JSNew(encodeClassVar(cls.className), Nil) DOT ctor,
              args map transformExpr)

        case LoadModule(cls) =>
          assert(cls.className.endsWith("$"),
              s"Trying to load module for non-module class $cls")
          val moduleName = cls.className.dropRight(1)
          JSApply(envField("m") DOT moduleName, Nil)

        case RecordFieldVarRef(varRef) =>
          varRef

        case Select(qualifier, item, _) =>
          transformExpr(qualifier) DOT item

        case Apply(receiver, method, args) =>
          JSApply(transformExpr(receiver) DOT method, args map transformExpr)

        case StaticApply(receiver, cls, method, args) =>
          val fun = encodeClassVar(cls.className).prototype DOT method
          JSApply(fun DOT "call", (receiver :: args) map transformExpr)

        case TraitImplApply(impl, method, args) =>
          JSApply(envField("i") DOT method, args map transformExpr)

        case UnaryOp(op, lhs) =>
          import UnaryOp._
          val newLhs = transformExpr(lhs)
          (op: @switch) match {
            case `typeof`         => JSUnaryOp("typeof", newLhs)
            case Int_+ | Double_+ => JSUnaryOp("+", newLhs)
            case Int_- | Double_- => JSUnaryOp("-", newLhs)
            case Int_~            => JSUnaryOp("~", newLhs)
            case Boolean_!        => JSUnaryOp("!", newLhs)
            case DoubleToInt      => JSBinaryOp("|", newLhs, IntLiteral(0))
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

          def or0(tree: Tree): Tree = JSBinaryOp("|", tree, IntLiteral(0))

          (op: @switch) match {
            case === => JSBinaryOp("===", newLhs, newRhs)
            case !== => JSBinaryOp("!==", newLhs, newRhs)

            case <  => JSBinaryOp("<", newLhs, newRhs)
            case <= => JSBinaryOp("<=", newLhs, newRhs)
            case >  => JSBinaryOp(">", newLhs, newRhs)
            case >= => JSBinaryOp(">=", newLhs, newRhs)

            case String_+ =>
              if (lhs.tpe == StringType || rhs.tpe == StringType)
                JSBinaryOp("+", newLhs, newRhs)
              else
                JSBinaryOp("+", JSBinaryOp("+", StringLiteral(""), newLhs), newRhs)

            case `in`         => JSBinaryOp("in", newLhs, newRhs)
            case `instanceof` => JSBinaryOp("instanceof", newLhs, newRhs)

            case Int_+ => or0(JSBinaryOp("+", newLhs, newRhs))
            case Int_- => or0(JSBinaryOp("-", newLhs, newRhs))
            case Int_* => genCallHelper("imul", newLhs, newRhs)
            case Int_/ => or0(JSBinaryOp("/", newLhs, newRhs))
            case Int_% => JSBinaryOp("%", newLhs, newRhs)

            case Int_|   => JSBinaryOp("|", newLhs, newRhs)
            case Int_&   => JSBinaryOp("&", newLhs, newRhs)
            case Int_^   => JSBinaryOp("^", newLhs, newRhs)
            case Int_<<  => JSBinaryOp("<<", newLhs, newRhs)
            case Int_>>> => or0(JSBinaryOp(">>>", newLhs, newRhs))
            case Int_>>  => JSBinaryOp(">>", newLhs, newRhs)

            case Double_+ => JSBinaryOp("+", newLhs, newRhs)
            case Double_- => JSBinaryOp("-", newLhs, newRhs)
            case Double_* => JSBinaryOp("*", newLhs, newRhs)
            case Double_/ => JSBinaryOp("/", newLhs, newRhs)
            case Double_% => JSBinaryOp("%", newLhs, newRhs)

            case Boolean_|  => !(!JSBinaryOp("|", newLhs, newRhs))
            case Boolean_&  => !(!JSBinaryOp("&", newLhs, newRhs))
            case Boolean_^  => !(!JSBinaryOp("^", newLhs, newRhs))
            case Boolean_|| => JSBinaryOp("||", newLhs, newRhs)
            case Boolean_&& => JSBinaryOp("&&", newLhs, newRhs)
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

        case JSFunctionApply(fun, args) =>
          /* Protect the fun so that if it is, e.g.,
           * path.f
           * we emit
           * ScalaJS.protect(path.f)(args...)
           * instead of
           * path.f(args...)
           * where
           * ScalaJS.protect = function(x) { return x; }
           * If we emit the latter, then `this` will be bound to `path` in
           * `f`, which is sometimes extremely harmful (e.g., for builtin
           * methods of `window`).
           */
          val transformedFun = transformExpr(fun)
          val protectedFun = transformedFun match {
            case _:JSDotSelect | _:JSBracketSelect =>
              genCallHelper("protect", transformedFun)
            case _ =>
              transformedFun
          }
          JSApply(protectedFun, args map transformExpr)

        case JSDotMethodApply(receiver, method, args) =>
          JSApply(JSDotSelect(transformExpr(receiver), method),
              args map transformExpr)

        case JSBracketMethodApply(receiver, method, args) =>
          JSApply(JSBracketSelect(transformExpr(receiver),
              transformExpr(method)), args map transformExpr)

        // Closures

        case Closure(thisType, args, resultType, body, captures) =>
          val desugared = if (captures.isEmpty) {
            Function(thisType, args, resultType, body)
          } else {
            val (formalCaptures, formalArgs) = args.splitAt(captures.size)
            JSApply(
                Function(NoType, formalCaptures, DynType, {
                  Function(thisType, formalArgs, resultType, body)
                }),
                captures)
          }
          transformExpr(desugared)

        // Remove types

        case Cast(expr, _) =>
          transformExpr(expr)

        case Function(thisType, params, resultType, body) =>
          val bodyWithReturn =
            if (resultType == NoType) body
            else Return(body)
          val newBody = transformStat(bodyWithReturn) match {
            case Block(stats :+ Return(Undefined(), None)) => Block(stats)
            case newBody                                   => newBody
          }
          Function(DynType, params, NoType, newBody)

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

  }

  // Helpers

  private[ir] def genCallHelper(helperName: String, args: Tree*)(
      implicit pos: Position): Tree =
    JSApply(envField(helperName), args.toList)

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
