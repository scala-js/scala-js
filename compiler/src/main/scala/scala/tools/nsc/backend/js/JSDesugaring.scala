/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package js

/** Desugaring of an Extended-JS to regular ES5 JavaScript
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
 *     * js.Assign, i.e., `x =`
 *     * js.VarDef, i.e., `var x =`
 *     * js.Return, i.e., `return`
 *     * (js.EmptyTree is also used as a trick for code reuse)
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
 *  var x$1 = {
 *    var x = foo(42);
 *    x*x
 *  }
 *  obj.meth(x$1);
 *
 *  Then, according to rule 2), the lhs `var x$1 =` is pushed inside the block:
 *
 *  {
 *    var x = foo(42);
 *    var x$1 = x*x;
 *  }
 *  obj.meth(x$1);
 *
 *  Because bare blocks are non-significant in JS, this is equivalent to
 *
 *  var x = foo(42);
 *  var x$1 = x*x;
 *  obj.meth(x$1);
 *
 *  --------------------------------------------------------------------------
 *
 *  JSDesugaring does all this in a single pass, but it helps to think that:
 *  * Rule 1) is implemented by unnest(), and used most notably in
 *    * transformStat() for statement-only constructs
 *    * pushLhsInto() for statement-or-expression constructs
 *  * Rule 2) is implemented by pushLhsInto()
 *  * Class desugaring is in transformClass() and its helpers
 */
trait JSDesugaring extends SubComponent {
  val global: scalajs.JSGlobal

  import global._

  /** Desugar a statement of Extended-JS into genuine ES5 JavaScript */
  def desugarJavaScript(tree: js.Tree): js.Tree = {
    new JSDesugar().transformStat(tree)
  }

  class JSDesugar extends js.Transformer {
    // Synthetic variables

    var syntheticVarCounter: Int = 0

    def newSyntheticVar()(implicit pos: Position): js.Ident = {
      syntheticVarCounter += 1
      js.Ident("jsx$" + syntheticVarCounter, None)
    }

    def resetSyntheticVarCounterIn[A](f: => A): A = {
      val savedCounter = syntheticVarCounter
      syntheticVarCounter = 0
      try f
      finally syntheticVarCounter = savedCounter
    }

    // Current class (for resolving super, essentially)

    var currentClassDef: js.ClassDef = null

    // Now the work

    /** Desugar a statement of Extended-JS into ES5 JS */
    override def transformStat(tree: js.Tree): js.Tree = {
      implicit val pos = tree.pos

      tree match {
        // Inside a FunDef, we can reset the synthetic var counter

        case js.FunDef(name, args, body) =>
          resetSyntheticVarCounterIn(super.transformStat(tree))

        // Statement-only (I mean, even in Extended-JS) language constructs

        case js.Skip() =>
          tree

        case js.VarDef(_, js.EmptyTree) =>
          tree

        case js.VarDef(_, rhs) =>
          pushLhsInto(tree, rhs)

        case js.Assign(select @ js.DotSelect(qualifier, item), rhs) =>
          unnest(qualifier, rhs) { (newQualifier, newRhs) =>
            js.Assign(
                js.DotSelect(transformExpr(newQualifier), item)(select.pos),
                transformExpr(newRhs))
          }

        case js.Assign(select @ js.BracketSelect(qualifier, item), rhs) =>
          unnest(List(qualifier, item, rhs)) {
            case List(newQualifier, newItem, newRhs) =>
              js.Assign(
                  js.BracketSelect(transformExpr(newQualifier),
                      transformExpr(newItem))(select.pos),
                  transformExpr(newRhs))
          }

        case js.Assign(_ : js.Ident, rhs) =>
          pushLhsInto(tree, rhs)

        case js.While(cond, body, label) =>
          /* We cannot simply unnest(cond) here, because that would eject the
           * evaluation of the condition out of the loop.
           */
          if (isExpression(cond)) super.transformStat(tree)
          else {
            js.While(js.BooleanLiteral(true), {
              transformStat {
                js.If(cond, body, js.Break())
              }
            }, label)
          }

        case js.DoWhile(body, cond, label) =>
          /* We cannot simply unnest(cond) here, because that would eject the
           * evaluation of the condition out of the loop.
           */
          if (isExpression(cond)) super.transformStat(tree)
          else {
            // TODO I think this breaks 'continue' statements for this loop
            js.While(js.BooleanLiteral(true), {
              transformStat {
                js.Block(List(body), js.If(cond, js.Skip(), js.Break()))
              }
            }, label)
          }

        case js.Switch(selector, cases, body) =>
          unnest(selector) { newSelector =>
            super.transformStat(js.Switch(newSelector, cases, body))
          }

        // Treat 'return' as an LHS

        case js.Return(expr) =>
          pushLhsInto(tree, expr)

        // Classes - that's another story

        case classDef : js.ClassDef =>
          transformClass(classDef)

        /* Anything else is an expression => pushLhsInto(js.EmptyTree, _)
         * In order not to duplicate all the code of pushLhsInto() here, we
         * use a trick: js.EmptyTree is a dummy LHS that says "do nothing
         * with the result of the rhs".
         * This is exactly what an expression statement is doing: it evaluates
         * the expression, but does nothing with its result.
         */

        case _ =>
          pushLhsInto(js.EmptyTree, tree)
      }
    }

    override def transformExpr(tree: js.Tree): js.Tree = {
      implicit val pos = tree.pos

      tree match {
        /** Super call */
        case js.Apply(sel @ js.Select(sup @ js.Super(), method), args) =>
          val newSup = js.DotSelect(
              currentClassDef.parent, js.Ident("prototype")(sup.pos))(sup.pos)
          val newSel = js.Select(newSup, method)(sel.pos)
          val newApply = js.ApplyMethod(newSel, js.Ident("call"), js.This() :: args)
          transformExpr(newApply)

        case js.Super() =>
          abort("Illegal super in JSDesugar: " + tree)

        case _ =>
          super.transformExpr(tree)
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
     *  could have side-effects or even whose evaluation could be influenced
     *  by the side-effects of another unnested argument.
     *
     *  Without deep effect analysis, which we do not do, we need to take
     *  a very pessimistic approach, and unnest any expression that contains
     *  an identifier.
     *  Hence the predicate `isPureExpressionWithoutIdent`.
     */
    def unnest(args: List[js.Tree])(
        makeStat: List[js.Tree] => js.Tree): js.Tree = {
      if (args forall isExpression) makeStat(args)
      else {
        // ArgInfo ::= (orignalTree, needsTemp: Boolean, tempOrOriginalTree)
        val argsInfo =
          for (arg <- args) yield {
            if (isPureExpressionWithoutIdent(arg)) (arg, false, arg)
            else (arg, true, newSyntheticVar()(arg.pos))
          }

        assert(argsInfo exists (_._2),
            "Reached computeTemps with no temp to compute")

        val computeTemps =
          for ((arg, true, temp : js.Ident) <- argsInfo) yield
            pushLhsInto(js.VarDef(temp, js.EmptyTree)(arg.pos), arg)

        val newStatement = makeStat(argsInfo map (_._3))
        flattenBlock(computeTemps :+ newStatement)(newStatement.pos)
      }
    }

    /** Same as above, for a single argument */
    def unnest(arg: js.Tree)(
        makeStat: js.Tree => js.Tree): js.Tree = {
      if (isExpression(arg)) makeStat(arg)
      else {
        val temp = newSyntheticVar()(arg.pos)
        val computeTemp =
          pushLhsInto(js.VarDef(temp, js.EmptyTree)(arg.pos), arg)
        val newStatement = makeStat(temp)
        js.Block(List(computeTemp), newStatement)(newStatement.pos)
      }
    }

    /** Same as above, for two arguments */
    def unnest(lhs: js.Tree, rhs: js.Tree)(
        makeStat: (js.Tree, js.Tree) => js.Tree): js.Tree = {
      unnest(List(lhs, rhs)) {
        case List(newLhs, newRhs) => makeStat(newLhs, newRhs)
      }
    }

    /** Common implementation for the functions below */
    private def isExpressionInternal(tree: js.Tree,
        allowIdents: Boolean, allowUnpure: Boolean): Boolean = {
      def test(tree: js.Tree): Boolean = tree match {
        // Atomic expressions
        case _ : js.Ident => allowIdents
        case _ : js.Literal => true
        case _ : js.Function => true
        case _ : js.This => true
        case _ : js.Super => true

        // Expressions preserving pureness
        case js.BinaryOp(_, lhs, rhs) =>
          test(lhs) && test(rhs)
        case js.UnaryOp(_, lhs) =>
          test(lhs)
        case js.ArrayConstr(items) =>
          items forall test
        case js.ObjectConstr(items) =>
          items forall (item => test(item._2))

        // Assume that super calls preserve pureness, otherwise it blows up
        case js.DotSelect(js.Super(), _) => true
        case js.BracketSelect(js.Super(), item) => test(item)

        // Expressions that are never pure
        case js.DotSelect(qualifier, item) =>
          allowUnpure && test(qualifier)
        case js.BracketSelect(qualifier, item) =>
          allowUnpure && test(qualifier) && test(item)
        case js.Apply(fun, args) =>
          allowUnpure && test(fun) && (args forall test)
        case js.New(fun, args) =>
          allowUnpure && test(fun) && (args forall test)

        // Non-expressions
        case _ => false
      }
      test(tree)
    }

    /** Test whether the given tree is a standard JS expression
     */
    def isExpression(tree: js.Tree): Boolean =
      isExpressionInternal(tree, allowIdents = true, allowUnpure = true)

    /** Test whether the given tree is a side-effect-free standard JS expression
     */
    def isPureExpression(tree: js.Tree): Boolean =
      isExpressionInternal(tree, allowIdents = true, allowUnpure = false)

    /** Test whether the given tree is a side-effect-free standard JS expression
     *  without any identifier
     */
    def isPureExpressionWithoutIdent(tree: js.Tree): Boolean =
      isExpressionInternal(tree, allowIdents = false, allowUnpure = false)

    /** Push an lhs into a (potentially complex) rhs
     *  lhs can be either a js.EmptyTree, a js.VarDef, a js.Assign or a
     *  js.Return
     */
    def pushLhsInto(lhs: js.Tree, rhs: js.Tree): js.Tree = {
      implicit val rhsPos = rhs.pos

      /** Push the current lhs further into a deeper rhs */
      @inline def redo(newRhs: js.Tree) = pushLhsInto(lhs, newRhs)

      rhs match {
        // Base case, rhs is already a regular JS expression

        case _ if isExpression(rhs) =>
          val newRhs = transformExpr(rhs)
          (lhs: @unchecked) match {
            case js.EmptyTree =>
              if (isPureExpression(newRhs)) js.Skip()
              else newRhs
            case js.VarDef(ident, _) => js.VarDef(ident, newRhs)
            case js.Assign(ident, _) => js.Assign(ident, newRhs)
            case js.Return(_) => js.Return(newRhs)
          }

        // Language constructs that are statement-only in standard JavaScript

        case js.Block(stats, expr) =>
          flattenBlock((stats map transformStat) :+ redo(expr))

        case js.Return(expr) =>
          pushLhsInto(rhs, expr)

        // TODO Move this case in transformStat()?
        case js.Break(_) | js.Continue(_) =>
          rhs

        case js.If(cond, thenp, elsep) =>
          unnest(cond) { newCond =>
            js.If(transformExpr(newCond), redo(thenp), redo(elsep))
          }

        case js.Try(block, errVar, handler, finalizer) =>
          val newHandler =
            if (handler == js.EmptyTree) handler else redo(handler)
          val newFinalizer =
            if (finalizer == js.EmptyTree) finalizer else transformStat(finalizer)
          js.Try(redo(block), errVar, newHandler, newFinalizer)

        // TODO Treat throw as an LHS?
        case js.Throw(expr) =>
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
        case js.Match(selector, cases, default) =>
          unnest(selector) { newSelector =>
            val newCases = {
              for {
                (values, body) <- cases
                newValues = (values map transformExpr)
                // add the break statement
                newBody = js.Block(List(redo(body)), js.Break())
                // desugar alternatives into several cases falling through
                caze <- (newValues.init map (v => (v, js.Skip()))) :+ (newValues.last, newBody)
              } yield {
                caze
              }
            }
            val newDefault =
              if (default == js.EmptyTree) default else redo(default)
            js.Switch(newSelector, newCases, newDefault)
          }

        // Applications (if we reach here their arguments are not expressions)

        case js.ApplyMethod(receiver, methodName, args) =>
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
            redo(js.ApplyMethod(newReceiver, methodName, newArgs))
          }

        case js.Apply(fun, args) =>
          unnest(fun :: args) { newFunAndArgs =>
            val newFun :: newArgs = newFunAndArgs
            redo(js.Apply(newFun, newArgs))
          }

        case js.New(fun, args) =>
          unnest(fun :: args) { newFunAndArgs =>
            val newFun :: newArgs = newFunAndArgs
            redo(js.New(newFun, newArgs))
          }

        // Operators (if we reach here their operands are not expressions)

        case js.DotSelect(qualifier, item) =>
          unnest(qualifier) { newQualifier =>
            redo(js.DotSelect(newQualifier, item))
          }

        case js.BracketSelect(qualifier, item) =>
          unnest(qualifier, item) { (newQualifier, newItem) =>
            redo(js.BracketSelect(newQualifier, newItem))
          }

        case js.UnaryOp(op, lhs) =>
          unnest(lhs) { newLhs =>
            redo(js.UnaryOp(op, newLhs))
          }

        case js.BinaryOp("&&", lhs, rhs) =>
          redo {
            js.If(lhs, rhs, js.BooleanLiteral(false))
          }

        case js.BinaryOp("||", lhs, rhs) =>
          redo {
            js.If(lhs, js.BooleanLiteral(true), rhs)
          }

        case js.BinaryOp(op, lhs, rhs) =>
          unnest(lhs, rhs) { (newLhs, newRhs) =>
            redo(js.BinaryOp(op, newLhs, newRhs))
          }

        // Compounds (if we reach here their items are not expressions)

        case js.ArrayConstr(items) =>
          unnest(items) { newItems =>
            redo(js.ArrayConstr(newItems))
          }

        case js.ObjectConstr(fields) =>
          val names = fields map (_._1)
          val items = fields map (_._2)
          unnest(items) { newItems =>
            redo(js.ObjectConstr(names.zip(newItems)))
          }

        // Classes

        case js.ClassDef(name, parent, defs) if lhs != js.EmptyTree =>
          ???

        case _ =>
          if (lhs == js.EmptyTree) {
            /* Go "back" to transformStat() after having dived into
             * expression statements. Remember that (lhs == js.EmptyTree)
             * is a trick that we use to "add" all the code of pushLhsInto()
             * to transformStat().
             */
            rhs match {
              case _:js.FunDef | _:js.Skip | _:js.VarDef | _:js.Assign |
                  _:js.While | _:js.DoWhile | _:js.Switch =>
                transformStat(rhs)
              case _ =>
                abort("Illegal tree in JSDesugar.pushLhsInto():\n" +
                    "lhs = " + lhs + "\n" + "rhs = " + rhs)
            }
          } else {
            abort("Illegal tree in JSDesugar.pushLhsInto():\n" +
                "lhs = " + lhs + "\n" + "rhs = " + rhs)
          }
      }
    }

    /** js.Block constructor that flattens any nested block */
    def flattenBlock(stats: List[js.Tree])(implicit pos: Position): js.Tree = {
      val flattenedStats = stats flatMap {
        case js.Skip() => Nil
        case js.Block(subStats, subStat) => subStats :+ subStat
        case other => other :: Nil
      }
      flattenedStats match {
        case Nil => js.Skip()
        case only :: Nil => only
        case _ => js.Block(flattenedStats.init, flattenedStats.last)
      }
    }

    // Classes -----------------------------------------------------------------

    /** Desugar an ECMAScript 6 class into ECMAScript 5 constructs */
    def transformClass(tree: js.ClassDef): js.Tree = {
      implicit val pos = tree.pos

      val savedCurrentClassDef = currentClassDef
      currentClassDef = tree

      val typeFunctionDef = genTypeFunctionDef(tree)
      val methodsDefs = for {
        m @ js.MethodDef(name, _, _) <- tree.defs
        if name.name != "constructor"
      } yield genAddMethodDefToPrototype(tree, m)

      val customDefs = for {
        d @ js.CustomDef(name, rhs) <- tree.defs
      } yield genAddCustomDefToPrototype(tree, d)

      val result = transformStat(flattenBlock(
          typeFunctionDef :: customDefs ::: methodsDefs))

      currentClassDef = savedCurrentClassDef

      result
    }

    /** Generate the type function definition for a class */
    def genTypeFunctionDef(tree: js.ClassDef): js.Tree = {
      val constructors = for {
        constr @ js.MethodDef(js.Ident("constructor", _), _, _) <- tree.defs
      } yield constr

      val constructor = constructors.headOption.getOrElse {
        implicit val pos = tree.pos
        js.MethodDef(js.Ident("constructor"), Nil,
            if (tree.parent == js.EmptyTree) js.Skip()
            else js.ApplyMethod(js.Super(), js.Ident("constructor"), Nil))
      }

      val js.MethodDef(_, args, body) = constructor

      {
        implicit val pos = tree.pos
        val typeVar = tree.name
        val funDef = js.FunDef(typeVar, args, body)
        val inheritProto =
          if (tree.parent == js.EmptyTree) js.Skip()
          else js.Assign(
              js.DotSelect(typeVar, js.Ident("prototype")),
              js.ApplyMethod(js.Ident("Object"), js.Ident("create"),
                  List(js.DotSelect(tree.parent, js.Ident("prototype")))))
        val reassignConstructor =
          genAddToPrototype(tree, js.Ident("constructor"), typeVar)

        flattenBlock(List(funDef, inheritProto, reassignConstructor))
      }
    }

    /** Generate the addition to the prototype of a method definition */
    def genAddMethodDefToPrototype(cd: js.ClassDef,
        method: js.MethodDef): js.Tree = {
      implicit val pos = method.pos
      val methodFun = js.Function(method.args, method.body)
      genAddToPrototype(cd, method.name, methodFun)
    }

    /** Generate the addition to the prototype of a custom definition */
    def genAddCustomDefToPrototype(cd: js.ClassDef,
        customDef: js.CustomDef): js.Tree = {
      implicit val pos = customDef.pos
      genAddToPrototype(cd, customDef.name, customDef.rhs)
    }

    /** Generate `classVar.prototype.name = value` */
    def genAddToPrototype(cd: js.ClassDef, name: js.PropertyName,
        value: js.Tree)(implicit pos: Position = value.pos): js.Tree = {
      js.Assign(
          js.Select(js.DotSelect(cd.name, js.Ident("prototype")), name),
          value)
    }
  }
}
