package scala.tools.nsc
package backend
package js

trait JSDesugaring extends SubComponent {
  val global: scalajs.JSGlobal

  import global._

  class JSDesugar extends js.Transformer {
    var syntheticVarCounter: Int = 0

    def newSyntheticVar()(implicit pos: Position): js.Ident = {
      syntheticVarCounter += 1
      js.Ident("jsx$" + syntheticVarCounter)
    }

    def resetSyntheticVarCounterIn[A](f: => A): A = {
      val savedCounter = syntheticVarCounter
      syntheticVarCounter = 0
      try f
      finally syntheticVarCounter = savedCounter
    }

    override def transformStat(tree: js.Tree): js.Tree = {
      implicit val pos = tree.pos

      tree match {
        // Anything pure can be discarded

        case _ if isPureExpression(tree) =>
          js.Skip()

        // Inside a FunDef, we can reset the synthetic var counter

        case js.FunDef(name, args, body) =>
          resetSyntheticVarCounterIn(super.transformStat(tree))

        // Definitions and assignments

        case js.VarDef(_, js.EmptyTree) =>
          tree

        case js.VarDef(_, rhs) =>
          unnestExprInto(tree, rhs)

        case js.Assign(select @ js.DotSelect(qualifier, item), rhs) =>
          expressify(qualifier, rhs) { (newQualifier, newRhs) =>
            js.Assign(
                js.DotSelect(transformExpr(newQualifier), item)(select.pos),
                transformExpr(newRhs))
          }

        case js.Assign(select @ js.BracketSelect(qualifier, item), rhs) =>
          expressify(List(qualifier, item, rhs)) {
            case List(newQualifier, newItem, newRhs) =>
              js.Assign(
                  js.BracketSelect(transformExpr(newQualifier),
                      transformExpr(newItem))(select.pos),
                  transformExpr(newRhs))
          }

        case js.Assign(_ : js.Ident, rhs) =>
          unnestExprInto(tree, rhs)

        // Statement-only language constructs

        case js.Skip() =>
          tree

        case js.Block(stats, stat) =>
          val newStats = (stats :+ stat) map transformStat
          // Flatten subblocks
          val flattenedStats = newStats flatMap {
            case js.Skip() => Nil
            case js.Block(subStats, subStat) => subStats :+ subStat
            case other => other :: Nil
          }
          flattenedStats match {
            case Nil => js.Skip()
            case only :: Nil => only
            case _ => js.Block(flattenedStats.init, flattenedStats.last)
          }

        case js.Return(expr) =>
          unnestExprInto(js.Return(js.EmptyTree), expr)

        case js.If(cond, thenp, elsep) =>
          expressify(cond) { newCond =>
            js.If(transformExpr(newCond),
                transformStat(thenp), transformStat(elsep))
          }

        case js.While(cond, body) =>
          if (isExpression(cond)) super.transformStat(tree)
          else ??? // we can't just 'expressify' here

        case js.Throw(expr) =>
          expressify(expr) { newExpr =>
            js.Throw(transformExpr(newExpr))
          }

        // Applications

        case js.Apply(fun, args) =>
          expressify(fun :: args) { newFunAndArgs =>
            val newFun :: newArgs = newFunAndArgs
            js.Apply(transformExpr(newFun), newArgs map transformExpr)
          }

        case js.New(fun, args) =>
          js.New(fun, args map transformExpr)

        // Operators (if we reach here their operands are not pure)

        case js.DotSelect(qualifier, item) =>
          expressify(qualifier) { newQualifier =>
            js.DotSelect(transformExpr(newQualifier), item)
          }

        case js.BracketSelect(qualifier, item) =>
          expressify(qualifier, item) { (newQualifier, newItem) =>
            js.BracketSelect(transformExpr(newQualifier), transformExpr(newItem))
          }

        case js.UnaryOp(op, lhs) =>
          expressify(lhs) { newLhs =>
            js.UnaryOp(op, transformExpr(newLhs))
          }

        case js.BinaryOp(op, lhs, rhs) =>
          expressify(lhs, rhs) { (newLhs, newRhs) =>
            js.BinaryOp(op, transformExpr(newLhs), transformExpr(newRhs))
          }

        // Compounds (if we reach here their items are not pure)

        case js.ArrayConstr(items) =>
          expressify(items) { newItems =>
            js.ArrayConstr(newItems map transformExpr)
          }

        case js.ObjectConstr(fields) =>
          val names = fields map (_._1)
          val items = fields map (_._2)
          expressify(items) { newItems =>
            js.ObjectConstr(names.zip(newItems) map {
              case (name, value) => (name, transformExpr(value))
            })
          }

        // Classes - that's another story

        case classDef : js.ClassDef =>
          transformClass(classDef)

        case _ =>
          abort("Illegal tree in JSDesugar.transformStat(): " + tree)
      }
    }

    /** Extract evaluation of unpure expressions in temporary variables
     *  TODO Figure out a better name for this function
     */
    def expressify(args: List[js.Tree])(
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
          for ((arg, true, temp) <- argsInfo) yield
            transformStat(js.VarDef(temp.asInstanceOf[js.Ident], arg)(arg.pos))

        val newStatement = makeStat(argsInfo map (_._3))
        js.Block(computeTemps, newStatement)(newStatement.pos)
      }
    }

    /** Same as above, for a single argument */
    def expressify(arg: js.Tree)(
        makeStat: js.Tree => js.Tree): js.Tree = {
      if (isExpression(arg)) makeStat(arg)
      else {
        val temp = newSyntheticVar()(arg.pos)
        val computeTemp = transformStat(js.VarDef(temp, arg)(arg.pos))
        val newStatement = makeStat(temp)
        js.Block(List(computeTemp), newStatement)(newStatement.pos)
      }
    }

    /** Same as above, for two arguments */
    def expressify(lhs: js.Tree, rhs: js.Tree)(
        makeStat: (js.Tree, js.Tree) => js.Tree): js.Tree = {
      expressify(List(lhs, rhs)) {
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

        // Expressions that are never pure
        case js.DotSelect(qualifier, item) =>
          allowUnpure && test(qualifier)
        case js.BracketSelect(qualifier, item) =>
          allowUnpure && test(qualifier) && test(item)
        case js.Apply(fun, args) =>
          allowUnpure && test(fun) && (args forall test)
        case js.New(fun, args) =>
          allowUnpure && (args forall test)

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

    /** Unnest an rhs into an atomic lhs
     *  Example: turn
     *    x = {
     *      var y = 5;
     *      y + 3
     *    }
     *  into
     *    {
     *      var y = 5;
     *      x = y + 3
     *    }
     *
     *  lhs can be either a js.VarDef, a js.Assign or a js.Return
     */
    def unnestExprInto(lhs: js.Tree, rhs: js.Tree): js.Tree = {
      implicit val rhsPos = rhs.pos

      @inline def redo(newRhs: js.Tree) = unnestExprInto(lhs, newRhs)

      rhs match {
        // Base case, rhs is already a regular JS expression

        case _ if isExpression(rhs) =>
          (lhs: @unchecked) match {
            case js.VarDef(ident, _) => js.VarDef(ident, rhs)
            case js.Assign(ident, _) => js.Assign(ident, rhs)
            case js.Return(_) => js.Return(rhs)
          }

        // Language constructs that are statement-only in standard JavaScript

        case js.Block(stats, expr) =>
          transformStat {
            js.Block(stats, unnestExprInto(lhs, expr))
          }

        case js.Return(expr) =>
          transformStat(rhs)

        case js.If(cond, thenp, elsep) =>
          transformStat {
            js.If(cond, unnestExprInto(lhs, thenp), unnestExprInto(lhs, elsep))
          }

        case js.Try(block, errVar, handler, finalizer) =>
          transformStat {
            js.Try(unnestExprInto(lhs, block), errVar,
                unnestExprInto(lhs, handler), finalizer)
          }

        case js.Throw(expr) =>
          transformStat(rhs)

        // Applications (if we reach here their arguments are not expressions)

        case js.Apply(fun, args) =>
          redo {
            expressify(fun :: args) { newFunAndArgs =>
              val newFun :: newArgs = newFunAndArgs
              js.Apply(transformExpr(newFun), newArgs map transformExpr)
            }
          }

        case js.New(fun, args) =>
          redo {
            js.New(fun, args map transformExpr)
          }

        // Operators (if we reach here their operands are not expressions)

        case js.DotSelect(qualifier, item) =>
          redo {
            expressify(qualifier) { newQualifier =>
              js.DotSelect(transformExpr(newQualifier), item)
            }
          }

        case js.BracketSelect(qualifier, item) =>
          redo {
            expressify(qualifier, item) { (newQualifier, newItem) =>
              js.BracketSelect(transformExpr(newQualifier), transformExpr(newItem))
            }
          }

        case js.UnaryOp(op, lhs) =>
          redo {
            expressify(lhs) { newLhs =>
              js.UnaryOp(op, transformExpr(newLhs))
            }
          }

        case js.BinaryOp(op, lhs, rhs) =>
          redo {
            expressify(lhs, rhs) { (newLhs, newRhs) =>
              js.BinaryOp(op, transformExpr(newLhs), transformExpr(newRhs))
            }
          }

        // Compounds (if we reach here their items are not expressions)

        case js.ArrayConstr(items) =>
          redo {
            expressify(items) { newItems =>
              js.ArrayConstr(newItems map transformExpr)
            }
          }

        case js.ObjectConstr(fields) =>
          redo {
            val names = fields map (_._1)
            val items = fields map (_._2)
            expressify(items) { newItems =>
              js.ObjectConstr(names.zip(newItems) map {
                case (name, value) => (name, transformExpr(value))
              })
            }
          }

        // Classes

        case js.ClassDef(name, parents, defs) =>
          ???

        case _ =>
          abort("Illegal tree in JSDesugar.unnestExprInto(): " + rhs)
      }
    }

    /** Desugar an ECMAScript 6 class into ECMAScript 5 constructs */
    def transformClass(tree: js.ClassDef): js.Tree = {
      implicit val pos = tree.pos
      // TODO
      super.transformStat(tree)
    }
  }

  def desugarJavaScript(tree: js.Tree): js.Tree = {
    new JSDesugar().transformStat(tree)
  }
}
