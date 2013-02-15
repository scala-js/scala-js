package scala.tools.nsc
package backend
package js

trait JSDesugaring extends SubComponent {
  val global: scalajs.JSGlobal

  import global._

  class JSDesugar extends js.Transformer {
    // Synthetic variables

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

    // Current class (for resolving super, essentially)

    var currentClassDef: js.ClassDef = null

    // Now the work

    override def transformStat(tree: js.Tree): js.Tree = {
      implicit val pos = tree.pos

      tree match {
        // Inside a FunDef, we can reset the synthetic var counter

        case js.FunDef(name, args, body) =>
          resetSyntheticVarCounterIn(super.transformStat(tree))

        // Statement-only language constructs

        case js.Skip() =>
          tree

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

        case js.While(cond, body) =>
          if (isExpression(cond)) super.transformStat(tree)
          else {
            // we cannot just 'expressify' here
            js.While(js.BooleanLiteral(true), {
              transformStat {
                js.If(cond, body, js.Break())
              }
            })
          }

        // Return can be an expression, but can be unnested into, which is better

        case js.Return(expr) =>
          unnestExprInto(tree, expr)

        // Classes - that's another story

        case classDef : js.ClassDef =>
          transformClass(classDef)

        // Anything else is an expression => unnestExprInto(js.EmptyTree, _)

        case _ =>
          unnestExprInto(js.EmptyTree, tree)
      }
    }

    override def transformExpr(tree: js.Tree): js.Tree = {
      tree match {
        case sup @ js.Super() =>
          transformSuper(sup)
        case _ =>
          super.transformExpr(tree)
      }
    }

    /** Extract evaluation of expressions with idents in temporary variables
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
          for ((arg, true, temp : js.Ident) <- argsInfo) yield
            unnestExprInto(js.VarDef(temp, js.EmptyTree)(arg.pos), arg)

        val newStatement = makeStat(argsInfo map (_._3))
        flattenBlock(computeTemps :+ newStatement)(newStatement.pos)
      }
    }

    /** Same as above, for a single argument */
    def expressify(arg: js.Tree)(
        makeStat: js.Tree => js.Tree): js.Tree = {
      if (isExpression(arg)) makeStat(arg)
      else {
        val temp = newSyntheticVar()(arg.pos)
        val computeTemp =
          unnestExprInto(js.VarDef(temp, js.EmptyTree)(arg.pos), arg)
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
     *  lhs can be either a js.EmptyTree, a js.VarDef, a js.Assign or a
     *  js.Return
     */
    def unnestExprInto(lhs: js.Tree, rhs: js.Tree): js.Tree = {
      implicit val rhsPos = rhs.pos

      @inline def redo(newRhs: js.Tree) = unnestExprInto(lhs, newRhs)

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
          expressify(expr) { newExpr =>
            js.Return(transformExpr(newExpr))
          }

        case js.If(cond, thenp, elsep) =>
          expressify(cond) { newCond =>
            js.If(transformExpr(newCond), redo(thenp), redo(elsep))
          }

        case js.Try(block, errVar, handler, finalizer) =>
          js.Try(redo(block), errVar, redo(handler), transformStat(finalizer))

        case js.Throw(expr) =>
          expressify(expr) { newExpr =>
            js.Throw(transformExpr(newExpr))
          }

        // Applications (if we reach here their arguments are not expressions)

        case js.Apply(fun, args) =>
          expressify(fun :: args) { newFunAndArgs =>
            val newFun :: newArgs = newFunAndArgs
            redo(js.Apply(newFun, newArgs))
          }

        case js.New(fun, args) =>
          expressify(args) { newArgs =>
            redo(js.New(fun, args))
          }

        // Operators (if we reach here their operands are not expressions)

        case js.DotSelect(qualifier, item) =>
          expressify(qualifier) { newQualifier =>
            redo(js.DotSelect(newQualifier, item))
          }

        case js.BracketSelect(qualifier, item) =>
          expressify(qualifier, item) { (newQualifier, newItem) =>
            redo(js.BracketSelect(newQualifier, newItem))
          }

        case js.UnaryOp(op, lhs) =>
          expressify(lhs) { newLhs =>
            redo(js.UnaryOp(op, newLhs))
          }

        case js.BinaryOp(op, lhs, rhs) =>
          expressify(lhs, rhs) { (newLhs, newRhs) =>
            redo(js.BinaryOp(op, newLhs, newRhs))
          }

        // Compounds (if we reach here their items are not expressions)

        case js.ArrayConstr(items) =>
          expressify(items) { newItems =>
            redo(js.ArrayConstr(newItems))
          }

        case js.ObjectConstr(fields) =>
          val names = fields map (_._1)
          val items = fields map (_._2)
          expressify(items) { newItems =>
            redo(js.ObjectConstr(names.zip(newItems)))
          }

        // Classes

        case js.ClassDef(name, parent, defs) if lhs != js.EmptyTree =>
          ???

        case _ =>
          if (lhs == js.EmptyTree) transformStat(rhs)
          else abort("Illegal tree in JSDesugar.unnestExprInto(): " + rhs)
      }
    }

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

      val result = transformStat(flattenBlock(typeFunctionDef :: methodsDefs))

      currentClassDef = savedCurrentClassDef

      result
    }

    /** Transform 'super' */
    def transformSuper(sup: js.Super): js.Tree = {
      implicit val pos = sup.pos
      js.DotSelect(currentClassDef.parent, js.Ident("prototype"))
    }

    /** Generate the type function definition for a class */
    def genTypeFunctionDef(tree: js.ClassDef): js.Tree = {
      val constructors = for {
        constr @ js.MethodDef(js.Ident("constructor"), _, _) <- tree.defs
      } yield constr

      val constructor = constructors.headOption.getOrElse {
        implicit val pos = tree.pos
        js.MethodDef(js.Ident("constructor"), Nil,
            js.ApplyMethod(js.Super(), js.Ident("constructor"), Nil))
      }

      val js.MethodDef(_, args, body) = constructor

      {
        implicit val pos = tree.pos
        val typeVar = tree.name
        val funDef = js.FunDef(typeVar, args, body)
        val inheritProto =
          js.Assign(
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

    /** Generate `classVar.prototype.name = value` */
    def genAddToPrototype(cd: js.ClassDef, name: js.PropertyName,
        value: js.Tree)(implicit pos: Position = value.pos): js.Tree = {
      js.Assign(
          js.Select(js.DotSelect(cd.name, js.Ident("prototype")), name),
          value)
    }
  }

  def desugarJavaScript(tree: js.Tree): js.Tree = {
    new JSDesugar().transformStat(tree)
  }
}
