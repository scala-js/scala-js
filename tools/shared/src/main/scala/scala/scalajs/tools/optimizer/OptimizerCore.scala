/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.optimizer

import scala.annotation.{switch, tailrec}

import scala.collection.mutable

import scala.util.control.{NonFatal, ControlThrowable, TailCalls}
import scala.util.control.TailCalls.{done => _, _} // done is a too generic term

import scala.scalajs.ir._
import Definitions.{ObjectClass, isConstructorName, isReflProxyName}
import Infos.OptimizerHints
import Trees._
import Types._

import scala.scalajs.tools.logging._

/** Optimizer core.
 *  Designed to be "mixed in" [[IncOptimizer#MethodImpl#Optimizer]].
 *  This is the core of the optimizer. It contains all the smart things the
 *  optimizer does. To perform inlining, it relies on abstract protected
 *  methods to identify the target of calls.
 */
abstract class OptimizerCore {
  import OptimizerCore._

  type MethodID <: AbstractMethodID

  val myself: MethodID

  /** Returns the body of a method. */
  protected def getMethodBody(method: MethodID): MethodDef

  /** Returns the list of possible targets for a dynamically linked call. */
  protected def dynamicCall(intfName: String,
      methodName: String): List[MethodID]

  /** Returns the target of a static call. */
  protected def staticCall(className: String,
      methodName: String): Option[MethodID]

  /** Returns the target of a trait impl call. */
  protected def traitImplCall(traitImplName: String,
      methodName: String): Option[MethodID]

  /** Returns the list of ancestors of a class or interface. */
  protected def getAncestorsOf(encodedName: String): List[String]

  /** Tests whether the given module class has an elidable accessor.
   *  In other words, whether it is safe to discard a LoadModule of that
   *  module class which is not used.
   */
  protected def hasElidableModuleAccessor(moduleClassName: String): Boolean

  /** Tests whether the given class is inlineable.
   *  @return None if the class is not inlineable, Some(value) if it is, where
   *          value is a RecordValue with the initial value of its fields.
   */
  protected def tryNewInlineableClass(className: String): Option[RecordValue]

  private val usedLocalNames = mutable.Set.empty[String]
  private val usedLabelNames = mutable.Set.empty[String]
  private var statesInUse: List[State[_]] = Nil

  private var disableOptimisticOptimizations: Boolean = false
  private var rollbacksCount: Int = 0

  private val attemptedInlining = mutable.ListBuffer.empty[MethodID]

  private var curTrampolineId = 0

  def optimize(thisType: Type, originalDef: MethodDef): (MethodDef, Infos.MethodInfo) = {
    try {
      val MethodDef(name, params, resultType, body) = originalDef
      val (newParams, newBody) = try {
        transformIsolatedBody(Some(myself), thisType, params, resultType, body)
      } catch {
        case _: TooManyRollbacksException =>
          usedLocalNames.clear()
          usedLabelNames.clear()
          statesInUse = Nil
          disableOptimisticOptimizations = true
          transformIsolatedBody(Some(myself), thisType, params, resultType, body)
      }
      val m = MethodDef(name, newParams, resultType, newBody)(originalDef.pos)
      val info = recreateInfo(m)
      (m, info)
    } catch {
      case NonFatal(cause) =>
        throw new OptimizeException(myself, attemptedInlining.distinct.toList, cause)
      case e: Throwable =>
        // This is a fatal exception. Don't wrap, just output debug info error
        Console.err.println(exceptionMsg(myself, attemptedInlining.distinct.toList))
        throw e
    }
  }

  private def withState[A, B](state: State[A])(body: => B): B = {
    statesInUse ::= state
    try body
    finally statesInUse = statesInUse.tail
  }

  private def freshLocalName(base: String): String =
    freshNameGeneric(usedLocalNames, base)

  private def freshLabelName(base: String): String =
    freshNameGeneric(usedLabelNames, base)

  private val isReserved = isKeyword ++ Seq("arguments", "eval", "ScalaJS")

  private def freshNameGeneric(usedNames: mutable.Set[String], base: String): String = {
    val result = if (!usedNames.contains(base) && !isReserved(base)) {
      base
    } else {
      var i = 1
      while (usedNames.contains(base + "$" + i))
        i += 1
      base + "$" + i
    }
    usedNames += result
    result
  }

  private def tryOrRollback(body: CancelFun => TailRec[Tree])(
      fallbackFun: () => TailRec[Tree]): TailRec[Tree] = {
    if (disableOptimisticOptimizations) {
      fallbackFun()
    } else {
      val trampolineId = curTrampolineId
      val savedUsedLocalNames = usedLocalNames.toSet
      val savedUsedLabelNames = usedLabelNames.toSet
      val savedStates = statesInUse.map(_.makeBackup())

      body { () =>
        throw new RollbackException(trampolineId, savedUsedLocalNames,
            savedUsedLabelNames, savedStates, fallbackFun)
      }
    }
  }

  private def isSubclass(lhs: String, rhs: String): Boolean =
    getAncestorsOf(lhs).contains(rhs)

  private val isSubclassFun = isSubclass _
  private def isSubtype(lhs: Type, rhs: Type): Boolean =
    Types.isSubtype(lhs, rhs)(isSubclassFun)

  /** Transforms a statement.
   *
   *  For valid expression trees, it is always the case that
   *  {{{
   *  transformStat(tree)
   *  ===
   *  pretransformExpr(tree)(finishTransformStat)
   *  }}}
   */
  private def transformStat(tree: Tree)(implicit scope: Scope): Tree =
    transform(tree, isStat = true)

  /** Transforms an expression.
   *
   *  It is always the case that
   *  {{{
   *  transformExpr(tree)
   *  ===
   *  pretransformExpr(tree)(finishTransformExpr)
   *  }}}
   */
  private def transformExpr(tree: Tree)(implicit scope: Scope): Tree =
    transform(tree, isStat = false)

  /** Transforms a tree. */
  private def transform(tree: Tree, isStat: Boolean)(
      implicit scope: Scope): Tree = {

    @inline implicit def pos = tree.pos
    val result = tree match {
      // Definitions

      case VarDef(_, _, _, rhs) =>
        /* A local var that is last (or alone) in its block is not terribly
         * useful. Get rid of it.
         * (Non-last VarDefs in blocks are handled in transformBlock.)
         */
        transformStat(rhs)

      // Control flow constructs

      case tree: Block =>
        transformBlock(tree, isStat)

      case Labeled(ident @ Ident(label, _), tpe, body) =>
        trampoline {
          returnable(label, if (isStat) NoType else tpe, body, isStat,
              usePreTransform = false)(finishTransform(isStat))
        }

      case Assign(lhs, rhs) =>
        val cont = { (preTransLhs: PreTransform) =>
          resolveLocalDef(preTransLhs) match {
            case PreTransRecordTree(lhsTree, lhsOrigType, lhsCancelFun) =>
              val recordType = lhsTree.tpe.asInstanceOf[RecordType]
              pretransformNoLocalDef(rhs) {
                case PreTransRecordTree(rhsTree, rhsOrigType, rhsCancelFun) =>
                  if (rhsTree.tpe != recordType || rhsOrigType != lhsOrigType)
                    lhsCancelFun()
                  TailCalls.done(Assign(lhsTree, rhsTree))
                case _ =>
                  lhsCancelFun()
              }
            case PreTransTree(lhsTree, _) =>
              TailCalls.done(Assign(lhsTree, transformExpr(rhs)))
          }
        }
        trampoline {
          lhs match {
            case lhs: Select =>
              pretransformSelectCommon(lhs, isLhsOfAssign = true)(cont)
            case _ =>
              pretransformExpr(lhs)(cont)
          }
        }

      case Return(expr, optLabel) =>
        val optInfo = optLabel match {
          case Some(Ident(label, _)) =>
            Some(scope.env.labelInfos(label))
          case None =>
            scope.env.labelInfos.get("")
        }
        optInfo.fold[Tree] {
          Return(transformExpr(expr), None)
        } { info =>
          val newOptLabel = Some(Ident(info.newName, None))
          if (!info.acceptRecords) {
            val newExpr = transformExpr(expr)
            info.returnedTypes.value ::= (newExpr.tpe, RefinedType(newExpr.tpe))
            Return(newExpr, newOptLabel)
          } else trampoline {
            pretransformNoLocalDef(expr) { texpr =>
              texpr match {
                case PreTransRecordTree(newExpr, origType, cancelFun) =>
                  info.returnedTypes.value ::= (newExpr.tpe, origType)
                  TailCalls.done(Return(newExpr, newOptLabel))
                case PreTransTree(newExpr, tpe) =>
                  info.returnedTypes.value ::= (newExpr.tpe, tpe)
                  TailCalls.done(Return(newExpr, newOptLabel))
              }
            }
          }
        }

      case If(cond, thenp, elsep) =>
        val newCond = transformExpr(cond)
        newCond match {
          case BooleanLiteral(condValue) =>
            if (condValue) transform(thenp, isStat)
            else           transform(elsep, isStat)
          case _ =>
            val newThenp = transform(thenp, isStat)
            val newElsep = transform(elsep, isStat)
            val refinedType =
              constrainedLub(newThenp.tpe, newElsep.tpe, tree.tpe)
            foldIf(newCond, newThenp, newElsep)(refinedType)
        }

      case While(cond, body, optLabel) =>
        val newCond = transformExpr(cond)
        newCond match {
          case BooleanLiteral(false) => Skip()
          case _ =>
            optLabel match {
              case None =>
                While(newCond, transformStat(body), None)

              case Some(labelIdent @ Ident(label, _)) =>
                val newLabel = freshLabelName(label)
                val info = new LabelInfo(newLabel, acceptRecords = false)
                While(newCond, {
                  val bodyScope = scope.withEnv(
                      scope.env.withLabelInfo(label, info))
                  transformStat(body)(bodyScope)
                }, Some(Ident(newLabel, None)(labelIdent.pos)))
            }
        }

      case DoWhile(body, cond, None) =>
        val newBody = transformStat(body)
        val newCond = transformExpr(cond)
        newCond match {
          case BooleanLiteral(false) => newBody
          case _                     => DoWhile(newBody, newCond, None)
        }

      case Try(block, errVar, EmptyTree, finalizer) =>
        val newBlock = transform(block, isStat)
        val newFinalizer = transformStat(finalizer)
        Try(newBlock, errVar, EmptyTree, newFinalizer)(newBlock.tpe)

      case Try(block, errVar @ Ident(name, originalName), handler, finalizer) =>
        val newBlock = transform(block, isStat)

        val newName = freshLocalName(name)
        val newOriginalName = originalName.orElse(Some(name))
        val localDef = LocalDef(RefinedType(AnyType), true,
            ReplaceWithVarRef(newName, newOriginalName, new SimpleState(true)))
        val newHandler = {
          val handlerScope = scope.withEnv(scope.env.withLocalDef(name, localDef))
          transform(handler, isStat)(handlerScope)
        }

        val newFinalizer = transformStat(finalizer)

        val refinedType = constrainedLub(newBlock.tpe, newHandler.tpe, tree.tpe)
        Try(newBlock, Ident(newName, newOriginalName)(errVar.pos),
            newHandler, newFinalizer)(refinedType)

      case Throw(expr) =>
        Throw(transformExpr(expr))

      case Continue(optLabel) =>
        val newOptLabel = optLabel map { label =>
          Ident(scope.env.labelInfos(label.name).newName, None)(label.pos)
        }
        Continue(newOptLabel)

      case Match(selector, cases, default) =>
        Match(transformExpr(selector),
            cases map (c => (c._1 map transformExpr, transform(c._2, isStat))),
            transform(default, isStat))(tree.tpe)

      // Scala expressions

      case New(cls, ctor, args) =>
        New(cls, ctor, args map transformExpr)

      case StoreModule(cls, value) =>
        StoreModule(cls, transformExpr(value))

      case tree: Select =>
        trampoline {
          pretransformSelectCommon(tree, isLhsOfAssign = false)(
              finishTransform(isStat = false))
        }

      case tree: Apply =>
        trampoline {
          pretransformApply(tree, isStat, usePreTransform = false)(
              finishTransform(isStat))
        }

      case tree: StaticApply =>
        trampoline {
          pretransformStaticApply(tree, isStat, usePreTransform = false)(
              finishTransform(isStat))
        }

      case tree: TraitImplApply =>
        trampoline {
          pretransformTraitImplApply(tree, isStat, usePreTransform = false)(
              finishTransform(isStat))
        }

      case UnaryOp(op, arg) =>
        foldUnaryOp(op, transformExpr(arg))(tree.pos)

      /* Typical comparison happening in code like:
       *   val (x, y) = { ... }
       * whose translation will include:
       *   val temp = { ... }
       *   l: {
       *     if (temp != null) {
       *       val x = temp._1
       *       val y = temp._2
       *       return(l) doSomething
       *     }
       *     return(l) throw new MatchError(temp)
       *   }
       * in which temp ends up being a record type, which of course cannot be
       * null.
       */
      case BinaryOp(op @ (BinaryOp.=== | BinaryOp.!==), lhs, rhs) =>
        trampoline {
          pretransformExprs(lhs, rhs) { (tlhs, trhs) =>
            TailCalls.done(foldReferenceEquality(tlhs, trhs, op == BinaryOp.===))
          }
        }

      case BinaryOp(op, lhs, rhs) =>
        foldBinaryOp(op, transformExpr(lhs), transformExpr(rhs))

      case NewArray(tpe, lengths) =>
        NewArray(tpe, lengths map transformExpr)

      case ArrayValue(tpe, elems) =>
        ArrayValue(tpe, elems map transformExpr)

      case ArrayLength(array) =>
        ArrayLength(transformExpr(array))

      case ArraySelect(array, index) =>
        ArraySelect(transformExpr(array), transformExpr(index))(tree.tpe)

      case RecordValue(tpe, elems) =>
        RecordValue(tpe, elems map transformExpr)

      case IsInstanceOf(expr, ClassType(ObjectClass)) =>
        transformExpr(BinaryOp(BinaryOp.!==, expr, Null()))

      case IsInstanceOf(expr, tpe) =>
        trampoline {
          pretransformExpr(expr) { texpr =>
            val result = {
              if (isSubtype(texpr.tpe.base, tpe)) {
                if (texpr.tpe.isNullable)
                  BinaryOp(BinaryOp.!==, finishTransformExpr(texpr), Null())
                else
                  Block(finishTransformStat(texpr), BooleanLiteral(true))
              } else {
                if (texpr.tpe.isExact)
                  Block(finishTransformStat(texpr), BooleanLiteral(false))
                else
                  IsInstanceOf(finishTransformExpr(texpr), tpe)
              }
            }
            TailCalls.done(result)
          }
        }

      case AsInstanceOf(expr, ClassType(ObjectClass)) =>
        transformExpr(expr)

      case AsInstanceOf(expr, cls) =>
        trampoline {
          pretransformExpr(tree)(finishTransform(isStat))
        }

      case CallHelper(helperName, List(arg))
          if helperName.length == 2 && helperName(0) == 'u' =>
        trampoline {
          pretransformExpr(arg) { targ =>
            foldUnbox(helperName(1), targ)(finishTransform(isStat = false))
          }
        }

      case CallHelper("objectEquals", List(lhs, rhs)) =>
        trampoline {
          pretransformExprs(lhs, rhs) { (tlhs, trhs) =>
            TailCalls.done(foldObjectEquals(tlhs, trhs))
          }
        }

      case CallHelper(helperName, args) =>
        CallHelper(helperName, args.map(transformExpr))(tree.tpe)

      // JavaScript expressions

      case JSNew(ctor, args) =>
        JSNew(transformExpr(ctor), args map transformExpr)

      case JSDotSelect(qualifier, item) =>
        JSDotSelect(transformExpr(qualifier), item)

      case JSBracketSelect(qualifier, item) =>
        JSBracketSelect(transformExpr(qualifier), transformExpr(item))

      case JSFunctionApply(fun, args) =>
        JSFunctionApply(transformExpr(fun), args map transformExpr)

      case JSDotMethodApply(receiver, method, args) =>
        JSDotMethodApply(transformExpr(receiver), method,
            args map transformExpr)

      case JSBracketMethodApply(receiver, method, args) =>
        JSBracketMethodApply(transformExpr(receiver), transformExpr(method),
            args map transformExpr)

      case JSDelete(JSDotSelect(obj, prop)) =>
        JSDelete(JSDotSelect(transformExpr(obj), prop))

      case JSDelete(JSBracketSelect(obj, prop)) =>
        JSDelete(JSBracketSelect(transformExpr(obj), transformExpr(prop)))

      case JSUnaryOp(op, lhs) =>
        JSUnaryOp(op, transformExpr(lhs))

      case JSBinaryOp(op, lhs, rhs) =>
        JSBinaryOp(op, transformExpr(lhs), transformExpr(rhs))

      case JSArrayConstr(items) =>
        JSArrayConstr(items map transformExpr)

      case JSObjectConstr(fields) =>
        JSObjectConstr(fields map {
          case (name, value) => (name, transformExpr(value))
        })

      // Atomic expressions

      case _:VarRef | _:This =>
        trampoline {
          pretransformExpr(tree)(finishTransform(isStat))
        }

      case Closure(thisType, params, resultType, body, captures) =>
        val (newParams, newBody) =
          transformIsolatedBody(None, thisType, params, resultType, body)
        Closure(thisType, newParams, resultType, newBody,
            captures.map(transformExpr))

      // Type-related

      case Cast(expr, tpe) =>
        Cast(transformExpr(expr), tpe)

      // Trees that need not be transformed

      case _:Skip | _:Debugger | _:LoadModule | _:ClassOf |
          _:JSGlobal | _:Literal | EmptyTree =>
        tree
    }

    if (isStat) keepOnlySideEffects(result)
    else result
  }

  private def transformBlock(tree: Block, isStat: Boolean)(
      implicit scope: Scope): Tree = {
    def transformList(stats: List[Tree])(
        implicit scope: Scope): Tree = stats match {
      case last :: Nil =>
        transform(last, isStat)

      case (VarDef(Ident(name, originalName), vtpe, mutable, rhs)) :: rest =>
        trampoline {
          pretransformExpr(rhs) { trhs =>
            withBinding(Binding(name, originalName, vtpe, mutable, trhs)) {
              (restScope, cont1) =>
                val newRest = transformList(rest)(restScope)
                cont1(PreTransTree(newRest, RefinedType(newRest.tpe)))
            } (finishTransform(isStat))
          }
        }

      case stat :: rest =>
        val transformedStat = transformStat(stat)
        if (transformedStat.tpe == NothingType) transformedStat
        else Block(transformedStat, transformList(rest))(stat.pos)

      case Nil => // silence the exhaustivity warning in a sensible way
        Skip()(tree.pos)
    }
    transformList(tree.stats)(scope)
  }

  /** Pretransforms a list of trees as a list of [[PreTransform]]s.
   *  This is a convenience method to use pretransformExpr on a list.
   */
  private def pretransformExprs(trees: List[Tree])(
      cont: List[PreTransform] => TailRec[Tree])(
      implicit scope: Scope): TailRec[Tree] = {
    trees match {
      case first :: rest =>
        pretransformExpr(first) { tfirst =>
          pretransformExprs(rest) { trest =>
            cont(tfirst :: trest)
          }
        }

      case Nil =>
        cont(Nil)
    }
  }

  /** Pretransforms two trees as a pair of [[PreTransform]]s.
   *  This is a convenience method to use pretransformExpr on two trees.
   */
  private def pretransformExprs(tree1: Tree, tree2: Tree)(
      cont: (PreTransform, PreTransform) => TailRec[Tree])(
      implicit scope: Scope): TailRec[Tree] = {
    pretransformExpr(tree1) { ttree1 =>
      pretransformExpr(tree2) { ttree2 =>
        cont(ttree1, ttree2)
      }
    }
  }

  /** Pretransforms a tree and a list of trees as [[PreTransform]]s.
   *  This is a convenience method to use pretransformExpr.
   */
  private def pretransformExprs(first: Tree, rest: List[Tree])(
      cont: (PreTransform, List[PreTransform]) => TailRec[Tree])(
      implicit scope: Scope): TailRec[Tree] = {
    pretransformExpr(first) { tfirst =>
      pretransformExprs(rest) { trest =>
        cont(tfirst, trest)
      }
    }
  }

  /** Pretransforms a tree to get a refined type while avoiding to force
   *  things we might be able to optimize by folding and aliasing.
   */
  private def pretransformExpr(tree: Tree)(cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = tailcall {
    @inline implicit def pos = tree.pos

    tree match {
      case tree: Block =>
        pretransformBlock(tree)(cont)

      case VarRef(Ident(name, _), _) =>
        val localDef = scope.env.localDefs.getOrElse(name,
            sys.error(s"Cannot find local def '$name' at $pos\n" +
                s"While optimizing $myself\n" +
                s"Env is ${scope.env}\nInlining ${scope.implsBeingInlined}"))
        cont(PreTransLocalDef(localDef))

      case This() =>
        val localDef = scope.env.localDefs.getOrElse("this",
            sys.error(s"Found invalid 'this' at $pos\n" +
                s"While optimizing $myself\n" +
                s"Env is ${scope.env}\nInlining ${scope.implsBeingInlined}"))
        cont(PreTransLocalDef(localDef))

      case If(cond, thenp, elsep) =>
        val newCond = transformExpr(cond)
        newCond match {
          case BooleanLiteral(condValue) =>
            if (condValue) pretransformExpr(thenp)(cont)
            else           pretransformExpr(elsep)(cont)
          case _ =>
            tryOrRollback { cancelFun =>
              pretransformNoLocalDef(thenp) { tthenp =>
                pretransformNoLocalDef(elsep) { telsep =>
                  (tthenp, telsep) match {
                    case (PreTransRecordTree(thenTree, thenOrigType, thenCancelFun),
                        PreTransRecordTree(elseTree, elseOrigType, elseCancelFun)) =>
                      val commonType =
                        if (thenTree.tpe == elseTree.tpe &&
                            thenOrigType == elseOrigType) thenTree.tpe
                        else cancelFun()
                      val refinedOrigType =
                        constrainedLub(thenOrigType, elseOrigType, tree.tpe)
                      cont(PreTransRecordTree(
                          If(newCond, thenTree, elseTree)(commonType),
                          refinedOrigType,
                          cancelFun))

                    case (PreTransRecordTree(thenTree, thenOrigType, thenCancelFun), _)
                        if telsep.tpe.isNothingType =>
                      cont(PreTransRecordTree(
                          If(newCond, thenTree, finishTransformExpr(telsep))(thenTree.tpe),
                          thenOrigType,
                          thenCancelFun))

                    case (_, PreTransRecordTree(elseTree, elseOrigType, elseCancelFun))
                        if tthenp.tpe.isNothingType =>
                      cont(PreTransRecordTree(
                          If(newCond, finishTransformExpr(tthenp), elseTree)(elseTree.tpe),
                          elseOrigType,
                          elseCancelFun))

                    case _ =>
                      val newThenp = finishTransformExpr(tthenp)
                      val newElsep = finishTransformExpr(telsep)
                      val refinedType =
                        constrainedLub(newThenp.tpe, newElsep.tpe, tree.tpe)
                      cont(PreTransTree(
                          foldIf(newCond, newThenp, newElsep)(refinedType)))
                  }
                }
              }
            } { () =>
              val newThenp = transformExpr(thenp)
              val newElsep = transformExpr(elsep)
              val refinedType =
                constrainedLub(newThenp.tpe, newElsep.tpe, tree.tpe)
              cont(PreTransTree(
                  foldIf(newCond, newThenp, newElsep)(refinedType)))
            }
        }

      case Labeled(ident @ Ident(label, _), tpe, body) =>
        returnable(label, tpe, body, isStat = false, usePreTransform = true)(cont)

      case New(cls @ ClassType(wrapperName), ctor, List(closure: Closure))
          if wrapperName.startsWith(AnonFunctionClassPrefix) =>
        tryOrRollback { cancelFun =>
          val Closure(thisType, params, resultType, body, captures) = closure
          pretransformExprs(captures) { tcaptures =>
            val captureBindings = for {
              (ParamDef(Ident(name, origName), tpe, mutable), value) <-
                params zip tcaptures
            } yield {
              Binding(name, origName, tpe, mutable, value)
            }
            withNewLocalDefs(captureBindings) { (captureLocalDefs, cont1) =>
              val alreadyUsedState = new SimpleState[Boolean](false)
              val tclosure = TentativeClosureReplacement(
                  thisType, params, resultType, body, captureLocalDefs)
              val localDef = LocalDef(
                  RefinedType(cls, isExact = true, isNullable = false), false,
                  TentativeAnonFunReplacement(tclosure, alreadyUsedState, cancelFun))
              cont1(PreTransLocalDef(localDef))
            } (cont)
          }
        } { () =>
          cont(PreTransTree(
              New(cls, ctor, List(transformExpr(closure))),
              RefinedType(cls, isExact = true, isNullable = false)))
        }

      case New(cls @ ClassType(className), ctor, args) =>
        tryNewInlineableClass(className) match {
          case Some(initialValue) =>
            pretransformExprs(args) { targs =>
              tryOrRollback { cancelFun =>
                inlineClassConstructor(
                    cls, initialValue, ctor, targs, cancelFun)(cont)
              } { () =>
                cont(PreTransTree(
                    New(cls, ctor, targs.map(finishTransformExpr)),
                    RefinedType(cls, isExact = true, isNullable = false)))
              }
            }
          case None =>
            cont(PreTransTree(
                New(cls, ctor, args.map(transformExpr)),
                RefinedType(cls, isExact = true, isNullable = false)))
        }

      case tree: Select =>
        pretransformSelectCommon(tree, isLhsOfAssign = false)(cont)

      case tree: Apply =>
        pretransformApply(tree, isStat = false,
            usePreTransform = true)(cont)

      case tree: StaticApply =>
        pretransformStaticApply(tree, isStat = false,
            usePreTransform = true)(cont)

      case tree: TraitImplApply =>
        pretransformTraitImplApply(tree, isStat = false,
            usePreTransform = true)(cont)

      case AsInstanceOf(expr, tpe) =>
        pretransformExpr(expr) { texpr =>
          tpe match {
            case ClassType(ObjectClass) =>
              cont(texpr)
            case _ =>
              if (isSubtype(texpr.tpe.base, tpe)) {
                cont(texpr)
              } else {
                cont(PreTransTree(
                    AsInstanceOf(finishTransformExpr(texpr), tpe)))
              }
          }
        }

      case Cast(expr, tpe) =>
        pretransformExpr(expr) { texpr =>
          if (texpr.tpe.base == tpe)
            cont(texpr)
          else
            cont(PreTransTree(Cast(finishTransformExpr(texpr), tpe)))
        }

      case _ =>
        val result = transformExpr(tree)
        cont(PreTransTree(result, RefinedType(result.tpe)))
    }
  }

  private def pretransformBlock(tree: Block)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    def pretransformList(stats: List[Tree])(
        cont: PreTransCont)(
        implicit scope: Scope): TailRec[Tree] = stats match {
      case last :: Nil =>
        pretransformExpr(last)(cont)

      case (VarDef(Ident(name, originalName), vtpe, mutable, rhs)) :: rest =>
        pretransformExpr(rhs) { trhs =>
          withBinding(Binding(name, originalName, vtpe, mutable, trhs)) {
            (restScope, cont1) =>
              pretransformList(rest)(cont1)(restScope)
          } (cont)
        }

      case stat :: rest =>
        implicit val pos = tree.pos
        val transformedStat = transformStat(stat)
        transformedStat match {
          case Skip() =>
            pretransformList(rest)(cont)
          case _ =>
            if (transformedStat.tpe == NothingType)
              cont(PreTransTree(transformedStat, RefinedType.Nothing))
            else {
              pretransformList(rest) { trest =>
                cont(PreTransBlock(transformedStat :: Nil, trest))
              }
            }
        }

      case Nil => // silence the exhaustivity warning in a sensible way
        TailCalls.done(Skip()(tree.pos))
    }
    pretransformList(tree.stats)(cont)(scope)
  }

  private def pretransformSelectCommon(tree: Select, isLhsOfAssign: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    val Select(qualifier, item, mutable) = tree
    pretransformExpr(qualifier) { preTransQual =>
      pretransformSelectCommon(tree.tpe, preTransQual, item, mutable,
          isLhsOfAssign)(cont)(scope, tree.pos)
    }
  }

  private def pretransformSelectCommon(expectedType: Type,
      preTransQual: PreTransform, item: Ident, mutable: Boolean,
      isLhsOfAssign: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {
    preTransQual match {
      case PreTransLocalDef(LocalDef(_, _,
          InlineClassBeingConstructedReplacement(fieldLocalDefs, cancelFun))) =>
        val fieldLocalDef = fieldLocalDefs(item.name)
        if (!isLhsOfAssign || fieldLocalDef.mutable) {
          cont(PreTransLocalDef(fieldLocalDef))
        } else {
          /* This is an assignment to an immutable field of a inlineable class
           * being constructed, but that does not appear at the "top-level" of
           * one of its constructors. We cannot handle those, so we cancel.
           * (Assignments at the top-level are normal initializations of these
           * fields, and are transformed as vals in inlineClassConstructor.)
           */
          cancelFun()
        }
      case PreTransLocalDef(LocalDef(_, _,
          InlineClassInstanceReplacement(_, fieldLocalDefs, cancelFun))) =>
        val fieldLocalDef = fieldLocalDefs(item.name)
        if (!isLhsOfAssign || fieldLocalDef.mutable) {
          cont(PreTransLocalDef(fieldLocalDef))
        } else {
          /* In an ideal world, this should not happen (assigning to an
           * immutable field of an already constructed object). However, since
           * we cannot IR-check that this does not happen (see #1021), this is
           * effectively allowed by the IR spec. We are therefore not allowed
           * to crash. We cancel instead. This will become an actual field
           * (rather than an optimized local val) which is not considered pure
           * (for that same reason).
           */
          cancelFun()
        }
      case _ =>
        resolveLocalDef(preTransQual) match {
          case PreTransRecordTree(newQual, origType, cancelFun) =>
            val recordType = newQual.tpe.asInstanceOf[RecordType]
            val field = recordType.findField(item.name)
            val sel = Select(newQual, item, mutable)(field.tpe)
            sel.tpe match {
              case _: RecordType =>
                cont(PreTransRecordTree(sel, RefinedType(expectedType), cancelFun))
              case _ =>
                cont(PreTransTree(sel, RefinedType(sel.tpe)))
            }

          case PreTransTree(newQual, _) =>
            cont(PreTransTree(Select(newQual, item, mutable)(expectedType),
                RefinedType(expectedType)))
        }
    }
  }

  /** Resolves any LocalDef in a [[PreTransform]]. */
  private def resolveLocalDef(preTrans: PreTransform): PreTransGenTree = {
    implicit val pos = preTrans.pos
    preTrans match {
      case PreTransBlock(stats, result) =>
        resolveLocalDef(result) match {
          case PreTransRecordTree(tree, tpe, cancelFun) =>
            PreTransRecordTree(Block(stats :+ tree), tpe, cancelFun)
          case PreTransTree(tree, tpe) =>
            PreTransTree(Block(stats :+ tree), tpe)
        }

      case PreTransLocalDef(localDef @ LocalDef(tpe, mutable, replacement)) =>
        replacement match {
          case ReplaceWithRecordVarRef(name, originalName,
              recordType, used, cancelFun) =>
            used.value = true
            PreTransRecordTree(
                VarRef(Ident(name, originalName), mutable)(recordType),
                tpe, cancelFun)

          case InlineClassInstanceReplacement(recordType, fieldLocalDefs, cancelFun) =>
            if (!isImmutableType(recordType))
              cancelFun()
            PreTransRecordTree(
                RecordValue(recordType, recordType.fields.map(
                    f => fieldLocalDefs(f.name).newReplacement)),
                tpe, cancelFun)

          case _ =>
            PreTransTree(localDef.newReplacement, localDef.tpe)
        }

      case preTrans: PreTransGenTree =>
        preTrans
    }
  }

  /** Combines pretransformExpr and resolveLocalDef in one convenience method. */
  private def pretransformNoLocalDef(tree: Tree)(
      cont: PreTransGenTree => TailRec[Tree])(
      implicit scope: Scope): TailRec[Tree] = {
    pretransformExpr(tree) { ttree =>
      cont(resolveLocalDef(ttree))
    }
  }

  /** Finishes a pretransform, either a statement or an expression. */
  private def finishTransform(isStat: Boolean): PreTransCont = { preTrans =>
    TailCalls.done {
      if (isStat) finishTransformStat(preTrans)
      else        finishTransformExpr(preTrans)
    }
  }

  /** Finishes an expression pretransform to get a normal [[Tree]].
   *  This method (together with finishTransformStat) must not be called more
   *  than once per pretransform and per translation.
   *  By "per translation", we mean in an alternative path through
   *  `tryOrRollback`. It could still be called several times as long as
   *  it is once in the 'try' part and once in the 'fallback' part.
   */
  private def finishTransformExpr(preTrans: PreTransform): Tree = {
    implicit val pos = preTrans.pos
    preTrans match {
      case PreTransBlock(stats, result) =>
        Block(stats :+ finishTransformExpr(result))
      case PreTransLocalDef(localDef) =>
        localDef.newReplacement
      case PreTransRecordTree(_, _, cancelFun) =>
        cancelFun()
      case PreTransTree(tree, _) =>
        tree
    }
  }

  /** Finishes a statement pretransform to get a normal [[Tree]].
   *  This method (together with finishTransformExpr) must not be called more
   *  than once per pretransform and per translation.
   *  By "per translation", we mean in an alternative path through
   *  `tryOrRollback`. It could still be called several times as long as
   *  it is once in the 'try' part and once in the 'fallback' part.
   */
  private def finishTransformStat(stat: PreTransform): Tree = stat match {
    case PreTransBlock(stats, result) =>
      Block(stats :+ finishTransformStat(result))(stat.pos)
    case PreTransLocalDef(_) =>
      Skip()(stat.pos)
    case PreTransRecordTree(tree, _, _) =>
      keepOnlySideEffects(tree)
    case PreTransTree(tree, _) =>
      keepOnlySideEffects(tree)
  }

  /** Keeps only the side effects of a Tree (overapproximation). */
  private def keepOnlySideEffects(stat: Tree): Tree = stat match {
    case _:VarRef | _:This | _:Literal =>
      Skip()(stat.pos)
    case Block(init :+ last) =>
      Block(init :+ keepOnlySideEffects(last))(stat.pos)
    case LoadModule(ClassType(moduleClassName)) =>
      if (hasElidableModuleAccessor(moduleClassName)) Skip()(stat.pos)
      else stat
    case Closure(_, _, _, _, captures) =>
      Block(captures.map(keepOnlySideEffects))(stat.pos)
    case RecordValue(_, elems) =>
      Block(elems.map(keepOnlySideEffects))(stat.pos)
    case _ =>
      stat
  }

  private def pretransformApply(tree: Apply, isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    val Apply(receiver, methodIdent @ Ident(methodName, _), args) = tree
    implicit val pos = tree.pos

    pretransformExpr(receiver) { treceiver =>
      def treeNotInlined0(transformedArgs: List[Tree]) =
        cont(PreTransTree(Apply(finishTransformExpr(treceiver), methodIdent,
            transformedArgs)(tree.tpe)(tree.pos), RefinedType(tree.tpe)))

      def treeNotInlined = treeNotInlined0(args.map(transformExpr))

      treceiver.tpe.base match {
        case NothingType =>
          cont(treceiver)
        case NullType =>
          cont(PreTransTree(Block(
              finishTransformStat(treceiver),
              CallHelper("throwNullPointerException")(NothingType))))
        case ClassType(cls) =>
          tryInlineAnonFunction(treceiver, methodName, args, isStat,
              usePreTransform)(cont).getOrElse {

            if (isReflProxyName(methodName)) {
              // Never inline reflective proxies
              treeNotInlined
            } else {
              val impls =
                if (treceiver.tpe.isExact) staticCall(cls, methodName).toList
                else dynamicCall(cls, methodName)
              if (impls.isEmpty || impls.exists(scope.implsBeingInlined)) {
                // isEmpty could happen, have to leave it as is for the TypeError
                treeNotInlined
              } else if (impls.size == 1) {
                val target = impls.head
                pretransformExprs(args) { targs =>
                  if (target.inlineable || shouldInlineBecauseOfArgs(treceiver :: targs)) {
                    inline(Some(treceiver), targs, target, isStat,
                        usePreTransform)(cont)
                  } else {
                    treeNotInlined0(targs.map(finishTransformExpr))
                  }
                }
              } else {
                if (impls.forall(_.isTraitImplForwarder)) {
                  val reference = impls.head
                  val TraitImplApply(ClassType(traitImpl), Ident(methodName, _), _) =
                    getMethodBody(reference).body
                  if (!impls.tail.forall(getMethodBody(_).body match {
                    case TraitImplApply(ClassType(`traitImpl`),
                        Ident(`methodName`, _), _) => true
                    case _ => false
                  })) {
                    // Not all calling the same method in the same trait impl
                    treeNotInlined
                  } else {
                    pretransformExprs(args) { targs =>
                      inline(Some(treceiver), targs, reference, isStat,
                          usePreTransform)(cont)
                    }
                  }
                } else {
                  // TODO? Inline multiple non-trait-impl-forwarder with the exact same body?
                  treeNotInlined
                }
              }
            }
          }
        case _ =>
          /* Only AnyType should still happen here (if it is a reflective
           * call), if the IR is well typed. But in any case we're not going
           * to crash now, and instead just not inline the thing.
           */
          treeNotInlined
      }
    }
  }

  private def tryInlineAnonFunction(treceiver: PreTransform,
      methodName: String, args: List[Tree], isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): Option[TailRec[Tree]] = {
    treceiver match {
      case PreTransLocalDef(localDef) if !localDef.mutable =>
        localDef.replacement match {
          case TentativeAnonFunReplacement(closure, alreadyUsed, cancelFun) =>
            if (alreadyUsed.value)
              cancelFun() // no matter the method name
            if (methodName.matches("""^apply(__O)+$""")) {
              // Generic one, the one we can inline easily
              alreadyUsed.value = true
              Some(pretransformExprs(args) { targs =>
                inlineClosure(closure, targs, isStat, usePreTransform)(cont)
              })
            } else if (methodName.startsWith("apply$mc") &&
                !isReflProxyName(methodName)) {
              // A specialized one, we have to introduce the box/unbox
              alreadyUsed.value = true
              val shortName = methodName.split("__")(0)
              assert(shortName.endsWith("$sp"))
              val resultCharCode :: paramCharCodes =
                shortName.stripPrefix("apply$mc").stripSuffix("$sp").toList
              assert(isPrimitiveCharCode(resultCharCode))
              assert(paramCharCodes.length == args.size)
              assert(paramCharCodes.forall(isPrimitiveCharCode))
              Some(pretransformExprs(args) { targs =>
                val boxedTArgs = for {
                  (charCode, targ) <- paramCharCodes zip targs
                } yield {
                  if (charCode == 'C') {
                    val bcClassType = ClassType(Definitions.BoxedCharacterClass)
                    PreTransTree(CallHelper("bC", finishTransformExpr(targ))(
                        bcClassType)(targ.pos),
                        RefinedType(bcClassType, isExact = true, isNullable = false))
                  } else
                    targ
                }
                val isVoid = resultCharCode == 'V'
                inlineClosure(closure, boxedTArgs, isVoid, usePreTransform) { tinlined =>
                  if (isVoid) cont(tinlined)
                  else foldUnbox(resultCharCode, tinlined)(cont)
                }
              })
            } else {
              None
            }
          case _ =>
            None
        }
      case _ =>
        None
    }
  }

  private def pretransformStaticApply(tree: StaticApply, isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    val StaticApply(receiver, clsType @ ClassType(cls),
        methodIdent @ Ident(methodName, _), args) = tree
    implicit val pos = tree.pos

    def treeNotInlined0(transformedReceiver: Tree, transformedArgs: List[Tree]) =
      cont(PreTransTree(StaticApply(transformedReceiver, clsType,
          methodIdent, transformedArgs)(tree.tpe), RefinedType(tree.tpe)))

    def treeNotInlined =
      treeNotInlined0(transformExpr(receiver), args.map(transformExpr))

    if (isReflProxyName(methodName)) {
      // Never inline reflective proxies
      treeNotInlined
    } else {
      val optTarget = staticCall(cls, methodName)
      if (optTarget.isEmpty) {
        // just in case
        treeNotInlined
      } else {
        val target = optTarget.get
        if (scope.implsBeingInlined(target)) {
          treeNotInlined
        } else {
          pretransformExprs(receiver, args) { (treceiver, targs) =>
            if (target.inlineable || shouldInlineBecauseOfArgs(treceiver :: targs)) {
              inline(Some(treceiver), targs, target, isStat, usePreTransform)(cont)
            } else {
              treeNotInlined0(finishTransformExpr(treceiver),
                  targs.map(finishTransformExpr))
            }
          }
        }
      }
    }
  }

  private def pretransformTraitImplApply(tree: TraitImplApply, isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    val TraitImplApply(implType @ ClassType(impl),
        methodIdent @ Ident(methodName, _), args) = tree
    implicit val pos = tree.pos

    def treeNotInlined0(transformedArgs: List[Tree]) =
      cont(PreTransTree(TraitImplApply(implType, methodIdent,
          transformedArgs)(tree.tpe), RefinedType(tree.tpe)))

    def treeNotInlined = treeNotInlined0(args.map(transformExpr))

    val optTarget = traitImplCall(impl, methodName)
    if (optTarget.isEmpty) {
      // just in case
      treeNotInlined
    } else {
      val target = optTarget.get
      if (scope.implsBeingInlined(target)) {
        treeNotInlined
      } else {
        pretransformExprs(args) { targs =>
          if (target.inlineable || shouldInlineBecauseOfArgs(targs)) {
            inline(None, targs, target, isStat, usePreTransform)(cont)
          } else {
            treeNotInlined0(targs.map(finishTransformExpr))
          }
        }
      }
    }
  }

  private def shouldInlineBecauseOfArgs(
      receiverAndArgs: List[PreTransform]): Boolean = {
    def isLikelyOptimizable(arg: PreTransform): Boolean = arg match {
      case PreTransBlock(_, result) =>
        isLikelyOptimizable(result)

      case PreTransLocalDef(localDef) =>
        localDef.replacement match {
          case TentativeAnonFunReplacement(_, _, _)         => true
          case ReplaceWithRecordVarRef(_, _, _, _, _)       => true
          case InlineClassBeingConstructedReplacement(_, _) => true
          case InlineClassInstanceReplacement(_, _, _)      => true
          case _                                            => false
        }

      case PreTransRecordTree(_, _, _) =>
        true

      case _ =>
        false
    }
    receiverAndArgs.exists(isLikelyOptimizable)
  }

  private def inline(optReceiver: Option[PreTransform],
      args: List[PreTransform], target: MethodID, isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    attemptedInlining += target

    val MethodDef(_, formals, resultType, body) = getMethodBody(target)

    body match {
      case Skip() =>
        assert(isStat, "Found Skip() in expression position")
        cont(PreTransTree(
            Block((optReceiver ++: args).map(finishTransformStat)),
            RefinedType.NoRefinedType))

      case _: Literal =>
        cont(PreTransTree(
            Block((optReceiver ++: args).map(finishTransformStat) :+ body),
            RefinedType(body.tpe)))

      case This() if args.isEmpty =>
        assert(optReceiver.isDefined,
            "There was a This(), there should be a receiver")
        cont(optReceiver.get)

      case Select(This(), field, mutable) if formals.isEmpty =>
        assert(optReceiver.isDefined,
            "There was a This(), there should be a receiver")
        pretransformSelectCommon(body.tpe, optReceiver.get, field, mutable,
            isLhsOfAssign = false)(cont)

      case Assign(lhs @ Select(This(), field, mutable), VarRef(Ident(rhsName, _), _))
          if formals.size == 1 && formals.head.name.name == rhsName =>
        assert(isStat, "Found Assign in expression position")
        assert(optReceiver.isDefined,
            "There was a This(), there should be a receiver")
        pretransformSelectCommon(lhs.tpe, optReceiver.get, field, mutable,
            isLhsOfAssign = true) { preTransLhs =>
          // TODO Support assignment of record
          cont(PreTransTree(
              Assign(finishTransformExpr(preTransLhs),
                  finishTransformExpr(args.head)),
              RefinedType.NoRefinedType))
        }

      case _ =>
        inlineBody(optReceiver, formals, resultType, body, args, isStat,
            usePreTransform)(cont)(scope.inlining(target), pos)
    }
  }

  private def inlineClosure(closure: TentativeClosureReplacement,
      targs: List[PreTransform], isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {
    val TentativeClosureReplacement(thisType, formals, resultType, body,
        captureLocalDefs) = closure
    inlineBody(None, formals, resultType, body,
        captureLocalDefs.map(PreTransLocalDef(_)) ++ targs, isStat,
        usePreTransform)(cont)
  }

  private def inlineBody(optReceiver: Option[PreTransform],
      formals: List[ParamDef], resultType: Type, body: Tree,
      args: List[PreTransform], isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = tailcall {

    val optReceiverBinding = optReceiver map { receiver =>
      Binding("this", None, receiver.tpe.base, false, receiver)
    }

    val argsBindings = for {
      (ParamDef(Ident(name, originalName), tpe, mutable), arg) <- formals zip args
    } yield {
      Binding(name, originalName, tpe, mutable, arg)
    }

    withBindings(optReceiverBinding ++: argsBindings) { (bodyScope, cont1) =>
      returnable("", resultType, body, isStat, usePreTransform)(
          cont1)(bodyScope, pos)
    } (cont) (scope.withEnv(OptEnv.Empty))
  }

  private def inlineClassConstructor(cls: ClassType, initialValue: RecordValue,
      ctor: Ident, args: List[PreTransform], cancelFun: CancelFun)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    val RecordValue(recordType, initialFieldValues) = initialValue

    pretransformExprs(initialFieldValues) { tinitialFieldValues =>
      val initialFieldBindings = for {
        (RecordType.Field(name, originalName, tpe, mutable), value) <-
          recordType.fields zip tinitialFieldValues
      } yield {
        Binding(name, originalName, tpe, mutable, value)
      }

      withNewLocalDefs(initialFieldBindings) { (initialFieldLocalDefList, cont1) =>
        val fieldNames = initialValue.tpe.fields.map(_.name)
        val initialFieldLocalDefs =
          Map(fieldNames zip initialFieldLocalDefList: _*)

        inlineClassConstructorBody(initialFieldLocalDefs, cls, cls, ctor, args,
            cancelFun) { (finalFieldLocalDefs, cont2) =>
          cont2(PreTransLocalDef(LocalDef(
              RefinedType(cls, isExact = true, isNullable = false),
              mutable = false,
              InlineClassInstanceReplacement(recordType, finalFieldLocalDefs, cancelFun))))
        } (cont1)
      } (cont)
    }
  }

  private def inlineClassConstructorBody(
      inputFieldsLocalDefs: Map[String, LocalDef], cls: ClassType,
      ctorClass: ClassType, ctor: Ident, args: List[PreTransform],
      cancelFun: CancelFun)(
      buildInner: (Map[String, LocalDef], PreTransCont) => TailRec[Tree])(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = tailcall {

    val target = staticCall(ctorClass.className, ctor.name).getOrElse(cancelFun())
    if (scope.implsBeingInlined.contains(target))
      cancelFun()

    val MethodDef(_, formals, _, BlockOrAlone(stats, This())) =
      getMethodBody(target)

    val argsBindings = for {
      (ParamDef(Ident(name, originalName), tpe, mutable), arg) <- formals zip args
    } yield {
      Binding(name, originalName, tpe, mutable, arg)
    }

    withBindings(argsBindings) { (bodyScope, cont1) =>
      val thisLocalDef = LocalDef(
          RefinedType(cls, isExact = true, isNullable = false), false,
          InlineClassBeingConstructedReplacement(inputFieldsLocalDefs, cancelFun))
      val statsScope = bodyScope.inlining(target).withEnv(bodyScope.env.withLocalDef(
          "this", thisLocalDef))
      inlineClassConstructorBodyList(thisLocalDef, inputFieldsLocalDefs, cls,
          stats, cancelFun)(buildInner)(cont1)(statsScope)
    } (cont) (scope.withEnv(OptEnv.Empty))
  }

  private def inlineClassConstructorBodyList(
      thisLocalDef: LocalDef, inputFieldsLocalDefs: Map[String, LocalDef],
      cls: ClassType, stats: List[Tree], cancelFun: CancelFun)(
      buildInner: (Map[String, LocalDef], PreTransCont) => TailRec[Tree])(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    stats match {
      case This() :: rest =>
        inlineClassConstructorBodyList(thisLocalDef, inputFieldsLocalDefs,
            cls, rest, cancelFun)(buildInner)(cont)

      case Assign(s @ Select(ths: This,
          Ident(fieldName, fieldOrigName), false), value) :: rest =>
        pretransformExpr(value) { tvalue =>
          withNewLocalDef(Binding(fieldName, fieldOrigName, s.tpe, false,
              tvalue)) { (localDef, cont1) =>
            if (localDef.contains(thisLocalDef)) {
              /* Uh oh, there is a `val x = ...this...`. We can't keep it,
               * because this field will not be updated with `newThisLocalDef`.
               */
              cancelFun()
            }
            val newFieldsLocalDefs =
              inputFieldsLocalDefs.updated(fieldName, localDef)
            val newThisLocalDef = LocalDef(
                RefinedType(cls, isExact = true, isNullable = false), false,
                InlineClassBeingConstructedReplacement(newFieldsLocalDefs, cancelFun))
            val restScope = scope.withEnv(scope.env.withLocalDef(
                "this", newThisLocalDef))
            inlineClassConstructorBodyList(newThisLocalDef, newFieldsLocalDefs,
                cls, rest, cancelFun)(buildInner)(cont1)(restScope)
          } (cont)
        }

      /* if (cond)
       *   throw e
       * else
       *   this.outer = value
       *
       * becomes
       *
       * this.outer =
       *   if (cond) throw e
       *   else value
       *
       * Typical shape of initialization of outer pointer of inner classes.
       */
      case If(cond, th: Throw,
          Assign(Select(This(), _, false), value)) :: rest =>
        // work around a bug of the compiler (these should be @-bindings)
        val stat = stats.head.asInstanceOf[If]
        val ass = stat.elsep.asInstanceOf[Assign]
        val lhs = ass.lhs
        inlineClassConstructorBodyList(thisLocalDef, inputFieldsLocalDefs, cls,
            Assign(lhs, If(cond, th, value)(lhs.tpe)(stat.pos))(ass.pos) :: rest,
            cancelFun)(buildInner)(cont)

      case StaticApply(ths: This, superClass, superCtor, args) :: rest
          if isConstructorName(superCtor.name) =>
        pretransformExprs(args) { targs =>
          inlineClassConstructorBody(inputFieldsLocalDefs, cls, superClass,
              superCtor, targs, cancelFun) { (outputFieldsLocalDefs, cont1) =>
            val newThisLocalDef = LocalDef(
                RefinedType(cls, isExact = true, isNullable = false), false,
                InlineClassBeingConstructedReplacement(outputFieldsLocalDefs, cancelFun))
            val restScope = scope.withEnv(scope.env.withLocalDef(
                "this", newThisLocalDef))
            inlineClassConstructorBodyList(newThisLocalDef, outputFieldsLocalDefs,
                cls, rest, cancelFun)(buildInner)(cont1)(restScope)
          } (cont)
        }

      case VarDef(Ident(name, originalName), tpe, mutable, rhs) :: rest =>
        pretransformExpr(rhs) { trhs =>
          withBinding(Binding(name, originalName, tpe, mutable, trhs)) { (restScope, cont1) =>
            inlineClassConstructorBodyList(thisLocalDef, inputFieldsLocalDefs,
                cls, rest, cancelFun)(buildInner)(cont1)(restScope)
          } (cont)
        }

      case stat :: rest =>
        val transformedStat = transformStat(stat)
        transformedStat match {
          case Skip() =>
            inlineClassConstructorBodyList(thisLocalDef, inputFieldsLocalDefs,
                cls, rest, cancelFun)(buildInner)(cont)
          case _ =>
            if (transformedStat.tpe == NothingType)
              cont(PreTransTree(transformedStat, RefinedType.Nothing))
            else {
              inlineClassConstructorBodyList(thisLocalDef, inputFieldsLocalDefs,
                  cls, rest, cancelFun) { (outputFieldsLocalDefs, cont1) =>
                buildInner(outputFieldsLocalDefs, { tinner =>
                  cont1(PreTransBlock(transformedStat :: Nil, tinner))
                })
              }(cont)
            }
        }

      case Nil =>
        buildInner(inputFieldsLocalDefs, cont)
    }
  }

  private def foldIf(cond: Tree, thenp: Tree, elsep: Tree)(tpe: Type)(
      implicit pos: Position): Tree = {
    @inline def default = If(cond, thenp, elsep)(tpe)
    cond match {
      case BooleanLiteral(v) =>
        if (v) thenp
        else elsep

      case _ =>
        @inline def negCond = foldUnaryOp(UnaryOp.Boolean_!, cond)
        if (thenp.tpe == BooleanType && elsep.tpe == BooleanType) {
          (thenp, elsep) match {
            case (BooleanLiteral(true), BooleanLiteral(false)) => cond
            case (BooleanLiteral(false), BooleanLiteral(true)) => negCond

            case (BooleanLiteral(true), _) =>
              foldBinaryOp(BinaryOp.Boolean_||, cond, elsep)
            case (_, BooleanLiteral(false)) =>
              foldBinaryOp(BinaryOp.Boolean_&&, cond, thenp)

            case (BooleanLiteral(false), _) =>
              foldBinaryOp(BinaryOp.Boolean_&&, negCond, elsep)
            case (_, BooleanLiteral(true)) =>
              foldBinaryOp(BinaryOp.Boolean_||, negCond, thenp)

            case (BinaryOp(BinaryOp.===, VarRef(rhsIdent, _), Null()),
                BinaryOp(BinaryOp.===, VarRef(lhsIdent2, _), VarRef(rhsIdent2, _)))
                if rhsIdent2 == rhsIdent =>
              cond match {
                case BinaryOp(BinaryOp.===, VarRef(lhsIdent, _), Null())
                    if lhsIdent2 == lhsIdent =>
                  /* if (lhs === null) rhs === null else lhs === rhs
                   * -> lhs === rhs
                   * This is the typical shape of a lhs == rhs test where
                   * the equals() method has been inlined as a reference
                   * equality test.
                   */
                  elsep
                case _ =>
                  default
              }

            case _ => default
          }
        } else {
          (thenp, elsep) match {
            case (Skip(), Skip()) => keepOnlySideEffects(cond)
            case (Skip(), _)      => foldIf(negCond, elsep, thenp)(tpe)

            case _ => default
          }
        }
    }
  }

  private def foldUnaryOp(op: UnaryOp.Code, arg: Tree)(
      implicit pos: Position): Tree = {
    import UnaryOp._
    @inline def default = UnaryOp(op, arg)
    (op: @switch) match {
      case Int_+ | Double_+ =>
        arg
      case Int_- =>
        arg match {
          case IntLiteral(v)     => IntLiteral(-v)
          case UnaryOp(Int_-, x) => x
          case _                 => default
        }
      case Int_~ =>
        arg match {
          case IntLiteral(v)     => IntLiteral(~v)
          case UnaryOp(Int_~, x) => x
          case _                 => default
        }
      case Double_- =>
        arg match {
          case IntOrDoubleLit(v) => DoubleLiteral(-v)
          case _                 => default
        }
      case Boolean_! =>
        arg match {
          case BooleanLiteral(v)            => BooleanLiteral(!v)
          case UnaryOp(Boolean_!, x)        => x
          case BinaryOp(BinaryOp.===, l, r) => BinaryOp(BinaryOp.!==, l, r)
          case BinaryOp(BinaryOp.!==, l, r) => BinaryOp(BinaryOp.===, l, r)
          case BinaryOp(BinaryOp.<,   l, r) => BinaryOp(BinaryOp.>=,  l, r)
          case BinaryOp(BinaryOp.<=,  l, r) => BinaryOp(BinaryOp.>,   l, r)
          case BinaryOp(BinaryOp.>,   l, r) => BinaryOp(BinaryOp.<=,  l, r)
          case BinaryOp(BinaryOp.>=,  l, r) => BinaryOp(BinaryOp.<,   l, r)
          case _                            => default
        }
      case DoubleToInt =>
        arg match {
          case DoubleLiteral(v)        => IntLiteral(v.toInt)
          case _ if arg.tpe == IntType => arg
          case _                       => default
        }
      case _ =>
        default
    }
  }

  private def foldBinaryOp(op: BinaryOp.Code, lhs: Tree, rhs: Tree)(
      implicit pos: Position): Tree = {
    import BinaryOp._
    @inline def default = BinaryOp(op, lhs, rhs)
    (op: @switch) match {
      case === | !== =>
        def lit(v: Boolean) = BooleanLiteral(if (op == ===) v else !v)
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r))         => lit(l == r)
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => lit(l == r)
          case (BooleanLiteral(l), BooleanLiteral(r)) => lit(l == r)
          case (StringLiteral(l), StringLiteral(r))   => lit(l == r)
          case (Undefined(), Undefined())             => lit(true)
          case (Null(), Null())                       => lit(true)
          case (_: Literal, _: Literal)               => lit(false)

          case (BooleanLiteral(l), _) =>
            if (l == (op == ===)) rhs
            else foldUnaryOp(UnaryOp.Boolean_!, rhs)
          case (_, BooleanLiteral(r)) =>
            if (r == (op == ===)) lhs
            else foldUnaryOp(UnaryOp.Boolean_!, lhs)

          case _ => default
        }

      case Int_+ =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l + r)
          case (_, IntLiteral(0))             => lhs
          case (IntLiteral(0), _)             => rhs
          case (_, UnaryOp(UnaryOp.Int_-, x)) => foldBinaryOp(Int_-, lhs, x)
          case _                              => default
        }

      case Int_- =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l - r)
          case (_, IntLiteral(0))             => lhs
          case (IntLiteral(0), _)             => foldUnaryOp(UnaryOp.Int_-, rhs)
          case (_, UnaryOp(UnaryOp.Int_-, x)) => foldBinaryOp(Int_+, lhs, x)
          case _                              => default
        }

      case Int_* =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l * r)
          case (_, IntLiteral(1))             => lhs
          case (IntLiteral(1), _)             => rhs
          case (_, IntLiteral(-1))            => foldUnaryOp(UnaryOp.Int_-, lhs)
          case (IntLiteral(-1), _)            => foldUnaryOp(UnaryOp.Int_-, rhs)
          case _                              => default
        }

      case Int_/ =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) if r != 0 => IntLiteral(l / r)
          case (_, IntLiteral(1))                       => lhs
          case (_, IntLiteral(-1))                      => foldUnaryOp(UnaryOp.Int_-, lhs)
          case _                                        => default
        }

      case Int_% =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) if r != 0 => IntLiteral(l % r)
          case _                                        => default
        }

      case Int_| =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l | r)
          case (_, IntLiteral(0))             => lhs
          case (IntLiteral(0), _)             => rhs
          case _                              => default
        }

      case Int_& =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l & r)
          case (_, IntLiteral(-1))            => lhs
          case (IntLiteral(-1), _)            => rhs
          case _                              => default
        }

      case Int_^ =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l ^ r)
          case (_, IntLiteral(0))             => lhs
          case (IntLiteral(0), _)             => rhs
          case (_, IntLiteral(-1))            => foldUnaryOp(UnaryOp.Int_~, lhs)
          case (IntLiteral(-1), _)            => foldUnaryOp(UnaryOp.Int_~, rhs)
          case _                              => default
        }

      case Int_<< =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r))    => IntLiteral(l << r)
          case (_, IntLiteral(x)) if x % 32 == 0 => lhs
          case _                                 => default
        }

      case Int_>>> =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r))    => IntLiteral(l >>> r)
          case (_, IntLiteral(x)) if x % 32 == 0 => lhs
          case _                                 => default
        }

      case Int_>> =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r))    => IntLiteral(l >> r)
          case (_, IntLiteral(x)) if x % 32 == 0 => lhs
          case _                                 => default
        }

      case Double_+ =>
        (lhs, rhs) match {
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => DoubleLiteral(l + r)
          case (_, IntOrDoubleLit(0))                 => lhs
          case (IntOrDoubleLit(0), _)                 => rhs
          case (_, UnaryOp(UnaryOp.Double_-, x))      => foldBinaryOp(Double_-, lhs, x)
          case _                                      => default
        }

      case Double_- =>
        (lhs, rhs) match {
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => DoubleLiteral(l - r)
          case (_, IntOrDoubleLit(0))                 => lhs
          case (IntOrDoubleLit(0), _)                 => foldUnaryOp(UnaryOp.Double_-, rhs)
          case (_, UnaryOp(UnaryOp.Double_-, x))      => foldBinaryOp(Double_+, lhs, x)
          case _                                      => default
        }

      case Double_* =>
        (lhs, rhs) match {
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => DoubleLiteral(l * r)
          case (_, IntOrDoubleLit(1))                 => lhs
          case (IntOrDoubleLit(1), _)                 => rhs
          case (_, IntOrDoubleLit(-1))                => foldUnaryOp(UnaryOp.Double_-, lhs)
          case (IntOrDoubleLit(-1), _)                => foldUnaryOp(UnaryOp.Double_-, rhs)
          case _                                      => default
        }

      case Double_/ =>
        (lhs, rhs) match {
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => DoubleLiteral(l / r)
          case (_, IntOrDoubleLit(1))                 => lhs
          case (_, IntOrDoubleLit(-1))                => foldUnaryOp(UnaryOp.Double_-, lhs)
          case _                                      => default
        }

      case Double_% =>
        (lhs, rhs) match {
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => DoubleLiteral(l % r)
          case _                                      => default
        }

      case Boolean_| =>
        (lhs, rhs) match {
          case (BooleanLiteral(l), BooleanLiteral(r)) => BooleanLiteral(l | r)
          case (_, BooleanLiteral(false))             => lhs
          case (BooleanLiteral(false), _)             => rhs
          case _                                      => default
        }

      case Boolean_& =>
        (lhs, rhs) match {
          case (BooleanLiteral(l), BooleanLiteral(r)) => BooleanLiteral(l & r)
          case (_, BooleanLiteral(true))              => lhs
          case (BooleanLiteral(true), _)              => rhs
          case _                                      => default
        }

      case Boolean_^ =>
        (lhs, rhs) match {
          case (BooleanLiteral(l), BooleanLiteral(r)) => BooleanLiteral(l ^ r)
          case (_, BooleanLiteral(false))             => lhs
          case (BooleanLiteral(false), _)             => rhs
          case (_, BooleanLiteral(true))              => foldUnaryOp(UnaryOp.Boolean_!, lhs)
          case (BooleanLiteral(true), _)              => foldUnaryOp(UnaryOp.Boolean_!, rhs)
          case _                                      => default
        }

      case Boolean_|| =>
        (lhs, rhs) match {
          case (BooleanLiteral(true), _)  => lhs
          case (BooleanLiteral(false), _) => rhs
          case (_, BooleanLiteral(false)) => lhs
          case _                          => default
        }

      case Boolean_&& =>
        (lhs, rhs) match {
          case (BooleanLiteral(false), _) => lhs
          case (BooleanLiteral(true), _)  => rhs
          case (_, BooleanLiteral(true))  => lhs
          case _                          => default
        }

      case < =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r))         => BooleanLiteral(l < r)
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => BooleanLiteral(l < r)
          case _                                      => default
        }

      case <= =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r))         => BooleanLiteral(l <= r)
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => BooleanLiteral(l <= r)
          case _                                      => default
        }

      case > =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r))         => BooleanLiteral(l > r)
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => BooleanLiteral(l > r)
          case _                                      => default
        }

      case >= =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r))         => BooleanLiteral(l >= r)
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => BooleanLiteral(l >= r)
          case _                                      => default
        }

      case _ =>
        default
    }
  }

  private def foldUnbox(charCode: Char, arg: PreTransform)(
      cont: PreTransCont): TailRec[Tree] = {
    (charCode: @switch) match {
      case 'Z' if arg.tpe.base == BooleanType => cont(arg)
      case 'I' if arg.tpe.base == IntType     => cont(arg)
      case 'F' | 'D' if arg.tpe.base == DoubleType || arg.tpe.base == IntType => cont(arg)
      case _ =>
        val resultType = (charCode: @switch) match {
          case 'Z'                   => BooleanType
          case 'C' | 'B' | 'S' | 'I' => IntType
          case 'J'                   => ClassType(Definitions.RuntimeLongClass)
          case 'F' | 'D'             => DoubleType
        }
        cont(PreTransTree(
            CallHelper("u"+charCode, finishTransformExpr(arg))(resultType)(arg.pos),
            RefinedType(resultType, isExact = true, isNullable = false)))
    }
  }

  private def foldReferenceEquality(tlhs: PreTransform, trhs: PreTransform,
      positive: Boolean = true)(implicit pos: Position): Tree = {
    (tlhs, trhs) match {
      case (_, PreTransTree(Null(), _)) if !tlhs.tpe.isNullable =>
        Block(
            finishTransformStat(tlhs),
            BooleanLiteral(!positive))
      case (PreTransTree(Null(), _), _) if !trhs.tpe.isNullable =>
        Block(
            finishTransformStat(trhs),
            BooleanLiteral(!positive))
      case _ =>
        foldBinaryOp(if (positive) BinaryOp.=== else BinaryOp.!==,
            finishTransformExpr(tlhs), finishTransformExpr(trhs))
    }
  }

  private def foldObjectEquals(tlhs: PreTransform, trhs: PreTransform)(
      implicit scope: Scope, pos: Position): Tree = {
    import Definitions._

    val EqualsMethod = "equals__O__Z"

    tlhs.tpe.base match {
      case NothingType =>
        finishTransformExpr(tlhs)

      case NullType =>
        Block(
            finishTransformStat(tlhs),
            CallHelper("throwNullPointerException")(NothingType))

      case ClassType(lhsClassName0) =>
        val lhsClassName =
          if (lhsClassName0 == BoxedLongClass) RuntimeLongClass
          else lhsClassName0
        lhsClassName match {
          case BoxedFloatClass | BoxedDoubleClass =>
            CallHelper("numberEquals",
                finishTransformCheckNull(tlhs),
                finishTransformExpr(trhs))(BooleanType)

          case _ if HijackedClasses.contains(lhsClassName) =>
            foldReferenceEquality(
                PreTransTree(finishTransformCheckNull(tlhs)),
                trhs)

          case _ if AncestorsOfHijackedClasses.contains(lhsClassName) =>
            CallHelper("objectEquals",
                finishTransformExpr(tlhs),
                finishTransformExpr(trhs))(BooleanType)

          case _ =>
            def treeNotInlined =
              Apply(finishTransformExpr(tlhs), Ident(EqualsMethod),
                  List(finishTransformExpr(trhs)))(BooleanType)

            val impls =
              if (tlhs.tpe.isExact) staticCall(lhsClassName, EqualsMethod).toList
              else dynamicCall(lhsClassName, EqualsMethod)
            if (impls.size != 1 || impls.exists(scope.implsBeingInlined)) {
              // isEmpty could happen, have to leave it as is for the TypeError
              treeNotInlined
            } else if (impls.size == 1) {
              val target = impls.head
              if (target.inlineable || shouldInlineBecauseOfArgs(List(tlhs, trhs))) {
                trampoline {
                  inline(Some(tlhs), trhs :: Nil, target, isStat = false,
                      usePreTransform = false)(finishTransform(isStat = false))
                }
              } else {
                treeNotInlined
              }
            } else {
              treeNotInlined
            }
        }

      case IntType | BooleanType | UndefType | StringType | ArrayType(_, _) =>
        foldReferenceEquality(
            PreTransTree(finishTransformCheckNull(tlhs)),
            trhs)

      case DoubleType =>
        CallHelper("numberEquals",
            finishTransformExpr(tlhs),
            finishTransformExpr(trhs))(BooleanType)

      case _ =>
        CallHelper("objectEquals",
            finishTransformExpr(tlhs),
            finishTransformExpr(trhs))(BooleanType)
    }
  }

  private def finishTransformCheckNull(preTrans: PreTransform)(
      implicit pos: Position): Tree = {
    if (preTrans.tpe.isNullable) {
      val transformed = finishTransformExpr(preTrans)
      CallHelper("checkNonNull", transformed)(transformed.tpe)
    } else {
      finishTransformExpr(preTrans)
    }
  }

  def transformIsolatedBody(optTarget: Option[MethodID],
      thisType: Type, params: List[ParamDef], resultType: Type,
      body: Tree): (List[ParamDef], Tree) = {
    val (paramLocalDefs, newParamDefs) = (for {
      p @ ParamDef(ident @ Ident(name, originalName), ptpe, mutable) <- params
    } yield {
      val newName = freshLocalName(name)
      val newOriginalName = originalName.orElse(Some(newName))
      val localDef = LocalDef(RefinedType(ptpe), mutable,
          ReplaceWithVarRef(newName, newOriginalName, new SimpleState(true)))
      val newParamDef = ParamDef(
          Ident(newName, newOriginalName)(ident.pos), ptpe, mutable)(p.pos)
      ((name -> localDef), newParamDef)
    }).unzip

    val thisLocalDef =
      if (thisType == NoType) None
      else {
        Some("this" -> LocalDef(
            RefinedType(thisType, isExact = false, isNullable = false),
            false, ReplaceWithThis()))
      }

    val allLocalDefs = thisLocalDef ++: paramLocalDefs

    val scope0 = optTarget.fold(Scope.Empty)(Scope.Empty.inlining(_))
    val scope = scope0.withEnv(OptEnv.Empty.withLocalDefs(allLocalDefs))
    val newBody =
      transform(body, resultType == NoType)(scope)

    (newParamDefs, newBody)
  }

  private def returnable(oldLabelName: String, resultType: Type,
      body: Tree, isStat: Boolean, usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = tailcall {
    val newLabel = freshLabelName(
        if (oldLabelName.isEmpty) "inlinereturn" else oldLabelName)

    def doMakeTree(newBody: Tree, returnedTypes: List[Type]): Tree = {
      val refinedType =
        returnedTypes.reduce(constrainedLub(_, _, resultType))
      val returnCount = returnedTypes.size - 1

      tryOptimizePatternMatch(oldLabelName, refinedType,
          returnCount, newBody) getOrElse {
        Labeled(Ident(newLabel, None), refinedType, newBody)
      }
    }

    val info = new LabelInfo(newLabel, acceptRecords = usePreTransform)
    withState(info.returnedTypes) {
      val bodyScope = scope.withEnv(scope.env.withLabelInfo(oldLabelName, info))

      if (usePreTransform) {
        assert(!isStat, "Cannot use pretransform in statement position")
        tryOrRollback { cancelFun =>
          pretransformExpr(body) { tbody0 =>
            val returnedTypes0 = info.returnedTypes.value
            if (returnedTypes0.isEmpty) {
              // no return to that label, we can eliminate it
              cont(tbody0)
            } else {
              val tbody = resolveLocalDef(tbody0)
              val (newBody, returnedTypes) = tbody match {
                case PreTransRecordTree(bodyTree, origType, _) =>
                  (bodyTree, (bodyTree.tpe, origType) :: returnedTypes0)
                case PreTransTree(bodyTree, tpe) =>
                  (bodyTree, (bodyTree.tpe, tpe) :: returnedTypes0)
              }
              val (actualTypes, origTypes) = returnedTypes.unzip
              val refinedOrigType =
                origTypes.reduce(constrainedLub(_, _, resultType))
              actualTypes.collectFirst {
                case actualType: RecordType => actualType
              }.fold[TailRec[Tree]] {
                // None of the returned types are records
                cont(PreTransTree(
                    doMakeTree(newBody, actualTypes), refinedOrigType))
              } { recordType =>
                if (actualTypes.exists(t => t != recordType && t != NothingType))
                  cancelFun()

                val resultTree = doMakeTree(newBody, actualTypes)

                if (origTypes.exists(t => t != refinedOrigType && !t.isNothingType))
                  cancelFun()

                cont(PreTransRecordTree(resultTree, refinedOrigType, cancelFun))
              }
            }
          } (bodyScope)
        } { () =>
          returnable(oldLabelName, resultType, body, isStat,
              usePreTransform = false)(cont)
        }
      } else {
        val newBody = transform(body, isStat)(bodyScope)
        val returnedTypes0 = info.returnedTypes.value.map(_._1)
        if (returnedTypes0.isEmpty) {
          // no return to that label, we can eliminate it
          cont(PreTransTree(newBody, RefinedType(newBody.tpe)))
        } else {
          val returnedTypes = newBody.tpe :: returnedTypes0
          val tree = doMakeTree(newBody, returnedTypes)
          cont(PreTransTree(tree, RefinedType(tree.tpe)))
        }
      }
    }
  }

  def tryOptimizePatternMatch(oldLabelName: String, refinedType: Type,
      returnCount: Int, newBody: Tree): Option[Tree] = {
    if (!oldLabelName.startsWith("matchEnd")) None
    else {
      newBody match {
        case Block(stats) =>
          @tailrec
          def createRevAlts(xs: List[Tree], acc: List[(Tree, Tree)]): List[(Tree, Tree)] = xs match {
            case If(cond, body, Skip()) :: xr =>
              createRevAlts(xr, (cond, body) :: acc)
            case remaining =>
              (EmptyTree, Block(remaining)(remaining.head.pos)) :: acc
          }
          val revAlts = createRevAlts(stats, Nil)

          if (revAlts.size == returnCount) {
            @tailrec
            def constructOptimized(revAlts: List[(Tree, Tree)], elsep: Tree): Option[Tree] = {
              revAlts match {
                case (cond, body) :: revAltsRest =>
                  body match {
                    case BlockOrAlone(prep,
                        Return(result, Some(Ident(newLabel, _)))) =>
                      val result1 =
                        if (refinedType == NoType) keepOnlySideEffects(result)
                        else result
                      val prepAndResult = Block(prep :+ result1)(body.pos)
                      if (cond == EmptyTree) {
                        assert(elsep == EmptyTree)
                        constructOptimized(revAltsRest, prepAndResult)
                      } else {
                        assert(elsep != EmptyTree)
                        constructOptimized(revAltsRest,
                            foldIf(cond, prepAndResult, elsep)(refinedType)(cond.pos))
                      }
                    case _ =>
                      None
                  }
                case Nil =>
                  Some(elsep)
              }
            }
            constructOptimized(revAlts, EmptyTree)
          } else None
        case _ =>
          None
      }
    }
  }

  private def withBindings(bindings: List[Binding])(
      buildInner: (Scope, PreTransCont) => TailRec[Tree])(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    withNewLocalDefs(bindings) { (localDefs, cont1) =>
      val newMappings = for {
        (binding, localDef) <- bindings zip localDefs
      } yield {
        binding.name -> localDef
      }
      buildInner(scope.withEnv(scope.env.withLocalDefs(newMappings)), cont1)
    } (cont)
  }

  private def withBinding(binding: Binding)(
      buildInner: (Scope, PreTransCont) => TailRec[Tree])(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    withNewLocalDef(binding) { (localDef, cont1) =>
      buildInner(scope.withEnv(scope.env.withLocalDef(binding.name, localDef)),
          cont1)
    } (cont)
  }

  private def withNewLocalDefs(bindings: List[Binding])(
      buildInner: (List[LocalDef], PreTransCont) => TailRec[Tree])(
      cont: PreTransCont): TailRec[Tree] = {
    bindings match {
      case first :: rest =>
        withNewLocalDef(first) { (firstLocalDef, cont1) =>
          withNewLocalDefs(rest) { (restLocalDefs, cont2) =>
            buildInner(firstLocalDef :: restLocalDefs, cont2)
          } (cont1)
        } (cont)

      case Nil =>
        buildInner(Nil, cont)
    }
  }

  private def isImmutableType(tpe: Type): Boolean = tpe match {
    case RecordType(fields) =>
      fields.forall(f => !f.mutable && isImmutableType(f.tpe))
    case _ =>
      true
  }

  private def withNewLocalDef(binding: Binding)(
      buildInner: (LocalDef, PreTransCont) => TailRec[Tree])(
      cont: PreTransCont): TailRec[Tree] = tailcall {
    val Binding(name, originalName, declaredType, mutable, value) = binding
    implicit val pos = value.pos

    def withDedicatedVar(tpe: RefinedType): TailRec[Tree] = {
      val newName = freshLocalName(name)
      val newOriginalName = originalName.orElse(Some(name))

      val used = new SimpleState(false)
      withState(used) {
        def doBuildInner(localDef: LocalDef)(varDef: => VarDef): TailRec[Tree] = {
          buildInner(localDef, { tinner =>
            if (used.value) {
              cont(PreTransBlock(varDef :: Nil, tinner))
            } else {
              tinner match {
                case PreTransLocalDef(`localDef`) =>
                  cont(value)
                case _ if tinner.contains(localDef) =>
                  cont(PreTransBlock(varDef :: Nil, tinner))
                case _ =>
                  val rhsSideEffects = finishTransformStat(value)
                  rhsSideEffects match {
                    case Skip() =>
                      cont(tinner)
                    case _ =>
                      if (rhsSideEffects.tpe == NothingType)
                        cont(PreTransTree(rhsSideEffects, RefinedType.Nothing))
                      else
                        cont(PreTransBlock(rhsSideEffects :: Nil, tinner))
                  }
                }
            }
          })
        }

        resolveLocalDef(value) match {
          case PreTransRecordTree(valueTree, valueTpe, cancelFun) =>
            val recordType = valueTree.tpe.asInstanceOf[RecordType]
            if (!isImmutableType(recordType))
              cancelFun()
            val localDef = LocalDef(valueTpe, mutable,
                ReplaceWithRecordVarRef(newName, newOriginalName, recordType,
                    used, cancelFun))
            doBuildInner(localDef) {
              VarDef(Ident(newName, newOriginalName), recordType, mutable,
                  valueTree)
            }

          case PreTransTree(valueTree, valueTpe) =>
            val localDef = LocalDef(tpe, mutable,
                ReplaceWithVarRef(newName, newOriginalName, used))
            doBuildInner(localDef) {
              VarDef(Ident(newName, newOriginalName), tpe.base, mutable,
                  valueTree)
            }
        }
      }
    }

    if (value.tpe.isNothingType) {
      cont(value)
    } else if (mutable) {
      withDedicatedVar(RefinedType(declaredType))
    } else {
      val refinedType = value.tpe
      value match {
        case PreTransBlock(stats, result) =>
          withNewLocalDef(binding.copy(value = result))(buildInner) { tresult =>
            cont(PreTransBlock(stats, tresult))
          }

        case PreTransLocalDef(localDef) if !localDef.mutable =>
          buildInner(localDef, cont)

        case PreTransTree(literal: Literal, _) =>
          buildInner(LocalDef(refinedType, false,
              ReplaceWithConstant(literal)), cont)

        case PreTransTree(VarRef(Ident(refName, refOriginalName), false), _) =>
          buildInner(LocalDef(refinedType, false,
              ReplaceWithVarRef(refName, refOriginalName,
                  new SimpleState(true))), cont)

        case _ =>
          withDedicatedVar(refinedType)
      }
    }
  }

  /** Finds a type as precise as possible which is a supertype of lhs and rhs
   *  but still a subtype of upperBound.
   *  Requires that lhs and rhs be subtypes of upperBound, obviously.
   */
  private def constrainedLub(lhs: RefinedType, rhs: RefinedType,
      upperBound: Type): RefinedType = {
    if (upperBound == NoType) RefinedType(upperBound)
    else if (lhs == rhs) lhs
    else if (lhs.isNothingType) rhs
    else if (rhs.isNothingType) lhs
    else {
      RefinedType(constrainedLub(lhs.base, rhs.base, upperBound),
          false, lhs.isNullable || rhs.isNullable)
    }
  }

  /** Finds a type as precise as possible which is a supertype of lhs and rhs
   *  but still a subtype of upperBound.
   *  Requires that lhs and rhs be subtypes of upperBound, obviously.
   */
  private def constrainedLub(lhs: Type, rhs: Type, upperBound: Type): Type = {
    // TODO Improve this
    if (upperBound == NoType) upperBound
    else if (lhs == rhs) lhs
    else if (lhs == NothingType) rhs
    else if (rhs == NothingType) lhs
    else upperBound
  }

  /** Trampolines a pretransform */
  private def trampoline(tailrec: => TailRec[Tree]): Tree = {
    curTrampolineId += 1

    val myTrampolineId = curTrampolineId

    try {
      var rec = () => tailrec

      while (true) {
        try {
          return rec().result
        } catch {
          case e: RollbackException if e.trampolineId == myTrampolineId =>
            rollbacksCount += 1
            if (rollbacksCount > MaxRollbacksPerMethod)
              throw new TooManyRollbacksException

            usedLocalNames.clear()
            usedLocalNames ++= e.savedUsedLocalNames
            usedLabelNames.clear()
            usedLabelNames ++= e.savedUsedLabelNames
            for ((state, backup) <- statesInUse zip e.savedStates)
              state.asInstanceOf[State[Any]].restore(backup)

            rec = e.cont
        }
      }

      sys.error("Reached end of infinite loop")
    } finally {
      curTrampolineId -= 1
    }
  }
}

object OptimizerCore {

  private final val MaxRollbacksPerMethod = 256

  private final class TooManyRollbacksException
      extends scala.util.control.ControlThrowable

  private val AnonFunctionClassPrefix = "sjsr_AnonFunction"

  private val isPrimitiveCharCode =
    Set('V', 'Z', 'C', 'B', 'S', 'I', 'J', 'F', 'D')

  private type CancelFun = () => Nothing
  private type PreTransCont = PreTransform => TailRec[Tree]

  private case class RefinedType(base: Type, isExact: Boolean,
      isNullable: Boolean) {
    def isNothingType: Boolean = base == NothingType
  }

  private object RefinedType {
    def apply(tpe: Type): RefinedType = tpe match {
      case BooleanType | IntType | DoubleType | StringType | UndefType |
          NothingType | _:RecordType | NoType =>
        RefinedType(tpe, isExact = true, isNullable = false)
      case NullType =>
        RefinedType(tpe, isExact = true, isNullable = true)
      case _ =>
        RefinedType(tpe, isExact = false, isNullable = true)
    }

    val NoRefinedType = RefinedType(NoType)
    val Nothing = RefinedType(NothingType)
  }

  private case class LocalDef(
      tpe: RefinedType,
      mutable: Boolean,
      replacement: LocalDefReplacement) {

    def newReplacement(implicit pos: Position): Tree = replacement match {
      case ReplaceWithVarRef(name, originalName, used) =>
        used.value = true
        VarRef(Ident(name, originalName), mutable)(tpe.base)

      case ReplaceWithRecordVarRef(_, _, _, _, cancelFun) =>
        cancelFun()

      case ReplaceWithThis() =>
        This()(tpe.base)

      case ReplaceWithConstant(value) =>
        value

      case TentativeAnonFunReplacement(closure, alreadyUsed, cancelFun) =>
        cancelFun()

      case InlineClassBeingConstructedReplacement(_, cancelFun) =>
        cancelFun()

      case InlineClassInstanceReplacement(_, _, cancelFun) =>
        cancelFun()
    }

    def contains(that: LocalDef): Boolean = {
      (this eq that) || (replacement match {
        case TentativeAnonFunReplacement(
            TentativeClosureReplacement(_, _, _, _, captureLocalDefs), _, _) =>
          captureLocalDefs.exists(_.contains(that))
        case InlineClassInstanceReplacement(_, fieldLocalDefs, _) =>
          fieldLocalDefs.valuesIterator.exists(_.contains(that))
        case _ =>
          false
      })
    }
  }

  private sealed abstract class LocalDefReplacement

  private final case class ReplaceWithVarRef(name: String,
      originalName: Option[String],
      used: SimpleState[Boolean]) extends LocalDefReplacement

  private final case class ReplaceWithRecordVarRef(name: String,
      originalName: Option[String],
      recordType: RecordType,
      used: SimpleState[Boolean],
      cancelFun: CancelFun) extends LocalDefReplacement

  private final case class ReplaceWithThis() extends LocalDefReplacement

  private final case class ReplaceWithConstant(
      value: Tree) extends LocalDefReplacement

  private final case class TentativeAnonFunReplacement(
      closure: TentativeClosureReplacement,
      alreadyUsed: SimpleState[Boolean],
      cancelFun: CancelFun) extends LocalDefReplacement

  private final case class TentativeClosureReplacement(
      thisType: Type, params: List[ParamDef], resultType: Type, body: Tree,
      captures: List[LocalDef])

  private final case class InlineClassBeingConstructedReplacement(
      fieldLocalDefs: Map[String, LocalDef],
      cancelFun: CancelFun) extends LocalDefReplacement

  private final case class InlineClassInstanceReplacement(
      recordType: RecordType,
      fieldLocalDefs: Map[String, LocalDef],
      cancelFun: CancelFun) extends LocalDefReplacement

  private final class LabelInfo(
      val newName: String,
      val acceptRecords: Boolean,
      /** (actualType, originalType), actualType can be a RecordType. */
      val returnedTypes: SimpleState[List[(Type, RefinedType)]] = new SimpleState(Nil))

  private class OptEnv(
      val localDefs: Map[String, LocalDef],
      val labelInfos: Map[String, LabelInfo]) {

    def withLocalDef(oldName: String, rep: LocalDef): OptEnv =
      new OptEnv(localDefs + (oldName -> rep), labelInfos)

    def withLocalDefs(reps: List[(String, LocalDef)]): OptEnv =
      new OptEnv(localDefs ++ reps, labelInfos)

    def withLabelInfo(oldName: String, info: LabelInfo): OptEnv =
      new OptEnv(localDefs, labelInfos + (oldName -> info))

    def withinFunction(paramLocalDefs: List[(String, LocalDef)]): OptEnv =
      new OptEnv(localDefs ++ paramLocalDefs, Map.empty)

    override def toString(): String = {
      "localDefs:"+localDefs.mkString("\n  ", "\n  ", "\n") +
      "labelInfos:"+labelInfos.mkString("\n  ", "\n  ", "")
    }
  }

  private object OptEnv {
    val Empty: OptEnv = new OptEnv(Map.empty, Map.empty)
  }

  private class Scope(val env: OptEnv,
      val implsBeingInlined: Set[AbstractMethodID]) {
    def withEnv(env: OptEnv): Scope =
      new Scope(env, implsBeingInlined)

    def inlining(impl: AbstractMethodID): Scope = {
      assert(!implsBeingInlined(impl), s"Circular inlining of $impl")
      new Scope(env, implsBeingInlined + impl)
    }
  }

  private object Scope {
    val Empty: Scope = new Scope(OptEnv.Empty, Set.empty)
  }

  /** The result of pretransformExpr().
   *  It has a `tpe` as precisely refined as if a full transformExpr() had
   *  been performed.
   *  It is also not dependent on the environment anymore. In some sense, it
   *  has "captured" its environment at definition site.
   */
  private sealed abstract class PreTransform {
    def pos: Position
    val tpe: RefinedType

    def contains(localDef: LocalDef): Boolean = this match {
      case PreTransBlock(_, result) =>
        result.contains(localDef)
      case PreTransLocalDef(thisLocalDef) =>
        thisLocalDef.contains(localDef)
      case _ =>
        false
    }
  }

  private final class PreTransBlock private (val stats: List[Tree],
      val result: PreTransLocalDef) extends PreTransform {
    def pos = result.pos
    val tpe = result.tpe

    assert(stats.nonEmpty)

    override def toString(): String =
      s"PreTransBlock($stats,$result)"
  }

  private object PreTransBlock {
    def apply(stats: List[Tree], result: PreTransform): PreTransform = {
      if (stats.isEmpty) result
      else {
        result match {
          case PreTransBlock(innerStats, innerResult) =>
            new PreTransBlock(stats ++ innerStats, innerResult)
          case result: PreTransLocalDef =>
            new PreTransBlock(stats, result)
          case PreTransRecordTree(tree, tpe, cancelFun) =>
            PreTransRecordTree(Block(stats :+ tree)(tree.pos), tpe, cancelFun)
          case PreTransTree(tree, tpe) =>
            PreTransTree(Block(stats :+ tree)(tree.pos), tpe)
        }
      }
    }

    def unapply(preTrans: PreTransBlock): Some[(List[Tree], PreTransLocalDef)] =
      Some(preTrans.stats, preTrans.result)
  }

  private sealed abstract class PreTransNoBlock extends PreTransform

  private final case class PreTransLocalDef(localDef: LocalDef)(
      implicit val pos: Position) extends PreTransNoBlock {
    val tpe: RefinedType = localDef.tpe
  }

  private sealed abstract class PreTransGenTree extends PreTransNoBlock

  private final case class PreTransRecordTree(tree: Tree,
      tpe: RefinedType, cancelFun: CancelFun) extends PreTransGenTree {
    def pos = tree.pos

    assert(tree.tpe.isInstanceOf[RecordType],
        s"Cannot create a PreTransRecordTree with non-record type ${tree.tpe}")
  }

  private final case class PreTransTree(tree: Tree,
      tpe: RefinedType) extends PreTransGenTree {
    def pos: Position = tree.pos

    assert(!tree.tpe.isInstanceOf[RecordType],
        s"Cannot create a Tree with record type ${tree.tpe}")
  }

  private object PreTransTree {
    def apply(tree: Tree): PreTransTree =
      PreTransTree(tree, RefinedType(tree.tpe))
  }

  private final case class Binding(name: String, originalName: Option[String],
      declaredType: Type, mutable: Boolean, value: PreTransform)

  private object IntOrDoubleLit {
    def unapply(tree: Literal): Option[Double] = tree match {
      case DoubleLiteral(v) => Some(v)
      case IntLiteral(v)    => Some(v.toDouble)
      case _                => None
    }
  }

  private trait State[A] {
    def makeBackup(): A
    def restore(backup: A): Unit
  }

  private class SimpleState[A](var value: A) extends State[A] {
    def makeBackup(): A = value
    def restore(backup: A): Unit = value = backup
  }

  trait AbstractMethodID {
    def inlineable: Boolean
    def isTraitImplForwarder: Boolean
  }

  /** Parts of [[GenIncOptimizer#MethodImpl]] with decisions about optimizations. */
  abstract class MethodImpl {
    def encodedName: String
    def optimizerHints: OptimizerHints
    def originalDef: MethodDef
    def thisType: Type

    var inlineable: Boolean = false
    var isTraitImplForwarder: Boolean = false

    protected def updateInlineable(): Unit = {
      val MethodDef(Ident(methodName, _), params, _, body) = originalDef

      isTraitImplForwarder = body match {
        // Shape of forwarders to trait impls
        case TraitImplApply(impl, method, args) =>
          ((args.size == params.size + 1) &&
              (args.head.isInstanceOf[This]) &&
              (args.tail.zip(params).forall {
                case (VarRef(Ident(aname, _), _),
                    ParamDef(Ident(pname, _), _, _)) => aname == pname
                case _ => false
              }))

        case _ => false
      }

      inlineable = optimizerHints.hasInlineAnnot || isTraitImplForwarder || {
        val MethodDef(_, params, _, body) = originalDef
        body match {
          case _:Skip | _:This | _:Literal                          => true

          // Shape of accessors
          case Select(This(), _, _) if params.isEmpty               => true
          case Assign(Select(This(), _, _), VarRef(_, _))
              if params.size == 1                                   => true

          // Shape of trivial call-super constructors
          case Block(stats)
              if params.isEmpty && isConstructorName(encodedName) &&
                  stats.forall(isTrivialConstructorStat)            => true

          // Simple method
          case SimpleMethodBody()                                   => true

          case _ => false
        }
      }
    }
  }

  private def isTrivialConstructorStat(stat: Tree): Boolean = stat match {
    case This() =>
      true
    case StaticApply(This(), _, _, Nil) =>
      true
    case TraitImplApply(_, Ident(methodName, _), This() :: Nil) =>
      methodName.contains("__$init$__")
    case _ =>
      false
  }

  private object SimpleMethodBody {
    @tailrec
    def unapply(body: Tree): Boolean = body match {
      case New(_, _, args)                   => areSimpleArgs(args)
      case Apply(receiver, _, args)          => areSimpleArgs(receiver :: args)
      case StaticApply(receiver, _, _, args) => areSimpleArgs(receiver :: args)
      case TraitImplApply(_, _, args)        => areSimpleArgs(args)
      case Select(qual, _, _)                => isSimpleArg(qual)
      case IsInstanceOf(inner, _)            => isSimpleArg(inner)

      case CallHelper(helper, List(inner)) =>
        isBoxUnboxHelper(helper) && unapply(inner)
      case Block(List(inner, Undefined())) =>
        unapply(inner)

      case AsInstanceOf(inner, _) => unapply(inner)
      case Cast(inner, _)         => unapply(inner)

      case _ => isSimpleArg(body)
    }

    private def areSimpleArgs(args: List[Tree]): Boolean =
      args.forall(isSimpleArg)

    @tailrec
    private def isSimpleArg(arg: Tree): Boolean = arg match {
      case New(_, _, Nil)                   => true
      case Apply(receiver, _, Nil)          => isTrivialArg(receiver)
      case StaticApply(receiver, _, _, Nil) => isTrivialArg(receiver)
      case TraitImplApply(_, _, Nil)        => true

      case ArrayLength(array)        => isTrivialArg(array)
      case ArraySelect(array, index) => isTrivialArg(array) && isTrivialArg(index)

      case CallHelper(helper, List(inner)) =>
        isBoxUnboxHelper(helper) && isSimpleArg(inner)

      case AsInstanceOf(inner, _) => isSimpleArg(inner)
      case Cast(inner, _)         => isSimpleArg(inner)

      case _ =>
        isTrivialArg(arg)
    }

    private def isTrivialArg(arg: Tree): Boolean = arg match {
      case _:VarRef | _:This | _:Literal | _:LoadModule | _:ClassOf =>
        true
      case Cast(inner, _) =>
        isTrivialArg(inner)
      case _ =>
        false
    }

    private val isBoxUnboxHelper =
      Set("bC", "uZ", "uC", "uB", "uS", "uI", "uJ", "uF", "uD")
  }

  private object BlockOrAlone {
    def unapply(tree: Tree): Some[(List[Tree], Tree)] = Some(tree match {
      case Block(init :+ last) => (init, last)
      case _                   => (Nil, tree)
    })
  }

  /** Recreates precise [[Infos.MethodInfo]] from the optimized [[MethodDef]]. */
  private def recreateInfo(methodDef: MethodDef): Infos.MethodInfo = {
    new RecreateInfoTraverser().recreateInfo(methodDef)
  }

  private final class RecreateInfoTraverser extends Traversers.Traverser {
    import RecreateInfoTraverser._

    private val calledMethods = mutable.Map.empty[String, mutable.Set[String]]
    private val calledMethodsStatic = mutable.Map.empty[String, mutable.Set[String]]
    private val instantiatedClasses = mutable.Set.empty[String]
    private val accessedModules = mutable.Set.empty[String]
    private val accessedClassData = mutable.Set.empty[String]

    def recreateInfo(methodDef: MethodDef): Infos.MethodInfo = {
      traverse(methodDef.body)
      Infos.MethodInfo(
          encodedName = methodDef.name.name,
          calledMethods = calledMethods.toMap.mapValues(_.toList),
          calledMethodsStatic = calledMethodsStatic.toMap.mapValues(_.toList),
          instantiatedClasses = instantiatedClasses.toList,
          accessedModules = accessedModules.toList,
          accessedClassData = accessedClassData.toList)
    }

    private def addCalledMethod(container: String, methodName: String): Unit =
      calledMethods.getOrElseUpdate(container, mutable.Set.empty) += methodName

    private def addCalledMethodStatic(container: String, methodName: String): Unit =
      calledMethodsStatic.getOrElseUpdate(container, mutable.Set.empty) += methodName

    private def typeToContainer(tpe: Type): String = tpe match {
      case ClassType(cls) => cls
      case _              => Definitions.ObjectClass
    }

    private def refTypeToClassData(tpe: ReferenceType): String = tpe match {
      case ClassType(cls)     => cls
      case ArrayType(base, _) => base
    }

    def addAccessedClassData(encodedName: String): Unit = {
      if (!AlwaysPresentClassData.contains(encodedName))
        accessedClassData += encodedName
    }

    def addAccessedClassData(tpe: ReferenceType): Unit =
      addAccessedClassData(refTypeToClassData(tpe))

    override def traverse(tree: Tree): Unit = {
      tree match {
        case New(ClassType(cls), ctor, _) =>
          instantiatedClasses += cls
          addCalledMethodStatic(cls, ctor.name)

        case Apply(receiver, method, _) =>
          addCalledMethod(typeToContainer(receiver.tpe), method.name)
        case StaticApply(_, ClassType(cls), method, _) =>
          addCalledMethodStatic(cls, method.name)
        case TraitImplApply(ClassType(impl), method, _) =>
          addCalledMethodStatic(impl, method.name)

        case LoadModule(ClassType(cls)) =>
          accessedModules += cls.stripSuffix("$")

        case NewArray(tpe, _) =>
          addAccessedClassData(tpe)
        case ArrayValue(tpe, _) =>
          addAccessedClassData(tpe)
        case IsInstanceOf(_, cls) =>
          addAccessedClassData(cls)
        case AsInstanceOf(_, cls) =>
          addAccessedClassData(cls)
        case ClassOf(cls) =>
          addAccessedClassData(cls)

        case CallHelper(helper, receiver :: _) =>
          import Definitions._
          HelperTargets.get(helper) foreach { method =>
            receiver.tpe match {
              case AnyType =>
                addCalledMethod(ObjectClass, method)
              case ClassType(cls) =>
                if (!HijackedClasses.contains(cls) || cls == BoxedLongClass)
                  addCalledMethod(cls, method)
              case _ =>
            }
          }

        case _ =>
      }
      super.traverse(tree)
    }

    private val HelperTargets = {
      import Definitions._
      Map[String, String](
        "objectToString"  -> "toString__T",
        "objectClone"     -> "clone__O",
        "objectFinalize"  -> "finalize__V",
        "objectNotify"    -> "notify__V",
        "objectNotifyAll" -> "notifyAll__V",
        "objectEquals"    -> "equals__O__Z",
        "objectHashCode"  -> "hashCode__I",

        "charSequenceLength"      -> "length__I",
        "charSequenceCharAt"      -> "charAt__I__C",
        "charSequenceSubSequence" -> s"subSequence__I__I__$CharSequenceClass",

        "comparableCompareTo" -> "compareTo__O__I",

        "numberByteValue"   -> "byteValue__B",
        "numberShortValue"  -> "shortValue__S",
        "numberIntValue"    -> "intValue__I",
        "numberLongValue"   -> "longValue__J",
        "numberFloatValue"  -> "floatValue__F",
        "numberDoubleValue" -> "doubleValue__D"
      )
    }
  }

  private object RecreateInfoTraverser {
    /** Class data that are never eliminated by dce, so we don't need to
     *  record them.
     */
    private val AlwaysPresentClassData = {
      import Definitions._
      Set("V", "Z", "C", "B", "S", "I", "J", "F", "D",
          ObjectClass, StringClass, RuntimeLongClass)
    }
  }

  private def exceptionMsg(myself: AbstractMethodID,
      attemptedInlining: List[AbstractMethodID]) = {
    val buf = new StringBuilder()

    buf.append("The Scala.js optimizer crashed while optimizing " + myself)

    buf.append("\nMethods attempted to inline:\n")

    for (m <- attemptedInlining) {
      buf.append("* ")
      buf.append(m)
      buf.append('\n')
    }

    buf.toString
  }

  private class RollbackException(val trampolineId: Int,
      val savedUsedLocalNames: Set[String],
      val savedUsedLabelNames: Set[String],
      val savedStates: List[Any],
      val cont: () => TailRec[Tree]) extends ControlThrowable

  class OptimizeException(val myself: AbstractMethodID,
      val attemptedInlining: List[AbstractMethodID], cause: Throwable
  ) extends Exception(exceptionMsg(myself, attemptedInlining), cause)

}
