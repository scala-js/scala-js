/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.frontend.optimizer

import scala.language.implicitConversions

import scala.annotation.{switch, tailrec}

import scala.collection.mutable

import scala.util.control.{NonFatal, ControlThrowable, TailCalls}
import scala.util.control.TailCalls.{done => _, _} // done is a too generic term

import org.scalajs.core.ir._
import Definitions.{ObjectClass, isConstructorName, isReflProxyName}
import Trees._
import Types._

import org.scalajs.core.tools.sem.{CheckedBehavior, Semantics}
import org.scalajs.core.tools.javascript.ESLevel
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.linker._
import org.scalajs.core.tools.linker.backend.emitter.LongImpl

/** Optimizer core.
 *  Designed to be "mixed in" [[IncOptimizer#MethodImpl#Optimizer]].
 *  This is the core of the optimizer. It contains all the smart things the
 *  optimizer does. To perform inlining, it relies on abstract protected
 *  methods to identify the target of calls.
 */
private[optimizer] abstract class OptimizerCore(
    semantics: Semantics, esLevel: ESLevel) {
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

  /** Returns the target of a call to a static method. */
  protected def callStatic(className: String,
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

  /** Used local names and whether they are mutable */
  private val usedLocalNames = mutable.Map.empty[String, Boolean]

  private val usedLabelNames = mutable.Set.empty[String]

  /** A list of the States that have been allocated so far, and must be saved.
   *
   *  This list only ever grows, even though, in theory, it will keep
   *  references to states that are not used anymore.
   *  This creates a "temporary memory leak", but the list is discarded when
   *  `optimize` terminates anyway because the whole OptimizerCore is discarded.
   *  It also means that RollbackException will save more state than strictly
   *  necessary, but it is not incorrect to do so.
   *
   *  Manual "memory management" of this list has caused issues such as #1515
   *  and #1843 in the past. So now we just let it grow in a "region-allocated"
   *  style of memory management.
   */
  private var statesInUse: List[State] = Nil

  private var disableOptimisticOptimizations: Boolean = false
  private var rollbacksCount: Int = 0

  private val attemptedInlining = mutable.ListBuffer.empty[MethodID]

  private var curTrampolineId = 0

  def optimize(thisType: Type, originalDef: MethodDef): LinkedMember[MethodDef] = {
    try {
      val MethodDef(static, name, params, resultType, body) = originalDef
      val (newParams, newBody1) = try {
        transformIsolatedBody(Some(myself), thisType, params, resultType, body)
      } catch {
        case _: TooManyRollbacksException =>
          usedLocalNames.clear()
          usedLabelNames.clear()
          statesInUse = Nil
          disableOptimisticOptimizations = true
          transformIsolatedBody(Some(myself), thisType, params, resultType, body)
      }
      val newBody =
        if (name.name == "init___") tryElimStoreModule(newBody1)
        else newBody1
      val m = MethodDef(static, name, newParams, resultType,
          newBody)(originalDef.optimizerHints, None)(originalDef.pos)
      val info = Infos.generateMethodInfo(m)

      new LinkedMember(info, m, None)
    } catch {
      case NonFatal(cause) =>
        throw new OptimizeException(myself, attemptedInlining.distinct.toList, cause)
      case e: Throwable =>
        // This is a fatal exception. Don't wrap, just output debug info error
        Console.err.println(exceptionMsg(
            myself, attemptedInlining.distinct.toList, e))
        throw e
    }
  }

  /** Try and eliminate a StoreModule followed only by trivial statements. */
  private def tryElimStoreModule(body: Tree): Tree = {
    implicit val pos = body.pos
    body match {
      case StoreModule(_, _) =>
        Skip()
      case Block(stats) =>
        val (before, from) = stats.span(!_.isInstanceOf[StoreModule])
        if (from.isEmpty) {
          body
        } else {
          val after = from.tail
          val afterIsTrivial = after.forall {
            case Assign(Select(This(), Ident(_, _)),
                _:Literal | _:VarRef) =>
              true
            case _ =>
              false
          }
          if (afterIsTrivial) Block(before ::: after)
          else body
        }
      case _ =>
        body
    }
  }

  private def newSimpleState[A](initialValue: A): SimpleState[A] = {
    val state = new SimpleState[A](initialValue)
    statesInUse ::= state
    state
  }

  private def freshLocalName(base: String, mutable: Boolean): String = {
    val result = freshNameGeneric(usedLocalNames.contains, base)
    usedLocalNames += result -> mutable
    result
  }

  private def freshLabelName(base: String): String = {
    val result = freshNameGeneric(usedLabelNames, base)
    usedLabelNames += result
    result
  }

  private val isReserved = isKeyword ++ Seq("arguments", "eval", "ScalaJS")

  private def freshNameGeneric(nameUsed: String => Boolean,
      base: String): String = {
    if (!nameUsed(base) && !isReserved(base)) {
      base
    } else {
      var i = 1
      while (nameUsed(base + "$" + i))
        i += 1
      base + "$" + i
    }
  }

  // Just a helper to make the callsites more understandable
  private def localIsMutable(name: String): Boolean = usedLocalNames(name)

  private def tryOrRollback(body: CancelFun => TailRec[Tree])(
      fallbackFun: () => TailRec[Tree]): TailRec[Tree] = {
    if (disableOptimisticOptimizations) {
      fallbackFun()
    } else {
      val trampolineId = curTrampolineId
      val savedUsedLocalNames = usedLocalNames.toMap
      val savedUsedLabelNames = usedLabelNames.toSet
      val stateBackups = statesInUse.map(_.makeBackup())

      body { () =>
        throw new RollbackException(trampolineId, savedUsedLocalNames,
            savedUsedLabelNames, stateBackups, fallbackFun)
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

  /** Transforms an expression or a JSSpread. */
  private def transformExprOrSpread(tree: Tree)(implicit scope: Scope): Tree = {
    tree match {
      case JSSpread(items) =>
        JSSpread(transformExpr(items))(tree.pos)
      case _ =>
        transformExpr(tree)
    }
  }

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
                val info = new LabelInfo(newLabel, acceptRecords = false,
                    returnedTypes = newSimpleState(Nil))
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

        val newName = freshLocalName(name, false)
        val newOriginalName = originalName.orElse(Some(name))
        val localDef = LocalDef(RefinedType(AnyType), true,
            ReplaceWithVarRef(newName, newOriginalName, newSimpleState(true), None))
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
        val newSelector = transformExpr(selector)
        newSelector match {
          case newSelector: Literal =>
            val body = cases collectFirst {
              case (alts, body) if alts.exists(literal_===(_, newSelector)) => body
            } getOrElse default
            transform(body, isStat)
          case _ =>
            Match(newSelector,
                cases map (c => (c._1, transform(c._2, isStat))),
                transform(default, isStat))(tree.tpe)
        }

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

      case tree: ApplyStatically =>
        trampoline {
          pretransformStaticApply(tree, isStat, usePreTransform = false)(
              finishTransform(isStat))
        }

      case tree: ApplyStatic =>
        trampoline {
          pretransformApplyStatic(tree, isStat, usePreTransform = false)(
              finishTransform(isStat))
        }

      case tree @ UnaryOp(_, arg) =>
        if (isStat) transformStat(arg)
        else transformUnaryOp(tree)

      case tree @ BinaryOp(op, lhs, rhs) =>
        if (isStat) Block(transformStat(lhs), transformStat(rhs))
        else transformBinaryOp(tree)

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
              // TODO This cast is suspicious
              if (isSubtype(texpr.tpe.base, tpe.asInstanceOf[Type])) {
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

      case Unbox(arg, charCode) =>
        trampoline {
          pretransformExpr(arg) { targ =>
            foldUnbox(targ, charCode)(finishTransform(isStat))
          }
        }

      case GetClass(expr) =>
        trampoline {
          pretransformExpr(expr) { texpr =>
            texpr.tpe match {
              case RefinedType(base: ReferenceType, true, false) =>
                TailCalls.done(Block(
                    finishTransformStat(texpr),
                    ClassOf(base)))
              case _ =>
                TailCalls.done(GetClass(finishTransformExpr(texpr)))
            }
          }
        }

      // JavaScript expressions

      case JSNew(ctor, args) =>
        JSNew(transformExpr(ctor), args map transformExprOrSpread)

      case JSDotSelect(qualifier, item) =>
        JSDotSelect(transformExpr(qualifier), item)

      case JSBracketSelect(qualifier, item) =>
        foldJSBracketSelect(transformExpr(qualifier), transformExpr(item))

      case tree: JSFunctionApply =>
        trampoline {
          pretransformJSFunctionApply(tree, isStat, usePreTransform = false)(
              finishTransform(isStat))
        }

      case JSDotMethodApply(receiver, method, args) =>
        JSDotMethodApply(transformExpr(receiver), method,
            args map transformExprOrSpread)

      case JSBracketMethodApply(receiver, method, args) =>
        JSBracketMethodApply(transformExpr(receiver), transformExpr(method),
            args map transformExprOrSpread)

      case JSSuperBracketSelect(cls, qualifier, item) =>
        JSSuperBracketSelect(cls, transformExpr(qualifier), transformExpr(item))

      case JSSuperBracketCall(cls, receiver, method, args) =>
        JSSuperBracketCall(cls, transformExpr(receiver), transformExpr(method),
            args map transformExprOrSpread)

      case JSSuperConstructorCall(args) =>
        JSSuperConstructorCall(args map transformExprOrSpread)

      case JSDelete(JSDotSelect(obj, prop)) =>
        JSDelete(JSDotSelect(transformExpr(obj), prop))

      case JSDelete(JSBracketSelect(obj, prop)) =>
        JSDelete(JSBracketSelect(transformExpr(obj), transformExpr(prop)))

      case JSUnaryOp(op, lhs) =>
        JSUnaryOp(op, transformExpr(lhs))

      case JSBinaryOp(op, lhs, rhs) =>
        JSBinaryOp(op, transformExpr(lhs), transformExpr(rhs))

      case JSArrayConstr(items) =>
        JSArrayConstr(items map transformExprOrSpread)

      case JSObjectConstr(fields) =>
        JSObjectConstr(fields map {
          case (name, value) => (name, transformExpr(value))
        })

      // Atomic expressions

      case _:VarRef | _:This =>
        trampoline {
          pretransformExpr(tree)(finishTransform(isStat))
        }

      case Closure(captureParams, params, body, captureValues) =>
        transformClosureCommon(captureParams, params, body,
            captureValues.map(transformExpr))

      // Trees that need not be transformed

      case _:Skip | _:Debugger | _:LoadModule | _:LoadJSConstructor |
          _:LoadJSModule | _:JSEnvInfo | _:JSLinkingInfo | _:Literal |
          EmptyTree =>
        tree

      case _ =>
        sys.error(s"Invalid tree in transform of class ${tree.getClass.getName}: $tree")
    }

    if (isStat) keepOnlySideEffects(result)
    else result
  }

  private def transformClosureCommon(captureParams: List[ParamDef],
      params: List[ParamDef], body: Tree, newCaptureValues: List[Tree])(
      implicit pos: Position): Closure = {

    val (allNewParams, newBody) =
      transformIsolatedBody(None, AnyType, captureParams ++ params, AnyType, body)
    val (newCaptureParams, newParams) =
      allNewParams.splitAt(captureParams.size)

    Closure(newCaptureParams, newParams, newBody, newCaptureValues)
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

      case VarRef(Ident(name, _)) =>
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

      case Match(selector, cases, default) =>
        val newSelector = transformExpr(selector)
        newSelector match {
          case newSelector: Literal =>
            val body = cases collectFirst {
              case (alts, body) if alts.exists(literal_===(_, newSelector)) => body
            } getOrElse default
            pretransformExpr(body)(cont)
          case _ =>
            cont(PreTransTree(Match(newSelector,
                cases map (c => (c._1, transformExpr(c._2))),
                transformExpr(default))(tree.tpe)))
        }

      case Labeled(ident @ Ident(label, _), tpe, body) =>
        returnable(label, tpe, body, isStat = false, usePreTransform = true)(cont)

      case New(cls, ctor, args) =>
        pretransformExprs(args) { targs =>
          pretransformNew(tree, cls, ctor, targs)(cont)
        }

      case tree: Select =>
        pretransformSelectCommon(tree, isLhsOfAssign = false)(cont)

      case tree: Apply =>
        pretransformApply(tree, isStat = false,
            usePreTransform = true)(cont)

      case tree: ApplyStatically =>
        pretransformStaticApply(tree, isStat = false,
            usePreTransform = true)(cont)

      case tree: ApplyStatic =>
        pretransformApplyStatic(tree, isStat = false,
            usePreTransform = true)(cont)

      case tree: JSFunctionApply =>
        pretransformJSFunctionApply(tree, isStat = false,
            usePreTransform = true)(cont)

      case AsInstanceOf(expr, tpe) =>
        pretransformExpr(expr) { texpr =>
          tpe match {
            case ClassType(ObjectClass) =>
              cont(texpr)
            case _ =>
              // TODO This cast is suspicious
              if (isSubtype(texpr.tpe.base, tpe.asInstanceOf[Type])) {
                cont(texpr)
              } else {
                cont(PreTransTree(
                    AsInstanceOf(finishTransformExpr(texpr), tpe)))
              }
          }
        }

      case Closure(captureParams, params, body, captureValues) =>
        pretransformExprs(captureValues) { tcaptureValues =>
          tryOrRollback { cancelFun =>
            val captureBindings = for {
              (ParamDef(Ident(name, origName), tpe, mutable, rest), value) <-
                captureParams zip tcaptureValues
            } yield {
              assert(!rest, s"Found a rest capture parameter at $pos")
              Binding(name, origName, tpe, mutable, value)
            }
            withNewLocalDefs(captureBindings) { (captureLocalDefs, cont1) =>
              val replacement = TentativeClosureReplacement(
                  captureParams, params, body, captureLocalDefs,
                  alreadyUsed = newSimpleState(false), cancelFun)
              val localDef = LocalDef(
                  RefinedType(AnyType, isExact = false, isNullable = false),
                  mutable = false,
                  replacement)
              cont1(PreTransLocalDef(localDef))
            } (cont)
          } { () =>
            val newClosure = transformClosureCommon(captureParams, params, body,
                tcaptureValues.map(finishTransformExpr))
            cont(PreTransTree(
                newClosure,
                RefinedType(AnyType, isExact = false, isNullable = false)))
          }
        }

      case _ =>
        val result = transformExpr(tree)
        cont(PreTransTree(result))
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
    val Select(qualifier, item) = tree
    pretransformExpr(qualifier) { preTransQual =>
      pretransformSelectCommon(tree.tpe, preTransQual, item,
          isLhsOfAssign)(cont)(scope, tree.pos)
    }
  }

  private def pretransformSelectCommon(expectedType: Type,
      preTransQual: PreTransform, item: Ident, isLhsOfAssign: Boolean)(
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
            val sel = Select(newQual, item)(field.tpe)
            sel.tpe match {
              case _: RecordType =>
                cont(PreTransRecordTree(sel, RefinedType(expectedType), cancelFun))
              case _ =>
                cont(PreTransTree(sel, RefinedType(sel.tpe)))
            }

          case PreTransTree(newQual, _) =>
            cont(PreTransTree(Select(newQual, item)(expectedType),
                RefinedType(expectedType)))
        }
    }
  }

  private def pretransformNew(tree: Tree, cls: ClassType, ctor: Ident,
      targs: List[PreTransform])(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    tryNewInlineableClass(cls.className) match {
      case Some(initialValue) =>
        tryOrRollback { cancelFun =>
          inlineClassConstructor(
              new AllocationSite(tree),
              cls, initialValue, ctor, targs, cancelFun)(cont)
        } { () =>
          cont(PreTransTree(
              New(cls, ctor, targs.map(finishTransformExpr)),
              RefinedType(cls, isExact = true, isNullable = false)))
        }
      case None =>
        cont(PreTransTree(
            New(cls, ctor, targs.map(finishTransformExpr)),
            RefinedType(cls, isExact = true, isNullable = false)))
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

      case PreTransLocalDef(localDef @ LocalDef(tpe, _, replacement)) =>
        replacement match {
          case ReplaceWithRecordVarRef(name, originalName,
              recordType, used, cancelFun) =>
            used.value = true
            PreTransRecordTree(
                VarRef(Ident(name, originalName))(recordType), tpe, cancelFun)

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
    case NewArray(_, lengths) =>
      Block(lengths.map(keepOnlySideEffects))(stat.pos)
    case Select(qualifier, _) =>
      keepOnlySideEffects(qualifier)
    case Closure(_, _, _, captureValues) =>
      Block(captureValues.map(keepOnlySideEffects))(stat.pos)
    case UnaryOp(_, arg) =>
      keepOnlySideEffects(arg)
    case If(cond, thenp, elsep) =>
      (keepOnlySideEffects(thenp), keepOnlySideEffects(elsep)) match {
        case (Skip(), Skip())     => keepOnlySideEffects(cond)
        case (newThenp, newElsep) => If(cond, newThenp, newElsep)(NoType)(stat.pos)
      }
    case BinaryOp(_, lhs, rhs) =>
      Block(keepOnlySideEffects(lhs), keepOnlySideEffects(rhs))(stat.pos)
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

    pretransformExprs(receiver, args) { (treceiver, targs) =>
      def treeNotInlined =
        cont(PreTransTree(Apply(finishTransformExpr(treceiver), methodIdent,
            targs.map(finishTransformExpr))(tree.tpe), RefinedType(tree.tpe)))

      treceiver.tpe.base match {
        case NothingType =>
          cont(treceiver)
        case NullType =>
          cont(PreTransTree(Block(
              finishTransformStat(treceiver),
              Throw(New(ClassType("jl_NullPointerException"),
                  Ident("init___", Some("<init>")), Nil)))))
        case _ =>
          if (isReflProxyName(methodName)) {
            // Never inline reflective proxies
            treeNotInlined
          } else {
            val cls = boxedClassForType(treceiver.tpe.base)
            val impls =
              if (treceiver.tpe.isExact) staticCall(cls, methodName).toList
              else dynamicCall(cls, methodName)
            val allocationSites =
              (treceiver :: targs).map(_.tpe.allocationSite)
            if (impls.isEmpty || impls.exists(impl =>
                scope.implsBeingInlined((allocationSites, impl)))) {
              // isEmpty could happen, have to leave it as is for the TypeError
              treeNotInlined
            } else if (impls.size == 1) {
              val target = impls.head
              val intrinsicCode = getIntrinsicCode(target)
              if (intrinsicCode >= 0) {
                callIntrinsic(intrinsicCode, Some(treceiver), targs,
                    isStat, usePreTransform)(cont)
              } else if (target.inlineable && (
                  target.shouldInline ||
                  shouldInlineBecauseOfArgs(target, treceiver :: targs))) {
                inline(allocationSites, Some(treceiver), targs, target,
                    isStat, usePreTransform)(cont)
              } else {
                treeNotInlined
              }
            } else {
              if (impls.forall(_.isForwarder)) {
                val reference = impls.head
                val areAllTheSame = getMethodBody(reference).body match {
                  // Trait impl forwarder
                  case ApplyStatic(ClassType(staticCls), Ident(methodName, _), _) =>
                    impls.tail.forall(getMethodBody(_).body match {
                      case ApplyStatic(ClassType(`staticCls`),
                          Ident(`methodName`, _), _) => true
                      case _ => false
                    })

                  // Bridge method
                  case MaybeBox(Apply(This(), Ident(methodName, _), referenceArgs), boxID) =>
                    impls.tail.forall(getMethodBody(_).body match {
                      case MaybeBox(Apply(This(), Ident(`methodName`, _), implArgs), `boxID`) =>
                        referenceArgs.zip(implArgs) forall {
                          case (MaybeUnbox(_, unboxID1), MaybeUnbox(_, unboxID2)) =>
                            unboxID1 == unboxID2
                        }
                      case _ => false
                    })

                  case body =>
                    throw new AssertionError("Invalid forwarder shape: " + body)
                }
                if (!areAllTheSame) {
                  // Not all doing the same thing
                  treeNotInlined
                } else {
                  inline(allocationSites, Some(treceiver), targs, reference,
                      isStat, usePreTransform)(cont)
                }
              } else {
                // TODO? Inline multiple non-trait-impl-forwarder with the exact same body?
                treeNotInlined
              }
            }
          }
      }
    }
  }

  private def boxedClassForType(tpe: Type): String = (tpe: @unchecked) match {
    case ClassType(cls)  => cls
    case AnyType         => Definitions.ObjectClass
    case UndefType       => Definitions.BoxedUnitClass
    case BooleanType     => Definitions.BoxedBooleanClass
    case IntType         => Definitions.BoxedIntegerClass
    case LongType        => Definitions.BoxedLongClass
    case FloatType       => Definitions.BoxedFloatClass
    case DoubleType      => Definitions.BoxedDoubleClass
    case StringType      => Definitions.StringClass
    case ArrayType(_, _) => Definitions.ObjectClass
  }

  private def pretransformStaticApply(tree: ApplyStatically, isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    val ApplyStatically(receiver, clsType @ ClassType(cls),
        methodIdent @ Ident(methodName, _), args) = tree
    implicit val pos = tree.pos

    def treeNotInlined0(transformedReceiver: Tree, transformedArgs: List[Tree]) =
      cont(PreTransTree(ApplyStatically(transformedReceiver, clsType,
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
        pretransformExprs(receiver, args) { (treceiver, targs) =>
          val intrinsicCode = getIntrinsicCode(target)
          if (intrinsicCode >= 0) {
            callIntrinsic(intrinsicCode, Some(treceiver), targs,
                isStat, usePreTransform)(cont)
          } else {
            val shouldInline = target.inlineable && (
                target.shouldInline ||
                shouldInlineBecauseOfArgs(target, treceiver :: targs))
            val allocationSites =
              (treceiver :: targs).map(_.tpe.allocationSite)
            val beingInlined =
              scope.implsBeingInlined((allocationSites, target))

            if (shouldInline && !beingInlined) {
              inline(allocationSites, Some(treceiver), targs, target,
                  isStat, usePreTransform)(cont)
            } else {
              treeNotInlined0(finishTransformExpr(treceiver),
                  targs.map(finishTransformExpr))
            }
          }
        }
      }
    }
  }

  private def pretransformApplyStatic(tree: ApplyStatic, isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    val ApplyStatic(classType @ ClassType(cls),
        methodIdent @ Ident(methodName, _), args) = tree
    implicit val pos = tree.pos

    def treeNotInlined0(transformedArgs: List[Tree]) =
      cont(PreTransTree(ApplyStatic(classType, methodIdent,
          transformedArgs)(tree.tpe), RefinedType(tree.tpe)))

    def treeNotInlined = treeNotInlined0(args.map(transformExpr))

    val optTarget = callStatic(cls, methodName)
    if (optTarget.isEmpty) {
      // just in case
      treeNotInlined
    } else {
      val target = optTarget.get
      pretransformExprs(args) { targs =>
        val intrinsicCode = getIntrinsicCode(target)
        if (intrinsicCode >= 0) {
          callIntrinsic(intrinsicCode, None, targs,
              isStat, usePreTransform)(cont)
        } else {
          val shouldInline = target.inlineable && (
              target.shouldInline || shouldInlineBecauseOfArgs(target, targs))
          val allocationSites = targs.map(_.tpe.allocationSite)
          val beingInlined =
            scope.implsBeingInlined((allocationSites, target))

          if (shouldInline && !beingInlined) {
            inline(allocationSites, None, targs, target,
                isStat, usePreTransform)(cont)
          } else {
            treeNotInlined0(targs.map(finishTransformExpr))
          }
        }
      }
    }
  }

  private def pretransformJSFunctionApply(tree: JSFunctionApply,
      isStat: Boolean, usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {
    val JSFunctionApply(fun, args) = tree
    implicit val pos = tree.pos

    if (args.exists(_.isInstanceOf[JSSpread])) {
      cont(PreTransTree(
          JSFunctionApply(transformExpr(fun), args.map(transformExprOrSpread))))
    } else {
      pretransformExpr(fun) { tfun =>
        tfun match {
          case PreTransLocalDef(LocalDef(_, false,
              closure @ TentativeClosureReplacement(
                  captureParams, params, body, captureLocalDefs,
                  alreadyUsed, cancelFun))) if !alreadyUsed.value =>
            alreadyUsed.value = true
            pretransformExprs(args) { targs =>
              inlineBody(
                  Some(PreTransTree(Undefined())), // `this` is `undefined`
                  captureParams ++ params, AnyType, body,
                  captureLocalDefs.map(PreTransLocalDef(_)) ++ targs, isStat,
                  usePreTransform)(cont)
            }

          case _ =>
            cont(PreTransTree(
                JSFunctionApply(finishTransformExpr(tfun), args.map(transformExpr))))
        }
      }
    }
  }

  private val ClassNamesThatShouldBeInlined = Set(
      "s_Predef$$less$colon$less",
      "s_Predef$$eq$colon$eq",

      "s_reflect_ManifestFactory$ByteManifest$",
      "s_reflect_ManifestFactory$ShortManifest$",
      "s_reflect_ManifestFactory$CharManifest$",
      "s_reflect_ManifestFactory$IntManifest$",
      "s_reflect_ManifestFactory$LongManifest$",
      "s_reflect_ManifestFactory$FloatManifest$",
      "s_reflect_ManifestFactory$DoubleManifest$",
      "s_reflect_ManifestFactory$BooleanManifest$",
      "s_reflect_ManifestFactory$UnitManifest$",
      "s_reflect_ManifestFactory$AnyManifest$",
      "s_reflect_ManifestFactory$ObjectManifest$",
      "s_reflect_ManifestFactory$AnyValManifest$",
      "s_reflect_ManifestFactory$NullManifest$",
      "s_reflect_ManifestFactory$NothingManifest$"
  )

  private def shouldInlineBecauseOfArgs(target: MethodID,
      receiverAndArgs: List[PreTransform]): Boolean = {
    def isTypeLikelyOptimizable(tpe: RefinedType): Boolean = tpe.base match {
      case ClassType(className) =>
        ClassNamesThatShouldBeInlined.contains(className)
      case _ =>
        false
    }

    def isLikelyOptimizable(arg: PreTransform): Boolean = arg match {
      case PreTransBlock(_, result) =>
        isLikelyOptimizable(result)

      case PreTransLocalDef(localDef) =>
        (localDef.replacement match {
          case TentativeClosureReplacement(_, _, _, _, _, _) => true
          case ReplaceWithRecordVarRef(_, _, _, _, _)        => true
          case InlineClassBeingConstructedReplacement(_, _)  => true
          case InlineClassInstanceReplacement(_, _, _)       => true
          case _ =>
            isTypeLikelyOptimizable(localDef.tpe)
        }) && {
          /* java.lang.Character is @inline so that *local* box/unbox pairs
           * can be eliminated. But we don't want that to force inlining of
           * a method only because we pass it a boxed Char.
           */
          localDef.tpe.base match {
            case ClassType(Definitions.BoxedCharacterClass) => false
            case _                                          => true
          }
        }

      case PreTransRecordTree(_, _, _) =>
        true

      case _ =>
        isTypeLikelyOptimizable(arg.tpe)
    }

    receiverAndArgs.exists(isLikelyOptimizable) || {
      target.toString == "s_reflect_ClassTag$.apply__jl_Class__s_reflect_ClassTag" &&
      (receiverAndArgs.tail.head match {
        case PreTransTree(ClassOf(_), _) => true
        case _                           => false
      })
    }
  }

  private def inline(allocationSites: List[Option[AllocationSite]],
      optReceiver: Option[PreTransform],
      args: List[PreTransform], target: MethodID, isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    require(target.inlineable)

    attemptedInlining += target

    val MethodDef(static, _, formals, resultType, body) = getMethodBody(target)
    assert(static == optReceiver.isEmpty,
        "There must be receiver if and only if the method is not static")

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

      case Select(This(), field) if formals.isEmpty =>
        assert(optReceiver.isDefined,
            "There was a This(), there should be a receiver")
        pretransformSelectCommon(body.tpe, optReceiver.get, field,
            isLhsOfAssign = false)(cont)

      case Assign(lhs @ Select(This(), field), VarRef(Ident(rhsName, _)))
          if formals.size == 1 && formals.head.name.name == rhsName =>
        assert(isStat, "Found Assign in expression position")
        assert(optReceiver.isDefined,
            "There was a This(), there should be a receiver")
        pretransformSelectCommon(lhs.tpe, optReceiver.get, field,
            isLhsOfAssign = true) { preTransLhs =>
          // TODO Support assignment of record
          cont(PreTransTree(
              Assign(finishTransformExpr(preTransLhs),
                  finishTransformExpr(args.head)),
              RefinedType.NoRefinedType))
        }

      case _ =>
        val targetID = (allocationSites, target)
        inlineBody(optReceiver, formals, resultType, body, args, isStat,
            usePreTransform)(cont)(scope.inlining(targetID), pos)
    }
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
      (ParamDef(Ident(name, originalName), tpe, mutable, rest), arg) <- formals zip args
    } yield {
      assert(!rest, s"Trying to inline a body with a rest parameter at $pos")
      Binding(name, originalName, tpe, mutable, arg)
    }

    withBindings(optReceiverBinding ++: argsBindings) { (bodyScope, cont1) =>
      returnable("", resultType, body, isStat, usePreTransform)(
          cont1)(bodyScope, pos)
    } (cont) (scope.withEnv(OptEnv.Empty))
  }

  private def callIntrinsic(code: Int, optTReceiver: Option[PreTransform],
      targs: List[PreTransform], isStat: Boolean, usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    import Intrinsics._

    implicit def string2ident(s: String): Ident = Ident(s, None)

    lazy val newReceiver = finishTransformExpr(optTReceiver.get)
    lazy val newArgs = targs.map(finishTransformExpr)

    @inline def contTree(result: Tree) = cont(PreTransTree(result))

    @inline def StringClassType = ClassType(Definitions.StringClass)

    def defaultApply(methodName: String, resultType: Type): TailRec[Tree] =
      contTree(Apply(newReceiver, Ident(methodName), newArgs)(resultType))

    def cursoryArrayElemType(tpe: ArrayType): Type = {
      if (tpe.dimensions != 1) AnyType
      else (tpe.baseClassName match {
        case "Z"                   => BooleanType
        case "B" | "C" | "S" | "I" => IntType
        case "F"                   => FloatType
        case "D"                   => DoubleType
        case _                     => AnyType
      })
    }

    def asRTLong(arg: Tree): Tree =
      AsInstanceOf(arg, ClassType(LongImpl.RuntimeLongClass))
    def firstArgAsRTLong: Tree =
      asRTLong(newArgs.head)

    (code: @switch) match {
      // java.lang.System

      case ArrayCopy =>
        assert(isStat, "System.arraycopy must be used in statement position")
        contTree(CallHelper("systemArraycopy", newArgs)(NoType))
      case IdentityHashCode =>
        contTree(CallHelper("systemIdentityHashCode", newArgs)(IntType))

      // scala.runtime.ScalaRunTime object

      case ArrayApply =>
        val List(array, index) = newArgs
        array.tpe match {
          case arrayTpe @ ArrayType(base, depth) =>
            val elemType = cursoryArrayElemType(arrayTpe)
            val select = ArraySelect(array, index)(elemType)
            if (base == "C")
              boxChar(select)(cont)
            else
              contTree(select)

          case _ =>
            defaultApply("array$undapply__O__I__O", AnyType)
        }

      case ArrayUpdate =>
        val List(tarray, tindex, tvalue) = targs
        tarray.tpe.base match {
          case arrayTpe @ ArrayType(base, depth) =>
            val array = finishTransformExpr(tarray)
            val index = finishTransformExpr(tindex)
            val elemType = cursoryArrayElemType(arrayTpe)
            val select = ArraySelect(array, index)(elemType)
            val cont1: PreTransCont = { tunboxedValue =>
              contTree(Assign(select, finishTransformExpr(tunboxedValue)))
            }
            base match {
              case "Z" | "B" | "S" | "I" | "L" | "F" | "D" if depth == 1 =>
                foldUnbox(tvalue, base.charAt(0))(cont1)
              case "C" if depth == 1 =>
                unboxChar(tvalue)(cont1)
              case _ =>
                cont1(tvalue)
            }
          case _ =>
            defaultApply("array$undupdate__O__I__O__V", AnyType)
        }

      case ArrayLength =>
        targs.head.tpe.base match {
          case _: ArrayType =>
            contTree(Trees.ArrayLength(newArgs.head))
          case _ =>
            defaultApply("array$undlength__O__I", IntType)
        }

      // scala.scalajs.runtime package object

      case PropertiesOf =>
        contTree(CallHelper("propertiesOf", newArgs)(AnyType))

      // java.lang.Integer

      case IntegerNLZ =>
        contTree(newArgs.head match {
          case IntLiteral(value) => IntLiteral(Integer.numberOfLeadingZeros(value))
          case newArg            => CallHelper("clz32", newArg)(IntType)
        })

      // java.lang.Long

      case LongToString =>
        contTree(Apply(firstArgAsRTLong, "toString__T", Nil)(StringClassType))
      case LongCompare =>
        contTree(Apply(firstArgAsRTLong, "compareTo__sjsr_RuntimeLong__I",
            List(asRTLong(newArgs(1))))(IntType))
      case LongDivideUnsigned =>
        contTree(Apply(firstArgAsRTLong, LongImpl.divideUnsigned,
            List(asRTLong(newArgs(1))))(LongType))
      case LongRemainderUnsigned =>
        contTree(Apply(firstArgAsRTLong, LongImpl.remainderUnsigned,
            List(asRTLong(newArgs(1))))(LongType))

      // scala.collection.mutable.ArrayBuilder

      case GenericArrayBuilderResult =>
        val List(runtimeClass, array) = newArgs
        val (resultType, isExact) = runtimeClass match {
          case ClassOf(elemType) => (ArrayType(elemType), true)
          case _                 => (AnyType, false)
        }
        cont(PreTransTree(CallHelper("makeNativeArrayWrapper",
            CallHelper("arrayDataOf",
                CallHelper("classDataOf", runtimeClass)(AnyType))(AnyType),
            array)(resultType),
            RefinedType(resultType, isExact = isExact, isNullable = false)))

      case ArrayBuilderZeroOf =>
        contTree(finishTransformExpr(targs.head) match {
          case ClassOf(ClassType(cls)) =>
            cls match {
              case "B" | "S" | "C" | "I" | "D" => IntLiteral(0)
              case "L"                         => LongLiteral(0L)
              case "F"                         => FloatLiteral(0.0f)
              case "Z"                         => BooleanLiteral(false)
              case "V"                         => Undefined()
              case _                           => Null()
            }
          case ClassOf(_) =>
            Null()
          case runtimeClass =>
            CallHelper("zeroOf", runtimeClass)(AnyType)
        })

      // java.lang.Class

      case ClassGetComponentType =>
        newReceiver match {
          case ClassOf(ArrayType(base, depth)) =>
            contTree(ClassOf(
                if (depth == 1) ClassType(base)
                else ArrayType(base, depth - 1)))
          case ClassOf(ClassType(_)) =>
            contTree(Null())
          case receiver =>
            defaultApply("getComponentType__jl_Class",
                ClassType(Definitions.ClassClass))
        }

      // java.lang.reflect.Array

      case ArrayNewInstance =>
        newArgs.head match {
          case ClassOf(elementTpe) =>
            contTree(NewArray(ArrayType(elementTpe), List(newArgs.tail.head)))
          case _ =>
            defaultApply("newInstance__jl_Class__I__O", AnyType)
        }

      // TypedArray conversions

      case ByteArrayToInt8Array =>
        contTree(CallHelper("byteArray2TypedArray", newArgs)(AnyType))
      case ShortArrayToInt16Array =>
        contTree(CallHelper("shortArray2TypedArray", newArgs)(AnyType))
      case CharArrayToUint16Array =>
        contTree(CallHelper("charArray2TypedArray", newArgs)(AnyType))
      case IntArrayToInt32Array =>
        contTree(CallHelper("intArray2TypedArray", newArgs)(AnyType))
      case FloatArrayToFloat32Array =>
        contTree(CallHelper("floatArray2TypedArray", newArgs)(AnyType))
      case DoubleArrayToFloat64Array =>
        contTree(CallHelper("doubleArray2TypedArray", newArgs)(AnyType))

      case Int8ArrayToByteArray =>
        contTree(CallHelper("typedArray2ByteArray", newArgs)(AnyType))
      case Int16ArrayToShortArray =>
        contTree(CallHelper("typedArray2ShortArray", newArgs)(AnyType))
      case Uint16ArrayToCharArray =>
        contTree(CallHelper("typedArray2CharArray", newArgs)(AnyType))
      case Int32ArrayToIntArray =>
        contTree(CallHelper("typedArray2IntArray", newArgs)(AnyType))
      case Float32ArrayToFloatArray =>
        contTree(CallHelper("typedArray2FloatArray", newArgs)(AnyType))
      case Float64ArrayToDoubleArray =>
        contTree(CallHelper("typedArray2DoubleArray", newArgs)(AnyType))
    }
  }

  private def boxChar(value: Tree)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {
    pretransformNew(value, ClassType(Definitions.BoxedCharacterClass),
        Ident("init___C"), List(PreTransTree(value)))(cont)
  }

  private def unboxChar(tvalue: PreTransform)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {
    val BoxesRunTimeModuleClassName = "sr_BoxesRunTime$"
    val treceiver = PreTransTree(LoadModule(
        ClassType(BoxesRunTimeModuleClassName)))
    val target = staticCall(BoxesRunTimeModuleClassName, "unboxToChar__O__C").getOrElse {
      throw new AssertionError("Cannot find method sr_BoxesRunTime$.unboxToChar__O__C")
    }
    val allocationSites = List(treceiver, tvalue).map(_.tpe.allocationSite)
    inline(allocationSites, Some(treceiver), List(tvalue), target,
        isStat = false, usePreTransform = true)(cont)
  }

  private def inlineClassConstructor(allocationSite: AllocationSite,
      cls: ClassType, initialValue: RecordValue,
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

        inlineClassConstructorBody(allocationSite, initialFieldLocalDefs,
            cls, cls, ctor, args, cancelFun) { (finalFieldLocalDefs, cont2) =>
          cont2(PreTransLocalDef(LocalDef(
              RefinedType(cls, isExact = true, isNullable = false,
                  allocationSite = Some(allocationSite)),
              mutable = false,
              InlineClassInstanceReplacement(recordType, finalFieldLocalDefs, cancelFun))))
        } (cont1)
      } (cont)
    }
  }

  private def inlineClassConstructorBody(
      allocationSite: AllocationSite,
      inputFieldsLocalDefs: Map[String, LocalDef], cls: ClassType,
      ctorClass: ClassType, ctor: Ident, args: List[PreTransform],
      cancelFun: CancelFun)(
      buildInner: (Map[String, LocalDef], PreTransCont) => TailRec[Tree])(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = tailcall {

    val target = staticCall(ctorClass.className, ctor.name).getOrElse(cancelFun())
    val targetID = (Some(allocationSite) :: args.map(_.tpe.allocationSite), target)
    if (scope.implsBeingInlined.contains(targetID))
      cancelFun()

    val targetMethodDef = getMethodBody(target)
    val formals = targetMethodDef.args
    val stats = targetMethodDef.body match {
      case Block(stats) => stats
      case singleStat   => List(singleStat)
    }

    val argsBindings = for {
      (ParamDef(Ident(name, originalName), tpe, mutable, _), arg) <- formals zip args
    } yield {
      Binding(name, originalName, tpe, mutable, arg)
    }

    withBindings(argsBindings) { (bodyScope, cont1) =>
      val thisLocalDef = LocalDef(
          RefinedType(cls, isExact = true, isNullable = false), false,
          InlineClassBeingConstructedReplacement(inputFieldsLocalDefs, cancelFun))
      val statsScope = bodyScope.inlining(targetID).withEnv(
          bodyScope.env.withLocalDef("this", thisLocalDef))
      inlineClassConstructorBodyList(allocationSite, thisLocalDef,
          inputFieldsLocalDefs, cls, stats, cancelFun)(
          buildInner)(cont1)(statsScope)
    } (cont) (scope.withEnv(OptEnv.Empty))
  }

  private def inlineClassConstructorBodyList(
      allocationSite: AllocationSite,
      thisLocalDef: LocalDef, inputFieldsLocalDefs: Map[String, LocalDef],
      cls: ClassType, stats: List[Tree], cancelFun: CancelFun)(
      buildInner: (Map[String, LocalDef], PreTransCont) => TailRec[Tree])(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    stats match {
      case This() :: rest =>
        inlineClassConstructorBodyList(allocationSite, thisLocalDef,
            inputFieldsLocalDefs, cls, rest, cancelFun)(buildInner)(cont)

      case Assign(s @ Select(ths: This,
          Ident(fieldName, fieldOrigName)), value) :: rest
          if !inputFieldsLocalDefs(fieldName).mutable =>
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
            inlineClassConstructorBodyList(allocationSite,
                newThisLocalDef, newFieldsLocalDefs, cls, rest, cancelFun)(
                buildInner)(cont1)(restScope)
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
      case If(cond, th: Throw, Assign(Select(This(), _), value)) :: rest =>
        // work around a bug of the compiler (these should be @-bindings)
        val stat = stats.head.asInstanceOf[If]
        val ass = stat.elsep.asInstanceOf[Assign]
        val lhs = ass.lhs
        inlineClassConstructorBodyList(allocationSite, thisLocalDef,
            inputFieldsLocalDefs, cls,
            Assign(lhs, If(cond, th, value)(lhs.tpe)(stat.pos))(ass.pos) :: rest,
            cancelFun)(buildInner)(cont)

      case ApplyStatically(ths: This, superClass, superCtor, args) :: rest
          if isConstructorName(superCtor.name) =>
        pretransformExprs(args) { targs =>
          inlineClassConstructorBody(allocationSite, inputFieldsLocalDefs,
              cls, superClass, superCtor, targs,
              cancelFun) { (outputFieldsLocalDefs, cont1) =>
            val newThisLocalDef = LocalDef(
                RefinedType(cls, isExact = true, isNullable = false), false,
                InlineClassBeingConstructedReplacement(outputFieldsLocalDefs, cancelFun))
            val restScope = scope.withEnv(scope.env.withLocalDef(
                "this", newThisLocalDef))
            inlineClassConstructorBodyList(allocationSite,
                newThisLocalDef, outputFieldsLocalDefs,
                cls, rest, cancelFun)(buildInner)(cont1)(restScope)
          } (cont)
        }

      case VarDef(Ident(name, originalName), tpe, mutable, rhs) :: rest =>
        pretransformExpr(rhs) { trhs =>
          withBinding(Binding(name, originalName, tpe, mutable, trhs)) { (restScope, cont1) =>
            inlineClassConstructorBodyList(allocationSite,
                thisLocalDef, inputFieldsLocalDefs,
                cls, rest, cancelFun)(buildInner)(cont1)(restScope)
          } (cont)
        }

      case stat :: rest =>
        val transformedStat = transformStat(stat)
        transformedStat match {
          case Skip() =>
            inlineClassConstructorBodyList(allocationSite,
                thisLocalDef, inputFieldsLocalDefs,
                cls, rest, cancelFun)(buildInner)(cont)
          case _ =>
            if (transformedStat.tpe == NothingType)
              cont(PreTransTree(transformedStat, RefinedType.Nothing))
            else {
              inlineClassConstructorBodyList(allocationSite,
                  thisLocalDef, inputFieldsLocalDefs,
                  cls, rest, cancelFun)(buildInner) { tinner =>
                cont(PreTransBlock(transformedStat :: Nil, tinner))
              }
            }
        }

      case Nil =>
        buildInner(inputFieldsLocalDefs, cont)
    }
  }

  private def foldIf(cond: Tree, thenp: Tree, elsep: Tree)(tpe: Type)(
      implicit pos: Position): Tree = {
    import BinaryOp._

    @inline def default = If(cond, thenp, elsep)(tpe)
    cond match {
      case BooleanLiteral(v) =>
        if (v) thenp
        else elsep

      case _ =>
        @inline def negCond = foldUnaryOp(UnaryOp.Boolean_!, cond)
        if (thenp.tpe == BooleanType && elsep.tpe == BooleanType) {
          (cond, thenp, elsep) match {
            case (_, BooleanLiteral(t), BooleanLiteral(e)) =>
              if (t == e) Block(keepOnlySideEffects(cond), thenp)
              else if (t) cond
              else        negCond

            case (_, BooleanLiteral(false), _) =>
              foldIf(negCond, elsep, BooleanLiteral(false))(tpe) // canonical && form
            case (_, _, BooleanLiteral(true)) =>
              foldIf(negCond, BooleanLiteral(true), thenp)(tpe) // canonical || form

            /* if (lhs === null) rhs === null else lhs === rhs
             * -> lhs === rhs
             * This is the typical shape of a lhs == rhs test where
             * the equals() method has been inlined as a reference
             * equality test.
             */
            case (BinaryOp(BinaryOp.===, VarRef(lhsIdent), Null()),
                BinaryOp(BinaryOp.===, VarRef(rhsIdent), Null()),
                BinaryOp(BinaryOp.===, VarRef(lhsIdent2), VarRef(rhsIdent2)))
                if lhsIdent2 == lhsIdent && rhsIdent2 == rhsIdent =>
              elsep

            // Example: (x > y) || (x == y)  ->  (x >= y)
            case (BinaryOp(op1 @ (Num_== | Num_!= | Num_< | Num_<= | Num_> | Num_>=), l1, r1),
                  BooleanLiteral(true),
                  BinaryOp(op2 @ (Num_== | Num_!= | Num_< | Num_<= | Num_> | Num_>=), l2, r2))
                if ((l1.isInstanceOf[Literal] || l1.isInstanceOf[VarRef]) &&
                    (r1.isInstanceOf[Literal] || r1.isInstanceOf[VarRef]) &&
                    (l1 == l2 && r1 == r2)) =>
              val canBeEqual =
                ((op1 == Num_==) || (op1 == Num_<=) || (op1 == Num_>=)) ||
                ((op2 == Num_==) || (op2 == Num_<=) || (op2 == Num_>=))
              val canBeLessThan =
                ((op1 == Num_!=) || (op1 == Num_<) || (op1 == Num_<=)) ||
                ((op2 == Num_!=) || (op2 == Num_<) || (op2 == Num_<=))
              val canBeGreaterThan =
                ((op1 == Num_!=) || (op1 == Num_>) || (op1 == Num_>=)) ||
                ((op2 == Num_!=) || (op2 == Num_>) || (op2 == Num_>=))

              fold3WayComparison(canBeEqual, canBeLessThan, canBeGreaterThan, l1, r1)

            // Example: (x >= y) && (x <= y)  ->  (x == y)
            case (BinaryOp(op1 @ (Num_== | Num_!= | Num_< | Num_<= | Num_> | Num_>=), l1, r1),
                  BinaryOp(op2 @ (Num_== | Num_!= | Num_< | Num_<= | Num_> | Num_>=), l2, r2),
                  BooleanLiteral(false))
                if ((l1.isInstanceOf[Literal] || l1.isInstanceOf[VarRef]) &&
                    (r1.isInstanceOf[Literal] || r1.isInstanceOf[VarRef]) &&
                    (l1 == l2 && r1 == r2)) =>
              val canBeEqual =
                ((op1 == Num_==) || (op1 == Num_<=) || (op1 == Num_>=)) &&
                ((op2 == Num_==) || (op2 == Num_<=) || (op2 == Num_>=))
              val canBeLessThan =
                ((op1 == Num_!=) || (op1 == Num_<) || (op1 == Num_<=)) &&
                ((op2 == Num_!=) || (op2 == Num_<) || (op2 == Num_<=))
              val canBeGreaterThan =
                ((op1 == Num_!=) || (op1 == Num_>) || (op1 == Num_>=)) &&
                ((op2 == Num_!=) || (op2 == Num_>) || (op2 == Num_>=))

              fold3WayComparison(canBeEqual, canBeLessThan, canBeGreaterThan, l1, r1)

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

  private def transformUnaryOp(tree: UnaryOp)(implicit scope: Scope): Tree = {
    import UnaryOp._

    implicit val pos = tree.pos
    val UnaryOp(op, arg) = tree

    op match {
      case LongToInt =>
        trampoline {
          pretransformExpr(arg) { (targ) =>
            TailCalls.done {
              foldUnaryOp(op, finishTransformOptLongExpr(targ))
            }
          }
        }

      case _ =>
        foldUnaryOp(op, transformExpr(arg))
    }
  }

  private def transformBinaryOp(tree: BinaryOp)(implicit scope: Scope): Tree = {
    import BinaryOp._

    implicit val pos = tree.pos
    val BinaryOp(op, lhs, rhs) = tree

    (op: @switch) match {
      case === | !== =>
        trampoline {
          pretransformExprs(lhs, rhs) { (tlhs, trhs) =>
            TailCalls.done(foldReferenceEquality(tlhs, trhs, op == ===))
          }
        }

      case Long_== | Long_!= | Long_< | Long_<= | Long_> | Long_>= =>
        trampoline {
          pretransformExprs(lhs, rhs) { (tlhs, trhs) =>
            TailCalls.done {
              if (isLiteralOrOptimizableLong(tlhs) &&
                  isLiteralOrOptimizableLong(trhs)) {
                foldBinaryOp(op, finishTransformOptLongExpr(tlhs),
                    finishTransformOptLongExpr(trhs))
              } else {
                foldBinaryOp(op, finishTransformExpr(tlhs),
                    finishTransformExpr(trhs))
              }
            }
          }
        }

      case _ =>
        foldBinaryOp(op, transformExpr(lhs), transformExpr(rhs))
    }
  }

  private def isLiteralOrOptimizableLong(texpr: PreTransform): Boolean = {
    texpr match {
      case PreTransTree(LongLiteral(_), _) =>
        true
      case PreTransLocalDef(LocalDef(_, _, replacement)) =>
        replacement match {
          case ReplaceWithVarRef(_, _, _, Some(_)) => true
          case ReplaceWithConstant(LongLiteral(_)) => true
          case _                                   => false
        }
      case _ =>
        false
    }
  }

  private def finishTransformOptLongExpr(targ: PreTransform): Tree = targ match {
    case PreTransLocalDef(LocalDef(tpe, false,
        ReplaceWithVarRef(_, _, _, Some(argValue)))) =>
      argValue()
    case _ =>
      finishTransformExpr(targ)
  }

  private def foldUnaryOp(op: UnaryOp.Code, arg: Tree)(
      implicit pos: Position): Tree = {
    import UnaryOp._
    @inline def default = UnaryOp(op, arg)
    (op: @switch) match {
      case Boolean_! =>
        arg match {
          case BooleanLiteral(v)     => BooleanLiteral(!v)
          case UnaryOp(Boolean_!, x) => x

          case BinaryOp(innerOp, l, r) =>
            val newOp = (innerOp: @switch) match {
              case BinaryOp.=== => BinaryOp.!==
              case BinaryOp.!== => BinaryOp.===

              case BinaryOp.Num_== => BinaryOp.Num_!=
              case BinaryOp.Num_!= => BinaryOp.Num_==
              case BinaryOp.Num_<  => BinaryOp.Num_>=
              case BinaryOp.Num_<= => BinaryOp.Num_>
              case BinaryOp.Num_>  => BinaryOp.Num_<=
              case BinaryOp.Num_>= => BinaryOp.Num_<

              case BinaryOp.Long_== => BinaryOp.Long_!=
              case BinaryOp.Long_!= => BinaryOp.Long_==
              case BinaryOp.Long_<  => BinaryOp.Long_>=
              case BinaryOp.Long_<= => BinaryOp.Long_>
              case BinaryOp.Long_>  => BinaryOp.Long_<=
              case BinaryOp.Long_>= => BinaryOp.Long_<

              case BinaryOp.Boolean_== => BinaryOp.Boolean_!=
              case BinaryOp.Boolean_!= => BinaryOp.Boolean_==

              case _ => -1
            }
            if (newOp == -1) default
            else BinaryOp(newOp, l, r)

          case _ => default
        }

      case IntToLong =>
        arg match {
          case IntLiteral(v) => LongLiteral(v.toLong)
          case _             => default
        }

      case LongToInt =>
        arg match {
          case LongLiteral(v)        => IntLiteral(v.toInt)
          case UnaryOp(IntToLong, x) => x

          case BinaryOp(BinaryOp.Long_+, x, y) =>
            foldBinaryOp(BinaryOp.Int_+,
                foldUnaryOp(LongToInt, x),
                foldUnaryOp(LongToInt, y))
          case BinaryOp(BinaryOp.Long_-, x, y) =>
            foldBinaryOp(BinaryOp.Int_-,
                foldUnaryOp(LongToInt, x),
                foldUnaryOp(LongToInt, y))

          case BinaryOp(BinaryOp.Long_>> | BinaryOp.Long_>>>,
              x, IntLiteral(32)) =>
            // x.hi__I()
            staticCall(LongImpl.RuntimeLongClass, LongImpl.hi).fold[Tree] {
              default
            } { target =>
              trampoline {
                /* Hack: use an empty scope to inline x.hi__I().
                 * `inline` requires a scope to keep track of the set methods
                 * currently being inlined, and prevent infinite inlining of
                 * recursive methods. In theory we should provide a proper
                 * scope, but we don't have one here. We give an empty scope
                 * instead, which is ok-ish because we "know" that x.hi__I()
                 * will not go into infinite recursion.
                 */
                implicit val scope = Scope.Empty
                inline(Nil, Some(PreTransTree(x)), Nil, target, isStat = false,
                    usePreTransform = false)(finishTransform(isStat = false))
              }
            }

          case _ =>
            // arg.lo__I()
            staticCall(LongImpl.RuntimeLongClass, LongImpl.lo).fold[Tree] {
              default
            } { target =>
              trampoline {
                /* Hack: use an empty scope to inline x.lo__I().
                 * See the comment on the same hack for x.hi__I() above for the
                 * rationale.
                 */
                implicit val scope = Scope.Empty
                inline(Nil, Some(PreTransTree(arg)), Nil, target, isStat = false,
                    usePreTransform = false)(finishTransform(isStat = false))
              }
            }
        }

      case LongToDouble =>
        arg match {
          case LongLiteral(v) => DoubleLiteral(v.toDouble)
          case _              => default
        }
      case DoubleToInt =>
        arg match {
          case _ if arg.tpe == IntType => arg
          case NumberLiteral(v)        => IntLiteral(v.toInt)
          case _                       => default
        }
      case DoubleToFloat =>
        arg match {
          case _ if arg.tpe == FloatType => arg
          case NumberLiteral(v)          => FloatLiteral(v.toFloat)
          case _                         => default
        }
      case DoubleToLong =>
        arg match {
          case _ if arg.tpe == IntType => foldUnaryOp(IntToLong, arg)
          case NumberLiteral(v)        => LongLiteral(v.toLong)
          case _                       => default
        }
      case _ =>
        default
    }
  }

  /** Performs === for two literals.
   *  The result is always known statically.
   */
  private def literal_===(lhs: Literal, rhs: Literal): Boolean = {
    (lhs, rhs) match {
      case (IntLiteral(l), IntLiteral(r))         => l == r
      case (FloatLiteral(l), FloatLiteral(r))     => l == r
      case (NumberLiteral(l), NumberLiteral(r))   => l == r
      case (LongLiteral(l), LongLiteral(r))       => l == r
      case (BooleanLiteral(l), BooleanLiteral(r)) => l == r
      case (StringLiteral(l), StringLiteral(r))   => l == r
      case (ClassOf(l), ClassOf(r))               => l == r
      case (Undefined(), Undefined())             => true
      case (Null(), Null())                       => true
      case _                                      => false
    }
  }

  /** Translate literals to their Scala.js String representation. */
  private def foldToStringForString_+(tree: Tree)(implicit pos : Position): Tree = tree match {
    case FloatLiteral(value) =>
      foldToStringForString_+(DoubleLiteral(value.toDouble))

    case DoubleLiteral(value) =>
      jsNumberToString(value).fold(tree)(StringLiteral(_))

    case LongLiteral(value)    => StringLiteral(value.toString)
    case IntLiteral(value)     => StringLiteral(value.toString)
    case BooleanLiteral(value) => StringLiteral(value.toString)
    case Null()                => StringLiteral("null")
    case Undefined()           => StringLiteral("undefined")
    case _                     => tree
  }

  /* Following the ECMAScript 6 specification */
  private def jsNumberToString(value: Double): Option[String] = {
    if (1.0.toString == "1") {
      // We are in a JS environment, so the host .toString() is the correct one.
      Some(value.toString)
    } else {
      value match {
        case _ if value.isNaN       => Some("NaN")
        case 0                      => Some("0")
        case _ if value < 0         => jsNumberToString(-value).map("-" + _)
        case _ if value.isInfinity  => Some("Infinity")
        case _ if value.isValidInt  => Some(value.toInt.toString)
        case _                      => None
      }
    }
  }

  private def foldBinaryOp(op: BinaryOp.Code, lhs: Tree, rhs: Tree)(
      implicit pos: Position): Tree = {
    import BinaryOp._
    @inline def default = BinaryOp(op, lhs, rhs)
    (op: @switch) match {
      case === | !== =>
        val positive = (op == ===)
        (lhs, rhs) match {
          case (lhs: Literal, rhs: Literal) =>
            BooleanLiteral(literal_===(lhs, rhs) == positive)

          case (_: Literal, _) => foldBinaryOp(op, rhs, lhs)
          case _               => default
        }

      case String_+ =>
        val lhs1 = foldToStringForString_+(lhs)
        val rhs1 = foldToStringForString_+(rhs)
        @inline def stringDefault = BinaryOp(String_+, lhs1, rhs1)
        (lhs1, rhs1) match {
          case (StringLiteral(s1), StringLiteral(s2)) =>
            StringLiteral(s1 + s2)
          case (_, StringLiteral("")) =>
            foldBinaryOp(op, rhs1, lhs1)
          case (StringLiteral(""), _) if rhs1.tpe == StringType =>
            rhs1
          case (_, BinaryOp(String_+, rl, rr)) =>
            foldBinaryOp(String_+, BinaryOp(String_+, lhs1, rl), rr)
          case (BinaryOp(String_+, ll, StringLiteral(lr)), StringLiteral(r)) =>
            BinaryOp(String_+, ll, StringLiteral(lr + r))
          case (BinaryOp(String_+, StringLiteral(""), lr), _) =>
            BinaryOp(String_+, lr, rhs1)
          case _ =>
            stringDefault
      }

      case Int_+ =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l + r)
          case (_, IntLiteral(_))             => foldBinaryOp(Int_+, rhs, lhs)
          case (IntLiteral(0), _)             => rhs

          case (IntLiteral(x),
              BinaryOp(innerOp @ (Int_+ | Int_-), IntLiteral(y), z)) =>
            foldBinaryOp(innerOp, IntLiteral(x+y), z)

          case _                              => default
        }

      case Int_- =>
        (lhs, rhs) match {
          case (_, IntLiteral(r)) => foldBinaryOp(Int_+, lhs, IntLiteral(-r))

          case (IntLiteral(x), BinaryOp(Int_+, IntLiteral(y), z)) =>
            foldBinaryOp(Int_-, IntLiteral(x-y), z)
          case (IntLiteral(x), BinaryOp(Int_-, IntLiteral(y), z)) =>
            foldBinaryOp(Int_+, IntLiteral(x-y), z)

          case (_, BinaryOp(Int_-, IntLiteral(0), x)) =>
            foldBinaryOp(Int_+, lhs, x)

          case _ => default
        }

      case Int_* =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l * r)
          case (_, IntLiteral(_))             => foldBinaryOp(Int_*, rhs, lhs)

          case (IntLiteral(1), _)  => rhs
          case (IntLiteral(-1), _) => foldBinaryOp(Int_-, IntLiteral(0), rhs)

          case _ => default
        }

      case Int_/ =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) if r != 0 => IntLiteral(l / r)

          case (_, IntLiteral(1))  => lhs
          case (_, IntLiteral(-1)) => foldBinaryOp(Int_-, IntLiteral(0), lhs)

          case _ => default
        }

      case Int_% =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) if r != 0 => IntLiteral(l % r)
          case (_, IntLiteral(1 | -1))                  =>
            Block(keepOnlySideEffects(lhs), IntLiteral(0))
          case _                                        => default
        }

      case Int_| =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l | r)
          case (_, IntLiteral(_))             => foldBinaryOp(Int_|, rhs, lhs)
          case (IntLiteral(0), _)             => rhs

          case (IntLiteral(x), BinaryOp(Int_|, IntLiteral(y), z)) =>
            foldBinaryOp(Int_|, IntLiteral(x | y), z)

          case _ => default
        }

      case Int_& =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l & r)
          case (_, IntLiteral(_))             => foldBinaryOp(Int_&, rhs, lhs)
          case (IntLiteral(-1), _)            => rhs

          case (IntLiteral(x), BinaryOp(Int_&, IntLiteral(y), z)) =>
            foldBinaryOp(Int_&, IntLiteral(x & y), z)

          case _ => default
        }

      case Int_^ =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l ^ r)
          case (_, IntLiteral(_))             => foldBinaryOp(Int_^, rhs, lhs)
          case (IntLiteral(0), _)             => rhs

          case (IntLiteral(x), BinaryOp(Int_^, IntLiteral(y), z)) =>
            foldBinaryOp(Int_^, IntLiteral(x ^ y), z)

          case _ => default
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

      case Long_+ =>
        (lhs, rhs) match {
          case (LongLiteral(l), LongLiteral(r)) => LongLiteral(l + r)
          case (_, LongLiteral(_))              => foldBinaryOp(Long_+, rhs, lhs)
          case (LongLiteral(0), _)              => rhs

          case (LongLiteral(x),
              BinaryOp(innerOp @ (Long_+ | Long_-), LongLiteral(y), z)) =>
            foldBinaryOp(innerOp, LongLiteral(x+y), z)

          case _ => default
        }

      case Long_- =>
        (lhs, rhs) match {
          case (_, LongLiteral(r)) => foldBinaryOp(Long_+, LongLiteral(-r), lhs)

          case (LongLiteral(x), BinaryOp(Long_+, LongLiteral(y), z)) =>
            foldBinaryOp(Long_-, LongLiteral(x-y), z)
          case (LongLiteral(x), BinaryOp(Long_-, LongLiteral(y), z)) =>
            foldBinaryOp(Long_+, LongLiteral(x-y), z)

          case (_, BinaryOp(BinaryOp.Long_-, LongLiteral(0L), x)) =>
            foldBinaryOp(Long_+, lhs, x)

          case _ => default
        }

      case Long_* =>
        (lhs, rhs) match {
          case (LongLiteral(l), LongLiteral(r)) => LongLiteral(l * r)
          case (_, LongLiteral(_))              => foldBinaryOp(Long_*, rhs, lhs)

          case (LongLiteral(1), _)  => rhs
          case (LongLiteral(-1), _) => foldBinaryOp(Long_-, LongLiteral(0), lhs)

          case _ => default
        }

      case Long_/ =>
        (lhs, rhs) match {
          case (_, LongLiteral(0))              => default
          case (LongLiteral(l), LongLiteral(r)) => LongLiteral(l / r)

          case (_, LongLiteral(1))  => lhs
          case (_, LongLiteral(-1)) => foldBinaryOp(Long_-, LongLiteral(0), lhs)

          case (LongFromInt(x), LongFromInt(y: IntLiteral)) if y.value != -1 =>
            LongFromInt(foldBinaryOp(Int_/, x, y))

          case _ => default
        }

      case Long_% =>
        (lhs, rhs) match {
          case (_, LongLiteral(0))              => default
          case (LongLiteral(l), LongLiteral(r)) => LongLiteral(l % r)

          case (_, LongLiteral(1L | -1L)) =>
            Block(keepOnlySideEffects(lhs), LongLiteral(0L))

          case (LongFromInt(x), LongFromInt(y)) =>
            LongFromInt(foldBinaryOp(Int_%, x, y))

          case _ => default
        }

      case Long_| =>
        (lhs, rhs) match {
          case (LongLiteral(l), LongLiteral(r)) => LongLiteral(l | r)
          case (_, LongLiteral(_))              => foldBinaryOp(Long_|, rhs, lhs)
          case (LongLiteral(0), _)              => rhs

          case (LongLiteral(x), BinaryOp(Long_|, LongLiteral(y), z)) =>
            foldBinaryOp(Long_|, LongLiteral(x | y), z)

          case _ => default
        }

      case Long_& =>
        (lhs, rhs) match {
          case (LongLiteral(l), LongLiteral(r)) => LongLiteral(l & r)
          case (_, LongLiteral(_))              => foldBinaryOp(Long_&, rhs, lhs)
          case (LongLiteral(-1), _)             => rhs

          case (LongLiteral(x), BinaryOp(Long_&, LongLiteral(y), z)) =>
            foldBinaryOp(Long_&, LongLiteral(x & y), z)

          case _ => default
        }

      case Long_^ =>
        (lhs, rhs) match {
          case (LongLiteral(l), LongLiteral(r)) => LongLiteral(l ^ r)
          case (_, LongLiteral(_))              => foldBinaryOp(Long_^, rhs, lhs)
          case (LongLiteral(0), _)              => rhs

          case (LongLiteral(x), BinaryOp(Long_^, LongLiteral(y), z)) =>
            foldBinaryOp(Long_^, LongLiteral(x ^ y), z)

          case _ => default
        }

      case Long_<< =>
        (lhs, rhs) match {
          case (LongLiteral(l), IntLiteral(r))   => LongLiteral(l << r)
          case (_, IntLiteral(x)) if x % 64 == 0 => lhs
          case _                                 => default
        }

      case Long_>>> =>
        (lhs, rhs) match {
          case (LongLiteral(l), IntLiteral(r))   => LongLiteral(l >>> r)
          case (_, IntLiteral(x)) if x % 64 == 0 => lhs
          case _                                 => default
        }

      case Long_>> =>
        (lhs, rhs) match {
          case (LongLiteral(l), IntLiteral(r))   => LongLiteral(l >> r)
          case (_, IntLiteral(x)) if x % 64 == 0 => lhs
          case _                                 => default
        }

      case Long_== | Long_!= =>
        val positive = (op == Long_==)
        (lhs, rhs) match {
          case (LongLiteral(l), LongLiteral(r)) =>
            BooleanLiteral((l == r) == positive)

          case (LongFromInt(x), LongFromInt(y)) =>
            foldBinaryOp(if (positive) === else !==, x, y)
          case (LongFromInt(x), LongLiteral(y)) =>
            assert(y > Int.MaxValue || y < Int.MinValue)
            Block(keepOnlySideEffects(x), BooleanLiteral(!positive))

          case (BinaryOp(Long_+, LongLiteral(x), y), LongLiteral(z)) =>
            foldBinaryOp(op, y, LongLiteral(z-x))
          case (BinaryOp(Long_-, LongLiteral(x), y), LongLiteral(z)) =>
            foldBinaryOp(op, y, LongLiteral(x-z))

          case (LongLiteral(_), _) => foldBinaryOp(op, rhs, lhs)
          case _                   => default
        }

      case Long_< | Long_<= | Long_> | Long_>= =>
        def flippedOp = (op: @switch) match {
          case Long_<  => Long_>
          case Long_<= => Long_>=
          case Long_>  => Long_<
          case Long_>= => Long_<=
        }

        def intOp = (op: @switch) match {
          case Long_<  => Num_<
          case Long_<= => Num_<=
          case Long_>  => Num_>
          case Long_>= => Num_>=
        }

        (lhs, rhs) match {
          case (LongLiteral(l), LongLiteral(r)) =>
            val result = (op: @switch) match {
              case Long_<  => l < r
              case Long_<= => l <= r
              case Long_>  => l > r
              case Long_>= => l >= r
            }
            BooleanLiteral(result)

          case (_, LongLiteral(Long.MinValue)) =>
            if (op == Long_< || op == Long_>=)
              Block(keepOnlySideEffects(lhs), BooleanLiteral(op == Long_>=))
            else
              foldBinaryOp(if (op == Long_<=) Long_== else Long_!=, lhs, rhs)

          case (_, LongLiteral(Long.MaxValue)) =>
            if (op == Long_> || op == Long_<=)
              Block(keepOnlySideEffects(lhs), BooleanLiteral(op == Long_<=))
            else
              foldBinaryOp(if (op == Long_>=) Long_== else Long_!=, lhs, rhs)

          case (LongFromInt(x), LongFromInt(y)) =>
            foldBinaryOp(intOp, x, y)
          case (LongFromInt(x), LongLiteral(y)) =>
            assert(y > Int.MaxValue || y < Int.MinValue)
            val result =
              if (y > Int.MaxValue) op == Long_< || op == Long_<=
              else                  op == Long_> || op == Long_>=
            Block(keepOnlySideEffects(x), BooleanLiteral(result))

          /* x + y.toLong > z
           *      -x on both sides
           *      requires x + y.toLong not to overflow, and z - x likewise
           * y.toLong > z - x
           */
          case (BinaryOp(Long_+, LongLiteral(x), y @ LongFromInt(_)), LongLiteral(z))
              if canAddLongs(x, Int.MinValue) &&
                 canAddLongs(x, Int.MaxValue) &&
                 canSubtractLongs(z, x) =>
            foldBinaryOp(op, y, LongLiteral(z-x))

          /* x - y.toLong > z
           *      -x on both sides
           *      requires x - y.toLong not to overflow, and z - x likewise
           * -(y.toLong) > z - x
           */
          case (BinaryOp(Long_-, LongLiteral(x), y @ LongFromInt(_)), LongLiteral(z))
              if canSubtractLongs(x, Int.MinValue) &&
                 canSubtractLongs(x, Int.MaxValue) &&
                 canSubtractLongs(z, x) =>
            if (z-x != Long.MinValue) {
              // Since -(y.toLong) does not overflow, we can negate both sides
              foldBinaryOp(flippedOp, y, LongLiteral(-(z-x)))
            } else {
              /* -(y.toLong) > Long.MinValue
               * Depending on the operator, this is either always true or
               * always false.
               */
              val result = (op == Long_>) || (op == Long_>=)
              Block(keepOnlySideEffects(y), BooleanLiteral(result))
            }

          /* x.toLong + y.toLong > Int.MaxValue.toLong
           *
           * This is basically testing whether x+y overflows in positive.
           * If x <= 0 or y <= 0, this cannot happen -> false.
           * If x > 0 and y > 0, this can be detected with x+y < 0.
           * Therefore, we rewrite as:
           *
           * x > 0 && y > 0 && x+y < 0.
           *
           * This requires to evaluate x and y once.
           */
          case (BinaryOp(Long_+, LongFromInt(x), LongFromInt(y)),
              LongLiteral(Int.MaxValue)) =>
            trampoline {
              withNewLocalDefs(List(
                  Binding("x", None, IntType, false, PreTransTree(x)),
                  Binding("y", None, IntType, false, PreTransTree(y)))) {
                (tempsLocalDefs, cont) =>
                  val List(tempXDef, tempYDef) = tempsLocalDefs
                  val tempX = tempXDef.newReplacement
                  val tempY = tempYDef.newReplacement
                  cont(PreTransTree(
                      AndThen(AndThen(
                          BinaryOp(Num_>, tempX, IntLiteral(0)),
                          BinaryOp(Num_>, tempY, IntLiteral(0))),
                          BinaryOp(Num_<, BinaryOp(Int_+, tempX, tempY), IntLiteral(0)))))
              } (finishTransform(isStat = false))
            }

          case (LongLiteral(_), _) => foldBinaryOp(flippedOp, rhs, lhs)
          case _                   => default
        }

      case Float_+ =>
        (lhs, rhs) match {
          case (FloatLiteral(l), FloatLiteral(r)) => FloatLiteral(l + r)
          case (FloatLiteral(0), _)               => rhs
          case (_, FloatLiteral(_))               => foldBinaryOp(Float_+, rhs, lhs)

          case (FloatLiteral(x),
              BinaryOp(innerOp @ (Float_+ | Float_-), FloatLiteral(y), z)) =>
            foldBinaryOp(innerOp, FloatLiteral(x+y), z)

          case _ => default
        }

      case Float_- =>
        (lhs, rhs) match {
          case (_, FloatLiteral(r)) => foldBinaryOp(Float_+, lhs, FloatLiteral(-r))

          case (FloatLiteral(x), BinaryOp(Float_+, FloatLiteral(y), z)) =>
            foldBinaryOp(Float_-, FloatLiteral(x-y), z)
          case (FloatLiteral(x), BinaryOp(Float_-, FloatLiteral(y), z)) =>
            foldBinaryOp(Float_+, FloatLiteral(x-y), z)

          case (_, BinaryOp(BinaryOp.Float_-, FloatLiteral(0), x)) =>
            foldBinaryOp(Float_+, lhs, x)

          case _ => default
        }

      case Float_* =>
        (lhs, rhs) match {
          case (FloatLiteral(l), FloatLiteral(r)) => FloatLiteral(l * r)
          case (_, FloatLiteral(_))               => foldBinaryOp(Float_*, rhs, lhs)

          case (FloatLiteral(1), _)  => rhs
          case (FloatLiteral(-1), _) => foldBinaryOp(Float_-, FloatLiteral(0), rhs)

          case _ => default
        }

      case Float_/ =>
        (lhs, rhs) match {
          case (FloatLiteral(l), FloatLiteral(r)) => FloatLiteral(l / r)

          case (_, FloatLiteral(1))  => lhs
          case (_, FloatLiteral(-1)) => foldBinaryOp(Float_-, FloatLiteral(0), lhs)

          case _ => default
        }

      case Float_% =>
        (lhs, rhs) match {
          case (FloatLiteral(l), FloatLiteral(r)) => FloatLiteral(l % r)
          case _                                  => default
        }

      case Double_+ =>
        (lhs, rhs) match {
          case (NumberLiteral(l), NumberLiteral(r)) => DoubleLiteral(l + r)
          case (NumberLiteral(0), _)                => rhs
          case (_, NumberLiteral(_))                => foldBinaryOp(Double_+, rhs, lhs)

          case (NumberLiteral(x),
              BinaryOp(innerOp @ (Double_+ | Double_-), NumberLiteral(y), z)) =>
            foldBinaryOp(innerOp, DoubleLiteral(x+y), z)

          case _ => default
        }

      case Double_- =>
        (lhs, rhs) match {
          case (_, NumberLiteral(r)) => foldBinaryOp(Double_+, lhs, DoubleLiteral(-r))

          case (NumberLiteral(x), BinaryOp(Double_+, NumberLiteral(y), z)) =>
            foldBinaryOp(Double_-, DoubleLiteral(x-y), z)
          case (NumberLiteral(x), BinaryOp(Double_-, NumberLiteral(y), z)) =>
            foldBinaryOp(Double_+, DoubleLiteral(x-y), z)

          case (_, BinaryOp(BinaryOp.Double_-, NumberLiteral(0), x)) =>
            foldBinaryOp(Double_+, lhs, x)

          case _ => default
        }

      case Double_* =>
        (lhs, rhs) match {
          case (NumberLiteral(l), NumberLiteral(r)) => DoubleLiteral(l * r)
          case (_, NumberLiteral(_))                => foldBinaryOp(Double_*, rhs, lhs)

          case (NumberLiteral(1), _)  => rhs
          case (NumberLiteral(-1), _) => foldBinaryOp(Double_-, DoubleLiteral(0), rhs)

          case _ => default
        }

      case Double_/ =>
        (lhs, rhs) match {
          case (NumberLiteral(l), NumberLiteral(r)) => DoubleLiteral(l / r)

          case (_, NumberLiteral(1))  => lhs
          case (_, NumberLiteral(-1)) => foldBinaryOp(Double_-, DoubleLiteral(0), lhs)

          case _ => default
        }

      case Double_% =>
        (lhs, rhs) match {
          case (NumberLiteral(l), NumberLiteral(r)) => DoubleLiteral(l % r)
          case _                                    => default
        }

      case Boolean_== | Boolean_!= =>
        val positive = (op == Boolean_==)
        (lhs, rhs) match {
          case (BooleanLiteral(l), _) =>
            if (l == positive) rhs
            else foldUnaryOp(UnaryOp.Boolean_!, rhs)
          case (_, BooleanLiteral(r)) =>
            if (r == positive) lhs
            else foldUnaryOp(UnaryOp.Boolean_!, lhs)
          case _ =>
            default
        }

      case Boolean_| =>
        (lhs, rhs) match {
          case (_, BooleanLiteral(false))             => lhs
          case (BooleanLiteral(false), _)             => rhs
          case _                                      => default
        }

      case Boolean_& =>
        (lhs, rhs) match {
          case (_, BooleanLiteral(true))              => lhs
          case (BooleanLiteral(true), _)              => rhs
          case _                                      => default
        }

      case Num_== | Num_!= =>
        val positive = (op == Num_==)
        (lhs, rhs) match {
          case (lhs: Literal, rhs: Literal) =>
            BooleanLiteral(literal_===(lhs, rhs) == positive)

          case (BinaryOp(Int_+, IntLiteral(x), y), IntLiteral(z)) =>
            foldBinaryOp(op, y, IntLiteral(z-x))
          case (BinaryOp(Int_-, IntLiteral(x), y), IntLiteral(z)) =>
            foldBinaryOp(op, y, IntLiteral(x-z))

          case (_: Literal, _) => foldBinaryOp(op, rhs, lhs)
          case _               => default
        }

      case Num_< | Num_<= | Num_> | Num_>= =>
        def flippedOp = (op: @switch) match {
          case Num_<  => Num_>
          case Num_<= => Num_>=
          case Num_>  => Num_<
          case Num_>= => Num_<=
        }

        if (lhs.tpe == IntType && rhs.tpe == IntType) {
          (lhs, rhs) match {
            case (IntLiteral(l), IntLiteral(r)) =>
              val result = (op: @switch) match {
                case Num_<  => l < r
                case Num_<= => l <= r
                case Num_>  => l > r
                case Num_>= => l >= r
              }
              BooleanLiteral(result)

            case (_, IntLiteral(Int.MinValue)) =>
              if (op == Num_< || op == Num_>=)
                Block(keepOnlySideEffects(lhs), BooleanLiteral(op == Num_>=))
              else
                foldBinaryOp(if (op == Num_<=) Num_== else Num_!=, lhs, rhs)

            case (_, IntLiteral(Int.MaxValue)) =>
              if (op == Num_> || op == Num_<=)
                Block(keepOnlySideEffects(lhs), BooleanLiteral(op == Num_<=))
              else
                foldBinaryOp(if (op == Num_>=) Num_== else Num_!=, lhs, rhs)

            case (IntLiteral(_), _) => foldBinaryOp(flippedOp, rhs, lhs)
            case _                  => default
          }
        } else {
          (lhs, rhs) match {
            case (NumberLiteral(l), NumberLiteral(r)) =>
              val result = (op: @switch) match {
                case Num_<  => l < r
                case Num_<= => l <= r
                case Num_>  => l > r
                case Num_>= => l >= r
              }
              BooleanLiteral(result)

            case _ => default
          }
        }

      case _ =>
        default
    }
  }

  private def fold3WayComparison(canBeEqual: Boolean, canBeLessThan: Boolean,
      canBeGreaterThan: Boolean, lhs: Tree, rhs: Tree)(
      implicit pos: Position): Tree = {
    import BinaryOp._
    if (canBeEqual) {
      if (canBeLessThan) {
        if (canBeGreaterThan)
          Block(keepOnlySideEffects(lhs), keepOnlySideEffects(rhs), BooleanLiteral(true))
        else
          foldBinaryOp(Num_<=, lhs, rhs)
      } else {
        if (canBeGreaterThan)
          foldBinaryOp(Num_>=, lhs, rhs)
        else
          foldBinaryOp(Num_==, lhs, rhs)
      }
    } else {
      if (canBeLessThan) {
        if (canBeGreaterThan)
          foldBinaryOp(Num_!=, lhs, rhs)
        else
          foldBinaryOp(Num_<, lhs, rhs)
      } else {
        if (canBeGreaterThan)
          foldBinaryOp(Num_>, lhs, rhs)
        else
          Block(keepOnlySideEffects(lhs), keepOnlySideEffects(rhs), BooleanLiteral(false))
      }
    }
  }

  private def foldUnbox(arg: PreTransform, charCode: Char)(
      cont: PreTransCont): TailRec[Tree] = {
    (charCode: @switch) match {
      case 'Z' if arg.tpe.base == BooleanType => cont(arg)
      case 'I' if arg.tpe.base == IntType     => cont(arg)
      case 'F' if arg.tpe.base == FloatType   => cont(arg)
      case 'J' if arg.tpe.base == LongType    => cont(arg)
      case 'D' if arg.tpe.base == DoubleType ||
          arg.tpe.base == IntType || arg.tpe.base == FloatType => cont(arg)
      case _ =>
        cont(PreTransTree(Unbox(finishTransformExpr(arg), charCode)(arg.pos)))
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

  private def foldJSBracketSelect(qualifier: Tree, item: Tree)(
      implicit pos: Position): Tree = {
    // !!! Must be in sync with scala.scalajs.runtime.LinkingInfo

    @inline def default =
      JSBracketSelect(qualifier, item)

    (qualifier, item) match {
      case (JSBracketSelect(JSLinkingInfo(), StringLiteral("semantics")),
          StringLiteral(semanticsStr)) =>
        def behavior2IntLiteral(behavior: CheckedBehavior) = {
          IntLiteral(behavior match {
            case CheckedBehavior.Compliant => 0
            case CheckedBehavior.Fatal     => 1
            case CheckedBehavior.Unchecked => 2
          })
        }
        semanticsStr match {
          case "asInstanceOfs" =>
            behavior2IntLiteral(semantics.asInstanceOfs)
          case "moduleInit" =>
            behavior2IntLiteral(semantics.moduleInit)
          case "strictFloats" =>
            BooleanLiteral(semantics.strictFloats)
          case "productionMode" =>
            BooleanLiteral(semantics.productionMode)
          case _ =>
            default
        }

      case (JSLinkingInfo(), StringLiteral("assumingES6")) =>
        BooleanLiteral(esLevel match {
          case ESLevel.ES5 => false
          case ESLevel.ES6 => true
        })

      case _ =>
        default
    }
  }

  def transformIsolatedBody(optTarget: Option[MethodID],
      thisType: Type, params: List[ParamDef], resultType: Type,
      body: Tree): (List[ParamDef], Tree) = {
    val (paramLocalDefs, newParamDefs) = (for {
      p @ ParamDef(ident @ Ident(name, originalName), ptpe, mutable, rest) <- params
    } yield {
      val newName = freshLocalName(name, mutable)
      val newOriginalName = originalName.orElse(Some(newName))
      val localDef = LocalDef(RefinedType(ptpe), mutable,
          ReplaceWithVarRef(newName, newOriginalName, newSimpleState(true), None))
      val newParamDef = ParamDef(
          Ident(newName, newOriginalName)(ident.pos), ptpe, mutable, rest)(p.pos)
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

    val allocationSites = List.fill(allLocalDefs.size)(None)
    val scope0 = optTarget.fold(Scope.Empty)(
        target => Scope.Empty.inlining((allocationSites, target)))
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

    val info = new LabelInfo(newLabel, acceptRecords = usePreTransform,
        returnedTypes = newSimpleState(Nil))
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

  /** Tries and optimizes the remainings of a pattern match as if/elses.
   *
   *  !!! There is quite of bit of code duplication with
   *      GenJSCode.genOptimizedLabeled.
   */
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
      val newName = freshLocalName(name, mutable)
      val newOriginalName = originalName.orElse(Some(name))

      val used = newSimpleState(false)

      def doBuildInner(localDef: LocalDef)(varDef: => VarDef)(
          cont: PreTransCont): TailRec[Tree] = {
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
          } (cont)

        case PreTransTree(valueTree, valueTpe) =>
          def doDoBuildInner(optValueTree: Option[() => Tree])(
              cont1: PreTransCont) = {
            val localDef = LocalDef(tpe, mutable, ReplaceWithVarRef(
                newName, newOriginalName, used, optValueTree))
            doBuildInner(localDef) {
              VarDef(Ident(newName, newOriginalName), tpe.base, mutable,
                  optValueTree.fold(valueTree)(_()))
            } (cont1)
          }
          if (mutable) {
            doDoBuildInner(None)(cont)
          } else (valueTree match {
            case LongFromInt(arg) =>
              withNewLocalDef(
                  Binding("x", None, IntType, false, PreTransTree(arg))) {
                (intLocalDef, cont1) =>
                  doDoBuildInner(Some(
                      () => LongFromInt(intLocalDef.newReplacement)))(
                      cont1)
              } (cont)

            case BinaryOp(op @ (BinaryOp.Long_+ | BinaryOp.Long_-),
                LongFromInt(intLhs), LongFromInt(intRhs)) =>
              withNewLocalDefs(List(
                  Binding("x", None, IntType, false, PreTransTree(intLhs)),
                  Binding("y", None, IntType, false, PreTransTree(intRhs)))) {
                (intLocalDefs, cont1) =>
                  val List(lhsLocalDef, rhsLocalDef) = intLocalDefs
                  doDoBuildInner(Some(
                      () => BinaryOp(op,
                          LongFromInt(lhsLocalDef.newReplacement),
                          LongFromInt(rhsLocalDef.newReplacement))))(
                      cont1)
              } (cont)

            case _ =>
              doDoBuildInner(None)(cont)
          })
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

        case PreTransTree(VarRef(Ident(refName, refOriginalName)), _)
            if !localIsMutable(refName) =>
          buildInner(LocalDef(refinedType, false,
              ReplaceWithVarRef(refName, refOriginalName,
                  newSimpleState(true), None)), cont)

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
            e.stateBackups.foreach(_.restore())

            rec = e.cont
        }
      }

      sys.error("Reached end of infinite loop")
    } finally {
      curTrampolineId -= 1
    }
  }
}

private[optimizer] object OptimizerCore {

  private final val MaxRollbacksPerMethod = 256

  private final class TooManyRollbacksException
      extends scala.util.control.ControlThrowable

  private val AnonFunctionClassPrefix = "sjsr_AnonFunction"

  private type CancelFun = () => Nothing
  private type PreTransCont = PreTransform => TailRec[Tree]

  private case class RefinedType private (base: Type, isExact: Boolean,
      isNullable: Boolean)(
      val allocationSite: Option[AllocationSite], dummy: Int = 0) {

    def isNothingType: Boolean = base == NothingType
  }

  private object RefinedType {
    def apply(base: Type, isExact: Boolean, isNullable: Boolean,
        allocationSite: Option[AllocationSite]): RefinedType =
      new RefinedType(base, isExact, isNullable)(allocationSite)

    def apply(base: Type, isExact: Boolean, isNullable: Boolean): RefinedType =
      RefinedType(base, isExact, isNullable, None)

    def apply(tpe: Type): RefinedType = tpe match {
      case IntType | FloatType | DoubleType =>
        RefinedType(tpe, isExact = false, isNullable = false)
      case BooleanType | StringType | UndefType | NothingType |
          _:RecordType | NoType =>
        RefinedType(tpe, isExact = true, isNullable = false)
      case NullType =>
        RefinedType(tpe, isExact = true, isNullable = true)
      case _ =>
        RefinedType(tpe, isExact = false, isNullable = true)
    }

    val NoRefinedType = RefinedType(NoType)
    val Nothing = RefinedType(NothingType)
  }

  private class AllocationSite(private val node: Tree) {
    override def equals(that: Any): Boolean = that match {
      case that: AllocationSite => this.node eq that.node
      case _                    => false
    }

    override def hashCode(): Int =
      System.identityHashCode(node)

    override def toString(): String =
      s"AllocationSite($node)"
  }

  private case class LocalDef(
      tpe: RefinedType,
      mutable: Boolean,
      replacement: LocalDefReplacement) {

    def newReplacement(implicit pos: Position): Tree = replacement match {
      case ReplaceWithVarRef(name, originalName, used, _) =>
        used.value = true
        VarRef(Ident(name, originalName))(tpe.base)

      case ReplaceWithRecordVarRef(_, _, _, _, cancelFun) =>
        cancelFun()

      case ReplaceWithThis() =>
        This()(tpe.base)

      case ReplaceWithConstant(value) =>
        value

      case TentativeClosureReplacement(_, _, _, _, _, cancelFun) =>
        cancelFun()

      case InlineClassBeingConstructedReplacement(_, cancelFun) =>
        cancelFun()

      case InlineClassInstanceReplacement(_, _, cancelFun) =>
        cancelFun()
    }

    def contains(that: LocalDef): Boolean = {
      (this eq that) || (replacement match {
        case TentativeClosureReplacement(_, _, _, captureLocalDefs, _, _) =>
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
      used: SimpleState[Boolean],
      longOpTree: Option[() => Tree]) extends LocalDefReplacement

  private final case class ReplaceWithRecordVarRef(name: String,
      originalName: Option[String],
      recordType: RecordType,
      used: SimpleState[Boolean],
      cancelFun: CancelFun) extends LocalDefReplacement

  private final case class ReplaceWithThis() extends LocalDefReplacement

  private final case class ReplaceWithConstant(
      value: Tree) extends LocalDefReplacement

  private final case class TentativeClosureReplacement(
      captureParams: List[ParamDef], params: List[ParamDef], body: Tree,
      captureValues: List[LocalDef],
      alreadyUsed: SimpleState[Boolean],
      cancelFun: CancelFun) extends LocalDefReplacement

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
      val returnedTypes: SimpleState[List[(Type, RefinedType)]])

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
      val implsBeingInlined: Set[(List[Option[AllocationSite]], AbstractMethodID)]) {
    def withEnv(env: OptEnv): Scope =
      new Scope(env, implsBeingInlined)

    def inlining(impl: (List[Option[AllocationSite]],
        AbstractMethodID)): Scope = {
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
    def apply(tree: Tree): PreTransTree = {
      val refinedTpe: RefinedType = tree match {
        case BlockOrAlone(_,
            _:LoadModule | _:NewArray | _:ArrayValue | _:GetClass |
            _:ClassOf) =>
          RefinedType(tree.tpe, isExact = true, isNullable = false)
        case _ =>
          RefinedType(tree.tpe)
      }
      PreTransTree(tree, refinedTpe)
    }
  }

  private final case class Binding(name: String, originalName: Option[String],
      declaredType: Type, mutable: Boolean, value: PreTransform)

  private object NumberLiteral {
    def unapply(tree: Literal): Option[Double] = tree match {
      case DoubleLiteral(v) => Some(v)
      case IntLiteral(v)    => Some(v.toDouble)
      case FloatLiteral(v)  => Some(v.toDouble)
      case _                => None
    }
  }

  private object LongFromInt {
    def apply(x: Tree)(implicit pos: Position): Tree = x match {
      case IntLiteral(v) => LongLiteral(v)
      case _             => UnaryOp(UnaryOp.IntToLong, x)
    }

    def unapply(tree: Tree): Option[Tree] = tree match {
      case LongLiteral(v) if v.toInt == v => Some(IntLiteral(v.toInt)(tree.pos))
      case UnaryOp(UnaryOp.IntToLong, x)  => Some(x)
      case _                              => None
    }
  }

  private object AndThen {
    def apply(lhs: Tree, rhs: Tree)(implicit pos: Position): Tree =
      If(lhs, rhs, BooleanLiteral(false))(BooleanType)
  }

  /** Tests whether `x + y` is valid without falling out of range. */
  private def canAddLongs(x: Long, y: Long): Boolean =
    if (y >= 0) x+y >= x
    else        x+y <  x

  /** Tests whether `x - y` is valid without falling out of range. */
  private def canSubtractLongs(x: Long, y: Long): Boolean =
    if (y >= 0) x-y <= x
    else        x-y >  x

  /** Tests whether `-x` is valid without falling out of range. */
  private def canNegateLong(x: Long): Boolean =
    x != Long.MinValue

  private object Intrinsics {
    final val ArrayCopy        = 1
    final val IdentityHashCode = ArrayCopy + 1

    final val ArrayApply  = IdentityHashCode + 1
    final val ArrayUpdate = ArrayApply       + 1
    final val ArrayLength = ArrayUpdate      + 1

    final val PropertiesOf = ArrayLength + 1

    final val IntegerNLZ = PropertiesOf + 1

    final val LongToString = IntegerNLZ + 1
    final val LongCompare = LongToString + 1
    final val LongDivideUnsigned = LongCompare + 1
    final val LongRemainderUnsigned = LongDivideUnsigned + 1

    final val ArrayBuilderZeroOf = LongRemainderUnsigned + 1
    final val GenericArrayBuilderResult = ArrayBuilderZeroOf + 1

    final val ClassGetComponentType = GenericArrayBuilderResult + 1

    final val ArrayNewInstance = ClassGetComponentType + 1

    final val ByteArrayToInt8Array      = ArrayNewInstance         + 1
    final val ShortArrayToInt16Array    = ByteArrayToInt8Array     + 1
    final val CharArrayToUint16Array    = ShortArrayToInt16Array   + 1
    final val IntArrayToInt32Array      = CharArrayToUint16Array   + 1
    final val FloatArrayToFloat32Array  = IntArrayToInt32Array     + 1
    final val DoubleArrayToFloat64Array = FloatArrayToFloat32Array + 1

    final val Int8ArrayToByteArray      = DoubleArrayToFloat64Array + 1
    final val Int16ArrayToShortArray    = Int8ArrayToByteArray      + 1
    final val Uint16ArrayToCharArray    = Int16ArrayToShortArray    + 1
    final val Int32ArrayToIntArray      = Uint16ArrayToCharArray    + 1
    final val Float32ArrayToFloatArray  = Int32ArrayToIntArray      + 1
    final val Float64ArrayToDoubleArray = Float32ArrayToFloatArray  + 1

    val intrinsics: Map[String, Int] = Map(
      "jl_System$.arraycopy__O__I__O__I__I__V" -> ArrayCopy,
      "jl_System$.identityHashCode__O__I"      -> IdentityHashCode,

      "sr_ScalaRunTime$.array$undapply__O__I__O"     -> ArrayApply,
      "sr_ScalaRunTime$.array$undupdate__O__I__O__V" -> ArrayUpdate,
      "sr_ScalaRunTime$.array$undlength__O__I"       -> ArrayLength,

      "sjsr_package$.propertiesOf__sjs_js_Any__sjs_js_Array" -> PropertiesOf,

      "jl_Integer$.numberOfLeadingZeros__I__I" -> IntegerNLZ,

      "jl_Long$.toString__J__T"              -> LongToString,
      "jl_Long$.compare__J__J__I"            -> LongCompare,
      "jl_Long$.divideUnsigned__J__J__J"     -> LongDivideUnsigned,
      "jl_Long$.remainderUnsigned__J__J__J"  -> LongRemainderUnsigned,

      "scm_ArrayBuilder$.scala$collection$mutable$ArrayBuilder$$zeroOf__jl_Class__O" -> ArrayBuilderZeroOf,
      "scm_ArrayBuilder$.scala$collection$mutable$ArrayBuilder$$genericArrayBuilderResult__jl_Class__sjs_js_Array__O" -> GenericArrayBuilderResult,

      "jl_Class.getComponentType__jl_Class" -> ClassGetComponentType,

      "jl_reflect_Array$.newInstance__jl_Class__I__O" -> ArrayNewInstance,

      "sjs_js_typedarray_package$.byteArray2Int8Array__AB__sjs_js_typedarray_Int8Array"         -> ByteArrayToInt8Array,
      "sjs_js_typedarray_package$.shortArray2Int16Array__AS__sjs_js_typedarray_Int16Array"      -> ShortArrayToInt16Array,
      "sjs_js_typedarray_package$.charArray2Uint16Array__AC__sjs_js_typedarray_Uint16Array"     -> CharArrayToUint16Array,
      "sjs_js_typedarray_package$.intArray2Int32Array__AI__sjs_js_typedarray_Int32Array"        -> IntArrayToInt32Array,
      "sjs_js_typedarray_package$.floatArray2Float32Array__AF__sjs_js_typedarray_Float32Array"  -> FloatArrayToFloat32Array,
      "sjs_js_typedarray_package$.doubleArray2Float64Array__AD__sjs_js_typedarray_Float64Array" -> DoubleArrayToFloat64Array,

      "sjs_js_typedarray_package$.int8Array2ByteArray__sjs_js_typedarray_Int8Array__AB"         -> Int8ArrayToByteArray,
      "sjs_js_typedarray_package$.int16Array2ShortArray__sjs_js_typedarray_Int16Array__AS"      -> Int16ArrayToShortArray,
      "sjs_js_typedarray_package$.uint16Array2CharArray__sjs_js_typedarray_Uint16Array__AC"     -> Uint16ArrayToCharArray,
      "sjs_js_typedarray_package$.int32Array2IntArray__sjs_js_typedarray_Int32Array__AI"        -> Int32ArrayToIntArray,
      "sjs_js_typedarray_package$.float32Array2FloatArray__sjs_js_typedarray_Float32Array__AF"  -> Float32ArrayToFloatArray,
      "sjs_js_typedarray_package$.float64Array2DoubleArray__sjs_js_typedarray_Float64Array__AD" -> Float64ArrayToDoubleArray
    ).withDefaultValue(-1)
  }

  private def getIntrinsicCode(target: AbstractMethodID): Int =
    Intrinsics.intrinsics(target.toString)

  private trait StateBackup {
    def restore(): Unit
  }

  private trait State {
    def makeBackup(): StateBackup
  }

  private class SimpleState[A](var value: A) extends State {
    private class Backup(savedValue: A) extends StateBackup {
      override def restore(): Unit = value = savedValue
    }

    def makeBackup(): StateBackup = new Backup(value)
  }

  trait AbstractMethodID {
    def inlineable: Boolean
    def shouldInline: Boolean
    def isForwarder: Boolean
  }

  /** Parts of [[GenIncOptimizer#MethodImpl]] with decisions about optimizations. */
  abstract class MethodImpl {
    def encodedName: String
    def optimizerHints: OptimizerHints
    def originalDef: MethodDef
    def thisType: Type

    var inlineable: Boolean = false
    var shouldInline: Boolean = false
    var isForwarder: Boolean = false

    protected def updateInlineable(): Unit = {
      val MethodDef(_, Ident(methodName, _), params, _, body) = originalDef

      isForwarder = body match {
        // Shape of forwarders to trait impls
        case ApplyStatic(impl, method, args) =>
          ((args.size == params.size + 1) &&
              (args.head.isInstanceOf[This]) &&
              (args.tail.zip(params).forall {
                case (VarRef(Ident(aname, _)),
                    ParamDef(Ident(pname, _), _, _, _)) => aname == pname
                case _ => false
              }))

        // Shape of bridges for generic methods
        case MaybeBox(Apply(This(), method, args), _) =>
          (args.size == params.size) &&
          args.zip(params).forall {
            case (MaybeUnbox(VarRef(Ident(aname, _)), _),
                ParamDef(Ident(pname, _), _, _, _)) => aname == pname
            case _ => false
          }

        case _ => false
      }

      inlineable = !optimizerHints.noinline
      shouldInline = inlineable && {
        optimizerHints.inline || isForwarder || {
          val MethodDef(_, _, params, _, body) = originalDef
          body match {
            case _:Skip | _:This | _:Literal                          => true

            // Shape of accessors
            case Select(This(), _) if params.isEmpty                  => true
            case Assign(Select(This(), _), VarRef(_))
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
  }

  private object MaybeBox {
    def unapply(tree: Tree): Some[(Tree, Any)] = tree match {
      case Apply(LoadModule(ClassType("sr_BoxesRunTime$")),
          Ident("boxToCharacter__C__jl_Character", _), List(arg)) =>
        Some((arg, "C"))
      case _ =>
        Some((tree, ()))
    }
  }

  private object MaybeUnbox {
    def unapply(tree: Tree): Some[(Tree, Any)] = tree match {
      case AsInstanceOf(arg, tpe) =>
        Some((arg, tpe))
      case Unbox(arg, charCode) =>
        Some((arg, charCode))
      case Apply(LoadModule(ClassType("sr_BoxesRunTime$")),
          Ident("unboxToChar__O__C", _), List(arg)) =>
        Some((arg, "C"))
      case _ =>
        Some((tree, ()))
    }
  }

  private def isTrivialConstructorStat(stat: Tree): Boolean = stat match {
    case This() =>
      true
    case ApplyStatically(This(), _, _, Nil) =>
      true
    case ApplyStatic(_, Ident(methodName, _), This() :: Nil) =>
      methodName.startsWith("$$init$__")
    case _ =>
      false
  }

  private object SimpleMethodBody {
    @tailrec
    def unapply(body: Tree): Boolean = body match {
      case New(_, _, args)                       => areSimpleArgs(args)
      case Apply(receiver, _, args)              => areSimpleArgs(receiver :: args)
      case ApplyStatically(receiver, _, _, args) => areSimpleArgs(receiver :: args)
      case ApplyStatic(_, _, args)               => areSimpleArgs(args)
      case Select(qual, _)                       => isSimpleArg(qual)
      case IsInstanceOf(inner, _)                => isSimpleArg(inner)

      case Block(List(inner, Undefined())) =>
        unapply(inner)

      case Unbox(inner, _)        => unapply(inner)
      case AsInstanceOf(inner, _) => unapply(inner)

      case _ => isSimpleArg(body)
    }

    private def areSimpleArgs(args: List[Tree]): Boolean =
      args.forall(isSimpleArg)

    @tailrec
    private def isSimpleArg(arg: Tree): Boolean = arg match {
      case New(_, _, Nil)                       => true
      case Apply(receiver, _, Nil)              => isTrivialArg(receiver)
      case ApplyStatically(receiver, _, _, Nil) => isTrivialArg(receiver)
      case ApplyStatic(_, _, Nil)               => true

      case ArrayLength(array)        => isTrivialArg(array)
      case ArraySelect(array, index) => isTrivialArg(array) && isTrivialArg(index)

      case Unbox(inner, _)        => isSimpleArg(inner)
      case AsInstanceOf(inner, _) => isSimpleArg(inner)

      case _ =>
        isTrivialArg(arg)
    }

    private def isTrivialArg(arg: Tree): Boolean = arg match {
      case _:VarRef | _:This | _:Literal | _:LoadModule =>
        true
      case _ =>
        false
    }
  }

  private object BlockOrAlone {
    def unapply(tree: Tree): Some[(List[Tree], Tree)] = Some(tree match {
      case Block(init :+ last) => (init, last)
      case _                   => (Nil, tree)
    })
  }

  private def exceptionMsg(myself: AbstractMethodID,
      attemptedInlining: List[AbstractMethodID], cause: Throwable) = {
    val buf = new StringBuilder()

    buf.append("The Scala.js optimizer crashed while optimizing " + myself +
        ": " + cause.toString)

    buf.append("\nMethods attempted to inline:\n")

    for (m <- attemptedInlining) {
      buf.append("* ")
      buf.append(m)
      buf.append('\n')
    }

    buf.toString
  }

  private class RollbackException(val trampolineId: Int,
      val savedUsedLocalNames: Map[String, Boolean],
      val savedUsedLabelNames: Set[String],
      val stateBackups: List[StateBackup],
      val cont: () => TailRec[Tree]) extends ControlThrowable

  class OptimizeException(val myself: AbstractMethodID,
      val attemptedInlining: List[AbstractMethodID], cause: Throwable
  ) extends Exception(exceptionMsg(myself, attemptedInlining, cause), cause)

}
