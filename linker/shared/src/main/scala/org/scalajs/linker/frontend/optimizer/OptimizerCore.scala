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

package org.scalajs.linker.frontend.optimizer

import scala.language.implicitConversions

import scala.annotation.{switch, tailrec}

import scala.collection.mutable

import scala.util.control.{NonFatal, ControlThrowable, TailCalls}
import scala.util.control.TailCalls.{done => _, _} // done is a too generic term

import org.scalajs.ir._
import Definitions.{ObjectClass, isConstructorName, isReflProxyName}
import Trees._
import Types._

import org.scalajs.logging._
import org.scalajs.linker._
import org.scalajs.linker.standard._
import org.scalajs.linker.backend.emitter.LongImpl
import org.scalajs.linker.backend.emitter.Transients.CallHelper

/** Optimizer core.
 *  Designed to be "mixed in" [[IncOptimizer#MethodImpl#Optimizer]].
 *  This is the core of the optimizer. It contains all the smart things the
 *  optimizer does. To perform inlining, it relies on abstract protected
 *  methods to identify the target of calls.
 */
private[optimizer] abstract class OptimizerCore(config: CommonPhaseConfig) {
  import OptimizerCore._

  type MethodID <: AbstractMethodID

  val myself: MethodID

  /** Returns the body of a method. */
  protected def getMethodBody(method: MethodID): MethodDef

  /** Returns the list of possible targets for a dynamically linked call. */
  protected def dynamicCall(intfName: String,
      methodName: String): List[MethodID]

  /** Returns the target of a static call. */
  protected def staticCall(className: String, namespace: MemberNamespace,
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

  private val localNameAllocator: FreshNameAllocator = new FreshNameAllocator

  /** An allocated local variable name is mutable iff it belongs to this set. */
  private var mutableLocalNames: Set[String] = Set.empty

  private val labelNameAllocator: FreshNameAllocator = new FreshNameAllocator

  /** A list of backups for all updates done to States so far (excluding
   *  those done in rolled back optimistic branches).
   *
   *  This list grows (from the head) every time the value of a `State` changes.
   *  Each time, a `StateBackup` is prepended with the previous value.
   *
   *  When starting an optimistic branch in `tryOrRollback`, we take a snapshot
   *  of the current chain of backups. When doing a rollback, we restore all
   *  the backups that have been added to the chain since the snapshot. We can
   *  do this by comparing the nodes of the chain with `eq`.
   *
   *  Manipulations of this list are amortized O(1). The act of modifying the
   *  value of a `State` "pays for" a) making the backup and b) restoring the
   *  backup. Indeed, a backup is restored at most once.
   */
  private var stateBackupChain: List[StateBackup] = Nil

  private var disableOptimisticOptimizations: Boolean = false
  private var rollbacksCount: Int = 0

  private val attemptedInlining = mutable.ListBuffer.empty[MethodID]

  private var curTrampolineId = 0

  private val useRuntimeLong = !config.coreSpec.esFeatures.allowBigIntsForLongs

  /** The record type for inlined `RuntimeLong`. */
  private lazy val inlinedRTLongRecordType =
    tryNewInlineableClass(LongImpl.RuntimeLongClass).map(_.tpe).get

  /** The name of the `lo` field of in the record type of `RuntimeLong`. */
  private lazy val inlinedRTLongLoField =
    inlinedRTLongRecordType.fields(0).name

  /** The name of the `lo` field of in the record type of `RuntimeLong`. */
  private lazy val inlinedRTLongHiField =
    inlinedRTLongRecordType.fields(1).name

  private val intrinsics =
    Intrinsics.buildIntrinsics(config.coreSpec.esFeatures)

  def optimize(thisType: Type, originalDef: MethodDef): MethodDef = {
    try {
      val MethodDef(static, name, params, resultType, optBody) = originalDef
      val body = optBody getOrElse {
        throw new AssertionError("Methods to optimize must be concrete")
      }

      val (newParams, newBody1) = try {
        transformIsolatedBody(Some(myself), thisType, params, resultType, body,
            Set.empty)
      } catch {
        case _: TooManyRollbacksException =>
          localNameAllocator.clear()
          mutableLocalNames = Set.empty
          labelNameAllocator.clear()
          stateBackupChain = Nil
          disableOptimisticOptimizations = true
          transformIsolatedBody(Some(myself), thisType, params, resultType,
              body, Set.empty)
      }
      val newBody =
        if (name.encodedName == "init___") tryElimStoreModule(newBody1)
        else newBody1
      MethodDef(static, name, newParams, resultType,
          Some(newBody))(originalDef.optimizerHints, None)(originalDef.pos)
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
            case Assign(Select(This(), _), _:Literal | _:VarRef) =>
              true
            case Assign(SelectStatic(_, _), _:Literal | _:VarRef) =>
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

  private def newSimpleState[A](initialValue: A): SimpleState[A] =
    new SimpleState[A](this, initialValue)

  private def addStateBackup(backup: StateBackup): Unit =
    stateBackupChain ::= backup

  private def freshLocalName(base: String, mutable: Boolean): String = {
    val result = localNameAllocator.freshName(base)
    if (mutable)
      mutableLocalNames += result
    result
  }

  private def freshLabelName(base: String): String =
    labelNameAllocator.freshName(base)

  // Just a helper to make the callsites more understandable
  private def localIsMutable(name: String): Boolean = mutableLocalNames(name)

  private def tryOrRollback(body: CancelFun => TailRec[Tree])(
      fallbackFun: () => TailRec[Tree]): TailRec[Tree] = {
    if (disableOptimisticOptimizations) {
      fallbackFun()
    } else {
      val trampolineId = curTrampolineId
      val localNameAllocatorSnapshot = localNameAllocator.snapshot()
      val savedMutableLocalNames = mutableLocalNames
      val labelNameAllocatorSnapshot = labelNameAllocator.snapshot()
      val savedStateBackupChain = stateBackupChain

      body { () =>
        throw new RollbackException(trampolineId, localNameAllocatorSnapshot,
            savedMutableLocalNames, labelNameAllocatorSnapshot,
            savedStateBackupChain, fallbackFun)
      }
    }
  }

  private def isSubclass(lhs: String, rhs: String): Boolean =
    getAncestorsOf(lhs).contains(rhs)

  private val isSubclassFun = isSubclass _

  private def isSubtype(lhs: Type, rhs: Type): Boolean = {
    Types.isSubtype(lhs, rhs)(isSubclassFun) || {
      (lhs, rhs) match {
        case (LongType | ClassType(Definitions.BoxedLongClass),
            ClassType(LongImpl.RuntimeLongClass)) =>
          true

        case (ClassType(LongImpl.RuntimeLongClass),
            ClassType(Definitions.BoxedLongClass)) =>
          true

        case _ =>
          false
      }
    }
  }

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
          pretransformLabeled(label, if (isStat) NoType else tpe, body, isStat,
              usePreTransform = false)(finishTransform(isStat))
        }

      case Assign(lhs, rhs) =>
        val cont = { (preTransLhs: PreTransform) =>
          resolveLocalDef(preTransLhs) match {
            case PreTransRecordTree(lhsTree, lhsOrigType, lhsCancelFun) =>
              val recordType = lhsTree.tpe.asInstanceOf[RecordType]

              def buildInner(trhs: PreTransform): TailRec[Tree] = {
                resolveLocalDef(trhs) match {
                  case PreTransRecordTree(rhsTree, rhsOrigType, rhsCancelFun) =>
                    if (rhsTree.tpe != recordType || rhsOrigType != lhsOrigType)
                      lhsCancelFun()
                    TailCalls.done(Assign(lhsTree, rhsTree))
                  case _ =>
                    lhsCancelFun()
                }
              }

              pretransformExpr(rhs) { trhs =>
                (trhs.tpe.base, lhsOrigType) match {
                  case (LongType, RefinedType(
                      ClassType(LongImpl.RuntimeLongClass), true, false)) =>
                    /* The lhs is a stack-allocated RuntimeLong, but the rhs is
                     * a primitive Long. We expand the primitive Long into a
                     * new stack-allocated RuntimeLong so that we do not need
                     * to cancel.
                     */
                    expandLongValue(trhs) { expandedRhs =>
                      buildInner(expandedRhs)
                    }

                  case _ =>
                    buildInner(trhs)
                }
              }

            case PreTransTree(lhsTree, _) =>
              TailCalls.done(Assign(lhsTree, transformExpr(rhs)))
          }
        }
        trampoline {
          lhs match {
            case lhs: Select =>
              pretransformSelectCommon(lhs, isLhsOfAssign = true)(cont)
            case lhs: JSSelect =>
              pretransformJSSelect(lhs, isLhsOfAssign = true)(cont)
            case _ =>
              pretransformExpr(lhs)(cont)
          }
        }

      case Return(expr, label) =>
        val info = scope.env.labelInfos(label.name)
        val newLabel = Ident(info.newName, None)
        if (!info.acceptRecords) {
          val newExpr = transformExpr(expr)
          info.returnedTypes.value ::= (newExpr.tpe, RefinedType(newExpr.tpe))
          Return(newExpr, newLabel)
        } else trampoline {
          pretransformNoLocalDef(expr) { texpr =>
            texpr match {
              case PreTransRecordTree(newExpr, origType, cancelFun) =>
                info.returnedTypes.value ::= (newExpr.tpe, origType)
                TailCalls.done(Return(newExpr, newLabel))
              case PreTransTree(newExpr, tpe) =>
                info.returnedTypes.value ::= (newExpr.tpe, tpe)
                TailCalls.done(Return(newExpr, newLabel))
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

      case While(cond, body) =>
        val newCond = transformExpr(cond)
        newCond match {
          case BooleanLiteral(false) => Skip()
          case _                     => While(newCond, transformStat(body))
        }

      case DoWhile(body, cond) =>
        val newBody = transformStat(body)
        val newCond = transformExpr(cond)
        newCond match {
          case BooleanLiteral(false) => newBody
          case _                     => DoWhile(newBody, newCond)
        }

      case ForIn(obj, keyVar @ Ident(name, originalName), body) =>
        val newObj = transformExpr(obj)
        val newName = freshLocalName(name, mutable = false)
        val newOriginalName = originalName.orElse(Some(name))
        val localDef = LocalDef(RefinedType(AnyType), mutable = false,
            ReplaceWithVarRef(newName, newOriginalName, newSimpleState(true), None))
        val newBody = {
          val bodyScope = scope.withEnv(scope.env.withLocalDef(name, localDef))
          transformStat(body)(bodyScope)
        }
        ForIn(newObj, Ident(newName, newOriginalName)(keyVar.pos), newBody)

      case TryCatch(block, errVar @ Ident(name, originalName), handler) =>
        val newBlock = transform(block, isStat)

        val newName = freshLocalName(name, false)
        val newOriginalName = originalName.orElse(Some(name))
        val localDef = LocalDef(RefinedType(AnyType), true,
            ReplaceWithVarRef(newName, newOriginalName, newSimpleState(true), None))
        val newHandler = {
          val handlerScope = scope.withEnv(scope.env.withLocalDef(name, localDef))
          transform(handler, isStat)(handlerScope)
        }

        val refinedType = constrainedLub(newBlock.tpe, newHandler.tpe, tree.tpe)
        TryCatch(newBlock, Ident(newName, newOriginalName)(errVar.pos),
            newHandler)(refinedType)

      case TryFinally(block, finalizer) =>
        val newBlock = transform(block, isStat)
        val newFinalizer = transformStat(finalizer)
        TryFinally(newBlock, newFinalizer)

      case Throw(expr) =>
        Throw(transformExpr(expr))

      case Match(selector, cases, default) =>
        val newSelector = transformExpr(selector)
        newSelector match {
          case IntLiteral(selectorValue) =>
            val body = cases.collectFirst {
              case (alts, body) if alts.exists(_.value == selectorValue) => body
            }.getOrElse(default)
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

      case tree: UnaryOp =>
        trampoline {
          pretransformUnaryOp(tree)(finishTransform(isStat))
        }

      case tree: BinaryOp =>
        trampoline {
          pretransformBinaryOp(tree)(finishTransform(isStat))
        }

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

      case IsInstanceOf(expr, ClassRef(ObjectClass)) =>
        transformExpr(BinaryOp(BinaryOp.!==, expr, Null()))

      case IsInstanceOf(expr, typeRef) =>
        trampoline {
          pretransformExpr(expr) { texpr =>
            val result = {
              val typeRefTpe = isAsInstanceOfTypeRefToType(typeRef)

              if (isSubtype(texpr.tpe.base, typeRefTpe)) {
                if (texpr.tpe.isNullable)
                  BinaryOp(BinaryOp.!==, finishTransformExpr(texpr), Null())
                else
                  Block(finishTransformStat(texpr), BooleanLiteral(true))
              } else {
                if (texpr.tpe.isExact)
                  Block(finishTransformStat(texpr), BooleanLiteral(false))
                else
                  IsInstanceOf(finishTransformExpr(texpr), typeRef)
              }
            }
            TailCalls.done(result)
          }
        }

      case AsInstanceOf(expr, ClassRef(ObjectClass)) =>
        transformExpr(expr)

      case AsInstanceOf(_, _) =>
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
            def constant(typeRef: TypeRef): TailRec[Tree] =
              TailCalls.done(Block(finishTransformStat(texpr), ClassOf(typeRef)))

            texpr.tpe match {
              case RefinedType(ClassType(LongImpl.RuntimeLongClass), true, false) =>
                constant(ClassRef(Definitions.BoxedLongClass))
              case RefinedType(ClassType(className), true, false) =>
                constant(ClassRef(className))
              case RefinedType(ArrayType(arrayTypeRef), true, false) =>
                constant(arrayTypeRef)
              case _ =>
                TailCalls.done(GetClass(finishTransformExpr(texpr)))
            }
          }
        }

      // JavaScript expressions

      case JSNew(ctor, args) =>
        JSNew(transformExpr(ctor), transformExprsOrSpreads(args))

      case JSPrivateSelect(qualifier, item) =>
        JSPrivateSelect(transformExpr(qualifier), item)

      case tree: JSSelect =>
        trampoline {
          pretransformJSSelect(tree, isLhsOfAssign = false)(
              finishTransform(isStat))
        }

      case tree: JSFunctionApply =>
        trampoline {
          pretransformJSFunctionApply(tree, isStat, usePreTransform = false)(
              finishTransform(isStat))
        }

      case JSMethodApply(receiver, method, args) =>
        JSMethodApply(transformExpr(receiver), transformExpr(method),
            transformExprsOrSpreads(args))

      case JSSuperSelect(superClass, qualifier, item) =>
        JSSuperSelect(transformExpr(superClass), transformExpr(qualifier),
            transformExpr(item))

      case JSSuperMethodCall(superClass, receiver, method, args) =>
        JSSuperMethodCall(transformExpr(superClass), transformExpr(receiver),
            transformExpr(method), transformExprsOrSpreads(args))

      case JSSuperConstructorCall(args) =>
        JSSuperConstructorCall(transformExprsOrSpreads(args))

      case JSImportCall(arg) =>
        JSImportCall(transformExpr(arg))

      case JSDelete(qualifier, item) =>
        JSDelete(transformExpr(qualifier), transformExpr(item))

      case JSUnaryOp(op, lhs) =>
        JSUnaryOp(op, transformExpr(lhs))

      case JSBinaryOp(op, lhs, rhs) =>
        JSBinaryOp(op, transformExpr(lhs), transformExpr(rhs))

      case JSArrayConstr(items) =>
        JSArrayConstr(transformExprsOrSpreads(items))

      case JSObjectConstr(fields) =>
        JSObjectConstr(fields map {
          case (name, value) =>
            val newName = name match {
              case _:StringLiteral | _:Ident =>
                name
              case ComputedName(nameExpr, logicalName) =>
                transformExpr(nameExpr) match {
                  case newName: StringLiteral =>
                    newName
                  case newName =>
                    ComputedName(newName, logicalName)
                }
            }
            (newName, transformExpr(value))
        })

      // Atomic expressions

      case _:VarRef | _:This =>
        trampoline {
          pretransformExpr(tree)(finishTransform(isStat))
        }

      case Closure(arrow, captureParams, params, body, captureValues) =>
        transformClosureCommon(arrow, captureParams, params, body,
            captureValues.map(transformExpr))

      case CreateJSClass(cls, captureValues) =>
        CreateJSClass(cls, captureValues.map(transformExpr))

      // Trees that need not be transformed

      case _:Skip | _:Debugger | _:LoadModule | _:SelectStatic |
          _:LoadJSConstructor | _:LoadJSModule | _:JSLinkingInfo | _:JSGlobalRef |
          _:Literal =>
        tree

      case _ =>
        throw new IllegalArgumentException(
            s"Invalid tree in transform of class ${tree.getClass.getName}: $tree")
    }

    if (isStat) keepOnlySideEffects(result)
    else result
  }

  private def transformClosureCommon(arrow: Boolean,
      captureParams: List[ParamDef], params: List[ParamDef], body: Tree,
      newCaptureValues: List[Tree])(
      implicit scope: Scope, pos: Position): Closure = {

    val thisType = if (arrow) NoType else AnyType
    val (allNewParams, newBody) = transformIsolatedBody(None, thisType,
        captureParams ++ params, AnyType, body, scope.implsBeingInlined)
    val (newCaptureParams, newParams) =
      allNewParams.splitAt(captureParams.size)

    Closure(arrow, newCaptureParams, newParams, newBody, newCaptureValues)
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
        val localDef = scope.env.localDefs.getOrElse(name, {
          throw new AssertionError(
              s"Cannot find local def '$name' at $pos\n" +
              s"While optimizing $myself\n" +
              s"Env is ${scope.env}\n" +
              s"Inlining ${scope.implsBeingInlined}")
        })
        cont(localDef.toPreTransform)

      case This() =>
        val localDef = scope.env.localDefs.getOrElse("this", {
          throw new AssertionError(
              s"Found invalid 'this' at $pos\n" +
              s"While optimizing $myself\n" +
              s"Env is ${scope.env}\n" +
              s"Inlining ${scope.implsBeingInlined}")
        })
        cont(localDef.toPreTransform)

      case tree: If =>
        pretransformIf(tree)(cont)

      case Match(selector, cases, default) =>
        val newSelector = transformExpr(selector)
        newSelector match {
          case IntLiteral(selectorValue) =>
            val body = cases.collectFirst {
              case (alts, body) if alts.exists(_.value == selectorValue) => body
            }.getOrElse(default)
            pretransformExpr(body)(cont)
          case _ =>
            cont(Match(newSelector,
                cases map (c => (c._1, transformExpr(c._2))),
                transformExpr(default))(tree.tpe).toPreTransform)
        }

      case Labeled(ident @ Ident(label, _), tpe, body) =>
        pretransformLabeled(label, tpe, body, isStat = false,
            usePreTransform = true)(cont)

      case New(cls, ctor, args) =>
        pretransformExprs(args) { targs =>
          pretransformNew(AllocationSite.Tree(tree), cls, ctor, targs)(cont)
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

      case tree: UnaryOp =>
        pretransformUnaryOp(tree)(cont)

      case tree: BinaryOp =>
        pretransformBinaryOp(tree)(cont)

      case tree: JSSelect =>
        pretransformJSSelect(tree, isLhsOfAssign = false)(cont)

      case tree: JSFunctionApply =>
        pretransformJSFunctionApply(tree, isStat = false,
            usePreTransform = true)(cont)

      case JSArrayConstr(items) =>
        /* Trying to virtualize more than 64 items in a JS array is probably
         * a bad idea, and will slow down the optimizer for no good reason.
         * See for example #2943.
         */
        if (items.size > 64 || items.exists(_.isInstanceOf[JSSpread])) {
          /* TODO This means spread in array constr does not compose under
           * this optimization. We could improve this with a
           * pretransformExprsOrSpreads() or something like that.
           */
          cont(JSArrayConstr(transformExprsOrSpreads(items)).toPreTransform)
        } else {
          val itemsNoSpread = items.asInstanceOf[List[Tree]]

          pretransformExprs(itemsNoSpread) { titems =>
            tryOrRollback { cancelFun =>
              val itemBindings = for {
                (titem, index) <- titems.zipWithIndex
              } yield {
                Binding("x" + index, None, AnyType, mutable = false, titem)
              }
              withNewLocalDefs(itemBindings) { (itemLocalDefs, cont1) =>
                val replacement = InlineJSArrayReplacement(
                    itemLocalDefs.toVector, cancelFun)
                val localDef = LocalDef(
                    RefinedType(AnyType, isExact = false, isNullable = false),
                    mutable = false,
                    replacement)
                cont1(localDef.toPreTransform)
              } (cont)
            } { () =>
              cont(PreTransTree(JSArrayConstr(titems.map(finishTransformExpr)),
                  RefinedType(AnyType, isExact = false, isNullable = false)))
            }
          }
        }

      case AsInstanceOf(expr, typeRef) =>
        pretransformExpr(expr) { texpr =>
          typeRef match {
            case ClassRef(ObjectClass) =>
              cont(texpr)
            case _ =>
              val typeRefTpe = isAsInstanceOfTypeRefToType(typeRef)
              if (isSubtype(texpr.tpe.base, typeRefTpe)) {
                cont(texpr)
              } else {
                cont(AsInstanceOf(finishTransformExpr(texpr), typeRef).toPreTransform)
              }
          }
        }

      case Closure(arrow, captureParams, params, body, captureValues) =>
        pretransformExprs(captureValues) { tcaptureValues =>
          def default(): TailRec[Tree] = {
            val newClosure = transformClosureCommon(arrow, captureParams,
                params, body, tcaptureValues.map(finishTransformExpr))
            cont(PreTransTree(
                newClosure,
                RefinedType(AnyType, isExact = false, isNullable = false)))
          }

          if (!arrow && params.exists(_.rest)) {
            /* TentativeClosureReplacement assumes there are no rest
             * parameters, because that would not be inlineable anyway.
             * Likewise, it assumes that there is no binding for `this`, which
             * is only true for arrow functions.
             * So we never try to inline non-arrow Closures, nor Closures with
             * a rest parameter. There are few use cases for either anyway.
             */
            default()
          } else {
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
                cont1(localDef.toPreTransform)
              } (cont)
            } { () =>
              default()
            }
          }
        }

      case _ =>
        cont(transformExpr(tree).toPreTransform)
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
                cont(PreTransBlock(transformedStat, trest))
              }
            }
        }

      case Nil => // silence the exhaustivity warning in a sensible way
        TailCalls.done(Skip()(tree.pos))
    }
    pretransformList(tree.stats)(cont)(scope)
  }

  private def pretransformIf(tree: If)(cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    implicit val pos = tree.pos
    val If(cond, thenp, elsep) = tree

    val newCond = transformExpr(cond)
    newCond match {
      case BooleanLiteral(condValue) =>
        if (condValue)
          pretransformExpr(thenp)(cont)
        else
          pretransformExpr(elsep)(cont)

      case _ =>
        tryOrRollback { cancelFun =>
          pretransformExprs(thenp, elsep) { (tthenp, telsep) =>
            if (tthenp.tpe.isNothingType) {
              cont(PreTransBlock(
                  If(newCond, finishTransformStat(tthenp), Skip())(NoType),
                  telsep))
            } else if (telsep.tpe.isNothingType) {
              val negCond = finishTransformExpr(
                  foldUnaryOp(UnaryOp.Boolean_!, newCond.toPreTransform))
              cont(PreTransBlock(
                  If(negCond, finishTransformStat(telsep), Skip())(NoType),
                  tthenp))
            } else {
              (resolveLocalDef(tthenp), resolveLocalDef(telsep)) match {
                case (PreTransRecordTree(thenTree, thenOrigType, thenCancelFun),
                    PreTransRecordTree(elseTree, elseOrigType, elseCancelFun)) =>
                  val commonType = {
                    if (thenTree.tpe == elseTree.tpe && thenOrigType == elseOrigType)
                      thenTree.tpe
                    else
                      cancelFun()
                  }
                  val refinedOrigType =
                    constrainedLub(thenOrigType, elseOrigType, tree.tpe)
                  cont(PreTransRecordTree(
                      If(newCond, thenTree, elseTree)(commonType),
                      refinedOrigType,
                      cancelFun))

                case (tthenpNoLocalDef, telsepNoLocalDef) =>
                  val newThenp = finishTransformExpr(tthenpNoLocalDef)
                  val newElsep = finishTransformExpr(telsepNoLocalDef)
                  val refinedType =
                    constrainedLub(newThenp.tpe, newElsep.tpe, tree.tpe)
                  cont(foldIf(newCond, newThenp, newElsep)(
                      refinedType).toPreTransform)
              }
            }
          }
        } { () =>
          val newThenp = transformExpr(thenp)
          val newElsep = transformExpr(elsep)
          val refinedType =
            constrainedLub(newThenp.tpe, newElsep.tpe, tree.tpe)
          cont(foldIf(newCond, newThenp, newElsep)(
              refinedType).toPreTransform)
        }
    }
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
          cont(fieldLocalDef.toPreTransform)
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
          cont(fieldLocalDef.toPreTransform)
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

      // Select the lo or hi "field" of a Long literal
      case PreTransLit(LongLiteral(value)) if useRuntimeLong =>
        val itemName = item.name
        assert(itemName == inlinedRTLongLoField ||
            itemName == inlinedRTLongHiField)
        assert(expectedType == IntType)
        val resultValue =
          if (itemName == inlinedRTLongLoField) value.toInt
          else (value >>> 32).toInt
        cont(PreTransLit(IntLiteral(resultValue)))

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

  private def pretransformNew(allocationSite: AllocationSite, cls: ClassRef,
      ctor: Ident, targs: List[PreTransform])(cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    val className = cls.className

    tryNewInlineableClass(className) match {
      case Some(initialValue) =>
        tryOrRollback { cancelFun =>
          inlineClassConstructor(allocationSite, cls, initialValue,
              ctor, targs, cancelFun)(cont)
        } { () =>
          cont(PreTransTree(
              New(cls, ctor, targs.map(finishTransformExpr)),
              RefinedType(ClassType(className), isExact = true, isNullable = false)))
        }
      case None =>
        cont(PreTransTree(
            New(cls, ctor, targs.map(finishTransformExpr)),
            RefinedType(ClassType(className), isExact = true, isNullable = false)))
    }
  }

  /** Resolves any LocalDef in a [[PreTransform]]. */
  private def resolveLocalDef(preTrans: PreTransform): PreTransGenTree = {
    implicit val pos = preTrans.pos
    preTrans match {
      case PreTransBlock(bindingsAndStats, result) =>
        resolveLocalDef(result) match {
          case PreTransRecordTree(tree, tpe, cancelFun) =>
            PreTransRecordTree(finishTransformBindings(bindingsAndStats, tree),
                tpe, cancelFun)
          case PreTransTree(tree, tpe) =>
            PreTransTree(finishTransformBindings(bindingsAndStats, tree), tpe)
        }

      case _:PreTransUnaryOp | _:PreTransBinaryOp =>
        PreTransTree(finishTransformExpr(preTrans))

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

  /** Resolves any [[RecordType]] in a [[PreTransform]].
   *
   *  If `preTrans` would resolve to a `PreTransRecordTree`, returns a `Some`
   *  of its (lowered) [[RecordType]] and its `cancelFun`. Otherwise, returns
   *  `None`.
   *
   *  Note that the record type is not the same as `preTrans.tpe.base`, which
   *  is the *original* type of the tree (not lowered to a record type).
   */
  private def resolveRecordType(
      preTrans: PreTransform): Option[(RecordType, CancelFun)] = {
    preTrans match {
      case PreTransBlock(_, result) =>
        resolveRecordType(result)

      case _:PreTransUnaryOp | _:PreTransBinaryOp =>
        None

      case PreTransLocalDef(localDef @ LocalDef(tpe, _, replacement)) =>
        replacement match {
          case ReplaceWithRecordVarRef(name, originalName,
              recordType, used, cancelFun) =>
            Some((recordType, cancelFun))

          case InlineClassInstanceReplacement(recordType, fieldLocalDefs, cancelFun) =>
            Some((recordType, cancelFun))

          case _ =>
            None
        }

      case PreTransRecordTree(tree, _, cancelFun) =>
        Some((tree.tpe.asInstanceOf[RecordType], cancelFun))

      case PreTransTree(_, _) =>
        None
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
      case PreTransBlock(bindingsAndStats, result) =>
        finishTransformBindings(bindingsAndStats, finishTransformExpr(result))
      case PreTransUnaryOp(op, lhs) =>
        UnaryOp(op, finishTransformExpr(lhs))
      case PreTransBinaryOp(op, lhs, rhs) =>
        BinaryOp(op, finishTransformExpr(lhs), finishTransformExpr(rhs))
      case PreTransLocalDef(localDef) =>
        localDef.newReplacement

      /* In general, it is not OK to allocate a new instance of an inlined
       * class from its record value, because that can break object identity
       * (not to mention we have no idea what the primary constructor does).
       * However, for RuntimeLong specifically, it is OK. It is useful to do
       * so because it allows us not to cancel the original stack allocation
       * of the Long value, which means that all previous usages of it can
       * stay on stack.
       *
       * We do something similar in LocalDef.newReplacement.
       */
      case PreTransRecordTree(tree, tpe, _)
          if tpe.base == ClassType(LongImpl.RuntimeLongClass) =>
        tree match {
          case RecordValue(_, List(lo, hi)) =>
            createNewLong(lo, hi)
          case recordVarRef: VarRef =>
            createNewLong(recordVarRef)
          case _ =>
            val varRefIdent = Ident(freshLocalName("x", mutable = false))
            val recordVarDef =
              VarDef(varRefIdent, tree.tpe, mutable = false, tree)
            Block(recordVarDef, createNewLong(recordVarDef.ref))
        }

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
    case PreTransBlock(bindingsAndStats, result) =>
      finishTransformBindings(bindingsAndStats, finishTransformStat(result))
    case PreTransUnaryOp(_, lhs) =>
      finishTransformStat(lhs)

    case PreTransBinaryOp(op, lhs, rhs) =>
      // Here we need to preserve the side-effects of integer division/modulo
      import BinaryOp._

      val newLhs = finishTransformStat(lhs)

      def finishNoSideEffects: Tree =
        Block(newLhs, finishTransformStat(rhs))(stat.pos)

      op match {
        case Int_/ | Int_% =>
          rhs match {
            case PreTransLit(IntLiteral(r)) if r != 0 =>
              finishNoSideEffects
            case _ =>
              Block(newLhs, BinaryOp(op, IntLiteral(0)(stat.pos),
                  finishTransformExpr(rhs))(stat.pos))(stat.pos)
          }
        case Long_/ | Long_% =>
          rhs match {
            case PreTransLit(LongLiteral(r)) if r != 0L =>
              finishNoSideEffects
            case _ =>
              Block(newLhs, BinaryOp(op, LongLiteral(0L)(stat.pos),
                  finishTransformExpr(rhs))(stat.pos))(stat.pos)
          }
        case _ =>
          finishNoSideEffects
      }

    case PreTransLocalDef(_) =>
      Skip()(stat.pos)
    case PreTransRecordTree(tree, _, _) =>
      keepOnlySideEffects(tree)
    case PreTransTree(tree, _) =>
      keepOnlySideEffects(tree)
  }

  /** Finishes the bindings and statements followed by a result to get a
   *  normal [[Tree]].
   *  This method must not be called more than once per `BindingOrStat` and
   *  per translation.
   *  By "per translation", we mean in an alternative path through
   *  `tryOrRollback`. It could still be called several times as long as
   *  it is once in the 'try' part and once in the 'fallback' part.
   */
  private def finishTransformBindings(bindingsAndStats: List[BindingOrStat],
      result: Tree): Tree = {
    bindingsAndStats.foldRight(result) {
      case (Left(PreTransBinding(localDef, value)), innerBody) =>
        implicit val pos = value.pos

        val LocalDef(tpe, mutable, replacement) = localDef

        val (name, originalName, used) = (replacement: @unchecked) match {
          case ReplaceWithVarRef(name, originalName, used, _) =>
            (name, originalName, used)
          case ReplaceWithRecordVarRef(name, originalName, _, used, _) =>
            (name, originalName, used)
        }

        if (used.value) {
          val ident = Ident(name, originalName)
          val varDef = resolveLocalDef(value) match {
            case PreTransRecordTree(valueTree, valueTpe, cancelFun) =>
              val recordType = valueTree.tpe.asInstanceOf[RecordType]
              if (!isImmutableType(recordType))
                cancelFun()
              VarDef(ident, recordType, mutable, valueTree)

            case PreTransTree(valueTree, valueTpe) =>
              VarDef(ident, tpe.base, mutable, valueTree)
          }

          Block(varDef, innerBody)
        } else {
          val valueSideEffects = finishTransformStat(value)
          Block(valueSideEffects, innerBody)
        }

      case (Right(stat), innerBody) =>
        Block(stat, innerBody)(innerBody.pos)
    }
  }

  /** Keeps only the side effects of a Tree (overapproximation). */
  private def keepOnlySideEffects(stat: Tree): Tree = stat match {
    case _:VarRef | _:This | _:Literal | _:SelectStatic =>
      Skip()(stat.pos)
    case VarDef(_, _, _, rhs) =>
      keepOnlySideEffects(rhs)
    case Block(init :+ last) =>
      keepOnlySideEffects(last) match {
        case Skip()      => keepOnlySideEffects(Block(init)(stat.pos))
        case lastEffects => Block(init :+ lastEffects)(stat.pos)
      }
    case LoadModule(ClassRef(moduleClassName)) =>
      if (hasElidableModuleAccessor(moduleClassName)) Skip()(stat.pos)
      else stat
    case NewArray(_, lengths) =>
      Block(lengths.map(keepOnlySideEffects))(stat.pos)
    case Select(qualifier, _) =>
      keepOnlySideEffects(qualifier)
    case Closure(_, _, _, _, captureValues) =>
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
    val Apply(flags, receiver, methodIdent, args) = tree
    implicit val pos = tree.pos

    pretransformExprs(receiver, args) { (treceiver, targs) =>
      pretransformApply(flags, treceiver, methodIdent, targs, tree.tpe, isStat,
          usePreTransform)(cont)
    }
  }

  private def pretransformApply(flags: ApplyFlags, treceiver: PreTransform,
      methodIdent: Ident, targs: List[PreTransform], resultType: Type,
      isStat: Boolean, usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    val methodName = methodIdent.name

    def treeNotInlined = {
      cont(PreTransTree(Apply(flags, finishTransformExpr(treceiver), methodIdent,
          targs.map(finishTransformExpr))(resultType), RefinedType(resultType)))
    }

    treceiver.tpe.base match {
      case NothingType =>
        cont(treceiver)
      case NullType =>
        cont(Block(
            finishTransformStat(treceiver),
            Throw(New(ClassRef("jl_NullPointerException"),
                Ident("init___", Some("<init>")), Nil))).toPreTransform)
      case _ =>
        if (isReflProxyName(methodName)) {
          // Never inline reflective proxies
          treeNotInlined
        } else {
          val cls = boxedClassForType(treceiver.tpe.base)
          val namespace = MemberNamespace.forNonStaticCall(flags)
          val impls =
            if (treceiver.tpe.isExact) staticCall(cls, namespace, methodName).toList
            else dynamicCall(cls, methodName)
          val allocationSites =
            (treceiver :: targs).map(_.tpe.allocationSite)
          if (impls.isEmpty || impls.exists(impl =>
              scope.implsBeingInlined((allocationSites, impl)))) {
            // isEmpty could happen, have to leave it as is for the TypeError
            treeNotInlined
          } else if (impls.size == 1) {
            val target = impls.head
            val intrinsicCode = intrinsics(flags, target)
            if (intrinsicCode >= 0) {
              callIntrinsic(intrinsicCode, flags, Some(treceiver), targs,
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
            if (canMultiInline(impls)) {
              inline(allocationSites, Some(treceiver), targs, impls.head,
                  isStat, usePreTransform)(cont)
            } else {
              treeNotInlined
            }
          }
        }
    }
  }

  private def canMultiInline(impls: List[MethodID]): Boolean = {
    // TODO? Inline multiple non-forwarders with the exact same body?
    impls.forall(impl => impl.isForwarder && impl.inlineable) &&
    (getMethodBody(impls.head).body.get match {
      // Trait impl forwarder
      case ApplyStatic(flags, ClassRef(staticCls), Ident(methodName, _), _) =>
        impls.tail.forall(getMethodBody(_).body.get match {
          case ApplyStatic(`flags`, ClassRef(`staticCls`), Ident(`methodName`, _), _) =>
            true
          case _ =>
            false
        })

      // Shape of forwards to default methods
      case ApplyStatically(flags, This(), cls, Ident(methodName, _), args) =>
        impls.tail.forall(getMethodBody(_).body.get match {
          case ApplyStatically(`flags`, This(), `cls`, Ident(`methodName`, _), _) =>
            true
          case _ =>
            false
        })

      // Bridge method
      case Apply(flags, This(), Ident(methodName, _), referenceArgs) =>
        impls.tail.forall(getMethodBody(_).body.get match {
          case Apply(`flags`, This(), Ident(`methodName`, _), implArgs) =>
            referenceArgs.zip(implArgs) forall {
              case (MaybeUnbox(_, unboxID1), MaybeUnbox(_, unboxID2)) =>
                unboxID1 == unboxID2
            }
          case _ =>
            false
        })

      case body =>
        throw new AssertionError("Invalid forwarder shape: " + body)
    })
  }

  private def boxedClassForType(tpe: Type): String = (tpe: @unchecked) match {
    case ClassType(cls) =>
      if (cls == Definitions.BoxedLongClass && useRuntimeLong)
        LongImpl.RuntimeLongClass
      else
        cls

    case AnyType      => Definitions.ObjectClass
    case UndefType    => Definitions.BoxedUnitClass
    case BooleanType  => Definitions.BoxedBooleanClass
    case CharType     => Definitions.BoxedCharacterClass
    case ByteType     => Definitions.BoxedByteClass
    case ShortType    => Definitions.BoxedShortClass
    case IntType      => Definitions.BoxedIntegerClass
    case LongType     =>
      if (useRuntimeLong) LongImpl.RuntimeLongClass
      else Definitions.BoxedLongClass
    case FloatType    => Definitions.BoxedFloatClass
    case DoubleType   => Definitions.BoxedDoubleClass
    case StringType   => Definitions.BoxedStringClass
    case ArrayType(_) => Definitions.ObjectClass
  }

  private def pretransformStaticApply(tree: ApplyStatically, isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    val ApplyStatically(flags, receiver, clsRef @ ClassRef(cls),
        methodIdent @ Ident(methodName, _), args) = tree
    implicit val pos = tree.pos

    def treeNotInlined0(transformedReceiver: Tree, transformedArgs: List[Tree]) =
      cont(PreTransTree(ApplyStatically(flags, transformedReceiver, clsRef,
          methodIdent, transformedArgs)(tree.tpe), RefinedType(tree.tpe)))

    def treeNotInlined =
      treeNotInlined0(transformExpr(receiver), args.map(transformExpr))

    if (isReflProxyName(methodName)) {
      // Never inline reflective proxies
      treeNotInlined
    } else {
      val optTarget = staticCall(cls, MemberNamespace.forNonStaticCall(flags),
          methodName)
      if (optTarget.isEmpty) {
        // just in case
        treeNotInlined
      } else {
        val target = optTarget.get
        pretransformExprs(receiver, args) { (treceiver, targs) =>
          val intrinsicCode = intrinsics(flags, target)
          if (intrinsicCode >= 0) {
            callIntrinsic(intrinsicCode, flags, Some(treceiver), targs,
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
    val ApplyStatic(flags, classRef @ ClassRef(cls),
        methodIdent @ Ident(methodName, _), args) = tree
    implicit val pos = tree.pos

    def treeNotInlined0(transformedArgs: List[Tree]) =
      cont(PreTransTree(ApplyStatic(flags, classRef, methodIdent,
          transformedArgs)(tree.tpe), RefinedType(tree.tpe)))

    def treeNotInlined = treeNotInlined0(args.map(transformExpr))

    val optTarget = staticCall(cls, MemberNamespace.forStaticCall(flags),
        methodName)
    if (optTarget.isEmpty) {
      // just in case
      treeNotInlined
    } else {
      val target = optTarget.get
      pretransformExprs(args) { targs =>
        val intrinsicCode = intrinsics(flags, target)
        if (intrinsicCode >= 0) {
          callIntrinsic(intrinsicCode, flags, None, targs,
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

  private def pretransformJSSelect(tree: JSSelect, isLhsOfAssign: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {

    val JSSelect(qual, item) = tree
    implicit val pos = tree.pos

    pretransformExprs(qual, item) { (tqual, titem0) =>
      val titem = optimizeJSBracketSelectItem(titem0)

      def default: TailRec[Tree] = {
        cont(PreTransTree(foldJSSelect(finishTransformExpr(tqual),
            finishTransformExpr(titem))))
      }

      titem match {
        case _ if isLhsOfAssign =>
          default

        case PreTransLit(itemLit) =>
          itemLit match {
            case IntLiteral(itemInt) =>
              tqual match {
                case PreTransLocalDef(LocalDef(_, false,
                    InlineJSArrayReplacement(itemLocalDefs, _))) =>
                  if (itemInt >= 0 && itemInt < itemLocalDefs.size)
                    cont(itemLocalDefs(itemInt).toPreTransform)
                  else
                    cont(PreTransLit(Undefined()))

                case _ =>
                  default
              }

            case StringLiteral("length") =>
              tqual match {
                case PreTransLocalDef(LocalDef(_, false,
                    InlineJSArrayReplacement(itemLocalDefs, _))) =>
                  cont(PreTransLit(IntLiteral(itemLocalDefs.size)))

                case _ =>
                  default
              }

            case _ =>
              default
          }

        case _ =>
          default
      }
    }
  }

  private def optimizeJSBracketSelectItem(item: PreTransform): PreTransform = {
    item match {
      case PreTransLit(StringLiteral(s)) =>
        scala.util.Try(s.toInt).toOption match {
          case Some(intValue) if intValue.toString == s =>
            PreTransLit(IntLiteral(intValue)(item.pos))
          case _ =>
            item
        }
      case _ =>
        item
    }
  }

  private def pretransformJSFunctionApply(tree: JSFunctionApply,
      isStat: Boolean, usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    val JSFunctionApply(fun, args) = tree
    implicit val pos = tree.pos

    if (args.exists(_.isInstanceOf[JSSpread])) {
      cont(JSFunctionApply(transformExpr(fun),
          transformExprsOrSpreads(args)).toPreTransform)
    } else {
      val argsNoSpread = args.asInstanceOf[List[Tree]]

      pretransformExpr(fun) { tfun =>
        tfun match {
          case PreTransLocalDef(LocalDef(_, false,
              closure @ TentativeClosureReplacement(
                  captureParams, params, body, captureLocalDefs,
                  alreadyUsed, cancelFun))) if !alreadyUsed.value =>
            alreadyUsed.value = true
            pretransformExprs(argsNoSpread) { targs =>
              inlineBody(
                  Some(PreTransLit(Undefined())), // `this` is `undefined`
                  captureParams ++ params, AnyType, body,
                  captureLocalDefs.map(_.toPreTransform) ++ targs, isStat,
                  usePreTransform)(cont)
            }

          case _ =>
            cont(JSFunctionApply(finishTransformExpr(tfun),
                argsNoSpread.map(transformExpr)).toPreTransform)
        }
      }
    }
  }

  private def transformExprsOrSpreads(trees: List[TreeOrJSSpread])(
      implicit scope: Scope): List[TreeOrJSSpread] = {

    /* This is basically a flatMap, but we do it manually because flatMap would
     * generate many garbage intermediate lists, when in fact the case JSSpread
     * should be fairly rare. In general, we avoid flatMaps over collections in
     * OptimizerCore.
     */

    val builder = List.newBuilder[TreeOrJSSpread]

    trees.foreach {
      case spread: JSSpread =>
        implicit val pos = spread.pos

        val newSpreadItems = trampoline {
          pretransformExpr(spread.items) { tspreadItems =>
            TailCalls.done {
              tspreadItems match {
                case PreTransLocalDef(LocalDef(_, false,
                    InlineJSArrayReplacement(itemLocalDefs, _))) =>
                  JSArrayConstr(
                      itemLocalDefs.toList.map(_.newReplacement(spread.pos)))

                case _ =>
                  finishTransformExpr(tspreadItems)
              }
            }
          }
        }

        newSpreadItems match {
          case JSArrayConstr(newFirsts) => builder ++= newFirsts
          case _                        => builder += JSSpread(newSpreadItems)
        }

      case tree: Tree =>
        builder += transformExpr(tree)
    }

    builder.result()
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

    def isLocalOnlyInlineType(tpe: RefinedType): Boolean = {
      /* RuntimeLong is @inline so that *local* box/unbox pairs and instances
       * can be eliminated. But we don't want that to force inlining of a
       * method only because we pass it an instance of RuntimeLong.
       */
      tpe.base match {
        case ClassType(LongImpl.RuntimeLongClass)       => true
        case _                                          => false
      }
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
        }) && !isLocalOnlyInlineType(localDef.tpe)

      case PreTransRecordTree(_, _, _) =>
        !isLocalOnlyInlineType(arg.tpe)

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

  private def inline(allocationSites: List[AllocationSite],
      optReceiver: Option[PreTransform],
      args: List[PreTransform], target: MethodID, isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    require(target.inlineable)

    attemptedInlining += target

    val MethodDef(flags, _, formals, resultType, optBody) = getMethodBody(target)
    assert(flags.namespace.isStatic == optReceiver.isEmpty,
        "There must be receiver if and only if the method is not static")
    val body = optBody.getOrElse {
      throw new AssertionError("A method to inline must be conrete")
    }

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
      implicit val scope = bodyScope
      if (usePreTransform) {
        assert(!isStat, "Cannot use pretransform in statement position")
        pretransformExpr(body)(cont1)
      } else {
        cont1(PreTransTree(transform(body, isStat)))
      }
    } (cont) (scope.withEnv(OptEnv.Empty))
  }

  private def callIntrinsic(code: Int, flags: ApplyFlags,
      optTReceiver: Option[PreTransform], targs: List[PreTransform],
      isStat: Boolean, usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    import Intrinsics._

    implicit def string2ident(s: String): Ident = Ident(s, None)

    lazy val newReceiver = finishTransformExpr(optTReceiver.get)
    lazy val newArgs = targs.map(finishTransformExpr)

    @inline def contTree(result: Tree) = cont(result.toPreTransform)

    @inline def StringClassType = ClassType(Definitions.BoxedStringClass)

    def defaultApply(methodName: String, resultType: Type): TailRec[Tree] =
      contTree(Apply(flags, newReceiver, Ident(methodName), newArgs)(resultType))

    def cursoryArrayElemType(tpe: ArrayType): Type = {
      if (tpe.arrayTypeRef.dimensions != 1) AnyType
      else (tpe.arrayTypeRef.baseClassName match {
        case "Z" => BooleanType
        case "C" => CharType
        case "B" => ByteType
        case "S" => ShortType
        case "I" => IntType
        case "F" => FloatType
        case "D" => DoubleType
        case _   => AnyType
      })
    }

    (code: @switch) match {
      // java.lang.System

      case ArrayCopy =>
        assert(isStat, "System.arraycopy must be used in statement position")
        contTree(callHelper("systemArraycopy", newArgs)(NoType))
      case IdentityHashCode =>
        contTree(callHelper("systemIdentityHashCode", newArgs)(IntType))

      // scala.runtime.ScalaRunTime object

      case ArrayApply =>
        val List(array, index) = newArgs
        array.tpe match {
          case arrayTpe @ ArrayType(ArrayTypeRef(base, _)) =>
            val elemType = cursoryArrayElemType(arrayTpe)
            contTree(ArraySelect(array, index)(elemType))

          case _ =>
            defaultApply("array$undapply__O__I__O", AnyType)
        }

      case ArrayUpdate =>
        val List(tarray, tindex, tvalue) = targs
        tarray.tpe.base match {
          case arrayTpe @ ArrayType(ArrayTypeRef(base, depth)) =>
            val array = finishTransformExpr(tarray)
            val index = finishTransformExpr(tindex)
            val elemType = cursoryArrayElemType(arrayTpe)
            val select = ArraySelect(array, index)(elemType)
            val cont1: PreTransCont = { tunboxedValue =>
              contTree(Assign(select, finishTransformExpr(tunboxedValue)))
            }
            base match {
              case "Z" | "C" | "B" | "S" | "I" | "L" | "F" | "D" if depth == 1 =>
                foldUnbox(tvalue, base.charAt(0))(cont1)
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

      // java.lang.Integer

      case IntegerNLZ =>
        contTree(newArgs.head match {
          case IntLiteral(value) => IntLiteral(Integer.numberOfLeadingZeros(value))
          case newArg            => callHelper("clz32", newArg)(IntType)
        })

      // java.lang.Long

      case LongToString =>
        pretransformApply(ApplyFlags.empty, targs.head, Ident("toString__T"),
            Nil, StringClassType, isStat, usePreTransform)(
            cont)
      case LongCompare =>
        pretransformApply(ApplyFlags.empty, targs.head,
            Ident("compareTo__sjsr_RuntimeLong__I"), targs.tail, IntType,
            isStat, usePreTransform)(
            cont)
      case LongDivideUnsigned =>
        pretransformApply(ApplyFlags.empty, targs.head,
            Ident(LongImpl.divideUnsigned), targs.tail,
            ClassType(LongImpl.RuntimeLongClass), isStat, usePreTransform)(
            cont)
      case LongRemainderUnsigned =>
        pretransformApply(ApplyFlags.empty, targs.head,
            Ident(LongImpl.remainderUnsigned), targs.tail,
            ClassType(LongImpl.RuntimeLongClass), isStat, usePreTransform)(
            cont)

      // scala.collection.mutable.ArrayBuilder

      case GenericArrayBuilderResult =>
        val List(runtimeClass, array) = newArgs
        val (resultType, isExact) = runtimeClass match {
          case ClassOf(elemTypeRef) => (ArrayType(ArrayTypeRef.of(elemTypeRef)), true)
          case _                    => (AnyType, false)
        }
        cont(PreTransTree(callHelper("makeNativeArrayWrapper",
            callHelper("arrayDataOf",
                callHelper("classDataOf", runtimeClass)(AnyType))(AnyType),
            array)(resultType),
            RefinedType(resultType, isExact = isExact, isNullable = false)))

      case ArrayBuilderZeroOf =>
        contTree(finishTransformExpr(targs.head) match {
          case ClassOf(ClassRef(cls)) =>
            /* Note that the 'C' case produces a literal int instead of char.
             * This ensures that we fill up the JS array with numbers 0 rather
             * than boxed '\0'. We need to do this because the result() method
             * (see intrinsic right above) will directly feed that JS array to
             * `makeNativeArrayWrapper`, which expects an array of numbers when
             * builing an `Array[Char]`.
             */
            cls match {
              case "C" => IntLiteral(0)
              case "B" => ByteLiteral(0)
              case "S" => ShortLiteral(0)
              case "I" => IntLiteral(0)
              case "L" => LongLiteral(0L)
              case "F" => FloatLiteral(0.0f)
              case "D" => DoubleLiteral(0.0)
              case "Z" => BooleanLiteral(false)
              case "V" => Undefined()
              case _   => Null()
            }
          case ClassOf(_) =>
            Null()
          case runtimeClass =>
            callHelper("zeroOf", runtimeClass)(AnyType)
        })

      // java.lang.Class

      case ClassGetComponentType =>
        newReceiver match {
          case ClassOf(ArrayTypeRef(base, depth)) =>
            contTree(ClassOf(
                if (depth == 1) ClassRef(base)
                else ArrayTypeRef(base, depth - 1)))
          case ClassOf(ClassRef(_)) =>
            contTree(Null())
          case receiver =>
            defaultApply("getComponentType__jl_Class",
                ClassType(Definitions.ClassClass))
        }

      // java.lang.reflect.Array

      case ArrayNewInstance =>
        newArgs.head match {
          case ClassOf(elementTypeRef) =>
            val arrayTypeRef = ArrayTypeRef.of(elementTypeRef)
            contTree(NewArray(arrayTypeRef, List(newArgs.tail.head)))
          case _ =>
            defaultApply("newInstance__jl_Class__I__O", AnyType)
        }

      // js.special

      case ObjectLiteral =>
        def default =
          defaultApply("objectLiteral__sc_Seq__sjs_js_Object", AnyType)

        val List(tprops) = targs
        tprops match {
          case PreTransMaybeBlock(bindingsAndStats,
              PreTransLocalDef(LocalDef(
                  RefinedType(ClassType("sjs_js_WrappedArray"), _, _),
                  false,
                  InlineClassInstanceReplacement(_, wrappedArrayFields, _)))) =>
            assert(wrappedArrayFields.size == 1)
            val jsArray = wrappedArrayFields.head._2
            jsArray.replacement match {
              case InlineJSArrayReplacement(elemLocalDefs, _)
                  if elemLocalDefs.forall(e => isSubtype(e.tpe.base, ClassType("T2"))) =>
                val fields: List[(PropertyName, Tree)] = for {
                  (elemLocalDef, idx) <- elemLocalDefs.toList.zipWithIndex
                } yield {
                  elemLocalDef match {
                    case LocalDef(RefinedType(ClassType("T2"), _, _), false,
                        InlineClassInstanceReplacement(recType, tupleFields, _)) =>
                      val List(key, value) =
                        recType.fields.map(f => tupleFields(f.name))
                      val keyProp = key.newReplacement match {
                        case keyProp: StringLiteral =>
                          keyProp
                        case keyTree =>
                          ComputedName(keyTree, "local" + idx)
                      }
                      (keyProp, value.newReplacement)

                    case _ =>
                      val flags = ApplyFlags.empty
                      val key = Apply(flags, elemLocalDef.newReplacement,
                          "$$und1__O", Nil)(AnyType)
                      val value = Apply(flags, elemLocalDef.newReplacement,
                          "$$und2__O", Nil)(AnyType)
                      (ComputedName(key, "local" + idx), value)
                  }
                }

                val resultTree = JSObjectConstr(fields)

                contTree(Block(finishTransformStat(optTReceiver.get),
                    finishTransformBindings(bindingsAndStats, resultTree)))

              case _ =>
                default
            }

          case _ =>
            tprops.tpe match {
              case RefinedType(ClassType("sci_Nil$"), _, false) =>
                contTree(Block(finishTransformStat(tprops), JSObjectConstr(Nil)))
              case _ =>
                default
            }
        }

      // TypedArray conversions

      case ByteArrayToInt8Array =>
        contTree(callHelper("byteArray2TypedArray", newArgs)(AnyType))
      case ShortArrayToInt16Array =>
        contTree(callHelper("shortArray2TypedArray", newArgs)(AnyType))
      case CharArrayToUint16Array =>
        contTree(callHelper("charArray2TypedArray", newArgs)(AnyType))
      case IntArrayToInt32Array =>
        contTree(callHelper("intArray2TypedArray", newArgs)(AnyType))
      case FloatArrayToFloat32Array =>
        contTree(callHelper("floatArray2TypedArray", newArgs)(AnyType))
      case DoubleArrayToFloat64Array =>
        contTree(callHelper("doubleArray2TypedArray", newArgs)(AnyType))

      case Int8ArrayToByteArray =>
        contTree(callHelper("typedArray2ByteArray", newArgs)(AnyType))
      case Int16ArrayToShortArray =>
        contTree(callHelper("typedArray2ShortArray", newArgs)(AnyType))
      case Uint16ArrayToCharArray =>
        contTree(callHelper("typedArray2CharArray", newArgs)(AnyType))
      case Int32ArrayToIntArray =>
        contTree(callHelper("typedArray2IntArray", newArgs)(AnyType))
      case Float32ArrayToFloatArray =>
        contTree(callHelper("typedArray2FloatArray", newArgs)(AnyType))
      case Float64ArrayToDoubleArray =>
        contTree(callHelper("typedArray2DoubleArray", newArgs)(AnyType))
    }
  }

  private def callHelper(helper: String, args: List[Tree])(tpe: Type)(
      implicit pos: Position): Tree = {
    Transient(CallHelper(helper, args))(tpe)
  }

  private def callHelper(helper: String, args: Tree*)(tpe: Type)(
      implicit pos: Position): Tree = {
    callHelper(helper, args.toList)(tpe)
  }

  private def inlineClassConstructor(allocationSite: AllocationSite,
      cls: ClassRef, initialValue: RecordValue,
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
          cont2(LocalDef(
              RefinedType(ClassType(cls.className), isExact = true,
                  isNullable = false, allocationSite = allocationSite),
              mutable = false,
              InlineClassInstanceReplacement(recordType, finalFieldLocalDefs,
                  cancelFun)).toPreTransform)
        } (cont1)
      } (cont)
    }
  }

  private def inlineClassConstructorBody(
      allocationSite: AllocationSite,
      inputFieldsLocalDefs: Map[String, LocalDef], cls: ClassRef,
      ctorClass: ClassRef, ctor: Ident, args: List[PreTransform],
      cancelFun: CancelFun)(
      buildInner: (Map[String, LocalDef], PreTransCont) => TailRec[Tree])(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = tailcall {

    val target = staticCall(ctorClass.className, MemberNamespace.Constructor,
        ctor.name).getOrElse(cancelFun())
    val targetID = (allocationSite :: args.map(_.tpe.allocationSite), target)
    if (scope.implsBeingInlined.contains(targetID))
      cancelFun()

    val targetMethodDef = getMethodBody(target)
    val formals = targetMethodDef.args
    val stats = targetMethodDef.body.get match {
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
          RefinedType(ClassType(cls.className), isExact = true, isNullable = false),
          false,
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
      cls: ClassRef, stats: List[Tree], cancelFun: CancelFun)(
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
            val newThisLocalDef = LocalDef(thisLocalDef.tpe, false,
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

      case ApplyStatically(flags, ths: This, superClass, superCtor, args) :: rest
          if flags.isConstructor =>
        pretransformExprs(args) { targs =>
          inlineClassConstructorBody(allocationSite, inputFieldsLocalDefs,
              cls, superClass, superCtor, targs,
              cancelFun) { (outputFieldsLocalDefs, cont1) =>
            val newThisLocalDef = LocalDef(thisLocalDef.tpe, false,
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
                cont(PreTransBlock(transformedStat, tinner))
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
        @inline def negCond =
          finishTransformExpr(foldUnaryOp(UnaryOp.Boolean_!, cond.toPreTransform))

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
            case (BinaryOp(op1 @ (Int_== | Int_!= | Int_< | Int_<= | Int_> | Int_>=), l1, r1),
                  BooleanLiteral(true),
                  BinaryOp(op2 @ (Int_== | Int_!= | Int_< | Int_<= | Int_> | Int_>=), l2, r2))
                if ((l1.isInstanceOf[Literal] || l1.isInstanceOf[VarRef]) &&
                    (r1.isInstanceOf[Literal] || r1.isInstanceOf[VarRef]) &&
                    (l1 == l2 && r1 == r2)) =>
              val canBeEqual =
                ((op1 == Int_==) || (op1 == Int_<=) || (op1 == Int_>=)) ||
                ((op2 == Int_==) || (op2 == Int_<=) || (op2 == Int_>=))
              val canBeLessThan =
                ((op1 == Int_!=) || (op1 == Int_<) || (op1 == Int_<=)) ||
                ((op2 == Int_!=) || (op2 == Int_<) || (op2 == Int_<=))
              val canBeGreaterThan =
                ((op1 == Int_!=) || (op1 == Int_>) || (op1 == Int_>=)) ||
                ((op2 == Int_!=) || (op2 == Int_>) || (op2 == Int_>=))

              finishTransformExpr(
                  fold3WayIntComparison(canBeEqual, canBeLessThan,
                      canBeGreaterThan, l1.toPreTransform, r1.toPreTransform))

            // Example: (x >= y) && (x <= y)  ->  (x == y)
            case (BinaryOp(op1 @ (Int_== | Int_!= | Int_< | Int_<= | Int_> | Int_>=), l1, r1),
                  BinaryOp(op2 @ (Int_== | Int_!= | Int_< | Int_<= | Int_> | Int_>=), l2, r2),
                  BooleanLiteral(false))
                if ((l1.isInstanceOf[Literal] || l1.isInstanceOf[VarRef]) &&
                    (r1.isInstanceOf[Literal] || r1.isInstanceOf[VarRef]) &&
                    (l1 == l2 && r1 == r2)) =>
              val canBeEqual =
                ((op1 == Int_==) || (op1 == Int_<=) || (op1 == Int_>=)) &&
                ((op2 == Int_==) || (op2 == Int_<=) || (op2 == Int_>=))
              val canBeLessThan =
                ((op1 == Int_!=) || (op1 == Int_<) || (op1 == Int_<=)) &&
                ((op2 == Int_!=) || (op2 == Int_<) || (op2 == Int_<=))
              val canBeGreaterThan =
                ((op1 == Int_!=) || (op1 == Int_>) || (op1 == Int_>=)) &&
                ((op2 == Int_!=) || (op2 == Int_>) || (op2 == Int_>=))

              finishTransformExpr(
                  fold3WayIntComparison(canBeEqual, canBeLessThan,
                      canBeGreaterThan, l1.toPreTransform, r1.toPreTransform))

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

  private def pretransformUnaryOp(tree: UnaryOp)(cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    implicit val pos = tree.pos
    val UnaryOp(op, arg) = tree

    pretransformExpr(arg) { tlhs =>
      expandLongOps(foldUnaryOp(op, tlhs))(cont)
    }
  }

  private def pretransformBinaryOp(tree: BinaryOp)(cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    implicit val pos = tree.pos
    val BinaryOp(op, lhs, rhs) = tree

    pretransformExprs(lhs, rhs) { (tlhs, trhs) =>
      expandLongOps(foldBinaryOp(op, tlhs, trhs))(cont)
    }
  }

  private def expandLongValue(value: PreTransform)(cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    assert(useRuntimeLong)

    /* To force the expansion, we first store the `value` in a temporary
     * variable of type `RuntimeLong` (not `Long`, otherwise we would go into
     * infinite recursion), then we create a `new RuntimeLong` with its lo and
     * hi part. Basically, we're doing:
     *
     * val t: RuntimeLong = value
     * new RuntimeLong(t.lo__I(), t.hi__I())
     */
    val rtLongClassType = ClassType(LongImpl.RuntimeLongClass)
    val rtLongBinding = Binding("t", None, rtLongClassType,
        mutable = false, value)
    withBinding(rtLongBinding) { (scope1, cont1) =>
      implicit val scope = scope1
      val tRef = VarRef(Ident("t", None))(rtLongClassType)
      val newTree = New(ClassRef(LongImpl.RuntimeLongClass),
          Ident(LongImpl.initFromParts),
          List(Apply(ApplyFlags.empty, tRef, Ident("lo__I"), Nil)(IntType),
              Apply(ApplyFlags.empty, tRef, Ident("hi__I"), Nil)(IntType)))
      pretransformExpr(newTree)(cont1)
    } (cont)
  }

  private def expandLongOps(pretrans: PreTransform)(cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    implicit val pos = pretrans.pos

    def rtLongClassType = ClassType(LongImpl.RuntimeLongClass)

    def rtLongModuleClassRef = ClassRef(LongImpl.RuntimeLongModuleClass)

    def expandLongModuleOp(methodName: String,
        arg: PreTransform): TailRec[Tree] = {
      val receiver = LoadModule(rtLongModuleClassRef).toPreTransform
      pretransformApply(ApplyFlags.empty, receiver, Ident(methodName),
          arg :: Nil, rtLongClassType, isStat = false,
          usePreTransform = true)(
          cont)
    }

    def expandUnaryOp(methodName: String, arg: PreTransform,
        resultType: Type = rtLongClassType): TailRec[Tree] = {
      pretransformApply(ApplyFlags.empty, arg, Ident(methodName), Nil,
          resultType, isStat = false, usePreTransform = true)(
          cont)
    }

    def expandBinaryOp(methodName: String, lhs: PreTransform,
        rhs: PreTransform,
        resultType: Type = rtLongClassType): TailRec[Tree] = {
      pretransformApply(ApplyFlags.empty, lhs, Ident(methodName), rhs :: Nil,
          resultType, isStat = false, usePreTransform = true)(
          cont)
    }

    pretrans match {
      case PreTransUnaryOp(op, arg) if useRuntimeLong =>
        import UnaryOp._

        (op: @switch) match {
          case IntToLong =>
            expandLongModuleOp(LongImpl.fromInt, arg)

          case LongToInt =>
            expandUnaryOp(LongImpl.toInt, arg, IntType)

          case LongToDouble =>
            expandUnaryOp(LongImpl.toDouble, arg, DoubleType)

          case DoubleToLong =>
            expandLongModuleOp(LongImpl.fromDouble, arg)

          case _ =>
            cont(pretrans)
        }

      case PreTransBinaryOp(op, lhs, rhs) if useRuntimeLong =>
        import BinaryOp._

        (op: @switch) match {
          case Long_+ => expandBinaryOp(LongImpl.+, lhs, rhs)

          case Long_- =>
            lhs match {
              case PreTransLit(LongLiteral(0L)) =>
                expandUnaryOp(LongImpl.UNARY_-, rhs)
              case _ =>
                expandBinaryOp(LongImpl.-, lhs, rhs)
            }

          case Long_* => expandBinaryOp(LongImpl.*, lhs, rhs)
          case Long_/ => expandBinaryOp(LongImpl./, lhs, rhs)
          case Long_% => expandBinaryOp(LongImpl.%, lhs, rhs)

          case Long_& => expandBinaryOp(LongImpl.&, lhs, rhs)
          case Long_| => expandBinaryOp(LongImpl.|, lhs, rhs)
          case Long_^ => expandBinaryOp(LongImpl.^, lhs, rhs)

          case Long_<<  => expandBinaryOp(LongImpl.<<, lhs, rhs)
          case Long_>>> => expandBinaryOp(LongImpl.>>>, lhs, rhs)
          case Long_>>  => expandBinaryOp(LongImpl.>>, lhs, rhs)

          case Long_== => expandBinaryOp(LongImpl.===, lhs, rhs)
          case Long_!= => expandBinaryOp(LongImpl.!==, lhs, rhs)
          case Long_<  => expandBinaryOp(LongImpl.<, lhs, rhs)
          case Long_<= => expandBinaryOp(LongImpl.<=, lhs, rhs)
          case Long_>  => expandBinaryOp(LongImpl.>, lhs, rhs)
          case Long_>= => expandBinaryOp(LongImpl.>=, lhs, rhs)

          case _ =>
            cont(pretrans)
        }

      case _ =>
        cont(pretrans)
    }
  }

  private def foldUnaryOp(op: UnaryOp.Code, arg: PreTransform)(
      implicit pos: Position): PreTransform = {
    import UnaryOp._

    @inline def default = PreTransUnaryOp(op, arg)

    (op: @switch) match {
      case Boolean_! =>
        arg match {
          case PreTransLit(BooleanLiteral(v)) =>
            PreTransLit(BooleanLiteral(!v))

          case PreTransUnaryOp(Boolean_!, x) => x

          case PreTransBinaryOp(innerOp, l, r) =>
            val newOp = (innerOp: @switch) match {
              case BinaryOp.=== => BinaryOp.!==
              case BinaryOp.!== => BinaryOp.===

              case BinaryOp.Int_== => BinaryOp.Int_!=
              case BinaryOp.Int_!= => BinaryOp.Int_==
              case BinaryOp.Int_<  => BinaryOp.Int_>=
              case BinaryOp.Int_<= => BinaryOp.Int_>
              case BinaryOp.Int_>  => BinaryOp.Int_<=
              case BinaryOp.Int_>= => BinaryOp.Int_<

              case BinaryOp.Long_== => BinaryOp.Long_!=
              case BinaryOp.Long_!= => BinaryOp.Long_==
              case BinaryOp.Long_<  => BinaryOp.Long_>=
              case BinaryOp.Long_<= => BinaryOp.Long_>
              case BinaryOp.Long_>  => BinaryOp.Long_<=
              case BinaryOp.Long_>= => BinaryOp.Long_<

              case BinaryOp.Double_== => BinaryOp.Double_!=
              case BinaryOp.Double_!= => BinaryOp.Double_==

              case BinaryOp.Boolean_== => BinaryOp.Boolean_!=
              case BinaryOp.Boolean_!= => BinaryOp.Boolean_==

              case _ => -1
            }
            if (newOp == -1) default
            else PreTransBinaryOp(newOp, l, r)

          case _ =>
            default
        }

      // Widening conversions

      case CharToInt =>
        arg match {
          case PreTransLit(CharLiteral(v)) =>
            PreTransLit(IntLiteral(v.toInt))
          case _ =>
            default
        }
      case ByteToInt =>
        arg match {
          case PreTransLit(ByteLiteral(v)) =>
            PreTransLit(IntLiteral(v.toInt))
          case _ =>
            default
        }
      case ShortToInt =>
        arg match {
          case PreTransLit(ShortLiteral(v)) =>
            PreTransLit(IntLiteral(v.toInt))
          case _ =>
            default
        }
      case IntToLong =>
        arg match {
          case PreTransLit(IntLiteral(v)) =>
            PreTransLit(LongLiteral(v.toLong))
          case _ =>
            default
        }
      case IntToDouble =>
        arg match {
          case PreTransLit(IntLiteral(v)) =>
            PreTransLit(DoubleLiteral(v.toDouble))
          case _ =>
            default
        }
      case FloatToDouble =>
        arg match {
          case PreTransLit(FloatLiteral(v)) =>
            PreTransLit(DoubleLiteral(v.toDouble))
          case _ =>
            default
        }

      // Narrowing conversions

      case IntToChar =>
        arg match {
          case PreTransLit(IntLiteral(v)) =>
            PreTransLit(CharLiteral(v.toChar))
          case PreTransUnaryOp(CharToInt, x) =>
            x
          case _ =>
            default
        }
      case IntToByte =>
        arg match {
          case PreTransLit(IntLiteral(v)) =>
            PreTransLit(ByteLiteral(v.toByte))
          case PreTransUnaryOp(ByteToInt, x) =>
            x
          case _ =>
            default
        }
      case IntToShort =>
        arg match {
          case PreTransLit(IntLiteral(v)) =>
            PreTransLit(ShortLiteral(v.toShort))
          case PreTransUnaryOp(ShortToInt, x) =>
            x
          case _ =>
            default
        }
      case LongToInt =>
        arg match {
          case PreTransLit(LongLiteral(v)) =>
            PreTransLit(IntLiteral(v.toInt))
          case PreTransUnaryOp(IntToLong, x) =>
            x
          case PreTransBinaryOp(BinaryOp.Long_+, x, y) =>
            foldBinaryOp(BinaryOp.Int_+,
                foldUnaryOp(LongToInt, x),
                foldUnaryOp(LongToInt, y))
          case PreTransBinaryOp(BinaryOp.Long_-, x, y) =>
            foldBinaryOp(BinaryOp.Int_-,
                foldUnaryOp(LongToInt, x),
                foldUnaryOp(LongToInt, y))
          case _ =>
            default
        }
      case DoubleToInt =>
        arg match {
          case PreTransLit(DoubleLiteral(v)) =>
            PreTransLit(IntLiteral(v.toInt))
          case PreTransUnaryOp(IntToDouble, x) =>
            x
          case _ =>
            default
        }
      case DoubleToFloat =>
        arg match {
          case PreTransLit(DoubleLiteral(v)) =>
            PreTransLit(FloatLiteral(v.toFloat))
          case PreTransUnaryOp(FloatToDouble, x) =>
            x
          case _  =>
            default
        }

      // Long <-> Double

      case LongToDouble =>
        arg match {
          case PreTransLit(LongLiteral(v)) =>
            PreTransLit(DoubleLiteral(v.toDouble))
          case PreTransUnaryOp(IntToLong, x) =>
            foldUnaryOp(IntToDouble, x)
          case _ =>
            default
        }
      case DoubleToLong =>
        arg match {
          case PreTransLit(DoubleLiteral(v)) =>
            PreTransLit(LongLiteral(v.toLong))
          case PreTransUnaryOp(IntToDouble, x) =>
            foldUnaryOp(IntToLong, x)
          case _ =>
            default
        }

      case _ =>
        default
    }
  }

  /** Performs === for two literals.
   *  The result is always known statically.
   *
   *  Bytes, Shorts, Ints, Floats and Doubles all live in the same "space" for
   *  `===` comparison, since they all upcast as primitive numbers.
   *
   *  Chars and Longs, however, never compare as `===`, since they are boxed
   *  chars and instances of `RuntimeLong`, respectively---unless we are using
   *  `BigInt`s for `Long`s, in which case those can be `===`.
   */
  private def literal_===(lhs: Literal, rhs: Literal): Boolean = {
    object AnyNumLiteral {
      def unapply(tree: Literal): Option[Double] = tree match {
        case ByteLiteral(v)   => Some(v.toDouble)
        case ShortLiteral(v)  => Some(v.toDouble)
        case IntLiteral(v)    => Some(v.toDouble)
        case FloatLiteral(v)  => Some(v.toDouble)
        case DoubleLiteral(v) => Some(v)
        case _                => None
      }
    }

    (lhs, rhs) match {
      case (BooleanLiteral(l), BooleanLiteral(r)) => l == r
      case (StringLiteral(l), StringLiteral(r))   => l == r
      case (ClassOf(l), ClassOf(r))               => l == r
      case (AnyNumLiteral(l), AnyNumLiteral(r))   => l == r
      case (LongLiteral(l), LongLiteral(r))       => l == r && !useRuntimeLong
      case (Undefined(), Undefined())             => true
      case (Null(), Null())                       => true
      case _                                      => false
    }
  }

  private def constantFoldBinaryOp_except_String_+(op: BinaryOp.Code,
      lhs: Literal, rhs: Literal)(implicit pos: Position): Literal = {
    import BinaryOp._

    @inline def int(lit: Literal): Int = (lit: @unchecked) match {
      case IntLiteral(value) => value
    }

    @inline def long(lit: Literal): Long = (lit: @unchecked) match {
      case LongLiteral(value) => value
    }

    @inline def float(lit: Literal): Float = (lit: @unchecked) match {
      case FloatLiteral(value) => value
    }

    @inline def double(lit: Literal): Double = (lit: @unchecked) match {
      case DoubleLiteral(value) => value
    }

    @inline def boolean(lit: Literal): Boolean = (lit: @unchecked) match {
      case BooleanLiteral(value) => value
    }

    (op: @switch) match {
      case === => BooleanLiteral(literal_===(lhs, rhs))
      case !== => BooleanLiteral(!literal_===(lhs, rhs))

      case String_+ =>
        throw new IllegalArgumentException(
            "constFoldBinaryOp_except_String_+ must not be called for String_+")

      case Int_+ => IntLiteral(int(lhs) + int(rhs))
      case Int_- => IntLiteral(int(lhs) - int(rhs))
      case Int_* => IntLiteral(int(lhs) * int(rhs))
      case Int_/ => IntLiteral(int(lhs) / int(rhs))
      case Int_% => IntLiteral(int(lhs) % int(rhs))

      case Int_|   => IntLiteral(int(lhs) | int(rhs))
      case Int_&   => IntLiteral(int(lhs) & int(rhs))
      case Int_^   => IntLiteral(int(lhs) ^ int(rhs))
      case Int_<<  => IntLiteral(int(lhs) << int(rhs))
      case Int_>>> => IntLiteral(int(lhs) >>> int(rhs))
      case Int_>>  => IntLiteral(int(lhs) >> int(rhs))

      case Int_== => BooleanLiteral(int(lhs) == int(rhs))
      case Int_!= => BooleanLiteral(int(lhs) != int(rhs))
      case Int_<  => BooleanLiteral(int(lhs) < int(rhs))
      case Int_<= => BooleanLiteral(int(lhs) <= int(rhs))
      case Int_>  => BooleanLiteral(int(lhs) > int(rhs))
      case Int_>= => BooleanLiteral(int(lhs) >= int(rhs))

      case Long_+ => LongLiteral(long(lhs) + long(rhs))
      case Long_- => LongLiteral(long(lhs) - long(rhs))
      case Long_* => LongLiteral(long(lhs) * long(rhs))
      case Long_/ => LongLiteral(long(lhs) / long(rhs))
      case Long_% => LongLiteral(long(lhs) % long(rhs))

      case Long_|   => LongLiteral(long(lhs) | long(rhs))
      case Long_&   => LongLiteral(long(lhs) & long(rhs))
      case Long_^   => LongLiteral(long(lhs) ^ long(rhs))
      case Long_<<  => LongLiteral(long(lhs) << int(rhs))
      case Long_>>> => LongLiteral(long(lhs) >>> int(rhs))
      case Long_>>  => LongLiteral(long(lhs) >> int(rhs))

      case Long_== => BooleanLiteral(long(lhs) == long(rhs))
      case Long_!= => BooleanLiteral(long(lhs) != long(rhs))
      case Long_<  => BooleanLiteral(long(lhs) < long(rhs))
      case Long_<= => BooleanLiteral(long(lhs) <= long(rhs))
      case Long_>  => BooleanLiteral(long(lhs) > long(rhs))
      case Long_>= => BooleanLiteral(long(lhs) >= long(rhs))

      case Float_+ => FloatLiteral(float(lhs) + float(rhs))
      case Float_- => FloatLiteral(float(lhs) - float(rhs))
      case Float_* => FloatLiteral(float(lhs) * float(rhs))
      case Float_/ => FloatLiteral(float(lhs) / float(rhs))
      case Float_% => FloatLiteral(float(lhs) % float(rhs))

      case Double_+ => DoubleLiteral(double(lhs) + double(rhs))
      case Double_- => DoubleLiteral(double(lhs) - double(rhs))
      case Double_* => DoubleLiteral(double(lhs) * double(rhs))
      case Double_/ => DoubleLiteral(double(lhs) / double(rhs))
      case Double_% => DoubleLiteral(double(lhs) % double(rhs))

      case Double_== => BooleanLiteral(double(lhs) == double(rhs))
      case Double_!= => BooleanLiteral(double(lhs) != double(rhs))
      case Double_<  => BooleanLiteral(double(lhs) < double(rhs))
      case Double_<= => BooleanLiteral(double(lhs) <= double(rhs))
      case Double_>  => BooleanLiteral(double(lhs) > double(rhs))
      case Double_>= => BooleanLiteral(double(lhs) >= double(rhs))

      case Boolean_== => BooleanLiteral(boolean(lhs) == boolean(rhs))
      case Boolean_!= => BooleanLiteral(boolean(lhs) != boolean(rhs))
      case Boolean_|  => BooleanLiteral(boolean(lhs) | boolean(rhs))
      case Boolean_&  => BooleanLiteral(boolean(lhs) & boolean(rhs))
    }
  }

  /** Translate literals to their Scala.js String representation. */
  private def foldToStringForString_+(preTrans: PreTransform)(
      implicit pos: Position): PreTransform = preTrans match {
    case PreTransLit(literal) =>
      def constant(s: String): PreTransform =
        PreTransLit(StringLiteral(s))

      def forFloatingPoint(value: Double): PreTransform =
        jsNumberToString(value).fold(preTrans)(s => constant(s))

      literal match {
        case CharLiteral(value)    => constant(value.toString)
        case ByteLiteral(value)    => constant(value.toString)
        case ShortLiteral(value)   => constant(value.toString)
        case IntLiteral(value)     => constant(value.toString)
        case LongLiteral(value)    => constant(value.toString)
        case FloatLiteral(value)   => forFloatingPoint(value)
        case DoubleLiteral(value)  => forFloatingPoint(value)
        case BooleanLiteral(value) => constant(value.toString)
        case Null()                => constant("null")
        case Undefined()           => constant("undefined")
        case _                     => preTrans
      }

    case _ =>
      preTrans
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

  private def foldBinaryOp(op: BinaryOp.Code, lhs: PreTransform,
      rhs: PreTransform)(
      implicit pos: Position): PreTransform = {
    import BinaryOp._

    def constant(lhsLit: Literal, rhsLit: Literal): PreTransform =
      PreTransLit(constantFoldBinaryOp_except_String_+(op, lhsLit, rhsLit))

    def nonConstant(): PreTransform = foldBinaryOpNonConstant(op, lhs, rhs)

    (lhs, rhs) match {
      case (PreTransLit(lhsLit), PreTransLit(rhsLit)) =>
        op match {
          case String_+ =>
            nonConstant()
          case Int_/ | Int_% =>
            rhsLit match {
              case IntLiteral(0) => nonConstant()
              case _             => constant(lhsLit, rhsLit)
            }
          case Long_/ | Long_% =>
            rhsLit match {
              case LongLiteral(0) => nonConstant()
              case _              => constant(lhsLit, rhsLit)
            }
          case _ =>
            constant(lhsLit, rhsLit)
        }

      case _ =>
        nonConstant()
    }
  }

  private def foldBinaryOpNonConstant(op: BinaryOp.Code, lhs: PreTransform,
      rhs: PreTransform)(
      implicit pos: Position): PreTransform = {
    import BinaryOp._

    @inline def default =
      PreTransBinaryOp(op, lhs, rhs)

    (op: @switch) match {
      case === | !== =>
        val positive = (op == ===)
        (lhs, rhs) match {
          case (_, PreTransLit(Null())) if !lhs.tpe.isNullable =>
            Block(
                finishTransformStat(lhs),
                BooleanLiteral(!positive)).toPreTransform

          case (PreTransLit(_), _) => foldBinaryOp(op, rhs, lhs)
          case _                   => default
        }

      case String_+ =>
        // Here things can be constant!
        val lhs1 = foldToStringForString_+(lhs)
        val rhs1 = foldToStringForString_+(rhs)

        @inline def stringDefault = PreTransBinaryOp(String_+, lhs1, rhs1)

        (lhs1, rhs1) match {
          case (PreTransLit(StringLiteral(s1)), PreTransLit(StringLiteral(s2))) =>
            PreTransLit(StringLiteral(s1 + s2))
          case (_, PreTransLit(StringLiteral(""))) =>
            foldBinaryOp(op, rhs1, lhs1)
          case (PreTransLit(StringLiteral("")), _) if rhs1.tpe.base == StringType =>
            rhs1
          case (_, PreTransBinaryOp(String_+, rl, rr)) =>
            foldBinaryOp(String_+, PreTransBinaryOp(String_+, lhs1, rl), rr)
          case (PreTransBinaryOp(String_+, ll, PreTransLit(StringLiteral(lr))),
              PreTransLit(StringLiteral(r))) =>
            PreTransBinaryOp(String_+, ll, PreTransLit(StringLiteral(lr + r)))
          case (PreTransBinaryOp(String_+, PreTransLit(StringLiteral("")), lr), _) =>
            PreTransBinaryOp(String_+, lr, rhs1)
          case _ =>
            stringDefault
      }

      case Int_+ =>
        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(_))) =>
            foldBinaryOp(Int_+, rhs, lhs)
          case (PreTransLit(IntLiteral(0)), _) =>
            rhs

          case (PreTransLit(IntLiteral(x)),
              PreTransBinaryOp(innerOp @ (Int_+ | Int_-),
                  PreTransLit(IntLiteral(y)), z)) =>
            foldBinaryOp(innerOp, PreTransLit(IntLiteral(x + y)), z)

          case _ => default
        }

      case Int_- =>
        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(r))) =>
            foldBinaryOp(Int_+, lhs, PreTransLit(IntLiteral(-r)))

          case (PreTransLit(IntLiteral(x)),
              PreTransBinaryOp(Int_+, PreTransLit(IntLiteral(y)), z)) =>
            foldBinaryOp(Int_-,
                PreTransLit(IntLiteral(x - y)), z)

          case (PreTransLit(IntLiteral(x)),
              PreTransBinaryOp(Int_-, PreTransLit(IntLiteral(y)), z)) =>
            foldBinaryOp(Int_+, PreTransLit(IntLiteral(x - y)), z)

          case (_, PreTransBinaryOp(Int_-, PreTransLit(IntLiteral(0)), x)) =>
            foldBinaryOp(Int_+, lhs, x)

          case _ => default
        }

      case Int_* =>
        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(_))) =>
            foldBinaryOp(Int_*, rhs, lhs)

          case (PreTransLit(IntLiteral(x)), _) =>
            x match {
              case -1 => foldBinaryOp(Int_-, PreTransLit(IntLiteral(0)), rhs)
              case 0  => PreTransBlock(finishTransformStat(rhs), lhs)
              case 1  => rhs

              // Exact power of 2
              case _ if (x & (x - 1)) == 0 =>
                /* Note that this would match 0, but 0 is handled above.
                 * It will also match Int.MinValue, but that is not a problem
                 * as the optimization also works (if you need convincing,
                 * simply interpret the multiplication as unsigned).
                 */
                foldBinaryOp(Int_<<, rhs,
                    PreTransLit(IntLiteral(Integer.numberOfTrailingZeros(x))))

              case _ => default
            }

          case _ => default
        }

      case Int_/ =>
        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(1)))  =>
            lhs
          case (_, PreTransLit(IntLiteral(-1))) =>
            foldBinaryOp(Int_-, PreTransLit(IntLiteral(0)), lhs)

          case _ => default
        }

      case Int_% =>
        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(1 | -1))) =>
            Block(finishTransformStat(lhs), IntLiteral(0)).toPreTransform

          case _ => default
        }

      case Int_| =>
        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(_))) => foldBinaryOp(Int_|, rhs, lhs)
          case (PreTransLit(IntLiteral(0)), _) => rhs

          case (PreTransLit(IntLiteral(-1)), _) =>
            PreTransBlock(finishTransformStat(rhs), lhs)

          case (PreTransLit(IntLiteral(x)),
              PreTransBinaryOp(Int_|, PreTransLit(IntLiteral(y)), z)) =>
            foldBinaryOp(Int_|, PreTransLit(IntLiteral(x | y)), z)

          case _ => default
        }

      case Int_& =>
        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(_)))  => foldBinaryOp(Int_&, rhs, lhs)
          case (PreTransLit(IntLiteral(-1)), _) => rhs

          case (PreTransLit(IntLiteral(0)), _) =>
            PreTransBlock(finishTransformStat(rhs), lhs)

          case (PreTransLit(IntLiteral(x)),
              PreTransBinaryOp(Int_&, PreTransLit(IntLiteral(y)), z)) =>
            foldBinaryOp(Int_&, PreTransLit(IntLiteral(x & y)), z)

          case _ => default
        }

      case Int_^ =>
        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(_))) => foldBinaryOp(Int_^, rhs, lhs)
          case (PreTransLit(IntLiteral(0)), _) => rhs

          case (PreTransLit(IntLiteral(x)),
              PreTransBinaryOp(Int_^, PreTransLit(IntLiteral(y)), z)) =>
            foldBinaryOp(Int_^, PreTransLit(IntLiteral(x ^ y)), z)

          case _ => default
        }

      case Int_<< =>
        (lhs, rhs) match {
          case (PreTransLit(IntLiteral(0)), _) =>
            PreTransBlock(finishTransformStat(rhs), lhs)

          case (PreTransBinaryOp(Int_<<, x, PreTransLit(IntLiteral(y))),
              PreTransLit(IntLiteral(z))) =>
            val dist = (y & 31) + (z & 31)
            if (dist >= 32)
              PreTransTree(Block(finishTransformStat(x), IntLiteral(0)))
            else
              PreTransBinaryOp(Int_<<, x, PreTransLit(IntLiteral(dist)))

          case (_, PreTransLit(IntLiteral(y))) =>
            val dist = y & 31
            if (dist == 0)
              lhs
            else
              PreTransBinaryOp(Int_<<, lhs, PreTransLit(IntLiteral(dist)))

          case _ => default
        }

      case Int_>>> =>
        (lhs, rhs) match {
          case (PreTransLit(IntLiteral(0)), _) =>
            PreTransBlock(finishTransformStat(rhs), lhs)

          case (PreTransBinaryOp(Int_>>>, x, PreTransLit(IntLiteral(y))),
              PreTransLit(IntLiteral(z))) =>
            val dist = (y & 31) + (z & 31)
            if (dist >= 32)
              PreTransTree(Block(finishTransformStat(x), IntLiteral(0)))
            else
              PreTransBinaryOp(Int_>>>, x, PreTransLit(IntLiteral(dist)))

          case (PreTransBinaryOp(op @ (Int_| | Int_& | Int_^),
              PreTransLit(IntLiteral(x)), y),
              z @ PreTransLit(IntLiteral(zValue))) =>
            foldBinaryOp(
                op,
                PreTransLit(IntLiteral(x >>> zValue)),
                foldBinaryOp(Int_>>>, y, z))

          case (_, PreTransLit(IntLiteral(y))) =>
            val dist = y & 31
            if (dist == 0)
              lhs
            else
              PreTransBinaryOp(Int_>>>, lhs, PreTransLit(IntLiteral(dist)))

          case _ => default
        }

      case Int_>> =>
        (lhs, rhs) match {
          case (PreTransLit(IntLiteral(0 | -1)), _) =>
            PreTransBlock(finishTransformStat(rhs), lhs)

          case (PreTransBinaryOp(Int_>>, x, PreTransLit(IntLiteral(y))),
              PreTransLit(IntLiteral(z))) =>
            val dist = Math.min((y & 31) + (z & 31), 31)
            PreTransBinaryOp(Int_>>, x, PreTransLit(IntLiteral(dist)))

          case (PreTransBinaryOp(Int_>>>, x, PreTransLit(IntLiteral(y))),
              PreTransLit(IntLiteral(_))) if (y & 31) != 0 =>
            foldBinaryOp(Int_>>>, lhs, rhs)

          case (_, PreTransLit(IntLiteral(y))) =>
            val dist = y & 31
            if (dist == 0)
              lhs
            else
              PreTransBinaryOp(Int_>>, lhs, PreTransLit(IntLiteral(dist)))

          case _ => default
        }

      case Long_+ =>
        (lhs, rhs) match {
          case (_, PreTransLit(LongLiteral(_))) => foldBinaryOp(Long_+, rhs, lhs)
          case (PreTransLit(LongLiteral(0)), _) => rhs

          case (PreTransLit(LongLiteral(x)),
              PreTransBinaryOp(innerOp @ (Long_+ | Long_-),
                  PreTransLit(LongLiteral(y)), z)) =>
            foldBinaryOp(innerOp, PreTransLit(LongLiteral(x + y)), z)

          case _ => default
        }

      case Long_- =>
        (lhs, rhs) match {
          case (_, PreTransLit(LongLiteral(r))) =>
            foldBinaryOp(Long_+, PreTransLit(LongLiteral(-r)), lhs)

          case (PreTransLit(LongLiteral(x)),
              PreTransBinaryOp(Long_+, PreTransLit(LongLiteral(y)), z)) =>
            foldBinaryOp(Long_-, PreTransLit(LongLiteral(x - y)), z)
          case (PreTransLit(LongLiteral(x)),
              PreTransBinaryOp(Long_-, PreTransLit(LongLiteral(y)), z)) =>
            foldBinaryOp(Long_+, PreTransLit(LongLiteral(x - y)), z)

          case (_, PreTransBinaryOp(BinaryOp.Long_-,
              PreTransLit(LongLiteral(0L)), x)) =>
            foldBinaryOp(Long_+, lhs, x)

          case _ => default
        }

      case Long_* =>
        (lhs, rhs) match {
          case (_, PreTransLit(LongLiteral(_))) =>
            foldBinaryOp(Long_*, rhs, lhs)

          case (PreTransLit(LongLiteral(x)), _) =>
            x match {
              case -1L => foldBinaryOp(Long_-, PreTransLit(LongLiteral(0)), rhs)
              case 0L  => PreTransBlock(finishTransformStat(rhs), lhs)
              case 1L  => rhs

              // Exact power of 2
              case _ if (x & (x - 1L)) == 0L =>
                /* Note that this would match 0L, but 0L is handled above.
                 * It will also match Long.MinValue, but that is not a problem
                 * as the optimization also works (if you need convincing,
                 * simply interpret the multiplication as unsigned).
                 */
                foldBinaryOp(Long_<<, rhs, PreTransLit(
                    IntLiteral(java.lang.Long.numberOfTrailingZeros(x))))

              case _ => default
            }

          case _ => default
        }

      case Long_/ =>
        (lhs, rhs) match {
          case (_, PreTransLit(LongLiteral(1))) =>
            lhs
          case (_, PreTransLit(LongLiteral(-1))) =>
            foldBinaryOp(Long_-, PreTransLit(LongLiteral(0)), lhs)

          case (LongFromInt(x), LongFromInt(PreTransLit(y: IntLiteral)))
              if y.value != -1 =>
            LongFromInt(foldBinaryOp(Int_/, x, PreTransLit(y)))

          case _ => default
        }

      case Long_% =>
        (lhs, rhs) match {
          case (_, PreTransLit(LongLiteral(1L | -1L))) =>
            Block(finishTransformStat(lhs), LongLiteral(0L)).toPreTransform

          case (LongFromInt(x), LongFromInt(y)) =>
            LongFromInt(foldBinaryOp(Int_%, x, y))

          case _ => default
        }

      case Long_| =>
        (lhs, rhs) match {
          case (_, PreTransLit(LongLiteral(_))) =>
            foldBinaryOp(Long_|, rhs, lhs)
          case (PreTransLit(LongLiteral(0)), _) =>
            rhs

          case (PreTransLit(LongLiteral(-1)), _) =>
            PreTransBlock(finishTransformStat(rhs), lhs)

          case (PreTransLit(LongLiteral(x)),
              PreTransBinaryOp(Long_|, PreTransLit(LongLiteral(y)), z)) =>
            foldBinaryOp(Long_|, PreTransLit(LongLiteral(x | y)), z)

          case _ => default
        }

      case Long_& =>
        (lhs, rhs) match {
          case (_, PreTransLit(LongLiteral(_))) =>
            foldBinaryOp(Long_&, rhs, lhs)
          case (PreTransLit(LongLiteral(-1)), _) =>
            rhs

          case (PreTransLit(LongLiteral(0)), _) =>
            PreTransBlock(finishTransformStat(rhs), lhs)

          case (PreTransLit(LongLiteral(x)),
              PreTransBinaryOp(Long_&, PreTransLit(LongLiteral(y)), z)) =>
            foldBinaryOp(Long_&, PreTransLit(LongLiteral(x & y)), z)

          case _ => default
        }

      case Long_^ =>
        (lhs, rhs) match {
          case (_, PreTransLit(LongLiteral(_))) =>
            foldBinaryOp(Long_^, rhs, lhs)
          case (PreTransLit(LongLiteral(0)), _) =>
            rhs

          case (PreTransLit(LongLiteral(x)),
              PreTransBinaryOp(Long_^, PreTransLit(LongLiteral(y)), z)) =>
            foldBinaryOp(Long_^, PreTransLit(LongLiteral(x ^ y)), z)

          case _ => default
        }

      case Long_<< =>
        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(x))) if x % 64 == 0 => lhs

          case _ => default
        }

      case Long_>>> =>
        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(x))) if x % 64 == 0 => lhs

          case _ => default
        }

      case Long_>> =>
        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(x))) if x % 64 == 0 => lhs

          case _ => default
        }

      case Long_== | Long_!= =>
        val positive = (op == Long_==)
        (lhs, rhs) match {
          case (LongFromInt(x), LongFromInt(y)) =>
            foldBinaryOp(if (positive) === else !==, x, y)
          case (LongFromInt(x), PreTransLit(LongLiteral(y))) =>
            assert(y > Int.MaxValue || y < Int.MinValue)
            Block(finishTransformStat(x),
                BooleanLiteral(!positive)).toPreTransform

          case (PreTransBinaryOp(Long_+, PreTransLit(LongLiteral(x)), y),
              PreTransLit(LongLiteral(z))) =>
            foldBinaryOp(op, y, PreTransLit(LongLiteral(z - x)))
          case (PreTransBinaryOp(Long_-, PreTransLit(LongLiteral(x)), y),
              PreTransLit(LongLiteral(z))) =>
            foldBinaryOp(op, y, PreTransLit(LongLiteral(x - z)))

          case (PreTransBinaryOp(Long_^, PreTransLit(LongLiteral(x)), y),
              PreTransLit(LongLiteral(z))) =>
            foldBinaryOp(op, y, PreTransLit(LongLiteral(x ^ z)))

          case (PreTransLit(LongLiteral(_)), _) => foldBinaryOp(op, rhs, lhs)

          case _ => default
        }

      case Long_< | Long_<= | Long_> | Long_>= =>
        def flippedOp = (op: @switch) match {
          case Long_<  => Long_>
          case Long_<= => Long_>=
          case Long_>  => Long_<
          case Long_>= => Long_<=
        }

        def intOp = (op: @switch) match {
          case Long_<  => Int_<
          case Long_<= => Int_<=
          case Long_>  => Int_>
          case Long_>= => Int_>=
        }

        (lhs, rhs) match {
          case (_, PreTransLit(LongLiteral(Long.MinValue))) =>
            if (op == Long_< || op == Long_>=) {
              Block(finishTransformStat(lhs),
                  BooleanLiteral(op == Long_>=)).toPreTransform
            } else {
              foldBinaryOp(if (op == Long_<=) Long_== else Long_!=, lhs, rhs)
            }

          case (_, PreTransLit(LongLiteral(Long.MaxValue))) =>
            if (op == Long_> || op == Long_<=) {
              Block(finishTransformStat(lhs),
                  BooleanLiteral(op == Long_<=)).toPreTransform
            } else {
              foldBinaryOp(if (op == Long_>=) Long_== else Long_!=, lhs, rhs)
            }

          case (LongFromInt(x), LongFromInt(y)) =>
            foldBinaryOp(intOp, x, y)
          case (LongFromInt(x), PreTransLit(LongLiteral(y))) =>
            assert(y > Int.MaxValue || y < Int.MinValue)
            val result =
              if (y > Int.MaxValue) op == Long_< || op == Long_<=
              else                  op == Long_> || op == Long_>=
            Block(finishTransformStat(x), BooleanLiteral(result)).toPreTransform

          /* x + y.toLong > z
           *      -x on both sides
           *      requires x + y.toLong not to overflow, and z - x likewise
           * y.toLong > z - x
           */
          case (PreTransBinaryOp(Long_+, PreTransLit(LongLiteral(x)), y @ LongFromInt(_)),
              PreTransLit(LongLiteral(z)))
              if canAddLongs(x, Int.MinValue) &&
                 canAddLongs(x, Int.MaxValue) &&
                 canSubtractLongs(z, x) =>
            foldBinaryOp(op, y, PreTransLit(LongLiteral(z-x)))

          /* x - y.toLong > z
           *      -x on both sides
           *      requires x - y.toLong not to overflow, and z - x likewise
           * -(y.toLong) > z - x
           */
          case (PreTransBinaryOp(Long_-, PreTransLit(LongLiteral(x)), y @ LongFromInt(_)),
              PreTransLit(LongLiteral(z)))
              if canSubtractLongs(x, Int.MinValue) &&
                 canSubtractLongs(x, Int.MaxValue) &&
                 canSubtractLongs(z, x) =>
            if (z-x != Long.MinValue) {
              // Since -(y.toLong) does not overflow, we can negate both sides
              foldBinaryOp(flippedOp, y, PreTransLit(LongLiteral(-(z-x))))
            } else {
              /* -(y.toLong) > Long.MinValue
               * Depending on the operator, this is either always true or
               * always false.
               */
              val result = (op == Long_>) || (op == Long_>=)
              Block(finishTransformStat(y),
                  BooleanLiteral(result)).toPreTransform
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
          case (PreTransBinaryOp(Long_+, LongFromInt(x), LongFromInt(y)),
              PreTransLit(LongLiteral(Int.MaxValue))) =>
            trampoline {
              /* HACK: We use an empty scope here for `withNewLocalDefs`.
               * It's OKish to do that because we're only defining Ints, and
               * we know withNewLocalDefs is not going to try and inline things
               * when defining ints, so we cannot go into infinite inlining.
               */
              val emptyScope = Scope.Empty

              withNewLocalDefs(List(
                  Binding("x", None, IntType, false, x),
                  Binding("y", None, IntType, false, y))) {
                (tempsLocalDefs, cont) =>
                  val List(tempXDef, tempYDef) = tempsLocalDefs
                  val tempX = tempXDef.newReplacement
                  val tempY = tempYDef.newReplacement
                  cont(AndThen(AndThen(
                      BinaryOp(Int_>, tempX, IntLiteral(0)),
                      BinaryOp(Int_>, tempY, IntLiteral(0))),
                      BinaryOp(Int_<, BinaryOp(Int_+, tempX, tempY), IntLiteral(0))
                  ).toPreTransform)
              } (finishTransform(isStat = false))(emptyScope)
            }.toPreTransform

          case (PreTransLit(LongLiteral(_)), _) =>
            foldBinaryOp(flippedOp, rhs, lhs)

          case _ => default
        }

      case Float_+ =>
        (lhs, rhs) match {
          case (PreTransLit(FloatLiteral(0)), _) =>
            rhs
          case (_, PreTransLit(FloatLiteral(_))) =>
            foldBinaryOp(Float_+, rhs, lhs)

          case (PreTransLit(FloatLiteral(x)),
              PreTransBinaryOp(innerOp @ (Float_+ | Float_-),
                  PreTransLit(FloatLiteral(y)), z)) =>
            foldBinaryOp(innerOp, PreTransLit(FloatLiteral(x + y)), z)

          case _ => default
        }

      case Float_- =>
        (lhs, rhs) match {
          case (_, PreTransLit(FloatLiteral(r))) =>
            foldBinaryOp(Float_+, lhs, PreTransLit(FloatLiteral(-r)))

          case (PreTransLit(FloatLiteral(x)),
              PreTransBinaryOp(Float_+, PreTransLit(FloatLiteral(y)), z)) =>
            foldBinaryOp(Float_-, PreTransLit(FloatLiteral(x - y)), z)
          case (PreTransLit(FloatLiteral(x)),
              PreTransBinaryOp(Float_-, PreTransLit(FloatLiteral(y)), z)) =>
            foldBinaryOp(Float_+, PreTransLit(FloatLiteral(x - y)), z)

          case (_, PreTransBinaryOp(BinaryOp.Float_-,
              PreTransLit(FloatLiteral(0)), x)) =>
            foldBinaryOp(Float_+, lhs, x)

          case _ => default
        }

      case Float_* =>
        (lhs, rhs) match {
          case (_, PreTransLit(FloatLiteral(_))) =>
            foldBinaryOp(Float_*, rhs, lhs)

          case (PreTransLit(FloatLiteral(1)), _) =>
            rhs
          case (PreTransLit(FloatLiteral(-1)), _) =>
            foldBinaryOp(Float_-, PreTransLit(FloatLiteral(0)), rhs)

          case _ => default
        }

      case Float_/ =>
        (lhs, rhs) match {
          case (_, PreTransLit(FloatLiteral(1))) =>
            lhs
          case (_, PreTransLit(FloatLiteral(-1))) =>
            foldBinaryOp(Float_-, PreTransLit(FloatLiteral(0)), lhs)

          case _ => default
        }

      case Float_% =>
        (lhs, rhs) match {
          case _ => default
        }

      case Double_+ =>
        (lhs, rhs) match {
          case (PreTransLit(DoubleLiteral(0)), _) =>
            rhs
          case (_, PreTransLit(DoubleLiteral(_))) =>
            foldBinaryOp(Double_+, rhs, lhs)

          case (PreTransLit(DoubleLiteral(x)),
              PreTransBinaryOp(innerOp @ (Double_+ | Double_-),
                  PreTransLit(DoubleLiteral(y)), z)) =>
            foldBinaryOp(innerOp, PreTransLit(DoubleLiteral(x + y)), z)

          case _ => default
        }

      case Double_- =>
        (lhs, rhs) match {
          case (_, PreTransLit(DoubleLiteral(r))) =>
            foldBinaryOp(Double_+, lhs, PreTransLit(DoubleLiteral(-r)))

          case (PreTransLit(DoubleLiteral(x)),
              PreTransBinaryOp(Double_+, PreTransLit(DoubleLiteral(y)), z)) =>
            foldBinaryOp(Double_-, PreTransLit(DoubleLiteral(x - y)), z)

          case (PreTransLit(DoubleLiteral(x)),
              PreTransBinaryOp(Double_-, PreTransLit(DoubleLiteral(y)), z)) =>
            foldBinaryOp(Double_+, PreTransLit(DoubleLiteral(x - y)), z)

          case (_, PreTransBinaryOp(BinaryOp.Double_-,
              PreTransLit(DoubleLiteral(0)), x)) =>
            foldBinaryOp(Double_+, lhs, x)

          case _ => default
        }

      case Double_* =>
        (lhs, rhs) match {
          case (_, PreTransLit(DoubleLiteral(_))) =>
            foldBinaryOp(Double_*, rhs, lhs)

          case (PreTransLit(DoubleLiteral(1)), _) =>
            rhs
          case (PreTransLit(DoubleLiteral(-1)), _) =>
            foldBinaryOp(Double_-, PreTransLit(DoubleLiteral(0)), rhs)

          case _ => default
        }

      case Double_/ =>
        (lhs, rhs) match {
          case (_, PreTransLit(DoubleLiteral(1))) =>
            lhs
          case (_, PreTransLit(DoubleLiteral(-1))) =>
            foldBinaryOp(Double_-, PreTransLit(DoubleLiteral(0)), lhs)

          case _ => default
        }

      case Double_% =>
        (lhs, rhs) match {
          case _ => default
        }

      case Boolean_== | Boolean_!= =>
        val positive = (op == Boolean_==)
        (lhs, rhs) match {
          case (PreTransLit(_), _) =>
            foldBinaryOp(op, rhs, lhs)

          case (PreTransLit(BooleanLiteral(l)), _) =>
            if (l == positive) rhs
            else foldUnaryOp(UnaryOp.Boolean_!, rhs)

          case _ =>
            default
        }

      case Boolean_| =>
        (lhs, rhs) match {
          case (_, PreTransLit(BooleanLiteral(false))) => lhs
          case (PreTransLit(BooleanLiteral(false)), _) => rhs

          case _ => default
        }

      case Boolean_& =>
        (lhs, rhs) match {
          case (_, PreTransLit(BooleanLiteral(true))) => lhs
          case (PreTransLit(BooleanLiteral(true)), _) => rhs

          case _ => default
        }

      case Int_== | Int_!= =>
        (lhs, rhs) match {
          case (PreTransBinaryOp(Int_+, PreTransLit(IntLiteral(x)), y),
              PreTransLit(IntLiteral(z))) =>
            foldBinaryOp(op, y, PreTransLit(IntLiteral(z - x)))

          case (PreTransBinaryOp(Int_-, PreTransLit(IntLiteral(x)), y),
              PreTransLit(IntLiteral(z))) =>
            foldBinaryOp(op, y, PreTransLit(IntLiteral(x - z)))

          case (PreTransBinaryOp(Int_^, PreTransLit(IntLiteral(x)), y),
              PreTransLit(IntLiteral(z))) =>
            foldBinaryOp(op, y, PreTransLit(IntLiteral(x ^ z)))

          case (PreTransLit(_), _) => foldBinaryOp(op, rhs, lhs)

          case _ => default
        }

      case Int_< | Int_<= | Int_> | Int_>= =>
        def flippedOp = (op: @switch) match {
          case Int_<  => Int_>
          case Int_<= => Int_>=
          case Int_>  => Int_<
          case Int_>= => Int_<=
        }

        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(y))) =>
            y match {
              case Int.MinValue =>
                if (op == Int_< || op == Int_>=) {
                  Block(finishTransformStat(lhs),
                      BooleanLiteral(op == Int_>=)).toPreTransform
                } else {
                  foldBinaryOp(if (op == Int_<=) Int_== else Int_!=, lhs, rhs)
                }

              case Int.MaxValue =>
                if (op == Int_> || op == Int_<=) {
                  Block(finishTransformStat(lhs),
                      BooleanLiteral(op == Int_<=)).toPreTransform
                } else {
                  foldBinaryOp(if (op == Int_>=) Int_== else Int_!=, lhs, rhs)
                }

              case _ if y == Int.MinValue + 1 && (op == Int_< || op == Int_>=) =>
                foldBinaryOp(if (op == Int_<) Int_== else Int_!=, lhs,
                    PreTransLit(IntLiteral(Int.MinValue)))

              case _ if y == Int.MaxValue - 1 && (op == Int_> || op == Int_<=) =>
                foldBinaryOp(if (op == Int_>) Int_== else Int_!=, lhs,
                    PreTransLit(IntLiteral(Int.MaxValue)))

              case _ => default
            }

          case (PreTransLit(IntLiteral(_)), _) =>
            foldBinaryOp(flippedOp, rhs, lhs)

          case _ => default
        }

      case _ =>
        default
    }
  }

  private def fold3WayIntComparison(canBeEqual: Boolean, canBeLessThan: Boolean,
      canBeGreaterThan: Boolean, lhs: PreTransform, rhs: PreTransform)(
      implicit pos: Position): PreTransform = {
    import BinaryOp._
    if (canBeEqual) {
      if (canBeLessThan) {
        if (canBeGreaterThan) {
          Block(
              finishTransformStat(lhs),
              finishTransformStat(rhs),
              BooleanLiteral(true)).toPreTransform
        } else {
          foldBinaryOp(Int_<=, lhs, rhs)
        }
      } else {
        if (canBeGreaterThan)
          foldBinaryOp(Int_>=, lhs, rhs)
        else
          foldBinaryOp(Int_==, lhs, rhs)
      }
    } else {
      if (canBeLessThan) {
        if (canBeGreaterThan)
          foldBinaryOp(Int_!=, lhs, rhs)
        else
          foldBinaryOp(Int_<, lhs, rhs)
      } else {
        if (canBeGreaterThan) {
          foldBinaryOp(Int_>, lhs, rhs)
        } else {
          Block(
              finishTransformStat(lhs),
              finishTransformStat(rhs),
              BooleanLiteral(false)).toPreTransform
        }
      }
    }
  }

  private def foldUnbox(arg: PreTransform, charCode: Char)(
      cont: PreTransCont): TailRec[Tree] = {
    (charCode: @switch) match {
      case 'Z' if arg.tpe.base == BooleanType => cont(arg)
      case 'C' if arg.tpe.base == CharType    => cont(arg)
      case 'B' if arg.tpe.base == ByteType    => cont(arg)
      case 'S' if arg.tpe.base == ShortType   => cont(arg)
      case 'I' if arg.tpe.base == IntType     => cont(arg)
      case 'F' if arg.tpe.base == FloatType   => cont(arg)
      case 'J' if arg.tpe.base == LongType    => cont(arg)
      case 'D' if arg.tpe.base == DoubleType  => cont(arg)
      case _ =>
        cont(Unbox(finishTransformExpr(arg), charCode)(arg.pos).toPreTransform)
    }
  }

  private def foldJSSelect(qualifier: Tree, item: Tree)(
      implicit pos: Position): Tree = {
    // !!! Must be in sync with scala.scalajs.runtime.LinkingInfo

    import config.coreSpec._

    @inline def default =
      JSSelect(qualifier, item)

    (qualifier, item) match {
      case (JSSelect(JSLinkingInfo(), StringLiteral("semantics")),
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
          case "arrayIndexOutOfBounds" =>
            behavior2IntLiteral(semantics.arrayIndexOutOfBounds)
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
        BooleanLiteral(esFeatures.useECMAScript2015)

      case (JSLinkingInfo(), StringLiteral("version")) =>
        StringLiteral(ScalaJSVersions.current)

      case _ =>
        default
    }
  }

  def transformIsolatedBody(optTarget: Option[MethodID],
      thisType: Type, params: List[ParamDef], resultType: Type,
      body: Tree,
      alreadyInlining: Set[Scope.InliningID]): (List[ParamDef], Tree) = {
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

    val inlining = optTarget.fold(alreadyInlining) { target =>
      val allocationSites =
        List.fill(allLocalDefs.size)(AllocationSite.Anonymous)
      alreadyInlining + ((allocationSites, target))
    }
    val scope = Scope.Empty
      .inlining(inlining)
      .withEnv(OptEnv.Empty.withLocalDefs(allLocalDefs))
    val newBody =
      transform(body, resultType == NoType)(scope)

    (newParamDefs, newBody)
  }

  private def pretransformLabeled(oldLabelName: String, resultType: Type,
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
        pretransformLabeled(oldLabelName, resultType, body, isStat,
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
   *      GenJSCode.genOptimizedMatchEndLabeled.
   */
  def tryOptimizePatternMatch(oldLabelName: String, refinedType: Type,
      returnCount: Int, body: Tree): Option[Tree] = {
    if (!oldLabelName.startsWith("matchEnd")) {
      None
    } else {
      body match {
        case Block(stats) =>
          @tailrec
          def createRevAlts(xs: List[Tree],
              acc: List[(Tree, Tree)]): (List[(Tree, Tree)], Tree) = xs match {
            case If(cond, body, Skip()) :: xr =>
              createRevAlts(xr, (cond, body) :: acc)
            case remaining =>
              (acc, Block(remaining)(remaining.head.pos))
          }
          val (revAlts, elsep) = createRevAlts(stats, Nil)

          if (revAlts.size == returnCount - 1) {
            def tryDropReturn(body: Tree): Option[Tree] = body match {
              case BlockOrAlone(prep, Return(result, _)) =>
                val result1 =
                  if (refinedType == NoType) keepOnlySideEffects(result)
                  else result
                Some(Block(prep :+ result1)(body.pos))

              case _ =>
                None
            }

            @tailrec
            def constructOptimized(revAlts: List[(Tree, Tree)],
                elsep: Tree): Option[Tree] = {
              revAlts match {
                case (cond, body) :: revAltsRest =>
                  // cannot use flatMap due to tailrec
                  tryDropReturn(body) match {
                    case Some(newBody) =>
                      constructOptimized(revAltsRest,
                          foldIf(cond, newBody, elsep)(refinedType)(cond.pos))

                    case None =>
                      None
                  }
                case Nil =>
                  Some(elsep)
              }
            }

            tryDropReturn(elsep).flatMap(constructOptimized(revAlts, _))
          } else {
            None
          }
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
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
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
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = tailcall {
    val Binding(name, originalName, declaredType, mutable, value) = binding
    implicit val pos = value.pos

    def withDedicatedVar(tpe: RefinedType): TailRec[Tree] = {
      val rtLongClassType = ClassType(LongImpl.RuntimeLongClass)

      if (tpe.base == LongType && declaredType != rtLongClassType &&
          useRuntimeLong) {
        /* If the value's type is a primitive Long, and the declared type is
         * not RuntimeLong, we want to force the expansion of the primitive
         * Long (which we know is in fact a RuntimeLong) into a local variable,
         * and then its two components into a Record. This makes sure that all
         * Longs are stack-allocated when they are put in a var/val, even if
         * they came from a method call or other opaque sources, and also if a
         * var is initialized with a literal long.
         *
         * We only do all that if the library contains a inlineable version of
         * RuntimeLong.
         */
        expandLongValue(value) { expandedValue =>
          val expandedBinding = Binding(name, originalName, rtLongClassType,
              mutable, expandedValue)
          withNewLocalDef(expandedBinding)(buildInner)(cont)
        }
      } else {
        // Otherwise, we effectively declare a new binding
        val newName = freshLocalName(name, mutable)
        val newOriginalName = originalName.orElse(Some(name))

        val used = newSimpleState(false)

        val (replacement, refinedType) = resolveRecordType(value) match {
          case Some((recordType, cancelFun)) =>
            (ReplaceWithRecordVarRef(newName, newOriginalName, recordType,
                used, cancelFun), value.tpe)

          case None =>
            (ReplaceWithVarRef(newName, newOriginalName, used, None), tpe)
        }

        val localDef = LocalDef(refinedType, mutable, replacement)
        val preTransBinding = PreTransBinding(localDef, value)

        buildInner(localDef, { tinner =>
          cont(addPreTransBinding(preTransBinding, tinner))
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
        case PreTransBlock(bindingsAndStats, result) =>
          withNewLocalDef(binding.copy(value = result))(buildInner) { tresult =>
            cont(addPreTransBindings(bindingsAndStats, tresult))
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

  /** Adds a [[PreTransBinding]] in front a result [[PreTransform]].
   *
   *  This can force the binding if the result is a [[PreTransGenTree]].
   */
  private def addPreTransBinding(binding: PreTransBinding,
      result: PreTransform): PreTransform = {
    /* This is not the same as
     *   addPreTransBindings(Left(binding) :: Nil, result)
     * because this function is able to optimize the case
     *   result: PreTransLocalDef
     * if `!result.contains(binding) && !binding.isAlreadyUsed`.
     */
    result match {
      case result: PreTransResult
          if !result.contains(binding.localDef) && !binding.isAlreadyUsed =>
        /* Eager dce of the binding to avoid unnecessary nesting in
         * PreTransBlock, for better optimization.
         */
        PreTransBlock(finishTransformStat(binding.value), result)

      case _ =>
        addPreTransBindings(Left(binding) :: Nil, result)
    }
  }

  /** Adds a sequence of [[PreTransBinding]]s and statements in front a result
   *  [[PreTransform]].
   *
   *  This can force the bindings if the result is a [[PreTransGenTree]].
   */
  private def addPreTransBindings(bindingsAndStats: List[BindingOrStat],
      result: PreTransform): PreTransform = {
    result match {
      case result: PreTransBlock =>
        PreTransBlock(bindingsAndStats, result)
      case result: PreTransResult =>
        PreTransBlock(bindingsAndStats, result)
      case PreTransRecordTree(tree, tpe, cancelFun) =>
        PreTransRecordTree(
            finishTransformBindings(bindingsAndStats, tree),
            tpe, cancelFun)
      case PreTransTree(tree, tpe) =>
        PreTransTree(
            finishTransformBindings(bindingsAndStats, tree),
            tpe)
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

  /** Converts a `TypeRef` on the rhs of an `In/AsInstanceOf` to the
   *  corresponding `Type`.
   */
  private def isAsInstanceOfTypeRefToType(typeRef: TypeRef): Type = {
    /* Since ClassRefs of primitive types and JS types are not admissible
     * in an Is- or AsInstanceOf node, we can translate all of them to
     * their corresponding ClassType (with the exception of j.l.Object).
     *
     * TODO Investigate whether Null and Nothing should be handled specially
     * here.
     */
    typeRef match {
      case ClassRef(ObjectClass) => AnyType
      case ClassRef(className)   => ClassType(className)
      case typeRef: ArrayTypeRef => ArrayType(typeRef)
    }
  }

  /** Trampolines a pretransform */
  private def trampoline(tailrec: => TailRec[Tree]): Tree = {
    // scalastyle:off return

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

            localNameAllocator.restore(e.localNameAllocatorSnapshot)
            mutableLocalNames = e.savedMutableLocalNames
            labelNameAllocator.restore(e.labelNameAllocatorSnapshot)

            val savedStateBackupChain = e.savedStateBackupChain
            var stateBackupsToRestore = stateBackupChain
            while (stateBackupsToRestore ne savedStateBackupChain) {
              stateBackupsToRestore.head.restore()
              stateBackupsToRestore = stateBackupsToRestore.tail
            }
            stateBackupChain = savedStateBackupChain

            rec = e.cont
        }
      }

      throw new AssertionError("Reached end of infinite loop")
    } finally {
      curTrampolineId -= 1
    }

    // scalastyle:on return
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
      isNullable: Boolean)(val allocationSite: AllocationSite, dummy: Int = 0) {

    def isNothingType: Boolean = base == NothingType
  }

  private object RefinedType {
    def apply(base: Type, isExact: Boolean, isNullable: Boolean,
        allocationSite: AllocationSite): RefinedType =
      new RefinedType(base, isExact, isNullable)(allocationSite)

    def apply(base: Type, isExact: Boolean, isNullable: Boolean): RefinedType =
      RefinedType(base, isExact, isNullable, AllocationSite.Anonymous)

    def apply(tpe: Type): RefinedType = tpe match {
      case AnyType | ClassType(_) | ArrayType(_) =>
        RefinedType(tpe, isExact = false, isNullable = true)
      case NullType =>
        RefinedType(tpe, isExact = true, isNullable = true)
      case NothingType | UndefType | BooleanType | CharType | LongType |
          StringType | NoType =>
        RefinedType(tpe, isExact = true, isNullable = false)
      case ByteType | ShortType | IntType | FloatType | DoubleType |
          RecordType(_) =>
        /* At run-time, a byte will answer true to `x.isInstanceOf[Int]`,
         * therefore `byte`s must be non-exact. The same reasoning applies to
         * other primitive numberic types.
         */
        RefinedType(tpe, isExact = false, isNullable = false)
    }

    val NoRefinedType = RefinedType(NoType)
    val Nothing = RefinedType(NothingType)
  }

  /**
   *  Global, lexical identity of an inlined object, given by the source
   *  location of its allocation.
   *
   *  A crucial property of AllocationSite is that there is a finite amount of
   *  them, function of the program source. It is not permitted to create
   *  AllocationSites out of trees generated by the optimizer, as it would
   *  potentially grow the supply to an infinite amount.
   */
  private sealed abstract class AllocationSite

  private object AllocationSite {
    object Anonymous extends AllocationSite {
      override def toString(): String = "AllocationSite(<anonymous>)"
    }

    def Tree(tree: Tree): AllocationSite = new TreeAllocationSite(tree)

    private class TreeAllocationSite(
        private val node: Tree) extends AllocationSite {
      override def equals(that: Any): Boolean = that match {
        case that: TreeAllocationSite => this.node eq that.node
        case _                        => false
      }

      override def hashCode(): Int =
        System.identityHashCode(node)

      override def toString(): String =
        s"AllocationSite($node)"
    }
  }

  private case class LocalDef(
      tpe: RefinedType,
      mutable: Boolean,
      replacement: LocalDefReplacement) {

    def toPreTransform(implicit pos: Position): PreTransform = {
      replacement match {
        case ReplaceWithConstant(value) => PreTransTree(value)
        case _                          => PreTransLocalDef(this)
      }
    }

    def newReplacement(implicit pos: Position): Tree = replacement match {
      case ReplaceWithVarRef(name, originalName, used, _) =>
        used.value = true
        VarRef(Ident(name, originalName))(tpe.base)

      /* Allocate an instance of RuntimeLong on the fly.
       * See the comment in finishTransformExpr about why it is desirable and
       * safe to do so.
       */
      case ReplaceWithRecordVarRef(name, originalName, recordType, used, _)
          if tpe.base == ClassType(LongImpl.RuntimeLongClass) =>
        used.value = true
        createNewLong(VarRef(Ident(name, originalName))(recordType))

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

      /* Allocate an instance of RuntimeLong on the fly.
       * See the comment in finishTransformExpr about why it is desirable and
       * safe to do so.
       */
      case InlineClassInstanceReplacement(recordType, fieldLocalDefs, _)
          if tpe.base == ClassType(LongImpl.RuntimeLongClass) =>
        val List(loField, hiField) = recordType.fields
        val lo = fieldLocalDefs(loField.name).newReplacement
        val hi = fieldLocalDefs(hiField.name).newReplacement
        createNewLong(lo, hi)

      case InlineClassInstanceReplacement(_, _, cancelFun) =>
        cancelFun()

      case InlineJSArrayReplacement(_, cancelFun) =>
        cancelFun()
    }

    def contains(that: LocalDef): Boolean = {
      (this eq that) || (replacement match {
        case TentativeClosureReplacement(_, _, _, captureLocalDefs, _, _) =>
          captureLocalDefs.exists(_.contains(that))
        case InlineClassInstanceReplacement(_, fieldLocalDefs, _) =>
          fieldLocalDefs.valuesIterator.exists(_.contains(that))
        case InlineJSArrayReplacement(elemLocalDefs, _) =>
          elemLocalDefs.exists(_.contains(that))
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

  private final case class InlineJSArrayReplacement(
      elemLocalDefs: Vector[LocalDef],
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
      val implsBeingInlined: Set[Scope.InliningID]) {
    def withEnv(env: OptEnv): Scope =
      new Scope(env, implsBeingInlined)

    def inlining(impl: Scope.InliningID): Scope = {
      assert(!implsBeingInlined(impl), s"Circular inlining of $impl")
      new Scope(env, implsBeingInlined + impl)
    }

    def inlining(impls: Set[Scope.InliningID]): Scope = {
      val intersection = implsBeingInlined.intersect(impls)
      assert(intersection.isEmpty, s"Circular inlining of $intersection")
      new Scope(env, implsBeingInlined ++ impls)
    }
  }

  private object Scope {
    type InliningID = (List[AllocationSite], AbstractMethodID)

    val Empty: Scope = new Scope(OptEnv.Empty, Set.empty)
  }

  /** The result of pretransformExpr().
   *
   *  A `PreTransform` is a virtualized representation of an expression. It
   *  serves two major purposes:
   *  - Holding references to virtual objects that are being partially
   *    evaluated (see notably `PreTransLocalDef`) or stack-allocated as
   *    records (see notably `PreTransRecordTree`).
   *  - Keep arguments of nodes that are potentially side-effect-free as
   *    virtual as possible, so that, should their value not be used, the
   *    variables that are referenced can also be dead-code-eliminated.
   *
   *  A `PreTransform` has a `tpe` as precisely refined as if a full
   *  `transformExpr()` had been performed.
   *  It is also not dependent on the environment anymore. In some sense, it
   *  has "captured" its environment at definition site.
   */
  private sealed abstract class PreTransform {
    def pos: Position
    val tpe: RefinedType

    def contains(localDef: LocalDef): Boolean = this match {
      case PreTransBlock(bindingsAndStats, result) =>
        result.contains(localDef) || bindingsAndStats.exists {
          case Left(PreTransBinding(_, value)) => value.contains(localDef)
          case Right(_)                        => false
        }
      case PreTransUnaryOp(_, lhs) =>
        lhs.contains(localDef)
      case PreTransBinaryOp(_, lhs, rhs) =>
        lhs.contains(localDef) || rhs.contains(localDef)
      case PreTransLocalDef(thisLocalDef) =>
        thisLocalDef.contains(localDef)
      case _: PreTransGenTree =>
        false
    }
  }

  /** A pretransformed binding, part of a [[PreTransBlock]].
   *
   *  Even though it is not encoded in the type system, `localDef.replacement`
   *  must be a [[ReplaceWithVarRef]] or a [[ReplaceWithRecordVarRef]].
   */
  private final case class PreTransBinding(localDef: LocalDef,
      value: PreTransform) {

    assert(
        localDef.replacement.isInstanceOf[ReplaceWithVarRef] ||
        localDef.replacement.isInstanceOf[ReplaceWithRecordVarRef],
        "Cannot create a PreTransBinding with non-var-ref replacement " +
        localDef.replacement)

    def isAlreadyUsed: Boolean = (localDef.replacement: @unchecked) match {
      case ReplaceWithVarRef(_, _, used, _)          => used.value
      case ReplaceWithRecordVarRef(_, _, _, used, _) => used.value
    }
  }

  private type BindingOrStat = Either[PreTransBinding, Tree]

  /** A pretransformed block, with bindings and statements followed by a result.
   *
   *  Statements in `bindingsAndStats` should have been stripped of their
   *  side-effect-free parts. They should not be `VarDef`s.
   *
   *  Variable definitions should be `PreTransBinding`s instead, so that they
   *  can eventually be dead-code-eliminated should their value never be used.
   */
  private final class PreTransBlock private (
      val bindingsAndStats: List[BindingOrStat],
      val result: PreTransResult) extends PreTransform {
    def pos: Position = result.pos
    val tpe = result.tpe

    assert(bindingsAndStats.nonEmpty)

    override def toString(): String =
      s"PreTransBlock($bindingsAndStats,$result)"
  }

  private object PreTransBlock {
    def apply(bindingsAndStats: List[BindingOrStat],
        result: PreTransResult): PreTransform = {
      if (bindingsAndStats.isEmpty) result
      else new PreTransBlock(bindingsAndStats, result)
    }

    def apply(bindingsAndStats: List[BindingOrStat],
        result: PreTransBlock): PreTransform = {
      new PreTransBlock(bindingsAndStats ::: result.bindingsAndStats,
          result.result)
    }

    def apply(binding: PreTransBinding, result: PreTransBlock): PreTransform = {
      new PreTransBlock(Left(binding) :: result.bindingsAndStats, result.result)
    }

    @deprecated(
        "You shouldn't be trying to create a PreTransBlock from a Tree stat " +
        "when the static type of the result is already a PreTransRecordTree. " +
        "Prefer directly creating the relevant PreTransRecordTree",
        "forever")
    def apply(stat: Tree, result: PreTransRecordTree): PreTransRecordTree = {
      PreTransRecordTree(Block(stat, result.tree)(result.pos), result.tpe,
          result.cancelFun)
    }

    @deprecated(
        "You shouldn't be trying to create a PreTransBlock from a Tree stat " +
        "when the static type of the result is already a PreTransTree. " +
        "Prefer directly creating the relevant PreTransTree",
        "forever")
    def apply(stat: Tree, result: PreTransTree): PreTransTree =
      PreTransTree(Block(stat, result.tree)(result.pos), result.tpe)

    def apply(stat: Tree, result: PreTransform): PreTransform = {
      if (stat.isInstanceOf[Skip]) result
      else {
        result match {
          case PreTransBlock(innerBindingsAndStats, innerResult) =>
            new PreTransBlock(Right(stat) :: innerBindingsAndStats, innerResult)
          case result: PreTransResult =>
            new PreTransBlock(Right(stat) :: Nil, result)
          case PreTransRecordTree(tree, tpe, cancelFun) =>
            PreTransRecordTree(Block(stat, tree)(tree.pos), tpe, cancelFun)
          case PreTransTree(tree, tpe) =>
            PreTransTree(Block(stat, tree)(tree.pos), tpe)
        }
      }
    }

    def unapply(preTrans: PreTransBlock): Some[(List[BindingOrStat], PreTransResult)] =
      Some(preTrans.bindingsAndStats, preTrans.result)
  }

  private object PreTransMaybeBlock {
    def unapply(preTrans: PreTransform): Some[(List[BindingOrStat], PreTransform)] = {
      preTrans match {
        case PreTransBlock(bindingsAndStats, result) =>
          Some((bindingsAndStats, result))
        case _ =>
          Some((Nil, preTrans))
      }
    }
  }

  /** A `PreTransform` that can be the result of a `PreTransBlock`.
   *
   *  This is basically any `PreTransform` except:
   *  - `PreTransBlock` itself (as `PreTransBlock`s flatten out)
   *  - `PreTransGenTree` subclasses, as they would force the `PreTransBlock`
   *    to become a `PreTransGenTree` itself.
   */
  private sealed abstract class PreTransResult extends PreTransform

  /** A `PreTransform` for a `UnaryOp`. */
  private final case class PreTransUnaryOp(op: UnaryOp.Code,
      lhs: PreTransform)(implicit val pos: Position)
      extends PreTransResult {

    val tpe: RefinedType = RefinedType(UnaryOp.resultTypeOf(op))
  }

  /** A `PreTransform` for a `BinaryOp`. */
  private final case class PreTransBinaryOp(op: BinaryOp.Code,
      lhs: PreTransform, rhs: PreTransform)(implicit val pos: Position)
      extends PreTransResult {

    val tpe: RefinedType = RefinedType(BinaryOp.resultTypeOf(op))
  }

  /** A virtual reference to a `LocalDef`. */
  private final case class PreTransLocalDef(localDef: LocalDef)(
      implicit val pos: Position) extends PreTransResult {
    val tpe: RefinedType = localDef.tpe
  }

  /** Either a `PreTransTree` or a `PreTransRecordTree`.
   *
   *  This is the result type `resolveLocalDef`.
   */
  private sealed abstract class PreTransGenTree extends PreTransform

  /** A completely transformed `Tree` with a `RecordType` wrapped in
   *  `PreTransform`.
   *
   *  The `tpe` of a `PreTransRecordTree` is the refined *original* type of
   *  the expression (such as a `ClassType` for a stack-allocated object),
   *  whereas `tree.tpe` is always the lowered `RecordType`.
   */
  private final case class PreTransRecordTree(tree: Tree,
      tpe: RefinedType, cancelFun: CancelFun) extends PreTransGenTree {
    def pos: Position = tree.pos

    assert(tree.tpe.isInstanceOf[RecordType],
        s"Cannot create a PreTransRecordTree with non-record type ${tree.tpe}")
  }

  /** A completely transformed `Tree` wrapped in `PreTransform`.
   *
   *  The `Tree` cannot have a `RecordType`. If it had, it should/would be a
   *  `PreTranRecordTree` instead.
   */
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

  private implicit class OptimizerTreeOps private[OptimizerCore] (
      val __private_self: Tree)
      extends AnyVal {

    @inline private def self: Tree = __private_self

    def toPreTransform: PreTransform = {
      self match {
        case UnaryOp(op, lhs) =>
          PreTransUnaryOp(op, lhs.toPreTransform)(self.pos)
        case BinaryOp(op, lhs, rhs) =>
          PreTransBinaryOp(op, lhs.toPreTransform, rhs.toPreTransform)(self.pos)
        case _ =>
          PreTransTree(self)
      }
    }
  }

  /** Extractor for a `PreTransTree` that contains a `Literal`. */
  private object PreTransLit {
    def apply(tree: Literal): PreTransTree =
      PreTransTree(tree)

    def unapply(preTrans: PreTransTree): Option[Literal] = preTrans.tree match {
      case tree: Literal => Some(tree)
      case _             => None
    }
  }

  private final case class Binding(name: String, originalName: Option[String],
      declaredType: Type, mutable: Boolean, value: PreTransform)

  private object LongFromInt {
    def apply(x: PreTransform)(implicit pos: Position): PreTransform = x match {
      case PreTransLit(IntLiteral(v)) =>
        PreTransLit(LongLiteral(v))
      case _ =>
        PreTransUnaryOp(UnaryOp.IntToLong, x)
    }

    def unapply(tree: PreTransform): Option[PreTransform] = tree match {
      case PreTransLit(LongLiteral(v)) if v.toInt == v =>
        Some(PreTransLit(IntLiteral(v.toInt)(tree.pos)))
      case PreTransUnaryOp(UnaryOp.IntToLong, x) =>
        Some(x)
      case _ =>
        None
    }
  }

  private object AndThen {
    def apply(lhs: Tree, rhs: Tree)(implicit pos: Position): Tree =
      If(lhs, rhs, BooleanLiteral(false))(BooleanType)
  }

  /** Creates a new instance of `RuntimeLong` from a record of its `lo` and
   *  `hi` parts.
   */
  private def createNewLong(recordVarRef: VarRef)(
      implicit pos: Position): Tree = {

    val RecordType(List(loField, hiField)) = recordVarRef.tpe
    createNewLong(
        Select(recordVarRef, Ident(loField.name, loField.originalName))(IntType),
        Select(recordVarRef, Ident(hiField.name, hiField.originalName))(IntType))
  }

  /** Creates a new instance of `RuntimeLong` from its `lo` and `hi` parts. */
  private def createNewLong(lo: Tree, hi: Tree)(
      implicit pos: Position): Tree = {

    New(ClassRef(LongImpl.RuntimeLongClass), Ident(LongImpl.initFromParts),
        List(lo, hi))
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

  private final class Intrinsics(intrinsicsMap: Map[String, Int]) {
    def apply(flags: ApplyFlags, target: AbstractMethodID): Int =
      if (flags.isPrivate || flags.isConstructor) -1
      else intrinsicsMap.getOrElse(target.toString(), -1)
  }

  private object Intrinsics {
    final val ArrayCopy        = 1
    final val IdentityHashCode = ArrayCopy + 1

    final val ArrayApply  = IdentityHashCode + 1
    final val ArrayUpdate = ArrayApply       + 1
    final val ArrayLength = ArrayUpdate      + 1

    final val IntegerNLZ = ArrayLength + 1

    final val LongToString = IntegerNLZ + 1
    final val LongCompare = LongToString + 1
    final val LongDivideUnsigned = LongCompare + 1
    final val LongRemainderUnsigned = LongDivideUnsigned + 1

    final val ArrayBuilderZeroOf = LongRemainderUnsigned + 1
    final val GenericArrayBuilderResult = ArrayBuilderZeroOf + 1

    final val ClassGetComponentType = GenericArrayBuilderResult + 1

    final val ArrayNewInstance = ClassGetComponentType + 1

    final val ObjectLiteral = ArrayNewInstance + 1

    final val ByteArrayToInt8Array      = ObjectLiteral            + 1
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

    // scalastyle:off line.size.limit
    private val baseIntrinsics: Map[String, Int] = Map(
      "jl_System$.arraycopy__O__I__O__I__I__V" -> ArrayCopy,
      "jl_System$.identityHashCode__O__I"      -> IdentityHashCode,

      "sr_ScalaRunTime$.array$undapply__O__I__O"     -> ArrayApply,
      "sr_ScalaRunTime$.array$undupdate__O__I__O__V" -> ArrayUpdate,
      "sr_ScalaRunTime$.array$undlength__O__I"       -> ArrayLength,

      "jl_Integer$.numberOfLeadingZeros__I__I" -> IntegerNLZ,

      "scm_ArrayBuilder$.scala$collection$mutable$ArrayBuilder$$zeroOf__jl_Class__O" -> ArrayBuilderZeroOf,
      "scm_ArrayBuilder$.scala$collection$mutable$ArrayBuilder$$genericArrayBuilderResult__jl_Class__sjs_js_Array__O" -> GenericArrayBuilderResult,

      "jl_Class.getComponentType__jl_Class" -> ClassGetComponentType,

      "jl_reflect_Array$.newInstance__jl_Class__I__O" -> ArrayNewInstance,

      "sjs_js_special_package$.objectLiteral__sc_Seq__sjs_js_Object" -> ObjectLiteral,

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
    )

    private val runtimeLongIntrinsics: Map[String, Int] = Map(
      "jl_Long$.toString__J__T"              -> LongToString,
      "jl_Long$.compare__J__J__I"            -> LongCompare,
      "jl_Long$.divideUnsigned__J__J__J"     -> LongDivideUnsigned,
      "jl_Long$.remainderUnsigned__J__J__J"  -> LongRemainderUnsigned
    )
    // scalastyle:on line.size.limit

    def buildIntrinsics(esFeatures: ESFeatures): Intrinsics = {
      val intrinsicsMap =
        if (esFeatures.allowBigIntsForLongs) baseIntrinsics
        else baseIntrinsics ++ runtimeLongIntrinsics

      new Intrinsics(intrinsicsMap)
    }
  }

  private trait StateBackup {
    def restore(): Unit
  }

  private class SimpleState[A](owner: OptimizerCore, private var _value: A) {
    def value: A = _value

    def value_=(v: A): Unit = {
      if (v.asInstanceOf[AnyRef] ne _value.asInstanceOf[AnyRef]) {
        owner.addStateBackup(new Backup(_value))
        _value = v
      }
    }

    private class Backup(savedValue: A) extends StateBackup {
      override def restore(): Unit = value = savedValue
    }
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
      val MethodDef(_, Ident(methodName, _), params, _, optBody) = originalDef
      val body = optBody getOrElse {
        throw new AssertionError("Methods in optimizer must be concrete")
      }

      isForwarder = body match {
        // Shape of forwarders to trait impls
        case ApplyStatic(_, impl, method, args) =>
          ((args.size == params.size + 1) &&
              (args.head.isInstanceOf[This]) &&
              (args.tail.zip(params).forall {
                case (VarRef(Ident(aname, _)),
                    ParamDef(Ident(pname, _), _, _, _)) => aname == pname
                case _ => false
              }))

        // Shape of forwards to default methods
        case ApplyStatically(_, This(), cls, method, args) =>
          args.size == params.size &&
          args.zip(params).forall {
            case (VarRef(Ident(aname, _)), ParamDef(Ident(pname, _), _, _, _)) =>
              aname == pname
            case _ =>
              false
          }

        // Shape of bridges for generic methods
        case Apply(_, This(), method, args) =>
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

  private object MaybeUnbox {
    def unapply(tree: Tree): Some[(Tree, Any)] = tree match {
      case AsInstanceOf(arg, tpe) =>
        Some((arg, tpe))
      case Unbox(arg, charCode) =>
        Some((arg, charCode))
      case _ =>
        Some((tree, ()))
    }
  }

  private def isTrivialConstructorStat(stat: Tree): Boolean = stat match {
    case This() =>
      true
    case ApplyStatically(_, This(), _, _, Nil) =>
      true
    case ApplyStatic(_, _, Ident(methodName, _), This() :: Nil) =>
      methodName.startsWith("$$init$__")
    case _ =>
      false
  }

  private object SimpleMethodBody {
    @tailrec
    def unapply(body: Tree): Boolean = body match {
      case New(_, _, args)                          => areSimpleArgs(args)
      case Apply(_, receiver, _, args)              => areSimpleArgs(receiver :: args)
      case ApplyStatically(_, receiver, _, _, args) => areSimpleArgs(receiver :: args)
      case ApplyStatic(_, _, _, args)               => areSimpleArgs(args)
      case Select(qual, _)                          => isSimpleArg(qual)
      case IsInstanceOf(inner, _)                   => isSimpleArg(inner)

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
      case New(_, _, Nil)                          => true
      case Apply(_, receiver, _, Nil)              => isTrivialArg(receiver)
      case ApplyStatically(_, receiver, _, _, Nil) => isTrivialArg(receiver)
      case ApplyStatic(_, _, _, Nil)               => true

      case ArrayLength(array)        => isTrivialArg(array)
      case ArraySelect(array, index) => isTrivialArg(array) && isTrivialArg(index)

      case Unbox(inner, _)        => isSimpleArg(inner)
      case AsInstanceOf(inner, _) => isSimpleArg(inner)
      case UnaryOp(_, inner)      => isSimpleArg(inner)

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
      val localNameAllocatorSnapshot: FreshNameAllocator.Snapshot,
      val savedMutableLocalNames: Set[String],
      val labelNameAllocatorSnapshot: FreshNameAllocator.Snapshot,
      val savedStateBackupChain: List[StateBackup],
      val cont: () => TailRec[Tree]) extends ControlThrowable

  class OptimizeException(val myself: AbstractMethodID,
      val attemptedInlining: List[AbstractMethodID], cause: Throwable
  ) extends Exception(exceptionMsg(myself, attemptedInlining, cause), cause)

  final class FreshNameAllocator private (
      private var usedNamesToNextCounter: Map[String, Int]) {
    import FreshNameAllocator._

    def this() = this(FreshNameAllocator.InitialMap)

    def clear(): Unit = usedNamesToNextCounter = InitialMap

    def freshName(base: String): String = {
      if (!usedNamesToNextCounter.contains(base)) {
        usedNamesToNextCounter = usedNamesToNextCounter.updated(base, 1)
        base
      } else {
        var i = usedNamesToNextCounter(base)
        var result = base + "$" + i
        while (usedNamesToNextCounter.contains(result)) {
          i += 1
          result = base + "$" + i
        }
        usedNamesToNextCounter =
          usedNamesToNextCounter.updated(base, i + 1).updated(result, 1)
        result
      }
    }

    def snapshot(): Snapshot = new Snapshot(usedNamesToNextCounter)

    def restore(snapshot: Snapshot): Unit =
      usedNamesToNextCounter = snapshot.usedNamesToNextCounter
  }

  object FreshNameAllocator {
    private val InitialMap = {
      val isReserved = isKeyword ++ Seq("arguments", "eval", "ScalaJS")
      isReserved.map(_ -> 1).toMap
    }

    final class Snapshot private[FreshNameAllocator] (
        private[FreshNameAllocator] val usedNamesToNextCounter: Map[String, Int])
  }

}
