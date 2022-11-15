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
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.logging._
import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable.RuntimeClassNameMapperImpl
import org.scalajs.linker.standard._
import org.scalajs.linker.backend.emitter.LongImpl
import org.scalajs.linker.backend.emitter.Transients._

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

  private def semantics: Semantics = config.coreSpec.semantics

  // Uncomment and adapt to print debug messages only during one method
  //lazy val debugThisMethod: Boolean =
  //  myself.toString() == "java.lang.FloatingPointBits$.numberHashCode;D;I"

  /** Returns the body of a method. */
  protected def getMethodBody(method: MethodID): MethodDef

  /** Returns the list of possible targets for a dynamically linked call. */
  protected def dynamicCall(intfName: ClassName,
      methodName: MethodName): List[MethodID]

  /** Returns the target of a static call. */
  protected def staticCall(className: ClassName, namespace: MemberNamespace,
      methodName: MethodName): MethodID

  /** Returns the list of ancestors of a class or interface. */
  protected def getAncestorsOf(className: ClassName): List[ClassName]

  /** Tests whether the given module class has an elidable accessor.
   *  In other words, whether it is safe to discard a LoadModule of that
   *  module class which is not used.
   */
  protected def hasElidableModuleAccessor(moduleClassName: ClassName): Boolean

  /** Tests whether the given class is inlineable.
   *
   *  @return
   *    `None` if the class is not inlineable, `Some(structure)` if it is.
   */
  protected def tryNewInlineableClass(
      className: ClassName): Option[InlineableClassStructure]

  /** Returns the jsNativeLoadSpec of the given import target if it is an Import.
   *
   *  Otherwise returns None.
   */
  protected def getJSNativeImportOf(
      target: ImportTarget): Option[JSNativeLoadSpec.Import]

  /** Returns true if the given (non-static) field is ever read. */
  protected def isFieldRead(className: ClassName, fieldName: FieldName): Boolean

  /** Returns true if the given static field is ever read. */
  protected def isStaticFieldRead(className: ClassName, fieldName: FieldName): Boolean

  private val localNameAllocator = new FreshNameAllocator.Local

  /** An allocated local variable name is mutable iff it belongs to this set. */
  private var mutableLocalNames: Set[LocalName] = Set.empty

  private val labelNameAllocator = new FreshNameAllocator.Label

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
  private lazy val inlinedRTLongStructure =
    tryNewInlineableClass(LongImpl.RuntimeLongClass).get

  /** The name of the `lo` field of in the record type of `RuntimeLong`. */
  private lazy val inlinedRTLongLoField =
    inlinedRTLongStructure.recordType.fields(0).name

  /** The name of the `lo` field of in the record type of `RuntimeLong`. */
  private lazy val inlinedRTLongHiField =
    inlinedRTLongStructure.recordType.fields(1).name

  private val intrinsics =
    Intrinsics.buildIntrinsics(config.coreSpec.esFeatures)

  def optimize(thisType: Type, originalDef: MethodDef): MethodDef = {
    try {
      val MethodDef(static, name, originalName, params, resultType, optBody) =
        originalDef
      val body = optBody getOrElse {
        throw new AssertionError("Methods to optimize must be concrete")
      }

      val (newParams, newBody1) = try {
        transformMethodDefBody(myself, thisType, params, resultType, body)
      } catch {
        case _: TooManyRollbacksException =>
          localNameAllocator.clear()
          mutableLocalNames = Set.empty
          labelNameAllocator.clear()
          stateBackupChain = Nil
          disableOptimisticOptimizations = true
          transformMethodDefBody(myself, thisType, params, resultType, body)
      }
      val newBody =
        if (originalDef.methodName == NoArgConstructorName) tryElimStoreModule(newBody1)
        else newBody1
      MethodDef(static, name, originalName, newParams, resultType,
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
            case Assign(Select(This(), _, _), _:Literal | _:VarRef) =>
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

  private def freshLocalNameWithoutOriginalName(base: LocalName,
      mutable: Boolean): LocalName = {
    val result = localNameAllocator.freshName(base)
    if (mutable)
      mutableLocalNames += result
    result
  }

  private def freshLocalName(base: LocalName, originalName: OriginalName,
      mutable: Boolean): (LocalName, OriginalName) = {
    val newName = freshLocalNameWithoutOriginalName(base, mutable)
    val newOriginalName = originalNameForFresh(base, originalName, newName)
    (newName, newOriginalName)
  }

  private def freshLocalName(base: Binding.Name,
      mutable: Boolean): (LocalName, OriginalName) = {
    base match {
      case Binding.This =>
        freshLocalName(LocalThisNameForFresh, thisOriginalName, mutable)
      case Binding.Local(name, originalName) =>
        freshLocalName(name, originalName, mutable)
    }
  }

  private def freshLabelName(base: LabelName): LabelName =
    labelNameAllocator.freshName(base)

  // Just a helper to make the callsites more understandable
  private def localIsMutable(name: LocalName): Boolean = mutableLocalNames(name)

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

  private def isSubclass(lhs: ClassName, rhs: ClassName): Boolean =
    getAncestorsOf(lhs).contains(rhs)

  private val isSubclassFun = isSubclass _

  private def isSubtype(lhs: Type, rhs: Type): Boolean = {
    assert(lhs != NoType)
    assert(rhs != NoType)

    Types.isSubtype(lhs, rhs)(isSubclassFun) || {
      (lhs, rhs) match {
        case (LongType | ClassType(BoxedLongClass),
            ClassType(LongImpl.RuntimeLongClass)) =>
          true

        case (ClassType(LongImpl.RuntimeLongClass),
            ClassType(BoxedLongClass)) =>
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
    val result: Tree = tree match {
      // Definitions

      case VarDef(_, _, _, _, rhs) =>
        /* A local var that is last (or alone) in its block is not terribly
         * useful. Get rid of it.
         * (Non-last VarDefs in blocks are handled in transformBlock.)
         */
        transformStat(rhs)

      // Control flow constructs

      case tree: Block =>
        transformBlock(tree, isStat)

      case Labeled(ident @ LabelIdent(label), tpe, body) =>
        trampoline {
          pretransformLabeled(label, if (isStat) NoType else tpe, body, isStat,
              usePreTransform = false)(finishTransform(isStat))
        }

      case Assign(lhs, rhs) =>
        val cont = { (tlhs: PreTransform) =>
          pretransformExpr(rhs) { trhs =>
            pretransformAssign(tlhs, trhs)(finishTransform(isStat))
          }
        }

        lhs match {
          case Select(qualifier, className, FieldIdent(name)) if !isFieldRead(className, name) =>
            // Field is never read. Drop assign, keep side effects only.
            Block(transformStat(qualifier), transformStat(rhs))

          case SelectStatic(className, FieldIdent(name)) if !isStaticFieldRead(className, name) =>
            // Field is never read. Drop assign, keep side effects only.
            transformStat(rhs)

          case JSPrivateSelect(qualifier, className, FieldIdent(name)) if !isFieldRead(className, name) =>
            // Field is never read. Drop assign, keep side effects only.
            Block(transformStat(qualifier), transformStat(rhs))

          case lhs: Select =>
            trampoline {
              pretransformSelectCommon(lhs, isLhsOfAssign = true)(cont)
            }

          case lhs: JSSelect =>
            trampoline {
              pretransformJSSelect(lhs, isLhsOfAssign = true)(cont)
            }

          case _ =>
            trampoline {
              pretransformExpr(lhs)(cont)
            }
        }

      case Return(expr, label) =>
        val info = scope.env.labelInfos(label.name)
        val newLabel = LabelIdent(info.newName)
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

      case ForIn(obj, keyVar @ LocalIdent(name), originalName, body) =>
        val newObj = transformExpr(obj)
        val (newName, newOriginalName) =
          freshLocalName(name, originalName, mutable = false)
        val localDef = LocalDef(RefinedType(AnyType), mutable = false,
            ReplaceWithVarRef(newName, newSimpleState(Used), None))
        val newBody = {
          val bodyScope = scope.withEnv(scope.env.withLocalDef(name, localDef))
          transformStat(body)(bodyScope)
        }
        ForIn(newObj, LocalIdent(newName)(keyVar.pos), newOriginalName, newBody)

      case TryCatch(block, errVar @ LocalIdent(name), originalName, handler) =>
        val newBlock = transform(block, isStat)

        val (newName, newOriginalName) =
          freshLocalName(name, originalName, mutable = false)
        val localDef = LocalDef(RefinedType(AnyType), true,
            ReplaceWithVarRef(newName, newSimpleState(Used), None))
        val newHandler = {
          val handlerScope = scope.withEnv(scope.env.withLocalDef(name, localDef))
          transform(handler, isStat)(handlerScope)
        }

        val refinedType = constrainedLub(newBlock.tpe, newHandler.tpe, tree.tpe)
        TryCatch(newBlock, LocalIdent(newName)(errVar.pos), newOriginalName,
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
          case selectorValue: MatchableLiteral =>
            val body = cases.collectFirst {
              case (alts, body) if alts.exists(matchableLiteral_===(_, selectorValue)) => body
            }.getOrElse(default)
            transform(body, isStat)
          case _ =>
            Match(newSelector,
                cases map (c => (c._1, transform(c._2, isStat))),
                transform(default, isStat))(tree.tpe)
        }

      // Scala expressions

      case New(className, ctor, args) =>
        New(className, ctor, args map transformExpr)

      case StoreModule(className, value) =>
        StoreModule(className, transformExpr(value))

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

      case tree: ApplyDynamicImport =>
        trampoline {
          pretransformApplyDynamicImport(tree, isStat)(finishTransform(isStat))
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

      case IsInstanceOf(expr, testType) =>
        trampoline {
          pretransformExpr(expr) { texpr =>
            val result = {
              if (isSubtype(texpr.tpe.base, testType)) {
                if (texpr.tpe.isNullable)
                  JSBinaryOp(JSBinaryOp.!==, finishTransformExpr(texpr), Null())
                else
                  Block(finishTransformStat(texpr), BooleanLiteral(true))
              } else {
                if (texpr.tpe.isExact)
                  Block(finishTransformStat(texpr), BooleanLiteral(false))
                else
                  IsInstanceOf(finishTransformExpr(texpr), testType)
              }
            }
            TailCalls.done(result)
          }
        }

      case AsInstanceOf(arg, tpe) =>
        trampoline {
          pretransformExpr(arg) { targ =>
            finishTransform(isStat)(foldAsInstanceOf(targ, tpe))
          }
        }

      case GetClass(expr) =>
        trampoline {
          pretransformExpr(expr) { texpr =>
            def constant(typeRef: TypeRef): TailRec[Tree] =
              TailCalls.done(Block(finishTransformStat(texpr), ClassOf(typeRef)))

            texpr.tpe match {
              case RefinedType(ClassType(LongImpl.RuntimeLongClass), true, false) =>
                constant(ClassRef(BoxedLongClass))
              case RefinedType(ClassType(className), true, false) =>
                constant(ClassRef(className))
              case RefinedType(ArrayType(arrayTypeRef), true, false) =>
                constant(arrayTypeRef)
              case _ =>
                TailCalls.done(GetClass(finishTransformExpr(texpr)))
            }
          }
        }

      case Clone(expr) =>
        Clone(transformExpr(expr))

      case IdentityHashCode(expr) =>
        IdentityHashCode(transformExpr(expr))

      case _:WrapAsThrowable | _:UnwrapFromThrowable =>
        trampoline {
          pretransformExpr(tree)(finishTransform(isStat))
        }

      // JavaScript expressions

      case JSNew(ctor, args) =>
        JSNew(transformExpr(ctor), transformExprsOrSpreads(args))

      case JSPrivateSelect(qualifier, className, field) =>
        JSPrivateSelect(transformExpr(qualifier), className, field)

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
        JSObjectConstr(fields.map { field =>
          (transformExpr(field._1), transformExpr(field._2))
        })

      // Atomic expressions

      case _:VarRef | _:This =>
        trampoline {
          pretransformExpr(tree)(finishTransform(isStat))
        }

      case Closure(arrow, captureParams, params, restParam, body, captureValues) =>
        trampoline {
          pretransformExprs(captureValues) { tcaptureValues =>
            transformClosureCommon(arrow, captureParams, params, restParam, body,
                tcaptureValues)(finishTransform(isStat))
          }
        }

      case CreateJSClass(className, captureValues) =>
        CreateJSClass(className, captureValues.map(transformExpr))

      case SelectJSNativeMember(className, MethodIdent(member)) =>
        transformJSLoadCommon(ImportTarget.Member(className, member), tree)

      case LoadJSModule(className) =>
        transformJSLoadCommon(ImportTarget.Class(className), tree)

      case LoadJSConstructor(className) =>
        transformJSLoadCommon(ImportTarget.Class(className), tree)

      // Trees that need not be transformed

      case _:Skip | _:Debugger | _:LoadModule | _:SelectStatic |
          _:JSNewTarget | _:JSImportMeta | _:JSLinkingInfo |
          _:JSGlobalRef | _:JSTypeOfGlobalRef | _:Literal =>
        tree

      case _ =>
        throw new IllegalArgumentException(
            s"Invalid tree in transform of class ${tree.getClass.getName}: $tree")
    }

    if (isStat) keepOnlySideEffects(result)
    else result
  }

  private def transformClosureCommon(arrow: Boolean,
      captureParams: List[ParamDef], params: List[ParamDef],
      restParam: Option[ParamDef], body: Tree,
      tcaptureValues: List[PreTransform])(cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    val (paramLocalDefs, newParams) = params.map(newParamReplacement(_)).unzip
    val (restParamLocalDef, newRestParam) = {
      // Option#unzip
      restParam.map(newParamReplacement(_)) match {
        case None         => (None, None)
        case Some((x, y)) => (Some(x), Some(y))
      }
    }

    val thisLocalDef =
      if (arrow) None
      else Some(newThisLocalDef(AnyType))

    val innerEnv = OptEnv.Empty
      .withThisLocalDef(thisLocalDef)
      .withLocalDefs(paramLocalDefs)
      .withLocalDefs(restParamLocalDef.toList)

    transformCapturingBody(captureParams, tcaptureValues, body, innerEnv) {
      (newCaptureParams, newCaptureValues, newBody) =>
        val newClosure = Closure(arrow, newCaptureParams, newParams, newRestParam, newBody, newCaptureValues)
        PreTransTree(newClosure, RefinedType(AnyType, isExact = false, isNullable = false))
    } (cont)
  }

  private def transformCapturingBody(captureParams: List[ParamDef],
      tcaptureValues: List[PreTransform], body: Tree, innerEnv: OptEnv)(
      inner: (List[ParamDef], List[Tree], Tree) => PreTransTree)(cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {
    /* Process captures.
     *
     * This is different than normal param replacement:
     *
     * - We inline literals into the closure.
     * - We introduce bindings for the (remaining) capture values. This allows to
     *   eliminate captures that are not used (but preserve side effects).
     * - If the capture value is a VarRef, we give the capture param the exact
     *   same name. This is to help the FunctionEmitter eliminate IIFEs.
     */
    val captureParamLocalDefs = List.newBuilder[(LocalName, LocalDef)]
    val newCaptureParamDefsAndRepls = List.newBuilder[(ParamDef, ReplaceWithVarRef)]
    val captureValueBindings = List.newBuilder[Binding]
    val captureParamLocalDefsForVarRefs = mutable.Map.empty[LocalName, LocalDef]

    for ((paramDef, tcaptureValue) <- captureParams.zip(tcaptureValues)) {
      val ParamDef(ident @ LocalIdent(paramName), originalName, ptpe, mutable) = paramDef

      assert(!mutable, s"Found mutable capture at ${paramDef.pos}")

      def addCaptureParam(newName: LocalName): LocalDef = {
        val newOriginalName = originalNameForFresh(paramName, originalName, newName)

        val replacement = ReplaceWithVarRef(newName, newSimpleState(Unused), None)
        val localDef = LocalDef(tcaptureValue.tpe, mutable, replacement)
        val localIdent = LocalIdent(newName)(ident.pos)
        val newParamDef = ParamDef(localIdent, newOriginalName, ptpe, mutable)(paramDef.pos)

        /* Note that the binding will never create a fresh name for a
         * ReplaceWithVarRef. So this will not put our name alignment at risk.
         */
        val valueBinding = Binding.temp(paramName, ptpe, mutable, tcaptureValue)

        captureParamLocalDefs += paramName -> localDef
        newCaptureParamDefsAndRepls += newParamDef -> replacement
        captureValueBindings += valueBinding

        localDef
      }

      tcaptureValue match {
        case PreTransLit(literal) =>
          captureParamLocalDefs += paramName -> LocalDef(tcaptureValue.tpe, false, ReplaceWithConstant(literal))

        case PreTransLocalDef(LocalDef(_, /* mutable = */ false, ReplaceWithVarRef(captureName, _, _))) =>
          captureParamLocalDefsForVarRefs.get(captureName).fold[Unit] {
            captureParamLocalDefsForVarRefs += captureName -> addCaptureParam(captureName)
          } { prevLocalDef =>
            /* #4716 Two capture values may have been aliased to the same VarRef.
             * They must use the same capture param LocalDef, otherwise we will
             * create duplicate capture params.
             */
            captureParamLocalDefs += paramName -> prevLocalDef
          }

        case _ =>
          addCaptureParam(freshLocalNameWithoutOriginalName(paramName, mutable))
      }
    }

    val innerScope = scope.withEnv(innerEnv.withLocalDefs(captureParamLocalDefs.result()))

    val newBody = transformExpr(body)(innerScope)

    withNewLocalDefs(captureValueBindings.result()) { (localDefs, cont1) =>
      val (finalCaptureParams, finalCaptureValues) = (for {
        (localDef, (param, replacement)) <- localDefs.iterator.zip(newCaptureParamDefsAndRepls.result().iterator)
        if replacement.used.value.isUsed
      } yield {
        param -> localDef.newReplacement
      }).toList.unzip

      cont1(inner(finalCaptureParams, finalCaptureValues, newBody))
    } (cont)
  }

  private def transformBlock(tree: Block, isStat: Boolean)(
      implicit scope: Scope): Tree = {
    def transformList(stats: List[Tree])(
        implicit scope: Scope): Tree = stats match {
      case last :: Nil =>
        transform(last, isStat)

      case (VarDef(nameIdent, originalName, vtpe, mutable, rhs)) :: rest =>
        trampoline {
          pretransformExpr(rhs) { trhs =>
            withBinding(Binding(nameIdent, originalName, vtpe, mutable, trhs)) {
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

      case VarRef(LocalIdent(name)) =>
        val localDef = scope.env.localDefs.getOrElse(name, {
          throw new AssertionError(
              s"Cannot find local def '$name' at $pos\n" +
              s"While optimizing $myself\n" +
              s"Env is ${scope.env}\n" +
              s"Inlining ${scope.implsBeingInlined}")
        })
        cont(localDef.toPreTransform)

      case This() =>
        val localDef = scope.env.thisLocalDef.getOrElse {
          throw new AssertionError(
              s"Found invalid 'this' at $pos\n" +
              s"While optimizing $myself\n" +
              s"Env is ${scope.env}\n" +
              s"Inlining ${scope.implsBeingInlined}")
        }
        cont(localDef.toPreTransform)

      case tree: If =>
        pretransformIf(tree)(cont)

      case Match(selector, cases, default) =>
        val newSelector = transformExpr(selector)
        newSelector match {
          case selectorValue: MatchableLiteral =>
            val body = cases.collectFirst {
              case (alts, body) if alts.exists(matchableLiteral_===(_, selectorValue)) => body
            }.getOrElse(default)
            pretransformExpr(body)(cont)
          case _ =>
            cont(Match(newSelector,
                cases map (c => (c._1, transformExpr(c._2))),
                transformExpr(default))(tree.tpe).toPreTransform)
        }

      case Labeled(ident @ LabelIdent(label), tpe, body) =>
        pretransformLabeled(label, tpe, body, isStat = false,
            usePreTransform = true)(cont)

      case New(className, ctor, args) =>
        pretransformExprs(args) { targs =>
          pretransformNew(AllocationSite.Tree(tree), className, ctor, targs)(cont)
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

      case tree: ApplyDynamicImport =>
        pretransformApplyDynamicImport(tree, isStat = false)(cont)

      case tree: UnaryOp =>
        pretransformUnaryOp(tree)(cont)

      case tree: BinaryOp =>
        pretransformBinaryOp(tree)(cont)

      case WrapAsThrowable(expr) =>
        pretransformExpr(expr) { texpr =>
          def default = {
            val refinedType: RefinedType = RefinedType(ThrowableClassType, isExact = false, isNullable = false)
            cont(PreTransTree(WrapAsThrowable(finishTransformExpr(texpr)), refinedType))
          }

          if (isSubtype(texpr.tpe.base, ThrowableClassType)) {
            if (texpr.tpe.isNullable)
              default
            else
              cont(texpr)
          } else {
            if (texpr.tpe.isExact) {
              pretransformNew(AllocationSite.Tree(tree), JavaScriptExceptionClass,
                  MethodIdent(AnyArgConstructorName), texpr :: Nil)(cont)
            } else {
              default
            }
          }
        }

      case UnwrapFromThrowable(expr) =>
        pretransformExpr(expr) { texpr =>
          def default =
            cont(PreTransTree(UnwrapFromThrowable(finishTransformExpr(texpr))))

          val baseTpe = texpr.tpe.base

          if (baseTpe == NothingType) {
            cont(texpr)
          } else if (baseTpe == NullType) {
            // Undefined behavior for NPE
            cont(texpr)
          } else if (isSubtype(baseTpe, JavaScriptExceptionClassType)) {
            if (texpr.tpe.isNullable) {
              default
            } else {
              pretransformSelectCommon(AnyType, texpr, JavaScriptExceptionClass,
                  FieldIdent(exceptionFieldName), isLhsOfAssign = false)(cont)
            }
          } else {
            if (texpr.tpe.isExact || !isSubtype(JavaScriptExceptionClassType, baseTpe))
              cont(texpr)
            else
              default
          }
        }

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
              withNewTempLocalDefs(titems) { (itemLocalDefs, cont1) =>
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

      case AsInstanceOf(expr, tpe) =>
        pretransformExpr(expr) { texpr =>
          cont(foldAsInstanceOf(texpr, tpe))
        }

      case Closure(arrow, captureParams, params, restParam, body, captureValues) =>
        pretransformExprs(captureValues) { tcaptureValues =>
          def default(): TailRec[Tree] = {
            transformClosureCommon(arrow, captureParams, params, restParam, body, tcaptureValues)(cont)
          }

          if (!arrow || restParam.isDefined) {
            /* TentativeClosureReplacement assumes there are no rest
             * parameters, because that would not be inlineable anyway.
             * Likewise, it assumes that there is no binding for `this` nor for
             * `new.target`, which is only true for arrow functions.
             * So we never try to inline non-arrow Closures, nor Closures with
             * a rest parameter. There are few use cases for either anyway.
             */
            default()
          } else {
            tryOrRollback { cancelFun =>
              val captureBindings = for {
                (ParamDef(nameIdent, originalName, tpe, mutable), value) <-
                  captureParams zip tcaptureValues
              } yield {
                Binding(nameIdent, originalName, tpe, mutable, value)
              }
              withNewLocalDefs(captureBindings) { (captureLocalDefs, cont1) =>
                val replacement = TentativeClosureReplacement(
                    captureParams, params, body, captureLocalDefs,
                    alreadyUsed = newSimpleState(Unused), cancelFun)
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

      case (VarDef(nameIdent, originalName, vtpe, mutable, rhs)) :: rest =>
        pretransformExpr(rhs) { trhs =>
          withBinding(Binding(nameIdent, originalName, vtpe, mutable, trhs)) {
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
    val Select(qualifier, className, field) = tree
    pretransformExpr(qualifier) { preTransQual =>
      pretransformSelectCommon(tree.tpe, preTransQual, className, field,
          isLhsOfAssign)(cont)(scope, tree.pos)
    }
  }

  private def pretransformSelectCommon(expectedType: Type,
      preTransQual: PreTransform, className: ClassName, field: FieldIdent,
      isLhsOfAssign: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {
    /* Note: Callers are expected to have already removed writes to fields that
     * are never read.
     */

    preTransQual match {
      case PreTransLocalDef(LocalDef(_, _,
          InlineClassBeingConstructedReplacement(_, fieldLocalDefs, cancelFun))) =>
        val fieldLocalDef = fieldLocalDefs(FieldID(className, field))
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
        val fieldLocalDef = fieldLocalDefs(FieldID(className, field))
        assert(!isLhsOfAssign || fieldLocalDef.mutable, s"assign to immutable field at $pos")
        cont(fieldLocalDef.toPreTransform)

      // Select the lo or hi "field" of a Long literal
      case PreTransLit(LongLiteral(value)) if useRuntimeLong =>
        val itemName = field.name
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
            val recordField = recordType.findField(field.name)
            val sel = RecordSelect(newQual, field)(recordField.tpe)
            sel.tpe match {
              case _: RecordType =>
                cont(PreTransRecordTree(sel, RefinedType(expectedType), cancelFun))
              case _ =>
                cont(PreTransTree(sel, RefinedType(sel.tpe)))
            }

          case PreTransTree(newQual, _) =>
            cont(PreTransTree(Select(newQual, className, field)(expectedType),
                RefinedType(expectedType)))
        }
    }
  }

  private def pretransformAssign(tlhs: PreTransform, trhs: PreTransform)(
      cont: PreTransCont)(implicit scope: Scope, pos: Position): TailRec[Tree] = {
    def contAssign(lhs: Tree, rhs: Tree) =
      cont(PreTransTree(Assign(lhs.asInstanceOf[AssignLhs], rhs)))

    resolveLocalDef(tlhs) match {
      case PreTransRecordTree(lhsTree, lhsOrigType, lhsCancelFun) =>
        val recordType = lhsTree.tpe.asInstanceOf[RecordType]

        def buildInner(trhs: PreTransform): TailRec[Tree] = {
          resolveLocalDef(trhs) match {
            case PreTransRecordTree(rhsTree, rhsOrigType, rhsCancelFun) =>
              if (rhsTree.tpe != recordType || rhsOrigType != lhsOrigType)
                lhsCancelFun()
              contAssign(lhsTree, rhsTree)
            case _ =>
              lhsCancelFun()
          }
        }

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

      case PreTransTree(lhsTree, _) =>
        contAssign(lhsTree, finishTransformExpr(trhs))
    }
  }

  private def pretransformNew(allocationSite: AllocationSite,
      className: ClassName, ctor: MethodIdent, targs: List[PreTransform])(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    tryNewInlineableClass(className) match {
      case Some(structure) =>
        tryOrRollback { cancelFun =>
          inlineClassConstructor(allocationSite, className, structure,
              ctor, targs, cancelFun)(cont)
        } { () =>
          cont(PreTransTree(
              New(className, ctor, targs.map(finishTransformExpr)),
              RefinedType(ClassType(className), isExact = true, isNullable = false)))
        }
      case None =>
        cont(PreTransTree(
            New(className, ctor, targs.map(finishTransformExpr)),
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

      case _:PreTransUnaryOp | _:PreTransBinaryOp | _:PreTransJSBinaryOp =>
        PreTransTree(finishTransformExpr(preTrans))

      case PreTransLocalDef(localDef @ LocalDef(tpe, _, replacement)) =>
        replacement match {
          case ReplaceWithRecordVarRef(name, recordType, used, cancelFun) =>
            used.value = Used
            PreTransRecordTree(
                VarRef(LocalIdent(name))(recordType), tpe, cancelFun)

          case InlineClassInstanceReplacement(structure, fieldLocalDefs, cancelFun) =>
            val recordType = structure.recordType
            if (!isImmutableType(recordType))
              cancelFun()
            PreTransRecordTree(
                RecordValue(recordType, structure.fieldIDs.map(
                    id => fieldLocalDefs(id).newReplacement)),
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

      case _:PreTransUnaryOp | _:PreTransBinaryOp | _:PreTransJSBinaryOp =>
        None

      case PreTransLocalDef(localDef @ LocalDef(tpe, _, replacement)) =>
        replacement match {
          case ReplaceWithRecordVarRef(name, recordType, used, cancelFun) =>
            Some((recordType, cancelFun))

          case InlineClassInstanceReplacement(structure, fieldLocalDefs, cancelFun) =>
            Some((structure.recordType, cancelFun))

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
      case PreTransJSBinaryOp(op, lhs, rhs) =>
        JSBinaryOp(op, finishTransformExpr(lhs), finishTransformExpr(rhs))
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
            val varRefIdent = LocalIdent(
                freshLocalNameWithoutOriginalName(LocalName("x"), mutable = false))
            val recordVarDef =
              VarDef(varRefIdent, NoOriginalName, tree.tpe, mutable = false, tree)
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
      // Here we need to preserve the side-effects of integer division/modulo and String_charAt
      import BinaryOp._

      def newLhs = finishTransformStat(lhs)

      def finishNoSideEffects: Tree =
        Block(newLhs, finishTransformStat(rhs))(stat.pos)

      (op: @switch) match {
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
        case String_charAt if semantics.stringIndexOutOfBounds != CheckedBehavior.Unchecked =>
          finishTransformExpr(stat)
        case _ =>
          finishNoSideEffects
      }

    case PreTransJSBinaryOp(op, lhs, rhs) =>
      if (op == JSBinaryOp.=== || op == JSBinaryOp.!==)
        Block(finishTransformStat(lhs), finishTransformStat(rhs))(stat.pos)
      else // other operators can have side effects that we must preserve
        finishTransformExpr(stat)
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
      case (Left(PreTransBinding(originalName, localDef, value)), innerBody) =>
        implicit val pos = value.pos

        val LocalDef(tpe, mutable, replacement) = localDef

        val (name, used) = (replacement: @unchecked) match {
          case ReplaceWithVarRef(name, used, _) =>
            (name, used)
          case ReplaceWithRecordVarRef(name, _, used, _) =>
            (name, used)
        }

        if (used.value.isUsed) {
          val ident = LocalIdent(name)
          val varDef = resolveLocalDef(value) match {
            case PreTransRecordTree(valueTree, valueTpe, cancelFun) =>
              val recordType = valueTree.tpe.asInstanceOf[RecordType]
              if (!isImmutableType(recordType))
                cancelFun()
              VarDef(ident, originalName, recordType, mutable, valueTree)

            case PreTransTree(valueTree, valueTpe) =>
              VarDef(ident, originalName, tpe.base, mutable, valueTree)
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
    case VarDef(_, _, _, _, rhs) =>
      keepOnlySideEffects(rhs)
    case Block(init :+ last) =>
      keepOnlySideEffects(last) match {
        case Skip()      => keepOnlySideEffects(Block(init)(stat.pos))
        case lastEffects => Block(init, lastEffects)(stat.pos)
      }
    case LoadModule(moduleClassName) =>
      if (hasElidableModuleAccessor(moduleClassName)) Skip()(stat.pos)
      else stat
    case NewArray(_, lengths) if lengths.forall(isNonNegativeIntLiteral(_)) =>
      Skip()(stat.pos)
    case NewArray(_, lengths) if semantics.negativeArraySizes == CheckedBehavior.Unchecked =>
      Block(lengths.map(keepOnlySideEffects))(stat.pos)
    case Select(qualifier, _, _) =>
      keepOnlySideEffects(qualifier)
    case Closure(_, _, _, _, _, captureValues) =>
      Block(captureValues.map(keepOnlySideEffects))(stat.pos)
    case UnaryOp(_, arg) =>
      keepOnlySideEffects(arg)
    case If(cond, thenp, elsep) =>
      (keepOnlySideEffects(thenp), keepOnlySideEffects(elsep)) match {
        case (Skip(), Skip())     => keepOnlySideEffects(cond)
        case (newThenp, newElsep) => If(cond, newThenp, newElsep)(NoType)(stat.pos)
      }

    case BinaryOp(op, lhs, rhs) =>
      // Here we need to preserve the side-effects of integer division/modulo and String_charAt
      import BinaryOp._

      implicit val pos = stat.pos

      def newLhs = keepOnlySideEffects(lhs)

      def finishNoSideEffects: Tree =
        Block(newLhs, keepOnlySideEffects(rhs))

      op match {
        case Int_/ | Int_% =>
          rhs match {
            case IntLiteral(r) if r != 0 =>
              finishNoSideEffects
            case _ =>
              Block(newLhs, BinaryOp(op, IntLiteral(0), rhs))
          }
        case Long_/ | Long_% =>
          rhs match {
            case LongLiteral(r) if r != 0L =>
              finishNoSideEffects
            case _ =>
              Block(newLhs, BinaryOp(op, LongLiteral(0L), rhs))
          }
        case String_charAt if semantics.stringIndexOutOfBounds != CheckedBehavior.Unchecked =>
          stat
        case _ =>
          finishNoSideEffects
      }

    case RecordValue(_, elems) =>
      Block(elems.map(keepOnlySideEffects))(stat.pos)
    case RecordSelect(record, _) =>
      keepOnlySideEffects(record)
    case WrapAsThrowable(expr) =>
      keepOnlySideEffects(expr)
    case UnwrapFromThrowable(expr) =>
      keepOnlySideEffects(expr)
    case _ =>
      stat
  }

  private def isNonNegativeIntLiteral(tree: Tree): Boolean = tree match {
    case IntLiteral(value) => value >= 0
    case _                 => false
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
      methodIdent: MethodIdent, targs: List[PreTransform], resultType: Type,
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
        // Apply on null is UB, just create a well-typed tree.
        cont(Block(finishTransformStat(treceiver), Throw(Null())).toPreTransform)
      case _ =>
        if (methodName.isReflectiveProxy) {
          // Never inline reflective proxies
          treeNotInlined
        } else {
          val className = boxedClassForType(treceiver.tpe.base)
          val namespace = MemberNamespace.forNonStaticCall(flags)

          /* When the receiver has an exact type, we can use static resolution
           * even for a dynamic call.
           * Otherwise, if the receiver has an ArrayType, we should perform
           * dynamic resolution in the Array[T] class. However, we don't model
           * the Array[T] class family, so we cannot do that. We emulate the
           * result by using static resolution in the representative class
           * (which is j.l.Object) instead. (addMethodCalled in Infos.scala
           * does the same thing.)
           */
          val useStaticResolution =
            treceiver.tpe.isExact || treceiver.tpe.base.isInstanceOf[ArrayType]

          val impls =
            if (useStaticResolution) List(staticCall(className, namespace, methodName))
            else dynamicCall(className, methodName)
          if (impls.size == 1) {
            pretransformSingleDispatch(flags, impls.head, Some(treceiver), targs, isStat, usePreTransform)(cont) {
              treeNotInlined
            }
          } else {
            val allocationSites =
              (treceiver :: targs).map(_.tpe.allocationSite)
            val shouldMultiInline = {
              impls.nonEmpty && // will fail at runtime.
              !impls.exists(impl => scope.implsBeingInlined((allocationSites, impl))) &&
              canMultiInline(impls)
            }

            if (shouldMultiInline) {
              /* When multi-inlining, we cannot use the enclosing class of the
               * target method as the declared type of the receiver, since we
               * have no guarantee that the receiver is in fact of that
               * particular class. It could be of any of the classes that the
               * targets belong to. Therefore, we have to keep the receiver's
               * static type as a declared type, which is our only safe choice.
               */
              val receiverType = treceiver.tpe.base
              inline(allocationSites, Some((receiverType, treceiver)), targs,
                  impls.head, isStat, usePreTransform)(cont)
            } else {
              treeNotInlined
            }
          }
        }
    }
  }

  private def canMultiInline(impls: List[MethodID]): Boolean = {
    // TODO? Inline multiple non-forwarders with the exact same body?
    impls.forall(impl => impl.attributes.isForwarder && impl.attributes.inlineable) &&
    (getMethodBody(impls.head).body.get match {
      // Trait impl forwarder
      case ApplyStatic(flags, staticCls, MethodIdent(methodName), _) =>
        impls.tail.forall(getMethodBody(_).body.get match {
          case ApplyStatic(`flags`, `staticCls`, MethodIdent(`methodName`), _) =>
            true
          case _ =>
            false
        })

      // Shape of forwards to default methods
      case ApplyStatically(flags, This(), className, MethodIdent(methodName), args) =>
        impls.tail.forall(getMethodBody(_).body.get match {
          case ApplyStatically(`flags`, This(), `className`, MethodIdent(`methodName`), _) =>
            true
          case _ =>
            false
        })

      // Bridge method
      case Apply(flags, This(), MethodIdent(methodName), referenceArgs) =>
        impls.tail.forall(getMethodBody(_).body.get match {
          case Apply(`flags`, This(), MethodIdent(`methodName`), implArgs) =>
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

  private def boxedClassForType(tpe: Type): ClassName = (tpe: @unchecked) match {
    case ClassType(className) =>
      if (className == BoxedLongClass && useRuntimeLong)
        LongImpl.RuntimeLongClass
      else
        className

    case AnyType      => ObjectClass
    case UndefType    => BoxedUnitClass
    case BooleanType  => BoxedBooleanClass
    case CharType     => BoxedCharacterClass
    case ByteType     => BoxedByteClass
    case ShortType    => BoxedShortClass
    case IntType      => BoxedIntegerClass
    case LongType     =>
      if (useRuntimeLong) LongImpl.RuntimeLongClass
      else BoxedLongClass
    case FloatType    => BoxedFloatClass
    case DoubleType   => BoxedDoubleClass
    case StringType   => BoxedStringClass
    case ArrayType(_) => ObjectClass
  }

  private def pretransformStaticApply(tree: ApplyStatically, isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    val ApplyStatically(flags, receiver, className,
        methodIdent @ MethodIdent(methodName), args) = tree
    implicit val pos = tree.pos

    def treeNotInlined0(transformedReceiver: Tree, transformedArgs: List[Tree]) =
      cont(PreTransTree(ApplyStatically(flags, transformedReceiver, className,
          methodIdent, transformedArgs)(tree.tpe), RefinedType(tree.tpe)))

    def treeNotInlined =
      treeNotInlined0(transformExpr(receiver), args.map(transformExpr))

    if (methodName.isReflectiveProxy) {
      // Never inline reflective proxies
      treeNotInlined
    } else {
      val target = staticCall(className, MemberNamespace.forNonStaticCall(flags),
          methodName)
      pretransformExprs(receiver, args) { (treceiver, targs) =>
        pretransformSingleDispatch(flags, target, Some(treceiver), targs, isStat, usePreTransform)(cont) {
          treeNotInlined0(finishTransformExpr(treceiver), targs.map(finishTransformExpr))
        }
      }
    }
  }

  private def receiverTypeFor(target: MethodID): Type =
    BoxedClassToPrimType.getOrElse(target.enclosingClassName, ClassType(target.enclosingClassName))

  private def pretransformApplyStatic(tree: ApplyStatic, isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    val ApplyStatic(flags, className,
        methodIdent @ MethodIdent(methodName), args) = tree
    implicit val pos = tree.pos

    val target = staticCall(className, MemberNamespace.forStaticCall(flags),
        methodName)
    pretransformExprs(args) { targs =>
      pretransformSingleDispatch(flags, target, None, targs, isStat, usePreTransform)(cont) {
        val newArgs = targs.map(finishTransformExpr)
        cont(PreTransTree(ApplyStatic(flags, className, methodIdent,
            newArgs)(tree.tpe), RefinedType(tree.tpe)))
      }
    }
  }

  private def pretransformApplyDynamicImport(tree: ApplyDynamicImport, isStat: Boolean)(
      cont: PreTransCont)(
      implicit outerScope: Scope): TailRec[Tree] = {

    val ApplyDynamicImport(flags, className, method, args) = tree
    implicit val pos = tree.pos

    def treeNotInlined0(transformedArgs: List[Tree]) =
      cont(PreTransTree(ApplyDynamicImport(flags, className, method, transformedArgs),
          RefinedType(AnyType)))

    def treeNotInlined = treeNotInlined0(args.map(transformExpr))

    val targetMethod =
      staticCall(className, MemberNamespace.forStaticCall(flags), method.name)

    if (!targetMethod.attributes.inlineable) {
      treeNotInlined
    } else {
      val maybeImportTarget = targetMethod.attributes.jsDynImportInlineTarget.orElse {
        targetMethod.attributes.jsDynImportThunkFor.flatMap { thunkTarget =>
          val id = staticCall(className, MemberNamespace.Public, thunkTarget)
          if (id.attributes.inlineable)
            id.attributes.jsDynImportInlineTarget
          else
            None
        }
      }

      val maybeInlined = for {
        importTarget <- maybeImportTarget
        jsNativeLoadSpec <- getJSNativeImportOf(importTarget)
      } yield {
        pretransformExprs(args) { targs =>
          tryOrRollback { cancelFun =>
            val moduleName = freshLocalNameWithoutOriginalName(
                LocalName("module"), mutable = false)

            val moduleParam =
              ParamDef(LocalIdent(moduleName), NoOriginalName, AnyType, mutable = false)

            val importReplacement = ImportReplacement(importTarget, moduleName,
                jsNativeLoadSpec.path, newSimpleState(Unused), cancelFun)

            val newScope = outerScope.withImportReplacement(importReplacement)

            val methodDef = getMethodBody(targetMethod)

            transformCapturingBody(methodDef.args, targs, methodDef.body.get, OptEnv.Empty) {
              (newCaptureParams, newCaptureValues, newBody) =>
                if (!importReplacement.used.value.isUsed)
                  cancelFun()

                val closure = Closure(arrow = true, newCaptureParams, List(moduleParam),
                    restParam = None, newBody, newCaptureValues)

                val newTree = JSImport(config.coreSpec.moduleKind, jsNativeLoadSpec.module, closure)

                PreTransTree(newTree)
            } (cont) (newScope, methodDef.pos)
          } { () =>
            treeNotInlined0(targs.map(finishTransformExpr))
          }
        }
      }

      maybeInlined.getOrElse(treeNotInlined)
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

  private def transformJSLoadCommon(target: ImportTarget, tree: Tree)(
      implicit scope: Scope, pos: Position): Tree = {
    scope.importReplacement match {
      case Some(ImportReplacement(expectedTarget, moduleVarName, path, used, cancelFun)) =>
        if (target != expectedTarget)
          cancelFun()

        used.value = Used
        val module = VarRef(LocalIdent(moduleVarName))(AnyType)
        path.foldLeft[Tree](module) { (inner, pathElem) =>
          JSSelect(inner, StringLiteral(pathElem))
        }

      case None =>
        tree
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
                  alreadyUsed, cancelFun)))
              if !alreadyUsed.value.isUsed && argsNoSpread.size <= params.size =>
            alreadyUsed.value = Used
            val missingArgCount = params.size - argsNoSpread.size
            val expandedArgs =
              if (missingArgCount == 0) argsNoSpread
              else argsNoSpread ::: List.fill(missingArgCount)(Undefined())
            pretransformExprs(expandedArgs) { targs =>
              /* In a JS function, the *declared* type of the `this` value is
               * always `AnyType`, like all the other parameters. In a
               * `JSFunctionApply`, its *actual* value is always `undefined`,
               * by spec of what `JSFunctionApply` does.
               */
              inlineBody(
                  Some((AnyType, PreTransLit(Undefined()))),
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
      "scala.Predef$$less$colon$less",
      "scala.Predef$$eq$colon$eq",

      "scala.reflect.ManifestFactory$ByteManifest$",
      "scala.reflect.ManifestFactory$ShortManifest$",
      "scala.reflect.ManifestFactory$CharManifest$",
      "scala.reflect.ManifestFactory$IntManifest$",
      "scala.reflect.ManifestFactory$LongManifest$",
      "scala.reflect.ManifestFactory$FloatManifest$",
      "scala.reflect.ManifestFactory$DoubleManifest$",
      "scala.reflect.ManifestFactory$BooleanManifest$",
      "scala.reflect.ManifestFactory$UnitManifest$",
      "scala.reflect.ManifestFactory$AnyManifest$",
      "scala.reflect.ManifestFactory$ObjectManifest$",
      "scala.reflect.ManifestFactory$AnyValManifest$",
      "scala.reflect.ManifestFactory$NullManifest$",
      "scala.reflect.ManifestFactory$NothingManifest$"
  ).map(ClassName(_))

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
          case TentativeClosureReplacement(_, _, _, _, _, _)    => true
          case ReplaceWithRecordVarRef(_, _, _, _)              => true
          case InlineClassBeingConstructedReplacement(_, _, _)  => true
          case InlineClassInstanceReplacement(_, _, _)          => true
          case _ =>
            isTypeLikelyOptimizable(localDef.tpe)
        }) && !isLocalOnlyInlineType(localDef.tpe)

      case PreTransRecordTree(_, _, _) =>
        !isLocalOnlyInlineType(arg.tpe)

      case _ =>
        isTypeLikelyOptimizable(arg.tpe)
    }

    receiverAndArgs.exists(isLikelyOptimizable) || {
      target.is(ClassTagModuleClass, ClassTagApplyMethodName) &&
      (receiverAndArgs.tail.head match {
        case PreTransTree(ClassOf(_), _) => true
        case _                           => false
      })
    }
  }

  private def inline(allocationSites: List[AllocationSite],
      optReceiver: Option[(Type, PreTransform)],
      args: List[PreTransform], target: MethodID, isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    require(target.attributes.inlineable)

    attemptedInlining += target

    val MethodDef(flags, _, _, formals, resultType, optBody) = getMethodBody(target)
    assert(flags.namespace.isStatic == optReceiver.isEmpty,
        "There must be receiver if and only if the method is not static")
    val body = optBody.getOrElse {
      throw new AssertionError("A method to inline must be conrete")
    }

    def finishTransformArgsAsStat(): Tree = {
      val newOptReceiver =
        optReceiver.fold[Tree](Skip())(r => finishTransformStat(r._2))
      val newArgs = args.map(finishTransformStat(_))
      Block(newOptReceiver :: newArgs)
    }

    body match {
      case Skip() =>
        assert(isStat, "Found Skip() in expression position")
        cont(PreTransTree(
            finishTransformArgsAsStat(),
            RefinedType.NoRefinedType))

      case _: Literal =>
        cont(PreTransTree(
            Block(finishTransformArgsAsStat(), body),
            RefinedType(body.tpe)))

      case This() if args.isEmpty =>
        assert(optReceiver.isDefined,
            "There was a This(), there should be a receiver")
        cont(optReceiver.get._2)

      case Select(This(), className, field) if formals.isEmpty =>
        assert(optReceiver.isDefined,
            "There was a This(), there should be a receiver")
        pretransformSelectCommon(body.tpe, optReceiver.get._2, className, field,
            isLhsOfAssign = false)(cont)

      case Assign(lhs @ Select(This(), className, field), VarRef(LocalIdent(rhsName)))
          if formals.size == 1 && formals.head.name.name == rhsName =>
        assert(isStat, "Found Assign in expression position")
        assert(optReceiver.isDefined,
            "There was a This(), there should be a receiver")

        val treceiver = optReceiver.get._2
        val trhs = args.head

        if (!isFieldRead(className, field.name)) {
          // Field is never read, discard assign, keep side effects only.
          cont(PreTransTree(Block(finishTransformStat(treceiver),
              finishTransformStat(trhs))))
        } else {
          pretransformSelectCommon(lhs.tpe, treceiver, className, field,
              isLhsOfAssign = true) { tlhs =>
            pretransformAssign(tlhs, args.head)(cont)
          }
        }

      case _ =>
        val targetID = (allocationSites, target)
        inlineBody(optReceiver, formals, resultType, body, args, isStat,
            usePreTransform)(cont)(scope.inlining(targetID), pos)
    }
  }

  private def inlineBody(optReceiver: Option[(Type, PreTransform)],
      formals: List[ParamDef], resultType: Type, body: Tree,
      args: List[PreTransform], isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = tailcall {

    val optReceiverBinding = optReceiver map { receiver =>
      /* If the declaredType is CharType, we must introduce a cast, because we
       * must communicate to the emitter that it has to unbox the value.
       * For other primitive types, unboxes/casts are not necessary, because
       * they would only convert `null` to the zero value of the type. However,
       * calling a method on `null` is UB, so we need not do anything about it.
       */
      val (declaredType, value0) = receiver
      val value =
        if (declaredType == CharType) foldAsInstanceOf(value0, declaredType)
        else value0
      Binding(Binding.This, declaredType, false, value)
    }

    assert(formals.size == args.size,
        "argument count mismatch: " +
        s"inlineBody was called with formals $formals but args $args")

    val argsBindings = for {
      (ParamDef(nameIdent, originalName, tpe, mutable), arg) <- formals zip args
    } yield {
      Binding(nameIdent, originalName, tpe, mutable, arg)
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

  private def pretransformSingleDispatch(flags: ApplyFlags, target: MethodID,
      optTReceiver: Option[PreTransform], targs: List[PreTransform], isStat: Boolean,
      usePreTransform: Boolean)(cont: PreTransCont)(treeNotInlined: => TailRec[Tree])(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    import Intrinsics._

    // In this method, we resolve intrinsics or fall-back to the default.

    val intrinsicCode = intrinsics(flags, target)

    def default = {
      val tall = optTReceiver.toList ::: targs
      val shouldInline = target.attributes.inlineable && (
          target.attributes.shouldInline ||
          shouldInlineBecauseOfArgs(target, tall))

      val allocationSites = tall.map(_.tpe.allocationSite)
      val beingInlined = scope.implsBeingInlined((allocationSites, target))
      if (shouldInline && !beingInlined) {
        val optReceiver = optTReceiver.map((receiverTypeFor(target), _))
        inline(allocationSites, optReceiver, targs, target, isStat, usePreTransform)(cont)
      } else {
        treeNotInlined
      }
    }

    @inline def contTree(result: Tree) = cont(result.toPreTransform)

    @inline def StringClassType = ClassType(BoxedStringClass)

    def cursoryArrayElemType(tpe: ArrayType): Type = {
      if (tpe.arrayTypeRef.dimensions != 1) AnyType
      else (tpe.arrayTypeRef.base match {
        case PrimRef(elemType) => elemType
        case ClassRef(_)       => AnyType
      })
    }

    (intrinsicCode: @switch) match {
      // Not an intrisic

      case -1 =>
        default

      // java.lang.System

      case ArrayCopy =>
        assert(isStat, "System.arraycopy must be used in statement position")
        val List(src, srcPos, dest, destPos, length) = targs.map(finishTransformExpr(_))
        contTree(Transient(SystemArrayCopy(src, srcPos, dest, destPos, length)))

      // scala.runtime.ScalaRunTime object

      case ArrayApply =>
        val List(tarray, tindex) = targs
        tarray.tpe.base match {
          case arrayTpe @ ArrayType(ArrayTypeRef(base, _)) =>
            val array = finishTransformExpr(tarray)
            val index = finishTransformExpr(tindex)
            val elemType = cursoryArrayElemType(arrayTpe)
            contTree(ArraySelect(array, index)(elemType))

          case _ =>
            default
        }

      case ArrayUpdate =>
        val List(tarray, tindex, tvalue) = targs
        tarray.tpe.base match {
          case arrayTpe @ ArrayType(ArrayTypeRef(base, depth)) =>
            val array = finishTransformExpr(tarray)
            val index = finishTransformExpr(tindex)
            val elemType = cursoryArrayElemType(arrayTpe)
            val select = ArraySelect(array, index)(elemType)
            val tunboxedValue = foldAsInstanceOf(tvalue, elemType)
            contTree(Assign(select, finishTransformExpr(tunboxedValue)))
          case _ =>
            default
        }

      case ArrayLength =>
        val tarray = targs.head
        tarray.tpe.base match {
          case _: ArrayType =>
            contTree(Trees.ArrayLength(finishTransformExpr(tarray)))
          case _ =>
            default
        }

      // java.lang.Integer

      case IntegerNLZ =>
        val tvalue = targs.head
        tvalue match {
          case PreTransLit(IntLiteral(value)) =>
            contTree(IntLiteral(Integer.numberOfLeadingZeros(value)))
          case _ =>
            default
        }

      // java.lang.Long

      case LongToString =>
        pretransformApply(ApplyFlags.empty, targs.head,
            MethodIdent(LongImpl.toString_), Nil, StringClassType,
            isStat, usePreTransform)(
            cont)
      case LongCompare =>
        pretransformApply(ApplyFlags.empty, targs.head,
            MethodIdent(LongImpl.compareToRTLong), targs.tail, IntType,
            isStat, usePreTransform)(
            cont)
      case LongDivideUnsigned =>
        pretransformApply(ApplyFlags.empty, targs.head,
            MethodIdent(LongImpl.divideUnsigned), targs.tail,
            ClassType(LongImpl.RuntimeLongClass), isStat, usePreTransform)(
            cont)
      case LongRemainderUnsigned =>
        pretransformApply(ApplyFlags.empty, targs.head,
            MethodIdent(LongImpl.remainderUnsigned), targs.tail,
            ClassType(LongImpl.RuntimeLongClass), isStat, usePreTransform)(
            cont)

      // scala.collection.mutable.ArrayBuilder

      case GenericArrayBuilderResult =>
        val List(runtimeClass, array) = targs.map(finishTransformExpr(_))
        val (resultType, isExact) = runtimeClass match {
          case ClassOf(elemTypeRef) => (ArrayType(ArrayTypeRef.of(elemTypeRef)), true)
          case _                    => (AnyType, false)
        }
        cont(PreTransTree(
            Transient(NativeArrayWrapper(runtimeClass, array)(resultType)),
            RefinedType(resultType, isExact = isExact, isNullable = false)))

      case ArrayBuilderZeroOf =>
        contTree(finishTransformExpr(targs.head) match {
          case ClassOf(PrimRef(tpe)) =>
            /* Note that for CharType we produce a literal int instead of char.
             * This ensures that we fill up the JS array with numbers 0 rather
             * than boxed '\0'. We need to do this because the result() method
             * (see intrinsic right above) will directly feed that JS array to
             * `makeNativeArrayWrapper`, which expects an array of numbers when
             * building an `Array[Char]`.
             */
            tpe match {
              case CharType => IntLiteral(0)
              case NoType   => Undefined()
              case _        => zeroOf(tpe)
            }
          case ClassOf(_) =>
            Null()
          case runtimeClass =>
            Transient(ZeroOf(runtimeClass))
        })

      // java.lang.Class

      case ClassGetComponentType =>
        optTReceiver.get match {
          case PreTransLit(ClassOf(ArrayTypeRef(base, depth))) =>
            contTree(ClassOf(
                if (depth == 1) base
                else ArrayTypeRef(base, depth - 1)))
          case PreTransLit(ClassOf(ClassRef(_))) =>
            contTree(Null())
          case receiver =>
            default
        }

      case ClassGetName =>
        optTReceiver.get match {
          case PreTransMaybeBlock(bindingsAndStats, PreTransLit(ClassOf(typeRef))) =>
            def mappedClassName(className: ClassName): String = {
              RuntimeClassNameMapperImpl.map(
                  config.coreSpec.semantics.runtimeClassNameMapper,
                  className.nameString)
            }

            val nameString = typeRef match {
              case primRef: PrimRef =>
                primRef.displayName
              case ClassRef(className) =>
                mappedClassName(className)
              case ArrayTypeRef(primRef: PrimRef, dimensions) =>
                "[" * dimensions + primRef.charCode
              case ArrayTypeRef(ClassRef(className), dimensions) =>
                "[" * dimensions + "L" + mappedClassName(className) + ";"
            }

            contTree(finishTransformBindings(
                bindingsAndStats, StringLiteral(nameString)))

          case PreTransMaybeBlock(bindingsAndStats, PreTransTree(GetClass(expr), _)) =>
            contTree(finishTransformBindings(
                bindingsAndStats, Transient(ObjectClassName(expr))))

          case _ =>
            default
        }

      // java.lang.reflect.Array

      case ArrayNewInstance =>
        val List(tcomponentType, tlength) = targs
        tcomponentType match {
          case PreTransTree(ClassOf(elementTypeRef), _) =>
            val arrayTypeRef = ArrayTypeRef.of(elementTypeRef)
            contTree(NewArray(arrayTypeRef, List(finishTransformExpr(tlength))))
          case _ =>
            default
        }

      // js.special

      case ObjectLiteral =>
        val List(tprops) = targs
        tprops match {
          case PreTransMaybeBlock(bindingsAndStats,
              PreTransLocalDef(LocalDef(
                  RefinedType(ClassType(JSWrappedArrayClass), _, _),
                  false,
                  InlineClassInstanceReplacement(_, wrappedArrayFields, _)))) =>
            assert(wrappedArrayFields.size == 1)
            val jsArray = wrappedArrayFields.head._2
            jsArray.replacement match {
              case InlineJSArrayReplacement(elemLocalDefs, _)
                  if elemLocalDefs.forall(e => isSubtype(e.tpe.base, ClassType(Tuple2Class))) =>
                val fields: List[(Tree, Tree)] = for {
                  (elemLocalDef, idx) <- elemLocalDefs.toList.zipWithIndex
                } yield {
                  elemLocalDef match {
                    case LocalDef(RefinedType(ClassType(Tuple2Class), _, _), false,
                        InlineClassInstanceReplacement(structure, tupleFields, _)) =>
                      val List(key, value) = structure.fieldIDs.map(tupleFields)
                      (key.newReplacement, value.newReplacement)

                    case _ =>
                      val flags = ApplyFlags.empty
                      val key = Apply(flags, elemLocalDef.newReplacement,
                          MethodIdent(TupleFirstMethodName), Nil)(AnyType)
                      val value = Apply(flags, elemLocalDef.newReplacement,
                          MethodIdent(TupleSecondMethodName), Nil)(AnyType)
                      (key, value)
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
              case RefinedType(ClassType(NilClass), _, false) =>
                contTree(Block(finishTransformStat(tprops), JSObjectConstr(Nil)))
              case _ =>
                default
            }
        }

      // TypedArray conversions

      case ByteArrayToInt8Array =>
        contTree(Transient(ArrayToTypedArray(finishTransformExpr(targs.head), ByteRef)))
      case ShortArrayToInt16Array =>
        contTree(Transient(ArrayToTypedArray(finishTransformExpr(targs.head), ShortRef)))
      case CharArrayToUint16Array =>
        contTree(Transient(ArrayToTypedArray(finishTransformExpr(targs.head), CharRef)))
      case IntArrayToInt32Array =>
        contTree(Transient(ArrayToTypedArray(finishTransformExpr(targs.head), IntRef)))
      case FloatArrayToFloat32Array =>
        contTree(Transient(ArrayToTypedArray(finishTransformExpr(targs.head), FloatRef)))
      case DoubleArrayToFloat64Array =>
        contTree(Transient(ArrayToTypedArray(finishTransformExpr(targs.head), DoubleRef)))

      case Int8ArrayToByteArray =>
        contTree(Transient(TypedArrayToArray(finishTransformExpr(targs.head), ByteRef)))
      case Int16ArrayToShortArray =>
        contTree(Transient(TypedArrayToArray(finishTransformExpr(targs.head), ShortRef)))
      case Uint16ArrayToCharArray =>
        contTree(Transient(TypedArrayToArray(finishTransformExpr(targs.head), CharRef)))
      case Int32ArrayToIntArray =>
        contTree(Transient(TypedArrayToArray(finishTransformExpr(targs.head), IntRef)))
      case Float32ArrayToFloatArray =>
        contTree(Transient(TypedArrayToArray(finishTransformExpr(targs.head), FloatRef)))
      case Float64ArrayToDoubleArray =>
        contTree(Transient(TypedArrayToArray(finishTransformExpr(targs.head), DoubleRef)))
    }
  }

  private def inlineClassConstructor(allocationSite: AllocationSite,
      className: ClassName, structure: InlineableClassStructure,
      ctor: MethodIdent, args: List[PreTransform], cancelFun: CancelFun)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    val initialFieldBindings = for {
      RecordType.Field(name, originalName, tpe, mutable) <- structure.recordType.fields
    } yield {
      Binding(Binding.Local(name.toLocalName, originalName), tpe, mutable,
          PreTransTree(zeroOf(tpe)))
    }

    withNewLocalDefs(initialFieldBindings) { (initialFieldLocalDefList, cont1) =>
      val initialFieldLocalDefs =
        structure.fieldIDs.zip(initialFieldLocalDefList).toMap

      inlineClassConstructorBody(allocationSite, structure, initialFieldLocalDefs,
          className, className, ctor, args, cancelFun) { (finalFieldLocalDefs, cont2) =>
        cont2(LocalDef(
            RefinedType(ClassType(className), isExact = true,
                isNullable = false, allocationSite = allocationSite),
            mutable = false,
            InlineClassInstanceReplacement(structure, finalFieldLocalDefs,
                cancelFun)).toPreTransform)
      } (cont1)
    } (cont)
  }

  private def inlineClassConstructorBody(
      allocationSite: AllocationSite, structure: InlineableClassStructure,
      inputFieldsLocalDefs: Map[FieldID, LocalDef], className: ClassName,
      ctorClass: ClassName, ctor: MethodIdent, args: List[PreTransform],
      cancelFun: CancelFun)(
      buildInner: (Map[FieldID, LocalDef], PreTransCont) => TailRec[Tree])(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = tailcall {

    val target = staticCall(ctorClass, MemberNamespace.Constructor, ctor.name)
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
      (ParamDef(nameIdent, originalName, tpe, mutable), arg) <- formals zip args
    } yield {
      Binding(nameIdent, originalName, tpe, mutable, arg)
    }

    withBindings(argsBindings) { (bodyScope, cont1) =>
      val thisLocalDef = LocalDef(
          RefinedType(ClassType(className), isExact = true, isNullable = false),
          false,
          InlineClassBeingConstructedReplacement(structure, inputFieldsLocalDefs, cancelFun))
      val statsScope = bodyScope.inlining(targetID).withEnv(
          bodyScope.env.withThisLocalDef(thisLocalDef))
      inlineClassConstructorBodyList(allocationSite, structure, thisLocalDef,
          inputFieldsLocalDefs, className, stats, cancelFun)(
          buildInner)(cont1)(statsScope)
    } (cont) (scope.withEnv(OptEnv.Empty))
  }

  private def inlineClassConstructorBodyList(
      allocationSite: AllocationSite, structure: InlineableClassStructure,
      thisLocalDef: LocalDef, inputFieldsLocalDefs: Map[FieldID, LocalDef],
      className: ClassName, stats: List[Tree], cancelFun: CancelFun)(
      buildInner: (Map[FieldID, LocalDef], PreTransCont) => TailRec[Tree])(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {

    def withStat(stat: Tree, rest: List[Tree]): TailRec[Tree] = {
      val transformedStat = transformStat(stat)
      transformedStat match {
        case Skip() =>
          inlineClassConstructorBodyList(allocationSite, structure,
              thisLocalDef, inputFieldsLocalDefs,
              className, rest, cancelFun)(buildInner)(cont)
        case _ =>
          if (transformedStat.tpe == NothingType)
            cont(PreTransTree(transformedStat, RefinedType.Nothing))
          else {
            inlineClassConstructorBodyList(allocationSite, structure,
                thisLocalDef, inputFieldsLocalDefs,
                className, rest, cancelFun)(buildInner) { tinner =>
              cont(PreTransBlock(transformedStat, tinner))
            }
          }
      }
    }

    stats match {
      case This() :: rest =>
        inlineClassConstructorBodyList(allocationSite, structure, thisLocalDef,
            inputFieldsLocalDefs, className, rest, cancelFun)(buildInner)(cont)

      case Assign(s @ Select(ths: This, className, field), value) :: rest
          if !inputFieldsLocalDefs.contains(FieldID(className, field)) =>
        // Field is being optimized away. Only keep side effects of the write.
        withStat(value, rest)

      case Assign(s @ Select(ths: This, className, field), value) :: rest
          if !inputFieldsLocalDefs(FieldID(className, field)).mutable =>
        pretransformExpr(value) { tvalue =>
          val fieldID = FieldID(className, field)
          val originalName = structure.fieldOriginalName(fieldID)
          val binding = Binding(
              Binding.Local(field.name.toLocalName, originalName),
              s.tpe, false, tvalue)
          withNewLocalDef(binding) { (localDef, cont1) =>
            if (localDef.contains(thisLocalDef)) {
              /* Uh oh, there is a `val x = ...this...`. We can't keep it,
               * because this field will not be updated with `newThisLocalDef`.
               */
              cancelFun()
            }
            val newFieldsLocalDefs =
              inputFieldsLocalDefs.updated(fieldID, localDef)
            val newThisLocalDef = LocalDef(thisLocalDef.tpe, false,
                InlineClassBeingConstructedReplacement(structure, newFieldsLocalDefs, cancelFun))
            val restScope =
              scope.withEnv(scope.env.withThisLocalDef(newThisLocalDef))
            inlineClassConstructorBodyList(allocationSite, structure,
                newThisLocalDef, newFieldsLocalDefs, className, rest, cancelFun)(
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
      case If(cond, th: Throw, Assign(Select(This(), _, _), value)) :: rest =>
        // work around a bug of the compiler (these should be @-bindings)
        val stat = stats.head.asInstanceOf[If]
        val ass = stat.elsep.asInstanceOf[Assign]
        val lhs = ass.lhs
        inlineClassConstructorBodyList(allocationSite, structure, thisLocalDef,
            inputFieldsLocalDefs, className,
            Assign(lhs, If(cond, th, value)(lhs.tpe)(stat.pos))(ass.pos) :: rest,
            cancelFun)(buildInner)(cont)

      case ApplyStatically(flags, ths: This, superClass, superCtor, args) :: rest
          if flags.isConstructor =>
        pretransformExprs(args) { targs =>
          inlineClassConstructorBody(allocationSite, structure,
              inputFieldsLocalDefs, className, superClass, superCtor, targs,
              cancelFun) { (outputFieldsLocalDefs, cont1) =>
            val newThisLocalDef = LocalDef(thisLocalDef.tpe, false,
                InlineClassBeingConstructedReplacement(structure, outputFieldsLocalDefs, cancelFun))
            val restScope =
              scope.withEnv(scope.env.withThisLocalDef(newThisLocalDef))
            inlineClassConstructorBodyList(allocationSite, structure,
                newThisLocalDef, outputFieldsLocalDefs,
                className, rest, cancelFun)(buildInner)(cont1)(restScope)
          } (cont)
        }

      case VarDef(nameIdent, originalName, tpe, mutable, rhs) :: rest =>
        pretransformExpr(rhs) { trhs =>
          withBinding(Binding(nameIdent, originalName, tpe, mutable, trhs)) { (restScope, cont1) =>
            inlineClassConstructorBodyList(allocationSite, structure,
                thisLocalDef, inputFieldsLocalDefs,
                className, rest, cancelFun)(buildInner)(cont1)(restScope)
          } (cont)
        }

      case stat :: rest =>
        withStat(stat, rest)

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
            case (JSBinaryOp(JSBinaryOp.===, VarRef(lhsIdent), Null()),
                JSBinaryOp(JSBinaryOp.===, VarRef(rhsIdent), Null()),
                JSBinaryOp(JSBinaryOp.===, VarRef(lhsIdent2), VarRef(rhsIdent2)))
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
    val tName = LocalName("t")
    val rtLongClassType = ClassType(LongImpl.RuntimeLongClass)
    val rtLongBinding = Binding.temp(tName, rtLongClassType, mutable = false,
        value)
    withBinding(rtLongBinding) { (scope1, cont1) =>
      implicit val scope = scope1
      val tRef = VarRef(LocalIdent(tName))(rtLongClassType)
      val newTree = New(LongImpl.RuntimeLongClass,
          MethodIdent(LongImpl.initFromParts),
          List(Apply(ApplyFlags.empty, tRef, MethodIdent(LongImpl.lo), Nil)(IntType),
              Apply(ApplyFlags.empty, tRef, MethodIdent(LongImpl.hi), Nil)(IntType)))
      pretransformExpr(newTree)(cont1)
    } (cont)
  }

  private def expandLongOps(pretrans: PreTransform)(cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    implicit val pos = pretrans.pos

    def rtLongClassType = ClassType(LongImpl.RuntimeLongClass)

    def expandLongModuleOp(methodName: MethodName,
        arg: PreTransform): TailRec[Tree] = {
      val receiver = LoadModule(LongImpl.RuntimeLongModuleClass).toPreTransform
      pretransformApply(ApplyFlags.empty, receiver, MethodIdent(methodName),
          arg :: Nil, rtLongClassType, isStat = false,
          usePreTransform = true)(
          cont)
    }

    def expandUnaryOp(methodName: MethodName, arg: PreTransform,
        resultType: Type = rtLongClassType): TailRec[Tree] = {
      pretransformApply(ApplyFlags.empty, arg, MethodIdent(methodName), Nil,
          resultType, isStat = false, usePreTransform = true)(
          cont)
    }

    def expandBinaryOp(methodName: MethodName, lhs: PreTransform,
        rhs: PreTransform,
        resultType: Type = rtLongClassType): TailRec[Tree] = {
      pretransformApply(ApplyFlags.empty, lhs, MethodIdent(methodName), rhs :: Nil,
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

          case LongToFloat =>
            expandUnaryOp(LongImpl.toFloat, arg, FloatType)

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

          case PreTransJSBinaryOp(innerOp, l, r) =>
            val newOp = innerOp match {
              case JSBinaryOp.=== => JSBinaryOp.!==
              case JSBinaryOp.!== => JSBinaryOp.===

              case _ => -1
            }
            if (newOp == -1) default
            else PreTransJSBinaryOp(newOp, l, r)

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

      // Long -> Float

      case LongToFloat =>
        arg match {
          case PreTransLit(LongLiteral(v)) =>
            PreTransLit(FloatLiteral(v.toFloat))
          case PreTransUnaryOp(IntToLong, x) =>
            foldUnaryOp(DoubleToFloat, foldUnaryOp(IntToDouble, x))
          case _ =>
            default
        }

      // String.length

      case String_length =>
        arg match {
          case PreTransLit(StringLiteral(s)) =>
            PreTransLit(IntLiteral(s.length()))
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
   *  `===` comparison, since they all upcast as primitive numbers. If
   *  `isJSStrictEq` is false, they are compared with `equals()` instead of
   *  `==` so that `NaN === NaN` and `+0.0 !== -0.0`.
   *
   *  Chars and Longs, however, never compare as `===`, since they are boxed
   *  chars and instances of `RuntimeLong`, respectively---unless we are using
   *  `BigInt`s for `Long`s, in which case those can be `===`.
   */
  private def literal_===(lhs: Literal, rhs: Literal,
      isJSStrictEq: Boolean): Boolean = {

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
      case (AnyNumLiteral(l), AnyNumLiteral(r))   => if (isJSStrictEq) l == r else l.equals(r)
      case (LongLiteral(l), LongLiteral(r))       => l == r && !useRuntimeLong
      case (Undefined(), Undefined())             => true
      case (Null(), Null())                       => true
      case _                                      => false
    }
  }

  /** Performs `===` for two matchable literals.
   *
   *  This corresponds to the test used by a `Match` at run-time, to decide
   *  which case is selected.
   *
   *  The result is always known statically.
   */
  private def matchableLiteral_===(lhs: MatchableLiteral,
      rhs: MatchableLiteral): Boolean = {
    (lhs, rhs) match {
      case (IntLiteral(l), IntLiteral(r))       => l == r
      case (StringLiteral(l), StringLiteral(r)) => l == r
      case (Null(), Null())                     => true
      case _                                    => false
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

  private val MaybeHijackedPrimNumberClasses = {
    /* In theory, we could figure out the ancestors from the global knowledge,
     * but that would be overkill.
     */
    Set(BoxedByteClass, BoxedShortClass, BoxedIntegerClass, BoxedFloatClass,
        BoxedDoubleClass, ObjectClass, ClassName("java.lang.CharSequence"),
        ClassName("java.io.Serializable"), ClassName("java.lang.Comparable"),
        ClassName("java.lang.Number"))
  }

  private def foldBinaryOp(op: BinaryOp.Code, lhs: PreTransform,
      rhs: PreTransform)(
      implicit pos: Position): PreTransform = {
    import BinaryOp._

    @inline def default: PreTransform =
      PreTransBinaryOp(op, lhs, rhs)

    @inline def booleanLit(value: Boolean): PreTransform =
      PreTransLit(BooleanLiteral(value))
    @inline def charLit(value: Char): PreTransform =
      PreTransLit(CharLiteral(value))
    @inline def intLit(value: Int): PreTransform =
      PreTransLit(IntLiteral(value))
    @inline def longLit(value: Long): PreTransform =
      PreTransLit(LongLiteral(value))
    @inline def floatLit(value: Float): PreTransform =
      PreTransLit(FloatLiteral(value))
    @inline def doubleLit(value: Double): PreTransform =
      PreTransLit(DoubleLiteral(value))

    (op: @switch) match {
      case === | !== =>
        // Try to optimize as a primitive JS strict equality

        def canBePrimitiveNum(tpe: RefinedType): Boolean = tpe.base match {
          case AnyType | ByteType | ShortType | IntType | FloatType | DoubleType =>
            true
          case ClassType(className) =>
            /* If `className` is a concrete superclass of a boxed number class,
             * then it can be exact, and in that case we know that it cannot be
             * a primitive number. In practice this happens only for
             * `java.lang.Object`, and especially for code generated for
             * non-local returns in Scala.
             */
            !tpe.isExact && MaybeHijackedPrimNumberClasses.contains(className)
          case _ =>
            false
        }

        def isWhole(tpe: RefinedType): Boolean = tpe.base match {
          case ByteType | ShortType | IntType =>
            true
          case ClassType(className) =>
            className == BoxedByteClass ||
            className == BoxedShortClass ||
            className == BoxedIntegerClass
          case _ =>
            false
        }

        def canOptimizeAsJSStrictEq(lhsTpe: RefinedType, rhsTpe: RefinedType): Boolean = (
            !canBePrimitiveNum(lhsTpe) ||
            !canBePrimitiveNum(rhsTpe) ||
            (isWhole(lhsTpe) && isWhole(rhsTpe))
        )

        (lhs, rhs) match {
          case (PreTransLit(l), PreTransLit(r)) =>
            val isSame = literal_===(l, r, isJSStrictEq = false)
            PreTransLit(BooleanLiteral(if (op == ===) isSame else !isSame))
          case _ if canOptimizeAsJSStrictEq(lhs.tpe, rhs.tpe) =>
            foldJSBinaryOp(
                if (op == ===) JSBinaryOp.=== else JSBinaryOp.!==,
                lhs, rhs)
          case _ =>
            default
        }

      case String_+ =>
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

      case Boolean_== | Boolean_!= =>
        val positive = (op == Boolean_==)
        (lhs, rhs) match {
          case (PreTransLit(BooleanLiteral(l)), PreTransLit(BooleanLiteral(r))) =>
            booleanLit(if (positive) l == r else l != r)
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
          case (PreTransLit(BooleanLiteral(l)), PreTransLit(BooleanLiteral(r))) =>
            booleanLit(l | r)

          case (_, PreTransLit(BooleanLiteral(false))) => lhs
          case (PreTransLit(BooleanLiteral(false)), _) => rhs

          case _ => default
        }

      case Boolean_& =>
        (lhs, rhs) match {
          case (PreTransLit(BooleanLiteral(l)), PreTransLit(BooleanLiteral(r))) =>
            booleanLit(l & r)

          case (_, PreTransLit(BooleanLiteral(true))) => lhs
          case (PreTransLit(BooleanLiteral(true)), _) => rhs

          case _ => default
        }

      case Int_+ =>
        (lhs, rhs) match {
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            intLit(l + r)
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
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            intLit(l - r)
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
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            intLit(l * r)
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
          case (_, PreTransLit(IntLiteral(0))) =>
            default
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            intLit(l / r)

          case (_, PreTransLit(IntLiteral(1)))  =>
            lhs
          case (_, PreTransLit(IntLiteral(-1))) =>
            foldBinaryOp(Int_-, PreTransLit(IntLiteral(0)), lhs)

          case _ => default
        }

      case Int_% =>
        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(0))) =>
            default
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            intLit(l % r)

          case (_, PreTransLit(IntLiteral(1 | -1))) =>
            Block(finishTransformStat(lhs), IntLiteral(0)).toPreTransform

          case _ => default
        }

      case Int_| =>
        (lhs, rhs) match {
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            intLit(l | r)

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
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            intLit(l & r)

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
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            intLit(l ^ r)

          case (_, PreTransLit(IntLiteral(_))) => foldBinaryOp(Int_^, rhs, lhs)
          case (PreTransLit(IntLiteral(0)), _) => rhs

          case (PreTransLit(IntLiteral(x)),
              PreTransBinaryOp(Int_^, PreTransLit(IntLiteral(y)), z)) =>
            foldBinaryOp(Int_^, PreTransLit(IntLiteral(x ^ y)), z)

          case _ => default
        }

      case Int_<< =>
        (lhs, rhs) match {
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            intLit(l << r)

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
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            intLit(l >>> r)

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
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            intLit(l >> r)

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

      case Int_== | Int_!= =>
        (lhs, rhs) match {
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            booleanLit(if (op == Int_==) l == r else l != r)

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
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            booleanLit((op: @switch) match {
              case Int_<  => l < r
              case Int_<= => l <= r
              case Int_>  => l > r
              case Int_>= => l >= r
            })

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

      case Long_+ =>
        (lhs, rhs) match {
          case (PreTransLit(LongLiteral(l)), PreTransLit(LongLiteral(r))) =>
            longLit(l + r)

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
          case (PreTransLit(LongLiteral(l)), PreTransLit(LongLiteral(r))) =>
            longLit(l - r)

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
          case (PreTransLit(LongLiteral(l)), PreTransLit(LongLiteral(r))) =>
            longLit(l * r)

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
          case (_, PreTransLit(LongLiteral(0L))) =>
            default
          case (PreTransLit(LongLiteral(l)), PreTransLit(LongLiteral(r))) =>
            longLit(l / r)

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
          case (_, PreTransLit(LongLiteral(0L))) =>
            default
          case (PreTransLit(LongLiteral(l)), PreTransLit(LongLiteral(r))) =>
            longLit(l % r)

          case (_, PreTransLit(LongLiteral(1L | -1L))) =>
            Block(finishTransformStat(lhs), LongLiteral(0L)).toPreTransform

          case (LongFromInt(x), LongFromInt(y)) =>
            LongFromInt(foldBinaryOp(Int_%, x, y))

          case _ => default
        }

      case Long_| =>
        (lhs, rhs) match {
          case (PreTransLit(LongLiteral(l)), PreTransLit(LongLiteral(r))) =>
            longLit(l | r)

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
          case (PreTransLit(LongLiteral(l)), PreTransLit(LongLiteral(r))) =>
            longLit(l & r)

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
          case (PreTransLit(LongLiteral(l)), PreTransLit(LongLiteral(r))) =>
            longLit(l ^ r)

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
          case (PreTransLit(LongLiteral(l)), PreTransLit(IntLiteral(r))) =>
            longLit(l << r)

          case (_, PreTransLit(IntLiteral(x))) if x % 64 == 0 => lhs

          case _ => default
        }

      case Long_>>> =>
        (lhs, rhs) match {
          case (PreTransLit(LongLiteral(l)), PreTransLit(IntLiteral(r))) =>
            longLit(l >>> r)

          case (_, PreTransLit(IntLiteral(x))) if x % 64 == 0 => lhs

          case _ => default
        }

      case Long_>> =>
        (lhs, rhs) match {
          case (PreTransLit(LongLiteral(l)), PreTransLit(IntLiteral(r))) =>
            longLit(l >> r)

          case (_, PreTransLit(IntLiteral(x))) if x % 64 == 0 => lhs

          case _ => default
        }

      case Long_== | Long_!= =>
        val positive = (op == Long_==)
        (lhs, rhs) match {
          case (PreTransLit(LongLiteral(l)), PreTransLit(LongLiteral(r))) =>
            booleanLit(if (op == Long_==) l == r else l != r)

          case (LongFromInt(x), LongFromInt(y)) =>
            foldBinaryOp(if (positive) Int_== else Int_!=, x, y)
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
          case (PreTransLit(LongLiteral(l)), PreTransLit(LongLiteral(r))) =>
            booleanLit((op: @switch) match {
              case Long_<  => l < r
              case Long_<= => l <= r
              case Long_>  => l > r
              case Long_>= => l >= r
            })

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

              withNewTempLocalDefs(List(x, y)) {
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
          case (PreTransLit(FloatLiteral(l)), PreTransLit(FloatLiteral(r))) =>
            floatLit(l + r)

          case _ => default
        }

      case Float_- =>
        (lhs, rhs) match {
          case (PreTransLit(FloatLiteral(l)), PreTransLit(FloatLiteral(r))) =>
            floatLit(l - r)

          case _ => default
        }

      case Float_* =>
        (lhs, rhs) match {
          case (PreTransLit(FloatLiteral(l)), PreTransLit(FloatLiteral(r))) =>
            floatLit(l * r)

          case (_, PreTransLit(FloatLiteral(_))) =>
            foldBinaryOp(Float_*, rhs, lhs)

          case (PreTransLit(FloatLiteral(1)), _) =>
            rhs
          case (PreTransLit(FloatLiteral(-1)),
              PreTransBinaryOp(Float_*, PreTransLit(FloatLiteral(-1)), z)) =>
            z

          case _ => default
        }

      case Float_/ =>
        (lhs, rhs) match {
          case (PreTransLit(FloatLiteral(l)), PreTransLit(FloatLiteral(r))) =>
            floatLit(l / r)

          case (_, PreTransLit(FloatLiteral(1))) =>
            lhs
          case (_, PreTransLit(FloatLiteral(-1))) =>
            foldBinaryOp(Float_*, PreTransLit(FloatLiteral(-1)), lhs)

          case _ => default
        }

      case Float_% =>
        (lhs, rhs) match {
          case (PreTransLit(FloatLiteral(l)), PreTransLit(FloatLiteral(r))) =>
            floatLit(l % r)

          case _ => default
        }

      case Double_+ =>
        (lhs, rhs) match {
          case (PreTransLit(DoubleLiteral(l)), PreTransLit(DoubleLiteral(r))) =>
            doubleLit(l + r)

          case _ => default
        }

      case Double_- =>
        (lhs, rhs) match {
          case (PreTransLit(DoubleLiteral(l)), PreTransLit(DoubleLiteral(r))) =>
            doubleLit(l - r)

          case _ => default
        }

      case Double_* =>
        (lhs, rhs) match {
          case (PreTransLit(DoubleLiteral(l)), PreTransLit(DoubleLiteral(r))) =>
            doubleLit(l * r)

          case (_, PreTransLit(DoubleLiteral(_))) =>
            foldBinaryOp(Double_*, rhs, lhs)

          case (PreTransLit(DoubleLiteral(1)), _) =>
            rhs
          case (PreTransLit(DoubleLiteral(-1)),
              PreTransBinaryOp(Double_*, PreTransLit(DoubleLiteral(-1)), z)) =>
            z

          case _ => default
        }

      case Double_/ =>
        (lhs, rhs) match {
          case (PreTransLit(DoubleLiteral(l)), PreTransLit(DoubleLiteral(r))) =>
            doubleLit(l / r)

          case (_, PreTransLit(DoubleLiteral(1))) =>
            lhs
          case (_, PreTransLit(DoubleLiteral(-1))) =>
            foldBinaryOp(Double_*, PreTransLit(DoubleLiteral(-1)), lhs)

          case _ => default
        }

      case Double_% =>
        (lhs, rhs) match {
          case (PreTransLit(DoubleLiteral(l)), PreTransLit(DoubleLiteral(r))) =>
            doubleLit(l % r)

          case _ => default
        }

      case Double_== =>
        (lhs, rhs) match {
          case (PreTransLit(DoubleLiteral(l)), PreTransLit(DoubleLiteral(r))) =>
            booleanLit(l == r)

          case _ => default
        }

      case Double_!= =>
        (lhs, rhs) match {
          case (PreTransLit(DoubleLiteral(l)), PreTransLit(DoubleLiteral(r))) =>
            booleanLit(l != r)

          case _ => default
        }

      case Double_< =>
        (lhs, rhs) match {
          case (PreTransLit(DoubleLiteral(l)), PreTransLit(DoubleLiteral(r))) =>
            booleanLit(l < r)

          case _ => default
        }

      case Double_<= =>
        (lhs, rhs) match {
          case (PreTransLit(DoubleLiteral(l)), PreTransLit(DoubleLiteral(r))) =>
            booleanLit(l <= r)

          case _ => default
        }

      case Double_> =>
        (lhs, rhs) match {
          case (PreTransLit(DoubleLiteral(l)), PreTransLit(DoubleLiteral(r))) =>
            booleanLit(l > r)

          case _ => default
        }

      case Double_>= =>
        (lhs, rhs) match {
          case (PreTransLit(DoubleLiteral(l)), PreTransLit(DoubleLiteral(r))) =>
            booleanLit(l >= r)

          case _ => default
        }

      case String_charAt =>
        (lhs, rhs) match {
          case (PreTransLit(StringLiteral(l)), PreTransLit(IntLiteral(r))) =>
            if (r < 0 || r >= l.length())
              default
            else
              charLit(l.charAt(r))

          case _ => default
        }
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

  private def foldJSBinaryOp(op: JSBinaryOp.Code, lhs: PreTransform,
      rhs: PreTransform)(
      implicit pos: Position): PreTransform = {
    import JSBinaryOp._

    def default: PreTransform =
      PreTransJSBinaryOp(op, lhs, rhs)

    op match {
      case JSBinaryOp.=== | JSBinaryOp.!== =>
        val positive = (op == JSBinaryOp.===)
        (lhs, rhs) match {
          case (PreTransLit(l), PreTransLit(r)) =>
            val isEq = literal_===(l, r, isJSStrictEq = true)
            PreTransLit(BooleanLiteral(if (positive) isEq else !isEq))

          case (_, PreTransLit(Null())) if !lhs.tpe.isNullable =>
            Block(
                finishTransformStat(lhs),
                BooleanLiteral(!positive)).toPreTransform

          case (PreTransLit(_), _) => foldBinaryOp(op, rhs, lhs)
          case _                   => default
        }

      case _ =>
        default
    }
  }

  private def foldAsInstanceOf(arg: PreTransform, tpe: Type)(
      implicit pos: Position): PreTransform = {
    if (isSubtype(arg.tpe.base, tpe))
      arg
    else
      AsInstanceOf(finishTransformExpr(arg), tpe).toPreTransform
  }

  private def foldJSSelect(qualifier: Tree, item: Tree)(
      implicit pos: Position): Tree = {
    // !!! Must be in sync with scala.scalajs.runtime.LinkingInfo

    import config.coreSpec.esFeatures

    (qualifier, item) match {
      case (JSLinkingInfo(), StringLiteral("productionMode")) =>
        BooleanLiteral(semantics.productionMode)

      case (JSLinkingInfo(), StringLiteral("esVersion")) =>
        IntLiteral(esFeatures.esVersion.edition)

      case (JSLinkingInfo(), StringLiteral("assumingES6")) =>
        BooleanLiteral(esFeatures.useECMAScript2015Semantics)

      case (JSLinkingInfo(), StringLiteral("version")) =>
        StringLiteral(ScalaJSVersions.current)

      case _ =>
        JSSelect(qualifier, item)
    }
  }

  private def transformMethodDefBody(target: MethodID, thisType: Type,
      params: List[ParamDef], resultType: Type, body: Tree): (List[ParamDef], Tree) = {

    val (paramLocalDefs, newParamDefs) = params.map(newParamReplacement(_)).unzip

    val thisLocalDef =
      if (thisType == NoType) None
      else Some(newThisLocalDef(thisType))

    val inlining = {
      val allocationSiteCount =
        paramLocalDefs.size + (if (thisLocalDef.isDefined) 1 else 0)
      val allocationSites =
        List.fill(allocationSiteCount)(AllocationSite.Anonymous)
      allocationSites -> target
    }

    val env = OptEnv.Empty
      .withThisLocalDef(thisLocalDef)
      .withLocalDefs(paramLocalDefs)

    val scope = Scope.Empty.inlining(inlining).withEnv(env)

    val newBody = transform(body, resultType == NoType)(scope)

    (newParamDefs, newBody)
  }

  private def pretransformLabeled(oldLabelName: LabelName, resultType: Type,
      body: Tree, isStat: Boolean, usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = tailcall {
    val newLabel = freshLabelName(oldLabelName)

    def doMakeTree(newBody: Tree, returnedTypes: List[Type]): Tree = {
      val refinedType =
        returnedTypes.reduce(constrainedLub(_, _, resultType))
      val returnCount = returnedTypes.size - 1

      tryOptimizePatternMatch(oldLabelName, newLabel, refinedType,
          returnCount, newBody) getOrElse {
        Labeled(LabelIdent(newLabel), refinedType, newBody)
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
  def tryOptimizePatternMatch(oldLabelName: LabelName, newLabelName: LabelName,
      refinedType: Type, returnCount: Int, body: Tree): Option[Tree] = {
    // Heuristic for speed: only try to optimize labels likely named 'matchEnd...'
    val isMaybeMatchEndLabel = {
      val oldEncodedName = oldLabelName.encoded
      oldEncodedName.length >= 8 && oldEncodedName(0) == 'm' &&
      oldEncodedName(1) == 'a' && oldEncodedName(2) == 't' // stop here
    }
    if (!isMaybeMatchEndLabel) {
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
              case BlockOrAlone(prep, Return(result, LabelIdent(`newLabelName`))) =>
                val result1 =
                  if (refinedType == NoType) keepOnlySideEffects(result)
                  else result
                Some(Block(prep, result1)(body.pos))

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

  private def newParamReplacement(paramDef: ParamDef): ((LocalName, LocalDef), ParamDef) = {
    val ParamDef(ident @ LocalIdent(name), originalName, ptpe, mutable) = paramDef

    val (newName, newOriginalName) = freshLocalName(name, originalName, mutable)

    val replacement = ReplaceWithVarRef(newName, newSimpleState(Unused), None)
    val localDef = LocalDef(RefinedType(ptpe), mutable, replacement)
    val localIdent = LocalIdent(newName)(ident.pos)
    val newParamDef = ParamDef(localIdent, newOriginalName, ptpe, mutable)(paramDef.pos)
    (name -> localDef, newParamDef)
  }

  private def newThisLocalDef(thisType: Type): LocalDef = {
    LocalDef(
        RefinedType(thisType, isExact = false, isNullable = false),
        false, ReplaceWithThis())
  }

  private def withBindings(bindings: List[Binding])(
      buildInner: (Scope, PreTransCont) => TailRec[Tree])(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    withNewLocalDefs(bindings) { (localDefs, cont1) =>
      val newEnv = bindings.zip(localDefs).foldLeft(scope.env) {
        (prevEnv, bindingAndLocalDef) =>
          prevEnv.withLocalDef(bindingAndLocalDef._1.name, bindingAndLocalDef._2)
      }
      buildInner(scope.withEnv(newEnv), cont1)
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

  private def withNewTempLocalDefs(texprs: List[PreTransform])(
      buildInner: (List[LocalDef], PreTransCont) => TailRec[Tree])(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    val bindings = {
      for ((texpr, index) <- texprs.zipWithIndex) yield
        Binding.temp(LocalName("x" + index), texpr)
    }
    withNewLocalDefs(bindings)(buildInner)(cont)
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
    val Binding(bindingName, declaredType, mutable, value) = binding
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
          val expandedBinding = Binding(bindingName, rtLongClassType,
              mutable, expandedValue)
          withNewLocalDef(expandedBinding)(buildInner)(cont)
        }
      } else {
        // Otherwise, we effectively declare a new binding
        val (newName, newOriginalName) = freshLocalName(bindingName, mutable)

        val used = newSimpleState[IsUsed](Unused)

        val (replacement, refinedType) = resolveRecordType(value) match {
          case Some((recordType, cancelFun)) =>
            (ReplaceWithRecordVarRef(newName, recordType, used, cancelFun), value.tpe)

          case None =>
            (ReplaceWithVarRef(newName, used, None), tpe)
        }

        val localDef = LocalDef(refinedType, mutable, replacement)
        val preTransBinding = PreTransBinding(newOriginalName, localDef, value)

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
      def computeRefinedType(): RefinedType = bindingName match {
        case _ if value.tpe.isExact || declaredType == AnyType =>
          /* If the value's type is exact, or if the declared type is `AnyType`,
           * the declared type cannot have any new information to give us, so
           * we directly return `value.tpe`. This avoids a useless `isSubtype`
           * call, which creates dependencies for incremental optimization.
           *
           * In addition, for the case `declaredType == AnyType` there is a
           * stronger reason: we don't actually know that `this` is non-null in
           * that case, since it could be the `this` value of a JavaScript
           * function, which can accept `null`. (As of this writing, this is
           * theoretical, because the only place where we use a declared type
           * of `AnyType` is in `JSFunctionApply`, where the actual value for
           * `this` is always `undefined`.)
           */
          value.tpe

        case _: Binding.Local =>
          /* When binding a something else than `this`, we do not receive the
           * non-null information. Moreover, there is no situation where the
           * declared type would bring any new information, since that would
           * not be valid IR in the first place. Therefore, to avoid a useless
           * call to `isSubtype`, we directly return `value.tpe`.
           */
          value.tpe

        case Binding.This =>
          /* When binding to `this`, if the declared type is not `AnyType`,
           * we are in a situation where
           * a) we know the value must be non-null, and
           * b) the declaredType may bring more precise information than
           *    value.tpe.base (typically when inlining a polymorphic method
           *    that ends up having only one target in a subclass).
           * We can refine the type here based on that knowledge.
           */
          val improvedBaseType =
            if (isSubtype(value.tpe.base, declaredType)) value.tpe.base
            else declaredType
          val isExact = false // We catch the case value.tpe.isExact earlier
          RefinedType(improvedBaseType, isExact, isNullable = false)
      }

      value match {
        case PreTransBlock(bindingsAndStats, result) =>
          withNewLocalDef(binding.copy(value = result))(buildInner) { tresult =>
            cont(addPreTransBindings(bindingsAndStats, tresult))
          }

        case PreTransLocalDef(localDef) if !localDef.mutable =>
          /* Attention: the same-name optimization in transformCapturingBody
           * relies on immutable bindings to var refs not being renamed.
           */

          val refinedType = computeRefinedType()
          val newLocalDef = if (refinedType == value.tpe) {
            localDef
          } else {
            /* Only adjust if the replacement if ReplaceWithThis or
             * ReplaceWithVarRef, because other types have nothing to gain
             * (e.g., ReplaceWithConstant) or we want to keep them unwrapped
             * because they are examined in optimizations (notably all the
             * types with virtualized objects).
             */
            localDef.replacement match {
              case _:ReplaceWithThis | _:ReplaceWithVarRef =>
                LocalDef(refinedType, mutable = false,
                    ReplaceWithOtherLocalDef(localDef))
              case _ =>
                localDef
            }
          }
          buildInner(newLocalDef, cont)

        case PreTransTree(literal: Literal, _) =>
          /* A `Literal` always has the most precise type it could ever have.
           * There is no point using `computeRefinedType()`.
           */
          buildInner(LocalDef(value.tpe, false,
              ReplaceWithConstant(literal)), cont)

        case PreTransTree(VarRef(LocalIdent(refName)), _)
            if !localIsMutable(refName) =>
          buildInner(LocalDef(computeRefinedType(), false,
              ReplaceWithVarRef(refName, newSimpleState(Used), None)), cont)

        case _ =>
          withDedicatedVar(computeRefinedType())
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

  /** When creating a `freshName` based on a `Binding.This`, use this name as
   *  base.
   */
  private val LocalThisNameForFresh = LocalName("this")

  private val thisOriginalName: OriginalName = OriginalName("this")

  private val ClassTagModuleClass = ClassName("scala.reflect.ClassTag$")
  private val JavaScriptExceptionClass = ClassName("scala.scalajs.js.JavaScriptException")
  private val JSWrappedArrayClass = ClassName("scala.scalajs.js.WrappedArray")
  private val NilClass = ClassName("scala.collection.immutable.Nil$")
  private val Tuple2Class = ClassName("scala.Tuple2")

  private val JavaScriptExceptionClassType = ClassType(JavaScriptExceptionClass)
  private val ThrowableClassType = ClassType(ThrowableClass)

  private val exceptionFieldName = FieldName("exception")

  private val AnyArgConstructorName = MethodName.constructor(List(ClassRef(ObjectClass)))

  private val ObjectCloneName = MethodName("clone", Nil, ClassRef(ObjectClass))
  private val TupleFirstMethodName = MethodName("_1", Nil, ClassRef(ObjectClass))
  private val TupleSecondMethodName = MethodName("_2", Nil, ClassRef(ObjectClass))
  private val ClassTagApplyMethodName =
    MethodName("apply", List(ClassRef(ClassClass)), ClassRef(ClassName("scala.reflect.ClassTag")))

  final class InlineableClassStructure(
      /** `List[ownerClassName -> fieldDef]`. */
      private val allFields: List[(ClassName, FieldDef)]) {

    private[OptimizerCore] val fieldIDs: List[FieldID] =
      allFields.map(field => FieldID(field._1, field._2))

    private[OptimizerCore] val recordType: RecordType = {
      val allocator = new FreshNameAllocator.Field
      val recordFields = for {
        (className, f @ FieldDef(flags, FieldIdent(name), originalName, ftpe)) <- allFields
      } yield {
        assert(!flags.namespace.isStatic,
            s"unexpected static field in InlineableClassStructure at ${f.pos}")
        RecordType.Field(allocator.freshName(name), originalName, ftpe,
            flags.isMutable)
      }
      RecordType(recordFields)
    }

    private val recordFieldNames: Map[FieldID, RecordType.Field] = {
      val elems = for (((className, fieldDef), recordField) <- allFields.zip(recordType.fields))
        yield FieldID(className, fieldDef) -> recordField
      elems.toMap
    }

    private[OptimizerCore] def fieldOriginalName(fieldID: FieldID): OriginalName =
      recordFieldNames(fieldID).originalName

    override def equals(that: Any): Boolean = that match {
      case that: InlineableClassStructure =>
        this.allFields == that.allFields
      case _ =>
        false
    }

    override def hashCode(): Int = allFields.##

    override def toString(): String = {
      allFields
        .map(f => s"${f._1.nameString}::${f._2.name.name.nameString}: ${f._2.ftpe}")
        .mkString("InlineableClassStructure(", ", ", ")")
    }
  }

  private final val MaxRollbacksPerMethod = 256

  private final class TooManyRollbacksException
      extends scala.util.control.ControlThrowable

  private val AnonFunctionClassPrefix = "sjsr_AnonFunction"

  private type CancelFun = () => Nothing
  private type PreTransCont = PreTransform => TailRec[Tree]

  private final case class RefinedType private (base: Type, isExact: Boolean,
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
         * other primitive numeric types.
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

  private final case class LocalDef(
      tpe: RefinedType,
      mutable: Boolean,
      replacement: LocalDefReplacement) {

    def toPreTransform(implicit pos: Position): PreTransform = {
      replacement match {
        case ReplaceWithConstant(value) => PreTransTree(value)
        case _                          => PreTransLocalDef(this)
      }
    }

    def newReplacement(implicit pos: Position): Tree =
      newReplacementInternal(replacement)

    @tailrec
    private def newReplacementInternal(replacement: LocalDefReplacement)(
        implicit pos: Position): Tree = replacement match {

      case ReplaceWithVarRef(name, used, _) =>
        used.value = Used
        VarRef(LocalIdent(name))(tpe.base)

      /* Allocate an instance of RuntimeLong on the fly.
       * See the comment in finishTransformExpr about why it is desirable and
       * safe to do so.
       */
      case ReplaceWithRecordVarRef(name, recordType, used, _)
          if tpe.base == ClassType(LongImpl.RuntimeLongClass) =>
        used.value = Used
        createNewLong(VarRef(LocalIdent(name))(recordType))

      case ReplaceWithRecordVarRef(_, _, _, cancelFun) =>
        cancelFun()

      case ReplaceWithThis() =>
        This()(tpe.base)

      case ReplaceWithOtherLocalDef(localDef) =>
        newReplacementInternal(localDef.replacement)

      case ReplaceWithConstant(value) =>
        value

      case TentativeClosureReplacement(_, _, _, _, _, cancelFun) =>
        cancelFun()

      case InlineClassBeingConstructedReplacement(_, _, cancelFun) =>
        cancelFun()

      /* Allocate an instance of RuntimeLong on the fly.
       * See the comment in finishTransformExpr about why it is desirable and
       * safe to do so.
       */
      case InlineClassInstanceReplacement(structure, fieldLocalDefs, _)
          if tpe.base == ClassType(LongImpl.RuntimeLongClass) =>
        val List(loField, hiField) = structure.fieldIDs
        val lo = fieldLocalDefs(loField).newReplacement
        val hi = fieldLocalDefs(hiField).newReplacement
        createNewLong(lo, hi)

      case InlineClassInstanceReplacement(_, _, cancelFun) =>
        cancelFun()

      case InlineJSArrayReplacement(_, cancelFun) =>
        cancelFun()
    }

    def contains(that: LocalDef): Boolean = {
      (this eq that) || (replacement match {
        case ReplaceWithOtherLocalDef(localDef) =>
          localDef.contains(that)
        case TentativeClosureReplacement(_, _, _, captureLocalDefs, _, _) =>
          captureLocalDefs.exists(_.contains(that))
        case InlineClassBeingConstructedReplacement(_, fieldLocalDefs, _) =>
          fieldLocalDefs.valuesIterator.exists(_.contains(that))
        case InlineClassInstanceReplacement(_, fieldLocalDefs, _) =>
          fieldLocalDefs.valuesIterator.exists(_.contains(that))
        case InlineJSArrayReplacement(elemLocalDefs, _) =>
          elemLocalDefs.exists(_.contains(that))

        case _:ReplaceWithVarRef | _:ReplaceWithRecordVarRef |
             _:ReplaceWithThis | _:ReplaceWithConstant =>
          false
      })
    }
  }

  private sealed abstract class LocalDefReplacement

  private final case class ReplaceWithVarRef(name: LocalName,
      used: SimpleState[IsUsed],
      longOpTree: Option[() => Tree]) extends LocalDefReplacement

  private final case class ReplaceWithRecordVarRef(name: LocalName,
      recordType: RecordType,
      used: SimpleState[IsUsed],
      cancelFun: CancelFun) extends LocalDefReplacement

  private final case class ReplaceWithThis() extends LocalDefReplacement

  /** An alias to another `LocalDef`, used only to refine the type of that
   *  `LocalDef` in a specific scope.
   *
   *  This happens when refining the type of a `this` binding in an inlined
   *  method body.
   */
  private final case class ReplaceWithOtherLocalDef(localDef: LocalDef)
      extends LocalDefReplacement

  private final case class ReplaceWithConstant(
      value: Tree) extends LocalDefReplacement

  private final case class TentativeClosureReplacement(
      captureParams: List[ParamDef], params: List[ParamDef], body: Tree,
      captureValues: List[LocalDef],
      alreadyUsed: SimpleState[IsUsed],
      cancelFun: CancelFun) extends LocalDefReplacement

  private final case class InlineClassBeingConstructedReplacement(
      structure: InlineableClassStructure,
      fieldLocalDefs: Map[FieldID, LocalDef],
      cancelFun: CancelFun) extends LocalDefReplacement

  private final case class InlineClassInstanceReplacement(
      structure: InlineableClassStructure,
      fieldLocalDefs: Map[FieldID, LocalDef],
      cancelFun: CancelFun) extends LocalDefReplacement

  private final case class InlineJSArrayReplacement(
      elemLocalDefs: Vector[LocalDef],
      cancelFun: CancelFun) extends LocalDefReplacement

  /** Replaces an import target. Part of the ApplyDynamicImport inlining.
   *
   *  @note This is **not** a LocalDefReplacement.
   */
  private final case class ImportReplacement(target: ImportTarget,
      moduleVarName: LocalName, path: List[String],
      used: SimpleState[IsUsed], cancelFun: CancelFun)

  private final class LabelInfo(
      val newName: LabelName,
      val acceptRecords: Boolean,
      /** (actualType, originalType), actualType can be a RecordType. */
      val returnedTypes: SimpleState[List[(Type, RefinedType)]])

  private class OptEnv(
      val thisLocalDef: Option[LocalDef],
      val localDefs: Map[LocalName, LocalDef],
      val labelInfos: Map[LabelName, LabelInfo]) {

    def withThisLocalDef(rep: LocalDef): OptEnv =
      withThisLocalDef(Some(rep))

    def withThisLocalDef(rep: Option[LocalDef]): OptEnv =
      new OptEnv(rep, localDefs, labelInfos)

    def withLocalDef(oldName: LocalName, rep: LocalDef): OptEnv =
      new OptEnv(thisLocalDef, localDefs + (oldName -> rep), labelInfos)

    def withLocalDef(oldName: Binding.Name, rep: LocalDef): OptEnv = {
      oldName match {
        case Binding.This           => withThisLocalDef(rep)
        case Binding.Local(name, _) => withLocalDef(name, rep)
      }
    }

    def withLocalDefs(reps: List[(LocalName, LocalDef)]): OptEnv =
      new OptEnv(thisLocalDef, localDefs ++ reps, labelInfos)

    def withLabelInfo(oldName: LabelName, info: LabelInfo): OptEnv =
      new OptEnv(thisLocalDef, localDefs, labelInfos + (oldName -> info))

    override def toString(): String = {
      "thisLocalDef:\n  " + thisLocalDef.fold("<none>")(_.toString()) + "\n" +
      "localDefs:" + localDefs.mkString("\n  ", "\n  ", "\n") +
      "labelInfos:" + labelInfos.mkString("\n  ", "\n  ", "")
    }
  }

  private object OptEnv {
    val Empty: OptEnv = new OptEnv(None, Map.empty, Map.empty)
  }

  private class Scope private (
    val env: OptEnv,
    val implsBeingInlined: Set[Scope.InliningID],
    val importReplacement: Option[ImportReplacement]
  ) {
    def withEnv(env: OptEnv): Scope = copy(env = env)

    def inlining(impl: Scope.InliningID): Scope = {
      assert(!implsBeingInlined(impl), s"Circular inlining of $impl")
      copy(implsBeingInlined = implsBeingInlined + impl)
    }

    def inlining(impls: Set[Scope.InliningID]): Scope = {
      val intersection = implsBeingInlined.intersect(impls)
      assert(intersection.isEmpty, s"Circular inlining of $intersection")
      copy(implsBeingInlined = implsBeingInlined ++ impls)
    }

    def withImportReplacement(importReplacement: ImportReplacement): Scope = {
      assert(this.importReplacement.isEmpty, "Alreadying replacing " +
          s"$this.importReplacement while trying to replace $importReplacement")
      copy(importReplacement = Some(importReplacement))
    }

    private def copy(
      env: OptEnv = env,
      implsBeingInlined: Set[Scope.InliningID] = implsBeingInlined,
      importReplacement: Option[ImportReplacement] = importReplacement
    ): Scope = {
      new Scope(env, implsBeingInlined, importReplacement)
    }
  }

  private object Scope {
    type InliningID = (List[AllocationSite], AbstractMethodID)

    val Empty: Scope = new Scope(OptEnv.Empty, Set.empty, None)
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
          case Left(PreTransBinding(_, _, value)) => value.contains(localDef)
          case Right(_)                           => false
        }
      case PreTransUnaryOp(_, lhs) =>
        lhs.contains(localDef)
      case PreTransBinaryOp(_, lhs, rhs) =>
        lhs.contains(localDef) || rhs.contains(localDef)
      case PreTransJSBinaryOp(_, lhs, rhs) =>
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
  private final case class PreTransBinding(originalName: OriginalName,
      localDef: LocalDef, value: PreTransform) {

    assert(
        localDef.replacement.isInstanceOf[ReplaceWithVarRef] ||
        localDef.replacement.isInstanceOf[ReplaceWithRecordVarRef],
        "Cannot create a PreTransBinding with non-var-ref replacement " +
        localDef.replacement)

    def isAlreadyUsed: Boolean = (localDef.replacement: @unchecked) match {
      case ReplaceWithVarRef(_, used, _)          => used.value.isUsed
      case ReplaceWithRecordVarRef(_, _, used, _) => used.value.isUsed
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

  /** A `PreTransform` for a `JSBinaryOp`. */
  private final case class PreTransJSBinaryOp(op: JSBinaryOp.Code,
      lhs: PreTransform, rhs: PreTransform)(implicit val pos: Position)
      extends PreTransResult {

    val tpe: RefinedType = RefinedType(JSBinaryOp.resultTypeOf(op))
  }

  private object PreTransJSBinaryOp {
    def isWorthPreTransforming(op: JSBinaryOp.Code): Boolean =
      op == JSBinaryOp.=== || op == JSBinaryOp.!==
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
      val refinedTpe: RefinedType = BlockOrAlone.last(tree) match {
        case _:LoadModule | _:NewArray | _:ArrayValue | _:ClassOf =>
          RefinedType(tree.tpe, isExact = true, isNullable = false)
        case GetClass(x) if x.tpe != AnyType && x.tpe != ClassType(ObjectClass) =>
          /* If x.tpe is neither AnyType nor j.l.Object, it cannot be a JS
           * object, so its getClass() cannot be null.
           */
          RefinedType(tree.tpe, isExact = true, isNullable = false)
        case _ =>
          RefinedType(tree.tpe)
      }
      PreTransTree(tree, refinedTpe)
    }
  }

  private implicit class OptimizerTreeOps private[OptimizerCore] (
      private val self: Tree)
      extends AnyVal {

    def toPreTransform: PreTransform = {
      self match {
        case UnaryOp(op, lhs) =>
          PreTransUnaryOp(op, lhs.toPreTransform)(self.pos)
        case BinaryOp(op, lhs, rhs) =>
          PreTransBinaryOp(op, lhs.toPreTransform, rhs.toPreTransform)(self.pos)
        case JSBinaryOp(op, lhs, rhs) if PreTransJSBinaryOp.isWorthPreTransforming(op) =>
          PreTransJSBinaryOp(op, lhs.toPreTransform, rhs.toPreTransform)(self.pos)
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

  private final case class Binding(name: Binding.Name, declaredType: Type,
      mutable: Boolean, value: PreTransform)

  private object Binding {
    sealed abstract class Name

    case object This extends Name

    final case class Local(name: LocalName, originalName: OriginalName)
        extends Name

    def apply(localIdent: LocalIdent, originalName: OriginalName,
        declaredType: Type, mutable: Boolean, value: PreTransform): Binding = {
      apply(Local(localIdent.name, originalName), declaredType,
          mutable, value)
    }

    def temp(baseName: LocalName, declaredType: Type, mutable: Boolean,
        value: PreTransform): Binding = {
      apply(Local(baseName, NoOriginalName), declaredType, mutable, value)
    }

    def temp(baseName: LocalName, value: PreTransform): Binding =
      temp(baseName, value.tpe.base, false, value)
  }

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

  private object JSImport {
    /** Import module and call `callback` with it. */
    def apply(moduleKind: ModuleKind, module: String, callback: Closure)(implicit pos: Position): Tree = {
      def genThen(receiver: Tree, callback: Closure): Tree =
        JSMethodApply(receiver, StringLiteral("then"), List(callback))

      val importTree = moduleKind match {
        case ModuleKind.NoModule =>
          throw new AssertionError("Cannot import module in NoModule mode")

        case ModuleKind.ESModule =>
          JSImportCall(StringLiteral(module))

        case ModuleKind.CommonJSModule =>
          val require =
            JSFunctionApply(JSGlobalRef("require"), List(StringLiteral(module)))

          val unitPromise = JSMethodApply(
              JSGlobalRef("Promise"), StringLiteral("resolve"), List(Undefined()))
          genThen(unitPromise, Closure(arrow = true, Nil, Nil, None, require, Nil))
      }

      genThen(importTree, callback)
    }
  }

  /** Creates a new instance of `RuntimeLong` from a record of its `lo` and
   *  `hi` parts.
   */
  private def createNewLong(recordVarRef: VarRef)(
      implicit pos: Position): Tree = {

    val RecordType(List(loField, hiField)) = recordVarRef.tpe
    createNewLong(
        RecordSelect(recordVarRef, FieldIdent(loField.name))(IntType),
        RecordSelect(recordVarRef, FieldIdent(hiField.name))(IntType))
  }

  /** Creates a new instance of `RuntimeLong` from its `lo` and `hi` parts. */
  private def createNewLong(lo: Tree, hi: Tree)(
      implicit pos: Position): Tree = {

    New(LongImpl.RuntimeLongClass, MethodIdent(LongImpl.initFromParts),
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

  private final class Intrinsics(intrinsicsMap: Map[(ClassName, MethodName), Int]) {
    def apply(flags: ApplyFlags, target: AbstractMethodID): Int = {
      if (flags.isPrivate || flags.isConstructor) {
        -1
      } else {
        val key = (target.enclosingClassName, target.methodName)
        intrinsicsMap.getOrElse(key, -1)
      }
    }
  }

  private object Intrinsics {
    final val ArrayCopy   = 1

    final val ArrayApply  = ArrayCopy + 1
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
    final val ClassGetName = ClassGetComponentType + 1

    final val ArrayNewInstance = ClassGetName + 1

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

    private def m(name: String, paramTypeRefs: List[TypeRef],
        resultTypeRef: TypeRef): MethodName = {
      MethodName(name, paramTypeRefs, resultTypeRef)
    }

    private val V = VoidRef
    private val I = IntRef
    private val J = LongRef
    private val O = ClassRef(ObjectClass)
    private val ClassClassRef = ClassRef(ClassClass)
    private val StringClassRef = ClassRef(BoxedStringClass)
    private val SeqClassRef = ClassRef(ClassName("scala.collection.Seq"))
    private val JSObjectClassRef = ClassRef(ClassName("scala.scalajs.js.Object"))
    private val JSArrayClassRef = ClassRef(ClassName("scala.scalajs.js.Array"))

    private def a(base: NonArrayTypeRef): ArrayTypeRef = ArrayTypeRef(base, 1)

    private def typedarrayClassRef(baseName: String): ClassRef =
      ClassRef(ClassName(s"scala.scalajs.js.typedarray.${baseName}Array"))

    // scalastyle:off line.size.limit
    private val baseIntrinsics: List[(ClassName, List[(MethodName, Int)])] = List(
        ClassName("java.lang.System$") -> List(
            m("arraycopy", List(O, I, O, I, I), V) -> ArrayCopy
        ),
        ClassName("scala.runtime.ScalaRunTime$") -> List(
            m("array_apply", List(O, I), O) -> ArrayApply,
            m("array_update", List(O, I, O), V) -> ArrayUpdate,
            m("array_length", List(O), I) -> ArrayLength
        ),
        ClassName("java.lang.Integer$") -> List(
            m("numberOfLeadingZeros", List(I), I) -> IntegerNLZ
        ),
        ClassName("scala.collection.mutable.ArrayBuilder$") -> List(
            m("scala$collection$mutable$ArrayBuilder$$zeroOf", List(ClassClassRef), O) -> ArrayBuilderZeroOf,
            m("scala$collection$mutable$ArrayBuilder$$genericArrayBuilderResult", List(ClassClassRef, JSArrayClassRef), O) -> GenericArrayBuilderResult
        ),
        ClassName("java.lang.Class") -> List(
            m("getComponentType", Nil, ClassClassRef) -> ClassGetComponentType,
            m("getName", Nil, StringClassRef) -> ClassGetName
        ),
        ClassName("java.lang.reflect.Array$") -> List(
            m("newInstance", List(ClassClassRef, I), O) -> ArrayNewInstance
        ),
        ClassName("scala.scalajs.js.special.package$") -> List(
            m("objectLiteral", List(SeqClassRef), JSObjectClassRef) -> ObjectLiteral
        ),
        ClassName("scala.scalajs.js.typedarray.package$") -> List(
            m("byteArray2Int8Array", List(a(ByteRef)), typedarrayClassRef("Int8")) -> ByteArrayToInt8Array,
            m("shortArray2Int16Array", List(a(ShortRef)), typedarrayClassRef("Int16")) -> ShortArrayToInt16Array,
            m("charArray2Uint16Array", List(a(CharRef)), typedarrayClassRef("Uint16")) -> CharArrayToUint16Array,
            m("intArray2Int32Array", List(a(IntRef)), typedarrayClassRef("Int32")) -> IntArrayToInt32Array,
            m("floatArray2Float32Array", List(a(FloatRef)), typedarrayClassRef("Float32")) -> FloatArrayToFloat32Array,
            m("doubleArray2Float64Array", List(a(DoubleRef)), typedarrayClassRef("Float64")) -> DoubleArrayToFloat64Array,

            m("int8Array2ByteArray", List(typedarrayClassRef("Int8")), a(ByteRef)) -> Int8ArrayToByteArray,
            m("int16Array2ShortArray", List(typedarrayClassRef("Int16")), a(ShortRef)) -> Int16ArrayToShortArray,
            m("uint16Array2CharArray", List(typedarrayClassRef("Uint16")), a(CharRef)) -> Uint16ArrayToCharArray,
            m("int32Array2IntArray", List(typedarrayClassRef("Int32")), a(IntRef)) -> Int32ArrayToIntArray,
            m("float32Array2FloatArray", List(typedarrayClassRef("Float32")), a(FloatRef)) -> Float32ArrayToFloatArray,
            m("float64Array2DoubleArray", List(typedarrayClassRef("Float64")), a(DoubleRef)) -> Float64ArrayToDoubleArray
        )
    )

    private val runtimeLongIntrinsics: List[(ClassName, List[(MethodName, Int)])] = List(
        ClassName("java.lang.Long$") -> List(
            m("toString", List(J), ClassRef(BoxedStringClass)) -> LongToString,
            m("compare", List(J, J), I) -> LongCompare,
            m("divideUnsigned", List(J, J), J) -> LongDivideUnsigned,
            m("remainderUnsigned", List(J, J), J) -> LongRemainderUnsigned
        )
    )
    // scalastyle:on line.size.limit

    def buildIntrinsics(esFeatures: ESFeatures): Intrinsics = {
      val allIntrinsics =
        if (esFeatures.allowBigIntsForLongs) baseIntrinsics
        else baseIntrinsics ++ runtimeLongIntrinsics

      val intrinsicsMap = (for {
        (className, methodsAndCodes) <- allIntrinsics
        (methodName, code) <- methodsAndCodes
      } yield {
        (className, methodName) -> code
      }).toMap
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
    def enclosingClassName: ClassName
    def methodName: MethodName
    def attributes: MethodAttributes

    final def is(className: ClassName, methodName: MethodName): Boolean =
      this.enclosingClassName == className && this.methodName == methodName
  }

  /** Parts of [[GenIncOptimizer#MethodImpl]] with decisions about optimizations. */
  abstract class MethodImpl {
    def enclosingClassName: ClassName
    def methodName: MethodName
    def optimizerHints: OptimizerHints
    def originalDef: MethodDef
    def thisType: Type

    protected def computeNewAttributes(): MethodAttributes = {
      val MethodDef(_, MethodIdent(methodName), _, params, _, optBody) = originalDef
      val body = optBody getOrElse {
        throw new AssertionError("Methods in optimizer must be concrete")
      }

      val isForwarder = body match {
        // Shape of forwarders to trait impls
        case ApplyStatic(_, impl, method, args) =>
          ((args.size == params.size + 1) &&
              (args.head.isInstanceOf[This]) &&
              (args.tail.zip(params).forall {
                case (VarRef(LocalIdent(aname)),
                    ParamDef(LocalIdent(pname), _, _, _)) => aname == pname
                case _ => false
              }))

        // Shape of forwards to default methods
        case ApplyStatically(_, This(), className, method, args) =>
          args.size == params.size &&
          args.zip(params).forall {
            case (VarRef(LocalIdent(aname)), ParamDef(LocalIdent(pname), _, _, _)) =>
              aname == pname
            case _ =>
              false
          }

        // Shape of bridges for generic methods
        case Apply(_, This(), method, args) =>
          (args.size == params.size) &&
          args.zip(params).forall {
            case (MaybeUnbox(VarRef(LocalIdent(aname)), _),
                ParamDef(LocalIdent(pname), _, _, _)) => aname == pname
            case _ => false
          }

        case _ => false
      }

      val inlineable = !optimizerHints.noinline

      val shouldInline = inlineable && {
        optimizerHints.inline || isForwarder || {
          body match {
            case _:Skip | _:This | _:Literal =>
              true

            // Shape of accessors
            case Select(This(), _, _) if params.isEmpty =>
              true
            case Assign(Select(This(), _, _), VarRef(_)) if params.size == 1 =>
              true

            // Shape of trivial call-super constructors
            case Block(stats)
                if params.isEmpty && methodName.isConstructor &&
                    stats.forall(isTrivialConstructorStat) =>
              true

            // Simple method
            case SimpleMethodBody() =>
              true

            case _ =>
              false
          }
        }
      }

      val jsDynImportInlineTarget = body match {
        case MaybeUnbox(SelectJSNativeMember(className, MethodIdent(member)), _) =>
          Some(ImportTarget.Member(className, member))

        case MaybeUnbox(JSFunctionApply(SelectJSNativeMember(className,
            MethodIdent(member)), args), _) if args.forall(isSmallTree(_))=>
          Some(ImportTarget.Member(className, member))

        case MaybeUnbox(LoadJSModule(className), _) =>
          Some(ImportTarget.Class(className))

        case MaybeUnbox(JSSelect(LoadJSModule(className), arg), _) if isSmallTree(arg) =>
          Some(ImportTarget.Class(className))

        case MaybeUnbox(JSMethodApply(LoadJSModule(className), method, args), _)
            if isSmallTree(method) && args.forall(isSmallTree(_)) =>
          Some(ImportTarget.Class(className))

        case JSNew(LoadJSConstructor(className), args) if args.forall(isSmallTree(_)) =>
          Some(ImportTarget.Class(className))

        case _ =>
          None
      }

      val jsDynImportThunkFor = body match {
        case Apply(_, New(clazz, _, _), MethodIdent(target), _) if clazz == enclosingClassName =>
          Some(target)

        case _ =>
          None
      }

      new MethodAttributes(inlineable, shouldInline, isForwarder, jsDynImportInlineTarget, jsDynImportThunkFor)
    }
  }

  /* This is a "broken" case class so we get equals (and hashCode) for free.
   *
   * This hack is somewhat acceptable, because:
   * - it is only part of the OptimizerCore / IncOptimizer interface.
   * - the risk of getting equals wrong is high: it only affects the incremental
   *   behavior of the optimizer, which we have few tests for.
   */
  final case class MethodAttributes private[OptimizerCore] (
      private[OptimizerCore] val inlineable: Boolean,
      private[OptimizerCore] val shouldInline: Boolean,
      private[OptimizerCore] val isForwarder: Boolean,
      private[OptimizerCore] val jsDynImportInlineTarget: Option[ImportTarget],
      private[OptimizerCore] val jsDynImportThunkFor: Option[MethodName]
  )

  sealed trait ImportTarget

  object ImportTarget {
    case class Member(className: ClassName, member: MethodName) extends ImportTarget
    case class Class(className: ClassName) extends ImportTarget
  }

  private object MaybeUnbox {
    def unapply(tree: Tree): Some[(Tree, Any)] = tree match {
      case AsInstanceOf(arg, tpe) =>
        Some((arg, tpe))
      case _ =>
        Some((tree, ()))
    }
  }

  private val TraitInitSimpleMethodName = SimpleMethodName("$init$")

  private def isTrivialConstructorStat(stat: Tree): Boolean = stat match {
    case This() =>
      true
    case ApplyStatically(_, This(), _, _, Nil) =>
      true
    case ApplyStatic(_, _, MethodIdent(methodName), This() :: Nil) =>
      methodName.simpleName == TraitInitSimpleMethodName
    case _ =>
      false
  }

  /** Whether a tree is going to result in a small code size.
   *
   *  This is used to determine whether it is acceptable to move a tree accross
   *  a dynamic module load boundary.
   */
  private def isSmallTree(tree: TreeOrJSSpread): Boolean = tree match {
    case _:VarRef | _:Literal    => true
    case Select(This(), _, _)    => true
    case UnaryOp(_, lhs)         => isSmallTree(lhs)
    case BinaryOp(_, lhs, rhs)   => isSmallTree(lhs) && isSmallTree(rhs)
    case JSUnaryOp(_, lhs)       => isSmallTree(lhs)
    case JSBinaryOp(_, lhs, rhs) => isSmallTree(lhs) && isSmallTree(rhs)
    case _                       => false
  }

  private object SimpleMethodBody {
    @tailrec
    final def unapply(body: Tree): Boolean = body match {
      case New(_, _, args)                          => areSimpleArgs(args)
      case Apply(_, receiver, _, args)              => areSimpleArgs(receiver :: args)
      case ApplyStatically(_, receiver, _, _, args) => areSimpleArgs(receiver :: args)
      case ApplyStatic(_, _, _, args)               => areSimpleArgs(args)
      case Select(qual, _, _)                       => isSimpleArg(qual)
      case IsInstanceOf(inner, _)                   => isSimpleArg(inner)

      case Block(List(inner, Undefined())) =>
        unapply(inner)

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

    def last(tree: Tree): Tree = tree match {
      case Block(stats) => stats.last
      case _            => tree
    }
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
      val localNameAllocatorSnapshot: FreshNameAllocator.Snapshot[LocalName],
      val savedMutableLocalNames: Set[LocalName],
      val labelNameAllocatorSnapshot: FreshNameAllocator.Snapshot[LabelName],
      val savedStateBackupChain: List[StateBackup],
      val cont: () => TailRec[Tree]) extends ControlThrowable

  class OptimizeException(val myself: AbstractMethodID,
      val attemptedInlining: List[AbstractMethodID], cause: Throwable
  ) extends Exception(exceptionMsg(myself, attemptedInlining, cause), cause)

  private abstract class FreshNameAllocator[N <: Name] private (
      initialMap: Map[N, Int]) {

    import FreshNameAllocator._

    private var usedNamesToNextCounter: Map[N, Int] = initialMap

    def clear(): Unit = usedNamesToNextCounter = initialMap

    def freshName(base: N): N = {
      if (!usedNamesToNextCounter.contains(base)) {
        usedNamesToNextCounter = usedNamesToNextCounter.updated(base, 1)
        base
      } else {
        var i = usedNamesToNextCounter(base)
        var result = nameWithSuffix(base, "$" + i)
        while (usedNamesToNextCounter.contains(result)) {
          i += 1
          result = nameWithSuffix(base, "$" + i)
        }
        usedNamesToNextCounter =
          usedNamesToNextCounter.updated(base, i + 1).updated(result, 1)
        result
      }
    }

    protected def nameWithSuffix(name: N, suffix: String): N

    def snapshot(): Snapshot[N] = new Snapshot(usedNamesToNextCounter)

    def restore(snapshot: Snapshot[N]): Unit =
      usedNamesToNextCounter = snapshot.usedNamesToNextCounter
  }

  private object FreshNameAllocator {
    /** List of local and label names that the emitter will avoid in JS
     *  identifiers, and therefore will rewrite with non-ASCII characters.
     *
     *  Since we're renaming all local and label symbols through fresh
     *  allocators anyway, we take the opportunity to rename them in a nice way
     *  (with ASCII characters only).
     */
    private val EmitterReservedJSIdentifiers = List(
        "arguments", "await", "break", "case", "catch", "class", "const",
        "continue", "debugger", "default", "delete", "do", "else", "enum",
        "eval", "export", "extends", "false", "finally", "for", "function",
        "if", "implements", "import", "in", "instanceof", "interface", "let",
        "new", "null", "package", "private", "protected", "public", "return",
        "static", "super", "switch", "this", "throw", "true", "try", "typeof",
        "undefined", "var", "void", "while", "with", "yield"
    )

    private val InitialLocalMap: Map[LocalName, Int] =
      EmitterReservedJSIdentifiers.map(i => LocalName(i) -> 1).toMap

    final class Local extends FreshNameAllocator[LocalName](InitialLocalMap) {
      protected def nameWithSuffix(name: LocalName, suffix: String): LocalName =
        name.withSuffix(suffix)
    }

    private val InitialLabelMap: Map[LabelName, Int] =
      EmitterReservedJSIdentifiers.map(i => LabelName(i) -> 1).toMap

    final class Label extends FreshNameAllocator[LabelName](InitialLabelMap) {
      protected def nameWithSuffix(name: LabelName, suffix: String): LabelName =
        name.withSuffix(suffix)
    }

    private val InitialFieldMap: Map[FieldName, Int] =
      Map.empty

    final class Field extends FreshNameAllocator[FieldName](InitialFieldMap) {
      protected def nameWithSuffix(name: FieldName, suffix: String): FieldName =
        name.withSuffix(suffix)
    }

    final class Snapshot[N <: Name] private[FreshNameAllocator] (
        private[FreshNameAllocator] val usedNamesToNextCounter: Map[N, Int])
  }

  def originalNameForFresh(base: Name, originalName: OriginalName,
      freshName: Name): OriginalName = {
    if (originalName.isDefined || (freshName eq base)) originalName
    else OriginalName(base)
  }

  final class FieldID private (val ownerClassName: ClassName, val name: FieldName) {
    override def equals(that: Any): Boolean = that match {
      case that: FieldID =>
        this.ownerClassName == that.ownerClassName &&
        this.name == that.name
      case _ =>
        false
    }

    override def hashCode(): Int =
      ownerClassName.## ^ name.##

    override def toString(): String =
      s"FieldID($ownerClassName, $name)"
  }

  object FieldID {
    def apply(ownerClassName: ClassName, field: FieldIdent): FieldID =
      new FieldID(ownerClassName, field.name)

    def apply(ownerClassName: ClassName, fieldDef: FieldDef): FieldID =
      new FieldID(ownerClassName, fieldDef.name.name)
  }

  private sealed abstract class IsUsed {
    def isUsed: Boolean
  }
  private case object Used extends IsUsed {
    override def isUsed: Boolean = true
  }
  private case object Unused extends IsUsed {
    override def isUsed: Boolean = false
  }

}
