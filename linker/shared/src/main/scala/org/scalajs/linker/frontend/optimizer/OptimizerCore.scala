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
import org.scalajs.ir.WellKnownNames._

import org.scalajs.logging._
import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable.RuntimeClassNameMapperImpl
import org.scalajs.linker.standard._
import org.scalajs.linker.backend.emitter.LongImpl
import org.scalajs.linker.backend.emitter.Transients._
import org.scalajs.linker.backend.wasmemitter.WasmTransients._

/** Optimizer core.
 *  Designed to be "mixed in" [[IncOptimizer#MethodImpl#Optimizer]].
 *  This is the core of the optimizer. It contains all the smart things the
 *  optimizer does. To perform inlining, it relies on abstract protected
 *  methods to identify the target of calls.
 */
private[optimizer] abstract class OptimizerCore(
    config: CommonPhaseConfig, debugID: String) {
  import OptimizerCore._

  type MethodID <: AbstractMethodID

  protected val myself: Option[MethodID]

  private def semantics: Semantics = config.coreSpec.semantics

  private val isWasm: Boolean = config.coreSpec.targetIsWebAssembly

  // Uncomment and adapt to print debug messages only during one method
  //lazy val debugThisMethod: Boolean =
  //  debugID == "java.lang.FloatingPointBits$.numberHashCode;D;I"

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

  /** Tests whether *all* the constructors of the given class are elidable.
   *  In other words, whether it is safe to discard a New or LoadModule of that
   *  class which is not used.
   */
  protected def hasElidableConstructors(className: ClassName): Boolean

  /** Returns the inlineable field bodies of this module class.
   *
   *  If the class is not a module class, or if it does not have inlineable
   *  accessors, the resulting `InlineableFieldBodies` is always empty.
   */
  protected def inlineableFieldBodies(className: ClassName): InlineableFieldBodies

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
  protected def isFieldRead(fieldName: FieldName): Boolean

  /** Returns true if the given static field is ever read. */
  protected def isStaticFieldRead(fieldName: FieldName): Boolean

  /** Whether the given class is a JS type */
  protected def isJSType(className: ClassName): Boolean

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

  private val useRuntimeLong =
    !config.coreSpec.esFeatures.allowBigIntsForLongs && !isWasm

  private val intrinsics =
    Intrinsics.buildIntrinsics(config.coreSpec.esFeatures, isWasm)

  private val integerDivisions = new IntegerDivisions(useRuntimeLong)

  def optimize(thisType: Type, params: List[ParamDef],
      jsClassCaptures: List[ParamDef], resultType: Type, body: Tree,
      isNoArgCtor: Boolean): (List[ParamDef], Tree) = {
    try {
      try {
        transformMethodDefBody(myself, thisType, params, jsClassCaptures, resultType, body, isNoArgCtor)
      } catch {
        case _: TooManyRollbacksException =>
          localNameAllocator.clear()
          mutableLocalNames = Set.empty
          labelNameAllocator.clear()
          stateBackupChain = Nil
          disableOptimisticOptimizations = true
          transformMethodDefBody(myself, thisType, params, jsClassCaptures, resultType, body, isNoArgCtor)
      }
    } catch {
      case NonFatal(cause) =>
        throw new OptimizeException(debugID, attemptedInlining.distinct.toList, cause)
      case e: Throwable =>
        // This is a fatal exception. Don't wrap, just output debug info error
        Console.err.println(exceptionMsg(
            debugID, attemptedInlining.distinct.toList, e))
        throw e
    }
  }

  /** Try and eliminate a StoreModule followed only by trivial statements. */
  private def tryElimStoreModule(body: Tree): Tree = {
    implicit val pos = body.pos
    body match {
      case StoreModule() =>
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
            case Assign(SelectStatic(_), _:Literal | _:VarRef) =>
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
    assert(lhs != VoidType)
    assert(rhs != VoidType)

    Types.isSubtype(lhs, rhs)(isSubclassFun)
  }

  /** Predicts the result of a type test.
   *
   *  Predicts the result of an `IsInstanceOf(expr, testType)` where `exprType`
   *  is the type of `expr`. Generalized for arbitrary test types (not limited
   *  to types that are valid in an `IsInstanceOf` node).
   *
   *  Possible results are:
   *
   *  - `Subtype`: `exprType <: testType`. The type test always succeeds.
   *  - `SubtypeOrNull`: `exprType.toNonNullable <: testType` and the type test
   *    succeeds iff `expr` is not `null` (cannot happen when `testType` is
   *    nullable).
   *  - `NotAnInstance`: `expr` is guaranteed never to be an instance of
   *    `testType`. The type test always fails.
   *  - `NotAnInstanceUnlessNull`: the type test succeeds iff `expr` is `null`
   *    (cannot happen when `testType` is non-nullable).
   *  - `Unkwown`: no prediction. The type test may succeed or fail.
   *
   *  In the future, we may enhance this test with `NonSubtypeInstance` and
   *  `NonSubtypeInstanceOrNull`, which would communicate a successful type
   *  test *without* the (static) subtyping guarantee. Currently, this method
   *  does not detect any situation like that.
   */
  private def typeTestResult(exprType: RefinedType, testType: Type,
      testTypeKnownToBeFinal: Boolean = false): TypeTestResult = {

    def notANonNullInstance: TypeTestResult =
      if (exprType.isNullable && testType.isNullable) TypeTestResult.NotAnInstanceUnlessNull
      else TypeTestResult.NotAnInstance

    if (isSubtype(exprType.base.toNonNullable, testType)) {
      if (exprType.isNullable && !testType.isNullable) {
        if (exprType.base == NullType)
          TypeTestResult.NotAnInstance
        else
          TypeTestResult.SubtypeOrNull
      } else {
        TypeTestResult.Subtype
      }
    } else {
      val canRuleOutBasedOnExactness = exprType.base match {
        case _:ClassType | _:ArrayType =>
          // Reference types depend on the exactness of the RefinedType
          exprType.isExact
        case AnyType | AnyNotNullType =>
          false
        case ByteType | ShortType | IntType | FloatType | DoubleType =>
          /* The primitive numeric types may answer `true` to a type test among
           * themselves, even when static types are not subtypes of each other.
           */
          testType match {
            case ByteType | ShortType | IntType | FloatType | DoubleType =>
              false
            case ClassType(BoxedByteClass | BoxedShortClass |
                BoxedIntegerClass | BoxedFloatClass | BoxedDoubleClass, _) =>
              false
            case _ =>
              true
          }
        case _: PrimType =>
          /* Other primitive types can be considered "exact" for the purposes
           * of type tests.
           */
          true
        case _:ClosureType | _:RecordType =>
          // These types are only subtypes of themselves, modulo nullability
          true
      }

      if (canRuleOutBasedOnExactness)
        notANonNullInstance
      else if (testTypeKnownToBeFinal && !isSubtype(testType.toNonNullable, exprType.base))
        notANonNullInstance
      else
        TypeTestResult.Unknown
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

      case Labeled(label, tpe, body) =>
        trampoline {
          pretransformLabeled(label, if (isStat) VoidType else tpe, body, isStat,
              usePreTransform = false)(finishTransform(isStat))
        }

      case Assign(lhs, rhs) =>
        val cont = { (tlhs: PreTransform) =>
          pretransformExpr(rhs) { trhs =>
            pretransformAssign(tlhs, trhs)(finishTransform(isStat))
          }
        }

        lhs match {
          case Select(qualifier, FieldIdent(name)) if !isFieldRead(name) =>
            // Field is never read. Drop assign, keep side effects only.
            Block(transformStat(qualifier), transformStat(rhs))

          case SelectStatic(FieldIdent(name)) if !isStaticFieldRead(name) =>
            // Field is never read. Drop assign, keep side effects only.
            transformStat(rhs)

          case JSPrivateSelect(qualifier, FieldIdent(name)) if !isFieldRead(name) =>
            // Field is never read. Drop assign, keep side effects only.
            Block(transformStat(qualifier), transformStat(rhs))

          case lhs: Select =>
            trampoline {
              pretransformSelectCommon(lhs, isLhsOfAssign = true)(cont)
            }

          case lhs: ArraySelect =>
            trampoline {
              pretransformArraySelect(lhs, isLhsOfAssign = true)(cont)
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
        val info = scope.env.labelInfos(label)
        val newLabel = info.newName

        // Recall that `info.returnedTypes` does not want to contain NothingType
        if (info.isStat) {
          val newExpr = transformStat(expr)
          if (newExpr.tpe == NothingType) {
            newExpr
          } else {
            /* We need to track even the `VoidType`s. We need to know how many
             * `Return`s we produce in order to decide whether we can remove
             * the `Labeled`.
             */
            info.returnedTreeTypes.value ::= RefinedType(VoidType)
            Return(newExpr, newLabel)
          }
        } else if (!info.acceptRecords) {
          val newExpr = transformExpr(expr)
          if (newExpr.tpe == NothingType) {
            newExpr
          } else {
            info.returnedTreeTypes.value ::= RefinedType(newExpr.tpe)
            Return(newExpr, newLabel)
          }
        } else trampoline {
          pretransformAndResolve(expr) { texpr =>
            val resultTree: Tree = texpr match {
              case _ if texpr.tpe.isNothingType =>
                finishTransformExpr(texpr)
              case PreTransRecordTree(newExpr, structure, cancelFun) =>
                info.returnedStructures.value ::= structure
                Return(newExpr, newLabel)
              case PreTransTree(newExpr, tpe) =>
                info.returnedTreeTypes.value ::= tpe
                Return(newExpr, newLabel)
            }
            TailCalls.done(resultTree)
          }
        }

      case tree @ If(cond, thenp, elsep) =>
        trampoline {
          /* foldIf only ever folds things of type boolean. Avoid
           * pretransforming the branches when that is never going to be useful.
           */
          if (isStat || thenp.tpe != BooleanType || elsep.tpe != BooleanType) {
            pretransformExpr(cond) { tcond =>
              val resultTree = tcond match {
                case _ if tcond.tpe.isNothingType =>
                  finishTransformExpr(tcond)

                case PreTransLit(BooleanLiteral(condValue)) =>
                  val branchTaken = if (condValue) thenp else elsep
                  transform(branchTaken, isStat)

                case _ =>
                  val newThenp = transform(thenp, isStat)
                  val newElsep = transform(elsep, isStat)
                  if (isStat) {
                    foldIfStat(tcond, newThenp, newElsep)
                  } else {
                    val newCond = finishTransformExpr(tcond)
                    val constrainedType =
                      constrainedLub(newThenp.tpe, newElsep.tpe, tree.tpe, isStat)
                    If(newCond, newThenp, newElsep)(constrainedType)
                  }
              }

              TailCalls.done(resultTree)
            }
          } else {
            pretransformIf(tree)(finishTransform(isStat = false))
          }
        }

      case While(cond, body) =>
        val newCond = transformExpr(cond)
        newCond match {
          case BooleanLiteral(false) => Skip()
          case _                     => While(newCond, transformStat(body))
        }

      case ForIn(obj, keyVar @ LocalIdent(name), originalName, body) =>
        val newObj = transformExpr(obj)
        val (newName, newOriginalName) =
          freshLocalName(name, originalName, mutable = false)
        val localDef = LocalDef(RefinedType(AnyType), mutable = false,
            ReplaceWithVarRef(newName, newSimpleState(UsedAtLeastOnce)))
        val bodyScope = scope.withEnv(scope.env.withLocalDef(name, localDef))
        val newBody = transformStat(body)(bodyScope)
        ForIn(newObj, LocalIdent(newName)(keyVar.pos), newOriginalName, newBody)

      case TryCatch(block, errVar @ LocalIdent(name), originalName, handler) =>
        val newBlock = transform(block, isStat)

        val (newName, newOriginalName) =
          freshLocalName(name, originalName, mutable = false)
        val localDef = LocalDef(RefinedType(AnyType), true,
            ReplaceWithVarRef(newName, newSimpleState(UsedAtLeastOnce)))
        val newHandler = {
          val handlerScope = scope.withEnv(scope.env.withLocalDef(name, localDef))
          transform(handler, isStat)(handlerScope)
        }

        val refinedType = constrainedLub(newBlock.tpe, newHandler.tpe, tree.tpe, isStat)
        TryCatch(newBlock, LocalIdent(newName)(errVar.pos), newOriginalName,
            newHandler)(refinedType)

      case TryFinally(block, finalizer) =>
        val newBlock = transform(block, isStat)
        val newFinalizer = transformStat(finalizer)
        TryFinally(newBlock, newFinalizer)

      case Match(selector, cases, default) =>
        val newSelector = transformExpr(selector)
        newSelector match {
          case selectorValue: MatchableLiteral =>
            val body = cases.collectFirst {
              case (alts, body) if alts.exists(matchableLiteral_===(_, selectorValue)) => body
            }.getOrElse(default)
            transform(body, isStat)
          case _ =>
            val newCases = cases.map(c => (c._1, transform(c._2, isStat)))
            val newDefault = transform(default, isStat)

            val refinedType = (newDefault.tpe :: newCases.map(_._2.tpe))
              .reduce(constrainedLub(_, _, tree.tpe, isStat))

            Match(newSelector, newCases, newDefault)(refinedType)
        }

      case JSAwait(arg) =>
        JSAwait(transformExpr(arg))

      // Scala expressions

      case New(className, ctor, args) =>
        New(className, ctor, args map transformExpr)

      case LoadModule(className) =>
        if (semantics.moduleInit == CheckedBehavior.Compliant)
          tree
        else // cast away nullability to enable downstream optimizations
          makeCast(tree, ClassType(className, nullable = false))

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

      case tree: ApplyTypedClosure =>
        trampoline {
          pretransformApplyTypedClosure(tree, isStat, usePreTransform = false)(
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

      case NewArray(tpe, length) =>
        NewArray(tpe, transformExpr(length))

      case ArrayValue(tpe, elems) =>
        ArrayValue(tpe, elems map transformExpr)

      case tree: ArraySelect =>
        trampoline {
          pretransformArraySelect(tree, isLhsOfAssign = false)(
              finishTransform(isStat = false))
        }

      case RecordValue(tpe, elems) =>
        RecordValue(tpe, elems map transformExpr)

      case IsInstanceOf(expr, testType) =>
        trampoline {
          pretransformExpr(expr) { texpr =>
            val result = typeTestResult(texpr.tpe, testType) match {
              case TypeTestResult.Subtype =>
                Block(finishTransformStat(texpr), BooleanLiteral(true))
              case TypeTestResult.SubtypeOrNull =>
                BinaryOp(BinaryOp.!==, finishTransformExpr(texpr), Null())
              case TypeTestResult.NotAnInstance =>
                Block(finishTransformStat(texpr), BooleanLiteral(false))
              case TypeTestResult.Unknown =>
                IsInstanceOf(finishTransformExpr(texpr), testType)
              case TypeTestResult.NotAnInstanceUnlessNull =>
                throw new AssertionError(s"Unreachable; texpr.tpe was ${texpr.tpe} at $pos")
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

      // JavaScript expressions

      case JSNew(ctor, args) =>
        JSNew(transformExpr(ctor), transformExprsOrSpreads(args))

      case JSPrivateSelect(qualifier, field) =>
        JSPrivateSelect(transformExpr(qualifier), field)

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
        val newTree = JSBinaryOp(op, transformExpr(lhs), transformExpr(rhs))

        // Introduce casts for some idioms that are guaranteed to return certain types

        // Is `arg` guaranteed to evaluate to a JS `number` (and hence, not a `bigint`)?
        def isJSNumber(arg: Tree): Boolean = arg.tpe match {
          case IntType | DoubleType | ByteType | ShortType | FloatType => true
          case _                                                       => false
        }

        newTree match {
          /* Unless it throws, `x | y` returns either a signed 32-bit integer
           * (an `Int`) or a bigint.
           *
           * The only case in which it returns a bigint is when both arguments
           * are (convertible to) bigint's. Custom objects can be converted to
           * bigint's if their `valueOf()` method returns a bigint.
           *
           * Primitive numbers cannot be implicitly converted to bigint's.
           * `x | y` throws if one side is a number and the other is (converted
           * to) a bigint. Therefore, if at least one of the arguments is known
           * to be a primitive number, we know that `x | y` will return a
           * signed 32-bit integer (or throw).
           */
          case JSBinaryOp(JSBinaryOp.|, x, y) if isJSNumber(x) || isJSNumber(y) =>
            makeCast(newTree, IntType)

          // >>> always returns a positive number in the unsigned 32-bit range (it rejects bigints)
          case JSBinaryOp(JSBinaryOp.>>>, _, _) =>
            makeCast(newTree, DoubleType)

          case _ =>
            newTree
        }

      case JSArrayConstr(items) =>
        JSArrayConstr(transformExprsOrSpreads(items))

      case JSObjectConstr(fields) =>
        JSObjectConstr(fields.map { field =>
          (transformExpr(field._1), transformExpr(field._2))
        })

      // Atomic expressions

      case _:VarRef =>
        trampoline {
          pretransformExpr(tree)(finishTransform(isStat))
        }

      case Closure(flags, captureParams, params, restParam, resultType, body, captureValues) =>
        trampoline {
          pretransformExprs(captureValues) { tcaptureValues =>
            transformClosureCommon(flags, captureParams, params, restParam,
                resultType, body, tcaptureValues)(
                finishTransform(isStat))
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

      // Transients

      case Transient(PackLong(lo, hi)) =>
        Transient(PackLong(transformExpr(lo), transformExpr(hi)))

      // Trees that need not be transformed

      case _:Skip | _:Debugger | _:StoreModule |
          _:SelectStatic | _:JSNewTarget | _:JSImportMeta |
          _:JSGlobalRef | _:JSTypeOfGlobalRef | _:Literal =>
        tree

      case _:LinkTimeProperty | _:LinkTimeIf | _:NewLambda | _:RecordSelect |
          _:Transient =>
        throw new IllegalArgumentException(
            s"Invalid tree in transform of class ${tree.getClass.getName}: $tree")
    }

    if (isStat) keepOnlySideEffects(result)
    else result
  }

  private def transformClosureCommon(flags: ClosureFlags,
      captureParams: List[ParamDef], params: List[ParamDef],
      restParam: Option[ParamDef], resultType: Type, body: Tree,
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
      if (flags.arrow) None
      else Some(newThisLocalDef(AnyType))

    val innerEnv = OptEnv.Empty
      .withThisLocalDef(thisLocalDef)
      .withLocalDefs(paramLocalDefs)
      .withLocalDefs(restParamLocalDef.toList)

    transformCapturingBody(captureParams, tcaptureValues, body, innerEnv) {
      (newCaptureParams, newCaptureValues, newBody) =>
        val newClosure = {
          Closure(flags, newCaptureParams, newParams, newRestParam, resultType,
              newBody, newCaptureValues)
        }
        PreTransTree(newClosure, RefinedType(newClosure.tpe, isExact = flags.typed))
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

        val replacement = ReplaceWithVarRef(newName, newSimpleState(Unused))
        val localDef = LocalDef(tcaptureValue.tpe, mutable, replacement)
        val localIdent = LocalIdent(newName)(ident.pos)
        val newParamDef = ParamDef(localIdent, newOriginalName, tcaptureValue.tpe.base, mutable)(paramDef.pos)

        /* Note that the binding will never create a fresh name for a
         * ReplaceWithVarRef. So this will not put our name alignment at risk.
         */
        val valueBinding = Binding.temp(paramName, tcaptureValue)

        captureParamLocalDefs += paramName -> localDef
        newCaptureParamDefsAndRepls += newParamDef -> replacement
        captureValueBindings += valueBinding

        localDef
      }

      tcaptureValue match {
        case PreTransLit(literal) =>
          captureParamLocalDefs += paramName -> LocalDef(tcaptureValue.tpe, false, ReplaceWithConstant(literal))

        case PreTransLocalDef(LocalDef(_, /* mutable = */ false, ReplaceWithVarRef(captureName, _)))
            if !captureName.isThis =>
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
                cont1(PreTransTree(newRest))
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

  /** Pretransforms a tree to something that will not resolve to a record.
   *
   *  The way this is done is non optimal. We pretransformExpr the tree, and
   *  cancel if after the fact if it was a record. In the future we might want
   *  a way to pass down the requirement not to attempt a record in the first
   *  place.
   */
  private def pretransformExprNoRecord(tree: Tree)(cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    pretransformExpr(tree) { texpr =>
      cancelIfRecord(texpr)
      cont(texpr)
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

      case VarRef(name) =>
        val localDef = scope.env.localDefs.getOrElse(name, {
          throw new AssertionError(
              s"Cannot find local def '$name' at $pos\n" +
              s"While optimizing $debugID\n" +
              s"Env is ${scope.env}\n" +
              s"Inlining ${scope.implsBeingInlined}")
        })
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

      case Labeled(label, tpe, body) =>
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

      case tree: ApplyTypedClosure =>
        pretransformApplyTypedClosure(tree, isStat = false,
            usePreTransform = true)(cont)

      case tree: UnaryOp =>
        pretransformUnaryOp(tree)(cont)

      case tree: BinaryOp =>
        pretransformBinaryOp(tree)(cont)

      case ArrayValue(typeRef, items) =>
        /* Trying to virtualize more than 64 items in an array is probably
         * a bad idea, and will slow down the optimizer for no good reason.
         * See for example #2943.
         */
        if (items.size > 64) {
          cont(ArrayValue(typeRef, items.map(transformExpr(_))).toPreTransform)
        } else {
          pretransformExprs(items) { titems =>
            tryOrRollback { cancelFun =>
              withNewTempLocalDefs(titems) { (itemLocalDefs, cont1) =>
                val replacement = InlineArrayReplacement(
                    typeRef, itemLocalDefs.toVector, cancelFun)
                val localDef = LocalDef(
                    RefinedType(tree.tpe),
                    mutable = false,
                    replacement)
                cont1(localDef.toPreTransform)
              } (cont)
            } { () =>
              cont(PreTransTree(ArrayValue(typeRef, titems.map(finishTransformExpr))))
            }
          }
        }

      case tree: ArraySelect =>
        pretransformArraySelect(tree, isLhsOfAssign = false)(cont)

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
                    RefinedType(AnyNotNullType),
                    mutable = false,
                    replacement)
                cont1(localDef.toPreTransform)
              } (cont)
            } { () =>
              cont(PreTransTree(JSArrayConstr(titems.map(finishTransformExpr))))
            }
          }
        }

      case AsInstanceOf(expr, tpe) =>
        pretransformExpr(expr) { texpr =>
          cont(foldAsInstanceOf(texpr, tpe))
        }

      case Closure(flags, captureParams, params, restParam, resultType, body, captureValues) =>
        pretransformExprs(captureValues) { tcaptureValues =>
          def default(): TailRec[Tree] = {
            transformClosureCommon(flags, captureParams, params, restParam,
                resultType, body, tcaptureValues)(cont)
          }

          if (!flags.arrow || flags.async || restParam.isDefined) {
            /* TentativeClosureReplacement assumes there are no rest
             * parameters, because that would not be inlineable anyway.
             * Likewise, it assumes that there is no binding for `this` nor for
             * `new.target`, which is only true for arrow functions.
             * Async closures cannot be inlined, due to their semantics.
             * So we only ever try to inline non-async arrow Closures without
             * rest parameters.
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
                    flags, captureParams, params, resultType, body, captureLocalDefs,
                    alreadyUsed = newSimpleState(Unused), cancelFun)
                val localDef = LocalDef(
                    RefinedType(tree.tpe, isExact = flags.typed),
                    mutable = false,
                    replacement)
                cont1(localDef.toPreTransform)
              } (cont)
            } { () =>
              default()
            }
          }
        }


      case Transient(PackLong(lo, hi)) =>
        pretransformExprs(lo, hi) { (tlo, thi) =>
          val loBinding = Binding.temp(LocalName("lo"), tlo)
          val hiBinding = Binding.temp(LocalName("hi"), thi)
          withNewLocalDefs(List(loBinding, hiBinding)) { (localDefs, cont1) =>
            val List(loLocalDef, hiLocalDef) = localDefs
            val pairLocalDef = LocalDef(RefinedType(LongType), mutable = false,
                LongPairReplacement(loLocalDef, hiLocalDef))
            cont1(pairLocalDef.toPreTransform)
          } (cont)
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
              cont(PreTransTree(transformedStat))
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

    /* Should we extract the given branch to a statement If?
     *
     * If a branch has type `nothing`, we can extract it as
     *   { if (cond) branch; otherBranch }
     * rather than
     *   if (cond) branch else otherBranch
     *
     * This is particularly important when the `otherBranch` resolves to
     * a record. In that case, this rewrite avoids canceling the record.
     */
    def shouldExtractBranchToStat(branch: PreTransform): Boolean =
      branch.tpe.isNothingType

    def doExtractBranchToStat(tcond: PreTransform, tthenp: PreTransform,
        telsep: PreTransform): TailRec[Tree] = {
      cont(PreTransBlock(
          If(finishTransformExpr(tcond), finishTransformStat(tthenp), Skip())(VoidType),
          telsep))
    }

    def default(tcond: PreTransform, tthenp: PreTransform,
        telsep: PreTransform): TailRec[Tree] = {
      val refinedType = constrainedLub(tthenp.tpe, telsep.tpe, tree.tpe)
      cont(foldIf(tcond, tthenp, telsep)(refinedType))
    }

    pretransformExpr(cond) { tcond =>
      tcond match {
        case _ if tcond.tpe.isNothingType =>
          cont(tcond)

        case PreTransLit(BooleanLiteral(condValue)) =>
          if (condValue)
            pretransformExpr(thenp)(cont)
          else
            pretransformExpr(elsep)(cont)

        case _ =>
          tryOrRollback { cancelFun =>
            pretransformExprs(thenp, elsep) { (tthenp, telsep) =>
              if (shouldExtractBranchToStat(tthenp)) {
                doExtractBranchToStat(tcond, tthenp, telsep)
              } else if (shouldExtractBranchToStat(telsep)) {
                doExtractBranchToStat(foldNot(tcond), telsep, tthenp)
              } else {
                (resolveRecordPreTransform(tthenp), resolveRecordPreTransform(telsep)) match {
                  case (PreTransRecordTree(thenTree, thenStructure, thenCancelFun),
                      PreTransRecordTree(elseTree, elseStructure, elseCancelFun)) =>
                    if (!thenStructure.sameClassAs(elseStructure))
                      cancelFun()
                    assert(thenTree.tpe == elseTree.tpe)
                    cont(PreTransRecordTree(
                        If(finishTransformExpr(tcond), thenTree, elseTree)(thenTree.tpe),
                        thenStructure,
                        cancelFun))

                  case (PreTransRecordTree(_, _, thenCancelFun), _) =>
                    thenCancelFun()

                  case (_, PreTransRecordTree(_, _, elseCancelFun)) =>
                    elseCancelFun()

                  case (tthenpNoRecord, telsepNoRecord) =>
                    default(tcond, tthenpNoRecord, telsepNoRecord)
                }
              }
            }
          } { () =>
            /* If the If is canceled as a whole, it's because *both* the thenp
             * and elsep resulted in a record, but either a) they did not have
             * the same structure or b) the If was canceled later on.
             * Either way, we want to retry but make sure that neither branch
             * results in a record.
             *
             * We may also come here because the whole function went into
             * disableOptimisticOptimizations mode. In that case, we would have
             * completely bypassed the above code. We still want to perform
             * pretransform's and folding for booleans, though.
             *
             * More generally, we might still want to get good RefinedType's
             * and the ability to dce at the PreTransform level.
             */
            pretransformExprNoRecord(thenp) { tthenp =>
              pretransformExprNoRecord(elsep) { telsep =>
                default(tcond, tthenp, telsep)
              }
            }
          }
      }
    }
  }

  private def pretransformSelectCommon(tree: Select, isLhsOfAssign: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    val Select(qualifier, field) = tree
    pretransformExpr(qualifier) { preTransQual =>
      pretransformSelectCommon(tree.tpe, preTransQual, optQualDeclaredType = None, field, isLhsOfAssign)(
          cont)(scope, tree.pos)
    }
  }

  private def pretransformSelectCommon(expectedType: Type,
      preTransQual: PreTransform, optQualDeclaredType: Option[Type],
      field: FieldIdent, isLhsOfAssign: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {
    /* Note: Callers are expected to have already removed writes to fields that
     * are never read.
     */

    preTransQual match {
      case PreTransLocalDef(LocalDef(_, _,
          InlineClassBeingConstructedReplacement(_, fieldLocalDefs, cancelFun))) =>
        val fieldLocalDef = fieldLocalDefs(field.name)
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
        val fieldLocalDef = fieldLocalDefs(field.name)
        assert(!isLhsOfAssign || fieldLocalDef.mutable, s"assign to immutable field at $pos")
        cont(fieldLocalDef.toPreTransform)

      case _ =>
        def default: TailRec[Tree] = {
          resolvePreTransform(preTransQual) match {
            case PreTransRecordTree(newQual, structure, cancelFun) =>
              val recordField = structure.findField(field.name)
              val sel = RecordSelect(newQual, SimpleFieldIdent(recordField.name))(recordField.tpe)
              sel.tpe match {
                case _: RecordType =>
                  /* We're trying to select a reified subrecord. We lost its
                   * structure so cannot build an accurate `PreTransRecordTree`.
                   * This did not happen in our test suite at the time of writing.
                   */
                  cancelFun()
                case _ =>
                  cont(PreTransTree(sel))
              }

            case tqual: PreTransTree =>
              val tqualCast = optQualDeclaredType match {
                case Some(ClassType(qualDeclaredClass, _)) =>
                  foldCast(tqual, ClassType(qualDeclaredClass, nullable = true))
                case _ =>
                  tqual
              }
              val newQual = finishTransformExpr(tqualCast)
              cont(PreTransTree(Select(newQual, field)(expectedType)))
          }
        }

        preTransQual.tpe.base match {
          // Try to inline an inlineable field body
          case ClassType(qualClassName, _) if !isLhsOfAssign =>
            if (myself.exists(m => m.enclosingClassName == qualClassName && m.methodName.isConstructor)) {
              /* Within the constructor of a class, we cannot trust the
               * inlineable field bodies of that class, since they only reflect
               * the values of fields when the instance is fully initialized.
               */
              default
            } else {
              inlineableFieldBodies(qualClassName).fieldBodies.get(field.name) match {
                case None =>
                  default
                case Some(fieldBody) =>
                  val qualSideEffects = checkNotNullStatement(preTransQual)
                  val fieldBodyTree = fieldBodyToTree(fieldBody)
                  pretransformExpr(fieldBodyTree) { preTransFieldBody =>
                    cont(PreTransBlock(qualSideEffects, preTransFieldBody))
                  }
              }
            }
          case NothingType =>
            cont(preTransQual)
          case NullType =>
            cont(checkNotNull(preTransQual))
          case _ =>
            default
        }
    }
  }

  private def fieldBodyToTree(fieldBody: InlineableFieldBodies.FieldBody): Tree = {
    import InlineableFieldBodies.FieldBody

    implicit val pos = fieldBody.pos

    fieldBody match {
      case FieldBody.Literal(literal, _) =>
        literal
      case FieldBody.LoadModule(moduleClassName, _) =>
        LoadModule(moduleClassName)
      case FieldBody.ModuleSelect(qualifier, fieldName, tpe, _) =>
        Select(fieldBodyToTree(qualifier), FieldIdent(fieldName))(tpe)
      case FieldBody.ModuleGetter(qualifier, methodName, tpe, _) =>
        Apply(ApplyFlags.empty, fieldBodyToTree(qualifier), MethodIdent(methodName), Nil)(tpe)
    }
  }

  private def pretransformArraySelect(tree: ArraySelect, isLhsOfAssign: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {

    val ArraySelect(array, index) = tree
    implicit val pos = tree.pos

    pretransformExprs(array, index) { (tarray, tindex) =>
      (tarray, tindex) match {
        case (PreTransLocalDef(LocalDef(tpe, /* mutable = */ false,
            replacement: InlineArrayReplacement)),
            PreTransLit(IntLiteral(indexValue)))
            if !isLhsOfAssign && replacement.elemLocalDefs.indices.contains(indexValue) =>
          cont(replacement.elemLocalDefs(indexValue).toPreTransform)

        case _ =>
          def newArray = finishTransformExpr(tarray)
          def newIndex = finishTransformExpr(tindex)

          tarray.tpe.base match {
            case NothingType | NullType if isLhsOfAssign =>
              /* We need to preserve a real ArraySelect for the Assign node.
               * However, we can drop the side effects of the index, since we
               * won't get that far.
               */
              cont(ArraySelect(newArray, IntLiteral(0))(NothingType).toPreTransform)
            case NothingType =>
              cont(tarray)
            case NullType =>
              cont(checkNotNull(tarray))
            case arrayType: ArrayType =>
              cont(ArraySelect(newArray, newIndex)(arrayElemType(arrayType)).toPreTransform)
            case tpe =>
              throw new AssertionError(
                  s"got non-array type $tpe after transforming ArraySelect at $pos")
          }
      }
    }
  }

  private def pretransformAssign(tlhs: PreTransform, trhs: PreTransform)(
      cont: PreTransCont)(implicit scope: Scope, pos: Position): TailRec[Tree] = {
    def contAssign(lhs: Tree, rhs: Tree) =
      cont(PreTransTree(Assign(lhs.asInstanceOf[AssignLhs], rhs)))

    resolvePreTransform(tlhs) match {
      case PreTransRecordTree(lhsTree, lhsStructure, lhsCancelFun) =>
        resolvePreTransform(trhs) match {
          case PreTransRecordTree(rhsTree, rhsStructure, rhsCancelFun) =>
            if (!lhsStructure.sameClassAs(rhsStructure))
              lhsCancelFun()
            assert(rhsTree.tpe == lhsTree.tpe)
            contAssign(lhsTree, rhsTree)
          case _ =>
            lhsCancelFun()
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
              New(className, ctor, targs.map(finishTransformExpr))))
        }
      case None =>
        cont(PreTransTree(
            New(className, ctor, targs.map(finishTransformExpr))))
    }
  }

  /** Resolves a [[PreTransform]] into a `PreTransGenTree`. */
  private def resolvePreTransform(preTrans: PreTransform): PreTransGenTree = {
    implicit val pos = preTrans.pos
    preTrans match {
      case PreTransBlock(bindingsAndStats, result) =>
        resolvePreTransform(result) match {
          case PreTransRecordTree(tree, structure, cancelFun) =>
            PreTransRecordTree(finishTransformBindings(bindingsAndStats, tree),
                structure, cancelFun)
          case PreTransTree(tree, tpe) =>
            PreTransTree(finishTransformBindings(bindingsAndStats, tree), tpe)
        }

      case _:PreTransUnaryOp | _:PreTransBinaryOp | _:PreTransIf =>
        PreTransTree(finishTransformExpr(preTrans), preTrans.tpe)

      case PreTransLocalDef(localDef) =>
        localDef.replacement match {
          case ReplaceWithRecordVarRef(name, structure, used, cancelFun) =>
            used.value = used.value.inc
            PreTransRecordTree(VarRef(name)(structure.recordType), structure, cancelFun)

          case InlineClassInstanceReplacement(structure, fieldLocalDefs, cancelFun) =>
            val recordType = structure.recordType
            if (!isImmutableType(recordType))
              cancelFun()
            PreTransRecordTree(
                RecordValue(recordType, structure.fieldNames.map(
                    id => fieldLocalDefs(id).newReplacement)),
                structure, cancelFun)

          case _ =>
            PreTransTree(localDef.newReplacement, localDef.tpe)
        }

      case preTrans: PreTransGenTree =>
        preTrans
    }
  }

  /** Resolves a `PreTransform` if it would result in a record.
   *
   *  Otherwise, leave it as is.
   */
  private def resolveRecordPreTransform(preTrans: PreTransform): PreTransform = {
    if (resolveRecordStructure(preTrans).isDefined)
      resolvePreTransform(preTrans)
    else
      preTrans
  }

  /** Resolves the [[InlineableClassStructure]] of a [[PreTransform]], if any.
   *
   *  If `preTrans` would resolve to a `PreTransRecordTree`, returns a `Some`
   *  of its [[InlineableClassStructure]] and its `cancelFun`. Otherwise,
   *  returns `None`.
   */
  private def resolveRecordStructure(
      preTrans: PreTransform): Option[(InlineableClassStructure, CancelFun)] = {
    preTrans match {
      case PreTransBlock(_, result) =>
        resolveRecordStructure(result)

      case _:PreTransUnaryOp | _:PreTransBinaryOp | _:PreTransIf =>
        None

      case PreTransLocalDef(localDef @ LocalDef(tpe, _, replacement)) =>
        replacement match {
          case ReplaceWithRecordVarRef(name, structure, used, cancelFun) =>
            Some((structure, cancelFun))

          case InlineClassInstanceReplacement(structure, fieldLocalDefs, cancelFun) =>
            Some((structure, cancelFun))

          case _ =>
            None
        }

      case PreTransRecordTree(tree, structure, cancelFun) =>
        Some((structure, cancelFun))

      case PreTransTree(_, _) =>
        None
    }
  }

  /** Cancels a PreTransform if it would resolve to a record. */
  private def cancelIfRecord(preTrans: PreTransform): Unit = {
    for ((_, cancelFun) <- resolveRecordStructure(preTrans))
      cancelFun()
  }

  /** Combines pretransformExpr and resolvePreTransform in one convenience method. */
  private def pretransformAndResolve(tree: Tree)(
      cont: PreTransGenTree => TailRec[Tree])(
      implicit scope: Scope): TailRec[Tree] = {
    pretransformExpr(tree) { ttree =>
      cont(resolvePreTransform(ttree))
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
      case PreTransIf(cond, thenp, elsep, tpe) =>
        If(finishTransformExpr(cond), finishTransformExpr(thenp),
            finishTransformExpr(elsep))(tpe.base)
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
   *
   *  !!! #5246 finishTransformStat must never return a Tree whose type is
   *  a RecordType. Those trees may be put into a PreTransTree, which cannot
   *  accept them. This method must aggressively get rid of any tree that may
   *  have a RecordType.
   */
  private def finishTransformStat(stat: PreTransform): Tree = stat match {
    case PreTransBlock(bindingsAndStats, result) =>
      finishTransformBindings(bindingsAndStats, finishTransformStat(result))

    case PreTransUnaryOp(op, lhs) =>
      if (!UnaryOp.isSideEffectFreeOp(op))
        finishTransformExpr(stat)
      else
        finishTransformStat(lhs)

    case PreTransBinaryOp(op, lhs, rhs) =>
      import BinaryOp._

      implicit val pos = stat.pos

      def newLhs = finishTransformStat(lhs)

      def finishNoSideEffects: Tree =
        Block(newLhs, finishTransformStat(rhs))

      def finishWithSideEffects: Tree =
        BinaryOp(op, finishTransformExpr(lhs), finishTransformExpr(rhs))

      (op: @switch) match {
        case Int_/ | Int_% | Int_unsigned_/ | Int_unsigned_% =>
          rhs match {
            case PreTransLit(IntLiteral(r)) if r != 0 =>
              finishNoSideEffects
            case _ =>
              Block(newLhs, BinaryOp(op, IntLiteral(0), finishTransformExpr(rhs)))
          }
        case Long_/ | Long_% | Long_unsigned_/ | Long_unsigned_% =>
          rhs match {
            case PreTransLit(LongLiteral(r)) if r != 0L =>
              finishNoSideEffects
            case _ =>
              Block(newLhs, BinaryOp(op, LongLiteral(0L), finishTransformExpr(rhs)))
          }
        case String_charAt if semantics.stringIndexOutOfBounds != CheckedBehavior.Unchecked =>
          finishWithSideEffects
        case Class_cast if semantics.asInstanceOfs != CheckedBehavior.Unchecked =>
          finishWithSideEffects
        case Class_newArray =>
          finishWithSideEffects
        case _ =>
          finishNoSideEffects
      }

    case PreTransIf(cond, thenp, elsep, tpe) =>
      val newThenp = finishTransformStat(thenp)
      val newElsep = finishTransformStat(elsep)
      foldIfStat(cond, newThenp, newElsep)(stat.pos)

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
          case ReplaceWithVarRef(name, used) =>
            (name, used)
          case ReplaceWithRecordVarRef(name, _, used, _) =>
            (name, used)
        }

        if (used.value.isUsed) {
          val ident = LocalIdent(name)
          resolvePreTransform(value) match {
            case PreTransRecordTree(valueTree, valueStructure, cancelFun) =>
              val recordType = valueStructure.recordType
              assert(valueTree.tpe == recordType)
              if (!isImmutableType(recordType))
                cancelFun()
              Block(VarDef(ident, originalName, recordType, mutable, valueTree), innerBody)

            case PreTransTree(valueTree, valueTpe) =>
              val optimized =
                if (used.value.count == 1 && config.minify) tryInsertAtFirstEvalContext(name, valueTree, innerBody)
                else None
              optimized.getOrElse {
                Block(VarDef(ident, originalName, tpe.base, mutable, valueTree), innerBody)
              }
          }
        } else {
          val valueSideEffects = finishTransformStat(value)
          Block(valueSideEffects, innerBody)
        }

      case (Right(stat), innerBody) =>
        Block(stat, innerBody)(innerBody.pos)
    }
  }

  /** Keeps only the side effects of a Tree (overapproximation).
   *
   *  !!! #5246 keepOnlySideEffects must never return a Tree whose type is
   *  a RecordType. Those trees may be put into a PreTransTree, which cannot
   *  accept them. This method must aggressively get rid of any tree that may
   *  have a RecordType. Last resort: protect it behind a Labeled block with
   *  type `void`.
   */
  private def keepOnlySideEffects(stat: Tree): Tree = stat match {
    case _:VarRef | _:Literal | _:SelectStatic =>
      Skip()(stat.pos)
    case VarDef(_, _, _, _, rhs) =>
      keepOnlySideEffects(rhs)
    case Block(init :+ last) =>
      keepOnlySideEffects(last) match {
        case Skip()      => keepOnlySideEffects(Block(init)(stat.pos))
        case lastEffects => Block(init, lastEffects)(stat.pos)
      }
    case New(className, _, args) =>
      if (hasElidableConstructors(className)) Block(args.map(keepOnlySideEffects(_)))(stat.pos)
      else stat
    case LoadModule(moduleClassName) =>
      if (hasElidableConstructors(moduleClassName)) Skip()(stat.pos)
      else stat
    case NewArray(_, length) if isNonNegativeIntLiteral(length) =>
      Skip()(stat.pos)
    case NewArray(_, length) if semantics.negativeArraySizes == CheckedBehavior.Unchecked =>
      keepOnlySideEffects(length)
    case ArrayValue(_, elems) =>
      Block(elems.map(keepOnlySideEffects(_)))(stat.pos)
    case ArraySelect(array, index) if semantics.arrayIndexOutOfBounds == CheckedBehavior.Unchecked =>
      Block(checkNotNullStatement(array)(stat.pos), keepOnlySideEffects(index))(stat.pos)
    case Select(qualifier, _) =>
      checkNotNullStatement(qualifier)(stat.pos)
    case Closure(_, _, _, _, _, _, captureValues) =>
      Block(captureValues.map(keepOnlySideEffects))(stat.pos)
    case UnaryOp(op, arg) if UnaryOp.isSideEffectFreeOp(op) =>
      keepOnlySideEffects(arg)
    case If(cond, thenp, elsep) =>
      (keepOnlySideEffects(thenp), keepOnlySideEffects(elsep)) match {
        case (Skip(), Skip())     => keepOnlySideEffects(cond)
        case (newThenp, newElsep) => If(cond, newThenp, newElsep)(VoidType)(stat.pos)
      }
    case Labeled(label, _, body) =>
      Labeled(label, VoidType, keepOnlySideEffects(body))(stat.pos)

    case BinaryOp(op, lhs, rhs) =>
      /* The logic here exceeds the complexity threshold for keeping a copy
       * compared to `finishTransformStat` at the Tree level. Instead, we
       * convert to a PreTransBinaryOp and delegate to the other function.
       */
      finishTransformStat(PreTransBinaryOp(op, lhs.toPreTransform, rhs.toPreTransform)(stat.pos))

    case RecordValue(_, elems) =>
      Block(elems.map(keepOnlySideEffects))(stat.pos)
    case RecordSelect(record, _) =>
      keepOnlySideEffects(record)

    /* By definition, a failed cast is always UB, so it cannot have side effects.
     * However, if the target type is `nothing`, we keep the cast not to lose
     * the information that anything that follows is dead code.
     */
    case Transient(Cast(expr, tpe)) =>
      implicit val pos = stat.pos
      val exprSideEffects = keepOnlySideEffects(expr)
      if (tpe != NothingType)
        exprSideEffects
      else
        Block(exprSideEffects, Transient(Cast(Null(), tpe)))

    case Transient(GetFPBitsDataView) =>
      Skip()(stat.pos)

    case _ =>
      if (stat.tpe.isInstanceOf[RecordType]) {
        // #5246 Last resort not to return a RecordType'd tree
        Labeled(freshLabelName(LabelDiscardBase), VoidType, stat)(stat.pos)
      } else {
        stat
      }
  }

  private def isNonNegativeIntLiteral(tree: Tree): Boolean = tree match {
    case IntLiteral(value) => value >= 0
    case _                 => false
  }

  /** Tries to insert `valTree` in place of the (unique) occurrence of `valName` in `body`.
   *
   *  This function assumes that `valName` is used only once, and only inside
   *  `body`. It does not assume that `valTree` or `body` are side-effect-free.
   *
   *  The replacement is done only if we can show that it will not affect
   *  evaluation order. In practice, this means that we only replace if we find
   *  the occurrence of `valName` in the first evaluation context of `body`.
   *  In other words, we verify that all the expressions that will evaluate
   *  before `valName` in `body` are pure.
   *
   *  We consider a `VarRef(y)` pure if `valTree` does not contain any
   *  assignment to `y`.
   *
   *  For example, we can replace `x` in the following bodies:
   *
   *  {{{
   *  x
   *  x + e
   *  x.foo(...)
   *  x.f
   *  e + x                 // if `e` is pure
   *  e0.foo(...e1, x, ...) // if `e0` is pure and non-null, and the `...e1`s are pure
   *  if (x) { ... } else { ... }
   *  }}}
   *
   *  Why is this interesting? Mostly because of inlining.
   *
   *  Inlining tends to create many bindings for the receivers and arguments of
   *  methods. We must do that to preserve evaluation order, and not to evaluate
   *  them multiple times. However, very often, the receiver and arguments are
   *  used exactly once in the inlined body, and in-order. Using this strategy,
   *  we can take the right-hand-sides of the synthetic bindings and inline them
   *  directly inside the body.
   *
   *  This in turn allows more trees to remain JS-level expressions, which means
   *  that `FunctionEmitter` has to `unnest` less often, further reducing the
   *  amount of temporary variables.
   *
   *  ---
   *
   *  Note that we can never cross any potential undefined behavior, even when
   *  the corresponding semantics are `Unchecked`. That is because the
   *  `valTree` could throw itself, preventing the normal behavior of the code
   *  to reach the undefined behavior in the first place. Consider for example:
   *
   *  {{{
   *  val x: Foo = ... // maybe null
   *  val y: Int = if (x == null) throw new Exception() else 1
   *  x.foo(y)
   *  }}}
   *
   *  We cannot inline `y` in this example, because that would change
   *  observable behavior if `x` is `null`.
   *
   *  It is OK to cross the potential UB if we can prove that it will not
   *  actually trigger, for example if we know that `x` is not null.
   *
   *  ---
   *
   *  We only call this function when the `minify` option is on. This is for two
   *  reasons:
   *
   *  - it can be detrimental to debuggability, as even user-written `val`s can
   *    disappear, and their right-hand-side be evaluated out-of-order compared
   *    to the source code;
   *  - it is non-linear, as we can perform several traversals of the same body,
   *    if it follows a sequence of `VarDef`s that can each be successfully
   *    inserted.
   */
  private def tryInsertAtFirstEvalContext(valName: LocalName, valTree: Tree, body: Tree): Option[Tree] = {
    import EvalContextInsertion._

    object valTreeInfo extends Traversers.LocalScopeTraverser {
      val mutatedLocalVars = mutable.Set.empty[LocalName]

      traverse(valTree)

      override def traverse(tree: Tree): Unit = {
        super.traverse(tree)
        tree match {
          case Assign(VarRef(name), _) => mutatedLocalVars += name
          case _                       => ()
        }
      }
    }

    def isNotNull(tree: Tree): Boolean = !tree.tpe.isNullable

    def recs(bodies: List[Tree]): EvalContextInsertion[List[Tree]] = bodies match {
      case Nil =>
        NotFoundPureSoFar
      case firstBody :: restBodies =>
        rec(firstBody) match {
          case Success(newFirstBody) => Success(newFirstBody :: restBodies)
          case NotFoundPureSoFar     => recs(restBodies).mapOrKeepGoing(firstBody :: _)
          case Failed                => Failed
        }
    }

    def rec(body: Tree): EvalContextInsertion[Tree] = {
      implicit val pos = body.pos

      body match {
        case VarRef(name) =>
          if (name == valName)
            Success(valTree)
          else if (valTreeInfo.mutatedLocalVars.contains(name))
            Failed
          else
            NotFoundPureSoFar

        case Skip() =>
          NotFoundPureSoFar

        case Block(stats) =>
          recs(stats).mapOrKeepGoing(Block(_))

        case Labeled(label, tpe, innerBody) =>
          rec(innerBody).mapOrKeepGoing(Labeled(label, tpe, _))

        case Return(expr, label) =>
          rec(expr).mapOrFailed(Return(_, label))

        case If(cond, thenp, elsep) =>
          rec(cond).mapOrFailed(If(_, thenp, elsep)(body.tpe))

        case Match(selector, cases, default) =>
          rec(selector).mapOrFailed(Match(_, cases, default)(body.tpe))

        case New(className, ctor, args) =>
          recs(args).mapOrKeepGoingIf(New(className, ctor, _))(
              keepGoingIf = hasElidableConstructors(className))

        case LoadModule(className) =>
          if (hasElidableConstructors(className)) NotFoundPureSoFar
          else Failed

        case Select(qual, field) =>
          rec(qual).mapOrFailed(Select(_, field)(body.tpe))

        case Apply(flags, receiver, method, args) =>
          rec(receiver) match {
            case Success(newReceiver) =>
              Success(Apply(flags, newReceiver, method, args)(body.tpe))
            case NotFoundPureSoFar if isNotNull(receiver) =>
              recs(args).mapOrFailed(Apply(flags, receiver, method, _)(body.tpe))
            case _ =>
              Failed
          }

        case ApplyStatically(flags, receiver, className, method, args) =>
          rec(receiver) match {
            case Success(newReceiver) =>
              Success(ApplyStatically(flags, newReceiver, className, method, args)(body.tpe))
            case NotFoundPureSoFar if isNotNull(receiver) =>
              recs(args).mapOrFailed(ApplyStatically(flags, receiver, className, method, _)(body.tpe))
            case _ =>
              Failed
          }

        case ApplyStatic(flags, className, method, args) =>
          recs(args).mapOrFailed(ApplyStatic(flags, className, method, _)(body.tpe))

        case UnaryOp(op, arg) =>
          rec(arg).mapOrKeepGoingIf(UnaryOp(op, _))(keepGoingIf = UnaryOp.isPureOp(op))

        case BinaryOp(op, lhs, rhs) =>
          import BinaryOp._

          rec(lhs) match {
            case Success(newLhs) => Success(BinaryOp(op, newLhs, rhs))
            case Failed          => Failed

            case NotFoundPureSoFar =>
              rec(rhs).mapOrKeepGoingIf(BinaryOp(op, lhs, _)) {
                (op: @switch) match {
                  case Int_/ | Int_% | Int_unsigned_/ | Int_unsigned_% |
                      Long_/ | Long_% | Long_unsigned_/ | Long_unsigned_% |
                      String_+ | String_charAt | Class_cast | Class_newArray =>
                    false
                  case _ =>
                    true
                }
              }
          }

        case NewArray(typeRef, length) =>
          rec(length).mapOrKeepGoing(NewArray(typeRef, _))

        case ArrayValue(typeRef, elems) =>
          recs(elems).mapOrKeepGoing(ArrayValue(typeRef, _))

        case ArraySelect(array, index) =>
          rec(array) match {
            case Success(newArray) =>
              Success(ArraySelect(newArray, index)(body.tpe))
            case NotFoundPureSoFar if isNotNull(array) =>
              rec(index).mapOrFailed(ArraySelect(array, _)(body.tpe))
            case _ =>
              Failed
          }

        case RecordValue(tpe, elems) =>
          recs(elems).mapOrKeepGoing(RecordValue(tpe, _))

        case RecordSelect(record, field) =>
          rec(record).mapOrKeepGoingIf(RecordSelect(_, field)(body.tpe)) {
            // We can keep going if the selected field is immutable
            val RecordType(fields) = record.tpe: @unchecked
            !fields.find(_.name == field.name).get.mutable
          }

        case IsInstanceOf(expr, testType) =>
          rec(expr).mapOrKeepGoing(IsInstanceOf(_, testType))

        case AsInstanceOf(expr, tpe) =>
          rec(expr).mapOrFailed(AsInstanceOf(_, tpe))

        case Transient(Cast(expr, tpe)) =>
          rec(expr).mapOrKeepGoing(newExpr => makeCast(newExpr, tpe))

        case JSUnaryOp(op, arg) =>
          rec(arg).mapOrFailed(JSUnaryOp(op, _))

        case JSBinaryOp(op, lhs, rhs) =>
          rec(lhs) match {
            case Success(newLhs) =>
              Success(JSBinaryOp(op, newLhs, rhs))
            case NotFoundPureSoFar =>
              rec(rhs).mapOrKeepGoingIf(JSBinaryOp(op, lhs, _))(
                  keepGoingIf = op == JSBinaryOp.=== || op == JSBinaryOp.!==)
            case Failed =>
              Failed
          }

        case JSArrayConstr(items) =>
          if (items.exists(_.isInstanceOf[JSSpread]))
            Failed // in theory we could do something better here, but the complexity is not worth it
          else
            recs(items.asInstanceOf[List[Tree]]).mapOrKeepGoing(JSArrayConstr(_))

        case _: Literal =>
          NotFoundPureSoFar
        case Transient(GetFPBitsDataView) =>
          NotFoundPureSoFar

        case Closure(flags, captureParams, params, restParam, resultType, body, captureValues) =>
          recs(captureValues).mapOrKeepGoing { newCaptureValues =>
            Closure(flags, captureParams, params, restParam, resultType, body, newCaptureValues)
          }

        case _ =>
          Failed
      }
    }

    rec(body) match {
      case Success(result) => Some(result)
      case Failed          => None

      case NotFoundPureSoFar =>
        /* The val was never actually used. This can happen even when the
         * variable was `used` exactly once, because `used` tracks the number
         * of times we have generated a `VarRef` for it. In some cases, the
         * generated `VarRef` is later discarded through `keepOnlySideEffects`
         * somewhere else.
         */
        Some(Block(keepOnlySideEffects(valTree), body)(body.pos))
    }
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
      cont(PreTransTree(Apply(flags,
          finishTransformExpr(treceiver), methodIdent,
          targs.map(finishTransformExpr))(resultType)))
    }

    treceiver.tpe.base match {
      case NothingType =>
        cont(treceiver) // throws
      case NullType =>
        cont(checkNotNull(treceiver))
      case _ =>
        if (methodName.isReflectiveProxy || flags.noinline) {
          // Never inline reflective proxies or explicit noinlines.
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
           * For primitives, we know we will have a single target even if we
           * perform dynamic resolution, so we can use the more efficient
           * static lookup as well.
           *
           * Overall, the only cases where we need dynamic resolution are for
           * non-exact class types and `any`/`any!`.
           */
          val useStaticResolution = treceiver.tpe match {
            case RefinedType(_: ClassType, exact)         => exact
            case RefinedType(_:PrimType | _:ArrayType, _) => true
            case RefinedType(AnyType | AnyNotNullType, _) => false

            case RefinedType(_:ClosureType | _:RecordType, _) =>
              throw new AssertionError(s"Invalid receiver type ${treceiver.tpe} at $pos")
          }

          val impls =
            if (useStaticResolution) List(staticCall(className, namespace, methodName))
            else dynamicCall(className, methodName)
          if (impls.size == 1) {
            pretransformSingleDispatch(flags, impls.head, Some(treceiver), targs, isStat, usePreTransform)(cont) {
              if (isWasm) {
                // Replace by an ApplyStatically to guarantee static dispatch
                val targetClassName = impls.head.enclosingClassName
                val castTReceiver = foldCast(treceiver, ClassType(targetClassName, nullable = true))
                cont(PreTransTree(ApplyStatically(flags,
                    finishTransformExpr(castTReceiver),
                    targetClassName, methodIdent,
                    targs.map(finishTransformExpr))(resultType)))
              } else {
                /* In case you get tempted to perform the same optimization on
                 * JS, we tried it before (in a much more involved way) and we
                 * found that it was not better or even worse:
                 * https://github.com/scala-js/scala-js/pull/4337
                 */
                treeNotInlined
              }
            }
          } else {
            val allocationSites =
              (treceiver :: targs).map(_.tpe.allocationSite)
            val shouldTryMultiInline = {
              impls.nonEmpty && // will fail at runtime.
              impls.forall(impl => impl.attributes.isForwarder && impl.attributes.inlineable) &&
              !impls.exists(impl => scope.implsBeingInlined((allocationSites, impl)))
            }

            if (shouldTryMultiInline) {
              tryMultiInline(impls, treceiver, targs, isStat, usePreTransform)(cont) {
                treeNotInlined
              }
            } else {
              treeNotInlined
            }
          }
        }
    }
  }

  private def tryMultiInline(impls: List[MethodID], treceiver: PreTransform,
      targs: List[PreTransform], isStat: Boolean, usePreTransform: Boolean)(
      cont: PreTransCont)(
      treeNotInlined: => TailRec[Tree])(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    val referenceMethodDef = getMethodBody(impls.head)

    (referenceMethodDef.body.get match {
      // Shape of forwarders to default methods
      case body @ ApplyStatically(flags, This(), className, MethodIdent(methodName), args) =>
        val allTheSame = impls.tail.forall(getMethodBody(_).body.get match {
          case ApplyStatically(`flags`, This(), `className`, MethodIdent(`methodName`), _) =>
            true
          case _ =>
            false
        })

        if (!allTheSame) {
          treeNotInlined
        } else {
          /* In this case, we can directly "splice in" the treceiver and targs
           * into a made-up ApplyStatically, as evaluation order is always
           * preserved. This potentially avoids useless bindings and keeps
           * things simple.
           *
           * We only need to cast the receiver after checking that it is not null,
           * since we need to pass it as the receiver of the `ApplyStatically`,
           * which expects a known type.
           */
          val treceiverCast = foldCast(checkNotNull(treceiver),
              ClassType(className, nullable = false))

          val target = staticCall(className,
              MemberNamespace.forNonStaticCall(flags), methodName)

          pretransformSingleDispatch(flags, target, Some(treceiverCast), targs,
              isStat, usePreTransform)(cont) {
            val newTree = ApplyStatically(flags,
                finishTransformExpr(treceiverCast),
                className, MethodIdent(methodName),
                targs.map(finishTransformExpr))(
                body.tpe)
            cont(newTree.toPreTransform)
          }
        }

      // Bridge method
      case body @ Apply(flags, This(), MethodIdent(methodName), referenceArgs) =>
        val allTheSame = impls.tail.forall(getMethodBody(_).body.get match {
          case Apply(`flags`, This(), MethodIdent(`methodName`), implArgs) =>
            referenceArgs.zip(implArgs) forall {
              case (MaybeUnbox(_, unboxID1), MaybeUnbox(_, unboxID2)) =>
                unboxID1 == unboxID2
            }
          case _ =>
            false
        })

        if (!allTheSame) {
          treeNotInlined
        } else {
          /* Interestingly, for this shape of multi-inlining, we do not need to
           * cast the receiver. We can keep it as is. Virtual dispatch and
           * reachability analysis will be happy to compute the possible targets
           * of the generated Apply given the type of our receiver.
           *
           * It's a good thing too, because it would be quite hard to figure out
           * what type to cast the receiver to!
           */

          if (!referenceArgs.exists(_.isInstanceOf[AsInstanceOf])) {
            // Common case where where we can splice in; evaluation order is preserved
            pretransformApply(flags, treceiver, MethodIdent(methodName), targs,
                body.tpe, isStat, usePreTransform)(cont)
          } else {
            /* If there is at least one unbox, we cannot splice in; we need to
             * use actual bindings and resolve the actual Apply tree to apply
             * the unboxes in the correct evaluation order.
             */

            /* Generate a new, fake body that we will inline. For type
             * preservation, the type of its `this` var ref is the type of our
             * receiver but non-nullable. For stability, the parameter names
             * are normalized (taking them from `body` would make the result
             * depend on which method came up first in the list of targets).
             */
            val thisType = treceiver.tpe.base.toNonNullable
            val normalizedParams: List[(LocalName, Type)] = {
              referenceMethodDef.args.zipWithIndex.map {
                case (referenceParam, i) => (LocalName("x" + i), referenceParam.ptpe)
              }
            }
            val normalizedBody = Apply(
              flags,
              This()(thisType),
              MethodIdent(methodName),
              normalizedParams.zip(referenceArgs).map {
                case ((name, ptpe), AsInstanceOf(_, castTpe)) =>
                  AsInstanceOf(VarRef(name)(ptpe), castTpe)
                case ((name, ptpe), _) =>
                  VarRef(name)(ptpe)
              }
            )(body.tpe)

            // Construct bindings; need to check null for the receiver to preserve evaluation order
            val receiverBinding =
              Binding.forReceiver(thisType, checkNotNull(treceiver))
            val argsBindings = normalizedParams.zip(targs).map {
              case ((name, ptpe), targ) =>
                Binding(name, NoOriginalName, ptpe, mutable = false, targ)
            }

            withBindings(receiverBinding :: argsBindings) { (bodyScope, cont1) =>
              implicit val scope = bodyScope
              if (usePreTransform) {
                assert(!isStat, "Cannot use pretransform in statement position")
                pretransformExpr(normalizedBody)(cont1)
              } else {
                cont1(PreTransTree(transform(normalizedBody, isStat)))
              }
            } (cont) (scope.withEnv(OptEnv.Empty))
          }
        }

      case body =>
        throw new AssertionError("Invalid forwarder shape: " + body)
    })
  }

  private def boxedClassForType(tpe: Type): ClassName = (tpe: @unchecked) match {
    case ClassType(className, _) =>
      className
    case AnyType | AnyNotNullType | _:ArrayType =>
      ObjectClass
    case tpe: PrimType =>
      PrimTypeToBoxedClass(tpe)
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
          methodIdent, transformedArgs)(tree.tpe)))

    if (methodName.isReflectiveProxy) {
      // Never inline reflective proxies
      treeNotInlined0(transformExpr(receiver), args.map(transformExpr))
    } else {
      val target = staticCall(className, MemberNamespace.forNonStaticCall(flags),
          methodName)
      pretransformExprs(receiver, args) { (treceiver, targs) =>
        pretransformSingleDispatch(flags, target, Some(treceiver), targs, isStat, usePreTransform)(cont) {
          treeNotInlined0(finishTransformExpr(treceiver),
              targs.map(finishTransformExpr))
        }
      }
    }
  }

  private def receiverTypeFor(target: MethodID): Type = {
    BoxedClassToPrimType.getOrElse(target.enclosingClassName,
        ClassType(target.enclosingClassName, nullable = false))
  }

  private def pretransformApplyStatic(tree: ApplyStatic, isStat: Boolean,
      usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    val ApplyStatic(flags, className, methodIdent, args) = tree
    implicit val pos = tree.pos

    pretransformExprs(args) { targs =>
      pretransformApplyStatic(flags, className, methodIdent, targs, tree.tpe,
          isStat, usePreTransform)(
          cont)
    }
  }

  private def pretransformApplyStatic(flags: ApplyFlags, className: ClassName,
      methodIdent: MethodIdent, targs: List[PreTransform], resultType: Type,
      isStat: Boolean, usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    val target = staticCall(className, MemberNamespace.forStaticCall(flags),
        methodIdent.name)
    pretransformSingleDispatch(flags, target, None, targs, isStat, usePreTransform)(cont) {
      val newArgs = targs.map(finishTransformExpr)
      cont(PreTransTree(
          ApplyStatic(flags, className, methodIdent, newArgs)(resultType)))
    }
  }

  private def pretransformApplyDynamicImport(tree: ApplyDynamicImport, isStat: Boolean)(
      cont: PreTransCont)(
      implicit outerScope: Scope): TailRec[Tree] = {

    val ApplyDynamicImport(flags, className, method, args) = tree
    implicit val pos = tree.pos

    def treeNotInlined0(transformedArgs: List[Tree]) =
      cont(PreTransTree(ApplyDynamicImport(flags, className, method, transformedArgs)))

    def treeNotInlined = treeNotInlined0(args.map(transformExpr))

    val targetMethod =
      staticCall(className, MemberNamespace.forStaticCall(flags), method.name)

    if (!targetMethod.attributes.inlineable || tree.flags.noinline) {
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

                val closure = Closure(ClosureFlags.arrow, newCaptureParams,
                    List(moduleParam), restParam = None, resultType = AnyType,
                    newBody, newCaptureValues)

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
        cont(PreTransTree(JSSelect(finishTransformExpr(tqual),
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

        used.value = used.value.inc
        val module = VarRef(moduleVarName)(AnyType)
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
                  flags, captureParams, params, resultType, body,
                  captureLocalDefs, alreadyUsed, cancelFun)))
              if !flags.typed && !alreadyUsed.value.isUsed && argsNoSpread.size <= params.size =>
            alreadyUsed.value = alreadyUsed.value.inc
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

  private def pretransformApplyTypedClosure(tree: ApplyTypedClosure,
      isStat: Boolean, usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    val ApplyTypedClosure(flags, fun, args) = tree
    implicit val pos = tree.pos

    pretransformExpr(fun) { tfun =>
      tfun match {
        case PreTransLocalDef(LocalDef(_, false,
            closure @ TentativeClosureReplacement(
                flags, captureParams, params, resultType, body,
                captureLocalDefs, alreadyUsed, cancelFun)))
            if flags.typed && !alreadyUsed.value.isUsed =>
          alreadyUsed.value = alreadyUsed.value.inc
          pretransformExprs(args) { targs =>
            inlineBody(
                optReceiver = None,
                captureParams ++ params, resultType, body,
                captureLocalDefs.map(_.toPreTransform) ++ targs, isStat,
                usePreTransform)(cont)
          }

        case _ =>
          cont(ApplyTypedClosure(flags, finishTransformExpr(tfun),
              args.map(transformExpr)).toPreTransform)
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
      case ClassType(className, _) =>
        ClassNamesThatShouldBeInlined.contains(className)
      case _ =>
        false
    }

    def isLikelyOptimizable(arg: PreTransform): Boolean = arg match {
      case PreTransBlock(_, result) =>
        isLikelyOptimizable(result)

      case PreTransLocalDef(localDef) =>
        localDef.replacement match {
          case _: TentativeClosureReplacement            => true
          case _: ReplaceWithRecordVarRef                => true
          case _: InlineClassBeingConstructedReplacement => true
          case _: InlineClassInstanceReplacement         => true
          case _ =>
            isTypeLikelyOptimizable(localDef.tpe)
        }

      case PreTransRecordTree(_, _, _) =>
        true

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
        optReceiver.fold[Tree](Skip())(r => checkNotNullStatement(r._2))
      val newArgs = args.map(finishTransformStat(_))
      Block(newOptReceiver :: newArgs)
    }

    body match {
      case Skip() =>
        assert(isStat, "Found Skip() in expression position")
        cont(PreTransTree(finishTransformArgsAsStat()))

      case _: Literal =>
        cont(PreTransTree(Block(finishTransformArgsAsStat(), body)))

      case This() if args.isEmpty =>
        assert(optReceiver.isDefined,
            "There was a `this`, there should be a receiver")
        cont(foldCast(checkNotNull(optReceiver.get._2), optReceiver.get._1))

      case Select(This(), field) if formals.isEmpty =>
        assert(optReceiver.isDefined,
            "There was a `this`, there should be a receiver")
        pretransformSelectCommon(body.tpe, optReceiver.get._2,
            optQualDeclaredType = Some(optReceiver.get._1),
            field, isLhsOfAssign = false)(cont)

      case Assign(lhs @ Select(This(), field), VarRef(rhsName))
          if formals.size == 1 && formals.head.name.name == rhsName =>
        assert(isStat, "Found Assign in expression position")
        assert(optReceiver.isDefined,
            "There was a `this`, there should be a receiver")

        val treceiver = optReceiver.get._2
        val trhs = args.head

        if (!isFieldRead(field.name)) {
          // Field is never read, discard assign, keep side effects only.
          cont(PreTransTree(finishTransformArgsAsStat()))
        } else {
          pretransformSelectCommon(lhs.tpe, treceiver,
              optQualDeclaredType = Some(optReceiver.get._1),
              field, isLhsOfAssign = true) { tlhs =>
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
      /* Introduce an explicit null check that would be part of the semantics
       * of `Apply` or `ApplyStatically`.
       * Then, cast the non-null receiver to the expected receiver type. This
       * may be required because we found a single potential call target in a
       * subclass of the static type of the receiver.
       */
      val (declaredType, value0) = receiver
      val value = foldCast(checkNotNull(value0), declaredType)
      Binding.forReceiver(declaredType, value)
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
      val shouldInline = {
        target.attributes.inlineable &&
        !flags.noinline && {
          target.attributes.shouldInline ||
          flags.inline ||
          shouldInlineBecauseOfArgs(target, tall)
        }
      }

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

    @inline def StringClassType = ClassType(BoxedStringClass, nullable = true)

    def longToInt(longExpr: Tree): Tree =
      UnaryOp(UnaryOp.LongToInt, longExpr)
    def wasmUnaryOp(op: WasmUnaryOp.Code, lhs: PreTransform): Tree =
      Transient(WasmUnaryOp(op, finishTransformExpr(lhs)))
    def wasmBinaryOp(op: WasmBinaryOp.Code, lhs: PreTransform, rhs: PreTransform): Tree =
      Transient(WasmBinaryOp(op, finishTransformExpr(lhs), finishTransformExpr(rhs)))

    (intrinsicCode: @switch) match {
      // Not an intrisic

      case -1 =>
        default

      // java.lang.System

      case ArrayCopy =>
        assert(isStat, "System.arraycopy must be used in statement position")
        val List(tsrc, tsrcPos, tdest, tdestPos, tlength) = targs
        withNewTempLocalDefs(targs) { (localDefs, cont1) =>
          val List(srcDef, srcPosDef, destDef, destPosDef, lengthDef) = localDefs
          cont1(PreTransTree(Transient(SystemArrayCopy(
            finishTransformExpr(checkNotNull(srcDef.toPreTransform)),
            srcPosDef.newReplacement,
            finishTransformExpr(checkNotNull(destDef.toPreTransform)),
            destPosDef.newReplacement,
            lengthDef.newReplacement
          ))))
        } (cont)

      // scala.runtime.ScalaRunTime object

      case ArrayApply =>
        val List(tarray, tindex) = targs
        tarray.tpe.base match {
          case arrayTpe @ ArrayType(ArrayTypeRef(base, _), _) =>
            /* Rewrite to `tarray[tindex]` as an `ArraySelect` node.
             * If `tarray` is `null`, an `ArraySelect`'s semantics will run
             * into a (UB) NPE *before* evaluating `tindex`, by spec. This is
             * not compatible with the method-call semantics of an intrinsic,
             * in which all arguments are evaluated before the body starts
             * executing (and can notice that the array is `null`).
             * Therefore, in the general case, we first evaluate `tarray` and
             * `tindex` in temp LocalDefs. When `tarray` is not nullable, we
             * can directly emit an `ArraySelect`: in the absence of that NPE
             * code path, the semantics of `ArraySelect` are equivalent to the
             * intrinsic.
             */
            val elemType = arrayElemType(arrayTpe)
            if (!tarray.tpe.isNullable) {
              val array = finishTransformExpr(tarray)
              val index = finishTransformExpr(tindex)
              val select = ArraySelect(array, index)(elemType)
              contTree(select)
            } else {
              withNewTempLocalDefs(targs) { (localDefs, cont1) =>
                val List(arrayDef, indexDef) = localDefs
                val select = ArraySelect(arrayDef.newReplacement, indexDef.newReplacement)(elemType)
                cont1(select.toPreTransform)
              } (cont)
            }
          case _ =>
            default
        }

      case ArrayUpdate =>
        val List(tarray, tindex, tvalue) = targs
        tarray.tpe.base match {
          case arrayTpe @ ArrayType(ArrayTypeRef(base, depth), _) =>
            /* Rewrite to `tarray[index] = tvalue` as an `Assign(ArraySelect, _)`.
             * See `ArrayApply` above for the handling of a nullable `tarray`.
             */
            val elemType = arrayElemType(arrayTpe)
            if (!tarray.tpe.isNullable) {
              val array = finishTransformExpr(tarray)
              val index = finishTransformExpr(tindex)
              val select = ArraySelect(array, index)(elemType)
              val tunboxedValue = foldAsInstanceOf(tvalue, elemType)
              val assign = Assign(select, finishTransformExpr(tunboxedValue))
              contTree(assign)
            } else {
              withNewTempLocalDefs(targs) { (localDefs, cont1) =>
                val List(arrayDef, indexDef, valueDef) = localDefs
                val select = ArraySelect(arrayDef.newReplacement, indexDef.newReplacement)(elemType)
                val tunboxedValue = foldAsInstanceOf(valueDef.toPreTransform, elemType)
                val assign = Assign(select, finishTransformExpr(tunboxedValue))
                cont1(assign.toPreTransform)
              } (cont)
            }
          case _ =>
            default
        }

      case ArrayLength =>
        val tarray = targs.head
        tarray.tpe.base match {
          case _: ArrayType =>
            cont(foldUnaryOp(UnaryOp.Array_length, checkNotNull(tarray)))
          case _ =>
            default
        }

      case ArrayToJSArray =>
        val tarray = targs.head
        tarray match {
          case PreTransLocalDef(LocalDef(_, /* mutable = */ false, replacement: InlineArrayReplacement)) =>
            tryOrRollback { cancelFun =>
              val jsArrayReplacement = InlineJSArrayReplacement(replacement.elemLocalDefs, cancelFun)
              val localDef = LocalDef(RefinedType(AnyNotNullType), mutable = false, jsArrayReplacement)
              cont(localDef.toPreTransform)
            } { () =>
              cont(JSArrayConstr(replacement.elemLocalDefs.map(_.newReplacement).toList).toPreTransform)
            }
          case _ =>
            default
        }

      // java.lang.Integer

      case IntegerNTZ =>
        val tvalue = targs.head
        tvalue match {
          case PreTransLit(IntLiteral(value)) =>
            contTree(IntLiteral(Integer.numberOfTrailingZeros(value)))
          case _ =>
            contTree(wasmUnaryOp(WasmUnaryOp.I32Ctz, tvalue))
        }
      case IntegerBitCount =>
        val tvalue = targs.head
        tvalue match {
          case PreTransLit(IntLiteral(value)) =>
            contTree(IntLiteral(Integer.bitCount(value)))
          case _ =>
            contTree(wasmUnaryOp(WasmUnaryOp.I32Popcnt, tvalue))
        }

      case IntegerRotateLeft =>
        val List(tvalue, tdistance) = targs
        (tvalue, tdistance) match {
          case (PreTransLit(IntLiteral(value)), PreTransLit(IntLiteral(distance))) =>
            contTree(IntLiteral(Integer.rotateLeft(value, distance)))
          case _ =>
            contTree(wasmBinaryOp(WasmBinaryOp.I32Rotl, tvalue, tdistance))
        }
      case IntegerRotateRight =>
        val List(tvalue, tdistance) = targs
        (tvalue, tdistance) match {
          case (PreTransLit(IntLiteral(value)), PreTransLit(IntLiteral(distance))) =>
            contTree(IntLiteral(Integer.rotateRight(value, distance)))
          case _ =>
            contTree(wasmBinaryOp(WasmBinaryOp.I32Rotr, tvalue, tdistance))
        }

      // java.lang.Long

      case LongNTZ =>
        val tvalue = targs.head
        tvalue match {
          case PreTransLit(LongLiteral(value)) =>
            contTree(IntLiteral(java.lang.Long.numberOfTrailingZeros(value)))
          case _ =>
            contTree(longToInt(wasmUnaryOp(WasmUnaryOp.I64Ctz, tvalue)))
        }
      case LongBitCount =>
        val tvalue = targs.head
        tvalue match {
          case PreTransLit(LongLiteral(value)) =>
            contTree(IntLiteral(java.lang.Long.bitCount(value)))
          case _ =>
            contTree(longToInt(wasmUnaryOp(WasmUnaryOp.I64Popcnt, tvalue)))
        }

      case LongRotateLeft =>
        val List(tvalue, tdistance) = targs
        (tvalue, tdistance) match {
          case (PreTransLit(LongLiteral(value)), PreTransLit(IntLiteral(distance))) =>
            contTree(LongLiteral(java.lang.Long.rotateLeft(value, distance)))
          case _ =>
            contTree(wasmBinaryOp(WasmBinaryOp.I64Rotl, tvalue,
                PreTransUnaryOp(UnaryOp.IntToLong, tdistance)))
        }
      case LongRotateRight =>
        val List(tvalue, tdistance) = targs
        (tvalue, tdistance) match {
          case (PreTransLit(LongLiteral(value)), PreTransLit(IntLiteral(distance))) =>
            contTree(LongLiteral(java.lang.Long.rotateRight(value, distance)))
          case _ =>
            contTree(wasmBinaryOp(WasmBinaryOp.I64Rotr, tvalue,
                PreTransUnaryOp(UnaryOp.IntToLong, tdistance)))
        }

      case LongToString =>
        val List(targ) = targs
        withSplitLong(targ) { (targLo, targHi, cont1) =>
          pretransformApplyStatic(ApplyFlags.empty, LongImpl.RuntimeLongClass,
              MethodIdent(LongImpl.toString_), List(targLo, targHi), StringClassType,
              isStat, usePreTransform)(
              cont1)
        } (cont)
      case LongCompare =>
        val List(tlhs, trhs) = targs
        withSplitLong(tlhs) { (tlhsLo, tlhsHi, cont1) =>
          withSplitLong(trhs) { (trhsLo, trhsHi, cont2) =>
            pretransformApplyStatic(ApplyFlags.empty, LongImpl.RuntimeLongClass,
                MethodIdent(LongImpl.compare), List(tlhsLo, tlhsHi, trhsLo, trhsHi), IntType,
                isStat, usePreTransform)(
                cont2)
          } (cont1)
        } (cont)

      // java.lang.Character

      case CharacterCodePointToString =>
        withNewLocalDef(Binding.temp(LocalName("codePoint"), targs.head)) { (cpLocalDef, cont1) =>
          cont1(PreTransTree(Block(
            If(
              Transient(WasmBinaryOp(WasmBinaryOp.I32GtU,
                  cpLocalDef.newReplacement, IntLiteral(Character.MAX_CODE_POINT))),
              UnaryOp(UnaryOp.Throw,
                  New(IllegalArgumentExceptionClass, MethodIdent(NoArgConstructorName), Nil)),
              Skip()
            )(VoidType),
            Transient(WasmStringFromCodePoint(cpLocalDef.newReplacement))
          )))
        } (cont)

      // java.lang.String

      case StringCodePointAt =>
        contTree(Transient(WasmCodePointAt(
          finishTransformExpr(optTReceiver.get),
          finishTransformExpr(targs.head)
        )))
      case StringSubstringStart =>
        contTree(Transient(WasmSubstring(
          finishTransformExpr(optTReceiver.get),
          finishTransformExpr(targs.head),
          None
        )))
      case StringSubstringStartEnd =>
        contTree(Transient(WasmSubstring(
          finishTransformExpr(optTReceiver.get),
          finishTransformExpr(targs(0)),
          Some(finishTransformExpr(targs(1)))
        )))

      // java.lang.Math

      case MathAbsLong =>
        val List(targ) = targs
        withSplitLong(targ) { (targLo, targHi, cont1) =>
          pretransformApplyStatic(ApplyFlags.empty, LongImpl.RuntimeLongClass,
              MethodIdent(LongImpl.abs), List(targLo, targHi), LongType,
              isStat, usePreTransform)(
              cont1)
        } (cont)

      case MathAbsFloat =>
        contTree(wasmUnaryOp(WasmUnaryOp.F32Abs, targs.head))
      case MathAbsDouble =>
        contTree(wasmUnaryOp(WasmUnaryOp.F64Abs, targs.head))
      case MathCeil =>
        contTree(wasmUnaryOp(WasmUnaryOp.F64Ceil, targs.head))
      case MathFloor =>
        contTree(wasmUnaryOp(WasmUnaryOp.F64Floor, targs.head))
      case MathRint =>
        contTree(wasmUnaryOp(WasmUnaryOp.F64Nearest, targs.head))
      case MathSqrt =>
        contTree(wasmUnaryOp(WasmUnaryOp.F64Sqrt, targs.head))

      case MathMinFloat =>
        contTree(wasmBinaryOp(WasmBinaryOp.F32Min, targs.head, targs.tail.head))
      case MathMinDouble =>
        contTree(wasmBinaryOp(WasmBinaryOp.F64Min, targs.head, targs.tail.head))
      case MathMaxFloat =>
        contTree(wasmBinaryOp(WasmBinaryOp.F32Max, targs.head, targs.tail.head))
      case MathMaxDouble =>
        contTree(wasmBinaryOp(WasmBinaryOp.F64Max, targs.head, targs.tail.head))

      case MathMultiplyFull =>
        def expand(targs: List[PreTransform]): TailRec[Tree] = {
          pretransformApplyStatic(ApplyFlags.empty,
              LongImpl.RuntimeLongClass,
              MethodIdent(LongImpl.multiplyFull),
              targs,
              LongType,
              isStat, usePreTransform)(
              cont)
        }

        targs match {
          case List(PreTransLit(IntLiteral(x)), PreTransLit(IntLiteral(y))) =>
            // cannot actually call multiplyHigh to constant-fold because it is JDK9+
            contTree(LongLiteral(x.toLong * y.toLong))
          case List(tlhs, trhs @ PreTransLit(_)) =>
            // normalize a single constant on the left; the implementation is optimized for that case
            expand(trhs :: tlhs :: Nil)
          case _ =>
            expand(targs)
        }

      // scala.collection.mutable.ArrayBuilder

      case GenericArrayBuilderResult =>
        // This is a private API: `runtimeClass` is known not to be `null`
        val List(runtimeClass, array) = targs.map(finishTransformExpr(_))
        val (resultType, isExact) = runtimeClass match {
          case ClassOf(elemTypeRef) =>
            (ArrayType(ArrayTypeRef.of(elemTypeRef), nullable = false), true)
          case _ =>
            (AnyNotNullType, false)
        }
        cont(PreTransTree(
            Transient(NativeArrayWrapper(runtimeClass, array)(resultType)),
            RefinedType(resultType, isExact = isExact)))

      case ArrayBuilderZeroOf =>
        // This is a private API: `runtimeClass` is known not to be `null`
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
              case VoidType => Undefined()
              case _        => zeroOf(tpe)
            }
          case ClassOf(_) =>
            Null()
          case runtimeClass =>
            Transient(ZeroOf(runtimeClass))
        })

      // java.lang.Class

      case ClassGetName =>
        optTReceiver.get match {
          case PreTransMaybeBlock(bindingsAndStats,
              PreTransTree(MaybeCast(UnaryOp(UnaryOp.GetClass, expr)), _)) =>
            contTree(finishTransformBindings(
                bindingsAndStats, Transient(ObjectClassName(expr))))

          // Same thing, but the argument stayed as a PreTransUnaryOp
          case PreTransMaybeBlock(bindingsAndStats,
              PreTransUnaryOp(UnaryOp.GetClass, texpr)) =>
            contTree(finishTransformBindings(
                bindingsAndStats, Transient(ObjectClassName(finishTransformExpr(texpr)))))

          case _ =>
            default
        }

      // js.special

      case ObjectLiteral =>
        val List(tprops) = targs
        tprops match {
          case PreTransMaybeBlock(bindingsAndStats,
              PreTransLocalDef(LocalDef(
                  RefinedType(ClassType(JSWrappedArrayClass | WrappedVarArgsClass, _), _),
                  false,
                  InlineClassInstanceReplacement(_, wrappedArrayFields, _)))) =>
            assert(wrappedArrayFields.size == 1)
            val jsArray = wrappedArrayFields.head._2
            jsArray.replacement match {
              case InlineJSArrayReplacement(elemLocalDefs, _)
                  if elemLocalDefs.forall(e => isSubtype(e.tpe.base, ClassType(Tuple2Class, nullable = true))) =>
                val fields: List[(Tree, Tree)] = for {
                  (elemLocalDef, idx) <- elemLocalDefs.toList.zipWithIndex
                } yield {
                  elemLocalDef match {
                    case LocalDef(RefinedType(ClassType(Tuple2Class, _), _), false,
                        InlineClassInstanceReplacement(structure, tupleFields, _)) =>
                      val List(key, value) = structure.fieldNames.map(tupleFields)
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
              case RefinedType(ClassType(NilClass, false), _) =>
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
      Binding(name.toLocalName, originalName, tpe, mutable, PreTransTree(zeroOf(tpe)))
    }

    withNewLocalDefs(initialFieldBindings) { (initialFieldLocalDefList, cont1) =>
      val initialFieldLocalDefs =
        structure.fieldNames.zip(initialFieldLocalDefList).toMap

      inlineClassConstructorBody(allocationSite, structure, initialFieldLocalDefs,
          className, className, ctor, args, cancelFun) { (finalFieldLocalDefs, cont2) =>
        cont2(LocalDef(
            RefinedType(ClassType(className, nullable = false), isExact = true,
                allocationSite = allocationSite),
            mutable = false,
            InlineClassInstanceReplacement(structure, finalFieldLocalDefs,
                cancelFun)).toPreTransform)
      } (cont1)
    } (cont)
  }

  private def inlineClassConstructorBody(
      allocationSite: AllocationSite, structure: InlineableClassStructure,
      inputFieldsLocalDefs: Map[FieldName, LocalDef], className: ClassName,
      ctorClass: ClassName, ctor: MethodIdent, args: List[PreTransform],
      cancelFun: CancelFun)(
      buildInner: (Map[FieldName, LocalDef], PreTransCont) => TailRec[Tree])(
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
          RefinedType(ClassType(className, nullable = false), isExact = true),
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
      thisLocalDef: LocalDef, inputFieldsLocalDefs: Map[FieldName, LocalDef],
      className: ClassName, stats: List[Tree], cancelFun: CancelFun)(
      buildInner: (Map[FieldName, LocalDef], PreTransCont) => TailRec[Tree])(
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
            cont(PreTransTree(transformedStat))
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
      case VarRef(_) :: rest =>
        // mostly for `this`
        inlineClassConstructorBodyList(allocationSite, structure, thisLocalDef,
            inputFieldsLocalDefs, className, rest, cancelFun)(buildInner)(cont)

      case Assign(s @ Select(This(), field), value) :: rest
          if !inputFieldsLocalDefs.contains(field.name) =>
        // Field is being optimized away. Only keep side effects of the write.
        withStat(value, rest)

      case Assign(s @ Select(This(), field), value) :: rest
          if !inputFieldsLocalDefs(field.name).mutable =>
        pretransformExpr(value) { tvalue =>
          val originalName = structure.fieldOriginalName(field.name)
          val binding = Binding(field.name.simpleName.toLocalName,
              originalName, s.tpe, mutable = false, tvalue)
          withNewLocalDef(binding) { (localDef, cont1) =>
            if (localDef.contains(thisLocalDef)) {
              /* Uh oh, there is a `val x = ...this...`. We can't keep it,
               * because this field will not be updated with `newThisLocalDef`.
               */
              cancelFun()
            }
            val newFieldsLocalDefs =
              inputFieldsLocalDefs.updated(field.name, localDef)
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
       * Typical shape of initialization of outer pointer of inner classes
       * coming from Scala.js < 1.15.1 (since 1.15.1, we intercept that shape
       * already in the compiler back-end).
       */
      case If(cond, th @ UnaryOp(UnaryOp.Throw, _), Assign(Select(This(), _), value)) :: rest =>
        // work around a bug of the compiler (these should be @-bindings)
        val stat = stats.head.asInstanceOf[If]
        val ass = stat.elsep.asInstanceOf[Assign]
        val lhs = ass.lhs
        inlineClassConstructorBodyList(allocationSite, structure, thisLocalDef,
            inputFieldsLocalDefs, className,
            Assign(lhs, If(cond, th, value)(lhs.tpe)(stat.pos))(ass.pos) :: rest,
            cancelFun)(buildInner)(cont)

      case ApplyStatically(flags, This(), superClass, superCtor, args) :: rest
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

  private def foldIfStat(tcond: PreTransform, thenp: Tree, elsep: Tree)(
      implicit pos: Position): Tree = {

    (thenp, elsep) match {
      case (Skip(), Skip()) =>
        finishTransformStat(tcond)
      case (Skip(), _) =>
        val newCond = finishTransformExpr(foldNot(tcond))
        If(newCond, elsep, thenp)(VoidType)
      case _ =>
        val newCond = finishTransformExpr(tcond)
        If(newCond, thenp, elsep)(VoidType)
    }
  }

  private def foldIf(cond: PreTransform, thenp: PreTransform, elsep: PreTransform)(
      tpe: RefinedType)(
      implicit pos: Position): PreTransform = {
    import BinaryOp._

    @inline def default = PreTransIf(cond, thenp, elsep, tpe)

    cond match {
      case PreTransLit(BooleanLiteral(v)) =>
        if (v) thenp
        else elsep

      case _ =>
        @inline def negCond = foldNot(cond)

        if (thenp.tpe.base == BooleanType && elsep.tpe.base == BooleanType) {
          (cond, thenp, elsep) match {
            case (_, PreTransLit(BooleanLiteral(t)), PreTransLit(BooleanLiteral(e))) =>
              if (t == e) PreTransBlock(finishTransformStat(cond), thenp)
              else if (t) cond
              else negCond

            case (_, PreTransLit(BooleanLiteral(false)), _) =>
              foldIf(negCond, elsep, thenp)(tpe) // canonical && form
            case (_, _, PreTransLit(BooleanLiteral(true))) =>
              foldIf(negCond, elsep, thenp)(tpe) // canonical || form

            /* if (lhs === null) rhs === null else lhs.as![T!] === rhs
             * -> lhs === rhs
             * This is the typical shape of a lhs == rhs test where
             * the equals() method has been inlined as a reference
             * equality test.
             */
            case (PreTransBinaryOp(BinaryOp.===, lhs: PreTransLocalDef, PreTransLit(Null())),
                PreTransBinaryOp(BinaryOp.===, rhs: PreTransLocalDef, PreTransLit(Null())),
                PreTransBinaryOp(BinaryOp.===, lhs2: PreTransLocalDef, rhs2: PreTransLocalDef))
                if lhs2 == lhs && rhs2 == rhs =>
              PreTransBinaryOp(BinaryOp.===, lhs, rhs)(elsep.pos)

            // Example: (x > y) || (x == y)  ->  (x >= y)
            case (PreTransBinaryOp(IntComparison(op1), l1, r1),
                  PreTransLit(BooleanLiteral(true)),
                  PreTransBinaryOp(IntComparison(op2), l2, r2))
                if isCombinable(l1) && isCombinable(r1) &&
                    isCombinable(l2) && isCombinable(r2) &&
                    op1.isCompatibleWith(op2) =>
              foldTwoIntComparisonsOr(l1, op1, r1, l2, op2, r2)

            // Example: (x >= y) && (x <= z)  ->  ((x-y) unsigned_<= (z-y))
            case (PreTransBinaryOp(IntComparison(op1), l1, r1),
                  PreTransBinaryOp(IntComparison(op2), l2, r2),
                  PreTransLit(BooleanLiteral(false)))
                if isCombinable(l1) && isCombinable(r1) &&
                    isCombinable(l2) && isCombinable(r2) &&
                    op1.isCompatibleWith(op2) =>
              foldTwoIntComparisonsAnd(l1, op1, r1, l2, op2, r2)

            case _ => default
          }
        } else {
          default
        }
    }
  }

  private def pretransformUnaryOp(tree: UnaryOp)(cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    implicit val pos = tree.pos
    val UnaryOp(op, arg) = tree

    pretransformExpr(arg) { tlhs =>
      def folded: PreTransform =
        foldUnaryOp(op, tlhs)

      op match {
        case UnaryOp.WrapAsThrowable =>
          typeTestResult(tlhs.tpe, ThrowableClassType.toNonNullable) match {
            case TypeTestResult.Subtype =>
              cont(tlhs)
            case TypeTestResult.NotAnInstance =>
              pretransformNew(AllocationSite.Tree(tree), JavaScriptExceptionClass,
                  MethodIdent(AnyArgConstructorName), tlhs :: Nil)(cont)
            case TypeTestResult.Unknown | TypeTestResult.SubtypeOrNull =>
              cont(folded)
            case TypeTestResult.NotAnInstanceUnlessNull =>
              throw new AssertionError(s"Unreachable; tlhs.tpe was ${tlhs.tpe} at $pos")
          }

        case UnaryOp.UnwrapFromThrowable =>
          typeTestResult(tlhs.tpe, JavaScriptExceptionClassType.toNonNullable, testTypeKnownToBeFinal = true) match {
            case TypeTestResult.Subtype | TypeTestResult.SubtypeOrNull =>
              pretransformSelectCommon(AnyType, tlhs, optQualDeclaredType = None,
                  FieldIdent(exceptionFieldName), isLhsOfAssign = false)(cont)
            case TypeTestResult.NotAnInstance =>
              cont(checkNotNull(tlhs))
            case TypeTestResult.Unknown =>
              cont(folded)
            case TypeTestResult.NotAnInstanceUnlessNull =>
              throw new AssertionError(s"Unreachable; tlhs.tpe was ${tlhs.tpe} at $pos")
          }

        case _ =>
          expandOps(folded)(cont)
      }
    }
  }

  private def pretransformBinaryOp(tree: BinaryOp)(cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    implicit val pos = tree.pos
    val BinaryOp(op, lhs, rhs) = tree

    pretransformExprs(lhs, rhs) { (tlhs, trhs) =>
      expandOps(foldBinaryOp(op, tlhs, trhs))(cont)
    }
  }

  /** Expands some unary and binary ops into lowered or optimized subexpressions.
   *
   *  - divisions and remainders by constants;
   *  - RuntimeLong-based operations.
   */
  private def expandOps(pretrans: PreTransform)(cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    implicit val pos = pretrans.pos

    def expandLongOp(methodName: MethodName, targs: PreTransform*)(cont: PreTransCont): TailRec[Tree] = {
      val impl = staticCall(LongImpl.RuntimeLongClass, MemberNamespace.PublicStatic, methodName)
      pretransformSingleDispatch(ApplyFlags.empty, impl, None, targs.toList,
          isStat = false, usePreTransform = true)(cont)(
          throw new AssertionError(s"failed to inline RuntimeLong method $methodName at $pos"))
    }

    def expandLongOpNoSplit(methodName: MethodName, targ: PreTransform): TailRec[Tree] = {
      expandLongOp(methodName, targ)(cont)
    }

    def expandLongOpSplit1(methodName: MethodName, targ: PreTransform): TailRec[Tree] = {
      withSplitLong(targ) { (tlo, thi, cont1) =>
        expandLongOp(methodName, tlo, thi)(cont1)
      } (cont)
    }

    def expandLongOpSplit2(methodName: MethodName, tlhs: PreTransform, trhs: PreTransform): TailRec[Tree] = {
      withSplitLong(tlhs) { (tlhsLo, tlhsHi, cont1) =>
        withSplitLong(trhs) { (trhsLo, trhsHi, cont2) =>
          expandLongOp(methodName, tlhsLo, tlhsHi, trhsLo, trhsHi)(cont2)
        } (cont1)
      } (cont)
    }

    def expandLongOpSplitLeft(methodName: MethodName, tlhs: PreTransform, trhs: PreTransform): TailRec[Tree] = {
      withSplitLong(tlhs) { (tlhsLo, tlhsHi, cont1) =>
        expandLongOp(methodName, tlhsLo, tlhsHi, trhs)(cont1)
      } (cont)
    }

    def isIntDivOp(op: BinaryOp.Code): Boolean = (op: @switch) match {
      case BinaryOp.Int_/ | BinaryOp.Int_% | BinaryOp.Int_unsigned_/ | BinaryOp.Int_unsigned_% =>
        true
      case _ =>
        false
    }

    def isLongDivOp(op: BinaryOp.Code): Boolean = (op: @switch) match {
      case BinaryOp.Long_/ | BinaryOp.Long_% | BinaryOp.Long_unsigned_/ | BinaryOp.Long_unsigned_% =>
        true
      case _ =>
        false
    }

    def expandOptimizedDivision(arg: PreTransform, body: Tree): TailRec[Tree] = {
      val argBinding = Binding(LocalIdent(IntegerDivisions.NumeratorArgName),
          NoOriginalName, arg.tpe.base, mutable = false, arg)

      withBinding(argBinding) { (bodyScope, cont1) =>
        implicit val scope = bodyScope
        pretransformExpr(body)(cont1)
      } (cont) (scope.withEnv(OptEnv.Empty))
    }

    pretrans match {
      case PreTransUnaryOp(op, arg) if useRuntimeLong =>
        import UnaryOp._

        (op: @switch) match {
          case IntToLong =>
            expandLongOpNoSplit(LongImpl.fromInt, arg)

          case LongToInt =>
            expandLongOpSplit1(LongImpl.toInt, arg)

          case LongToDouble =>
            expandLongOpSplit1(LongImpl.toDouble, arg)

          case DoubleToLong =>
            expandLongOpNoSplit(LongImpl.fromDouble, arg)

          case LongToFloat =>
            expandLongOpSplit1(LongImpl.toFloat, arg)

          case Double_toBits if config.coreSpec.esFeatures.esVersion >= ESVersion.ES2015 =>
            expandLongOp(LongImpl.fromDoubleBits,
                arg, PreTransTree(Transient(GetFPBitsDataView)))(cont)

          case Double_fromBits if config.coreSpec.esFeatures.esVersion >= ESVersion.ES2015 =>
            expandLongOpSplitLeft(LongImpl.bitsToDouble,
                arg, PreTransTree(Transient(GetFPBitsDataView)))

          case Long_clz =>
            expandLongOpSplit1(LongImpl.clz, arg)

          case UnsignedIntToLong =>
            expandLongOpNoSplit(LongImpl.fromUnsignedInt, arg)

          case _ =>
            cont(pretrans)
        }

      case PreTransBinaryOp(op, lhs, PreTransLit(IntLiteral(r)))
          if isIntDivOp(op) && integerDivisions.shouldRewriteDivision(op, r, isWasm) =>
        val optimizedBody = integerDivisions.makeOptimizedDivision(op, r)
        expandOptimizedDivision(lhs, optimizedBody)

      case PreTransBinaryOp(op, lhs, PreTransLit(LongLiteral(r)))
          if isLongDivOp(op) && integerDivisions.shouldRewriteDivision(op, r, isWasm) =>
        val optimizedBody = integerDivisions.makeOptimizedDivision(op, r)
        expandOptimizedDivision(lhs, optimizedBody)

      case PreTransBinaryOp(op, lhs, rhs) if useRuntimeLong =>
        import BinaryOp._

        (op: @switch) match {
          case Long_+ => expandLongOpSplit2(LongImpl.add, lhs, rhs)
          case Long_- => expandLongOpSplit2(LongImpl.sub, lhs, rhs)
          case Long_* => expandLongOpSplit2(LongImpl.mul, lhs, rhs)
          case Long_/ => expandLongOpSplit2(LongImpl.divide, lhs, rhs)
          case Long_% => expandLongOpSplit2(LongImpl.remainder, lhs, rhs)

          case Long_& => expandLongOpSplit2(LongImpl.and, lhs, rhs)
          case Long_| => expandLongOpSplit2(LongImpl.or, lhs, rhs)
          case Long_^ => expandLongOpSplit2(LongImpl.xor, lhs, rhs)

          case Long_<<  => expandLongOpSplitLeft(LongImpl.shl, lhs, rhs)
          case Long_>>> => expandLongOpSplitLeft(LongImpl.shr, lhs, rhs)
          case Long_>>  => expandLongOpSplitLeft(LongImpl.sar, lhs, rhs)

          case Long_== => expandLongOpSplit2(LongImpl.equals_, lhs, rhs)
          case Long_!= => expandLongOpSplit2(LongImpl.notEquals, lhs, rhs)
          case Long_<  => expandLongOpSplit2(LongImpl.lt, lhs, rhs)
          case Long_<= => expandLongOpSplit2(LongImpl.le, lhs, rhs)
          case Long_>  => expandLongOpSplit2(LongImpl.gt, lhs, rhs)
          case Long_>= => expandLongOpSplit2(LongImpl.ge, lhs, rhs)

          case Long_unsigned_/ => expandLongOpSplit2(LongImpl.divideUnsigned, lhs, rhs)
          case Long_unsigned_% => expandLongOpSplit2(LongImpl.remainderUnsigned, lhs, rhs)

          case Long_unsigned_<  => expandLongOpSplit2(LongImpl.ltu, lhs, rhs)
          case Long_unsigned_<= => expandLongOpSplit2(LongImpl.leu, lhs, rhs)
          case Long_unsigned_>  => expandLongOpSplit2(LongImpl.gtu, lhs, rhs)
          case Long_unsigned_>= => expandLongOpSplit2(LongImpl.geu, lhs, rhs)

          case _ =>
            cont(pretrans)
        }

      case _ =>
        cont(pretrans)
    }
  }

  private def foldNot(arg: PreTransform)(implicit pos: Position): PreTransform =
    foldUnaryOp(UnaryOp.Boolean_!, arg)

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
            import BinaryOp._

            val newOp = (innerOp: @switch) match {
              case === => !==
              case !== => ===

              /* All the integer comparisons.
               * We avoid using the IntComparison extractor here to preserve
               * the @switch on the match.
               */
              case Int_== | Int_!= | Int_< | Int_<= | Int_> | Int_>= |
                  Int_unsigned_< | Int_unsigned_<= | Int_unsigned_> | Int_unsigned_>= |
                  Long_== | Long_!= | Long_< | Long_<= | Long_> | Long_>= |
                  Long_unsigned_< | Long_unsigned_<= | Long_unsigned_> | Long_unsigned_>= =>
                (!IntComparison(innerOp)).toBinaryOpCode

              case Double_== => Double_!=
              case Double_!= => Double_==

              case Boolean_== => Boolean_!=
              case Boolean_!= => Boolean_==

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

          /* (double) <toLongUnsigned>(x)  -->  <unsignedIntToDouble>(x)
           *
           * On Wasm, there is a dedicated transient. On JS, that is (x >>> 0).
           *
           * The latter only kicks in when using bigints for longs. When using
           * RuntimeLong, we have eagerly expanded the `UnsignedIntToLong`
           * operation, but further inlining and folding will yield the same
           * result.
           */
          case PreTransUnaryOp(UnsignedIntToLong, x) =>
            val newX = finishTransformExpr(x)
            val resultTree =
              if (isWasm) Transient(WasmUnaryOp(WasmUnaryOp.F64ConvertI32U, newX))
              else makeCast(JSBinaryOp(JSBinaryOp.>>>, newX, IntLiteral(0)), DoubleType)
            resultTree.toPreTransform

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

      // Null check

      case CheckNotNull =>
        checkNotNull(arg)

      // Class operations

      case Class_name =>
        arg match {
          case PreTransLit(ClassOf(typeRef)) =>
            PreTransLit(StringLiteral(constantClassNameOf(typeRef)))
          case _ =>
            default
        }
      case Class_isPrimitive =>
        arg match {
          case PreTransLit(ClassOf(typeRef)) =>
            PreTransLit(BooleanLiteral(typeRef.isInstanceOf[PrimRef]))
          case _ =>
            default
        }
      case Class_isArray =>
        arg match {
          case PreTransLit(ClassOf(typeRef)) =>
            PreTransLit(BooleanLiteral(typeRef.isInstanceOf[ArrayTypeRef]))
          case _ =>
            default
        }
      case Class_componentType =>
        arg match {
          case PreTransLit(ClassOf(ArrayTypeRef(base, depth))) =>
            PreTransLit(ClassOf(
                if (depth == 1) base
                else ArrayTypeRef(base, depth - 1)))
          case PreTransLit(ClassOf(_)) =>
            PreTransLit(Null())
          case _ =>
            default
        }

      case Array_length =>
        arg match {
          case PreTransLocalDef(LocalDef(_, _, replacement: InlineArrayReplacement)) =>
            PreTransLit(IntLiteral(replacement.elemLocalDefs.size))
          case _ =>
            default
        }

      case GetClass =>
        def constant(typeRef: TypeRef): PreTransform =
          PreTransTree(Block(finishTransformStat(arg), ClassOf(typeRef)))

        arg.tpe match {
          case RefinedType(ClassType(className, false), true) =>
            constant(ClassRef(className))
          case RefinedType(ArrayType(arrayTypeRef, false), true) =>
            constant(arrayTypeRef)
          case RefinedType(AnyType | AnyNotNullType | ClassType(ObjectClass, _), _) =>
            // The result can be anything, including null
            default
          case _ =>
            /* If texpr.tpe is neither AnyType nor j.l.Object, it cannot be
             * a JS object, so its getClass() cannot be null. Cast away
             * nullability to help downstream optimizations.
             */
            foldCast(default, ClassType(ClassClass, nullable = false))
        }

      /* Floating point bit manipulation
       *
       * The {Float,Double}_fromBits opcodes are the "raw" variants, for which
       * the specific bit patterns of NaNs are not specified. We use the
       * canonicalizing variants when folding. This is allowed by the spec,
       * and ensures that the result of the optimizer is deterministic.
       */

      case Float_toBits =>
        arg match {
          case PreTransLit(FloatLiteral(v)) =>
            PreTransLit(IntLiteral(java.lang.Float.floatToIntBits(v)))
          case _ =>
            default
        }
      case Float_fromBits =>
        arg match {
          case PreTransLit(IntLiteral(v)) =>
            PreTransLit(FloatLiteral(java.lang.Float.intBitsToFloat(v)))
          case _ =>
            default
        }
      case Double_toBits =>
        arg match {
          case PreTransLit(DoubleLiteral(v)) =>
            PreTransLit(LongLiteral(java.lang.Double.doubleToLongBits(v)))
          case _ =>
            default
        }
      case Double_fromBits =>
        arg match {
          case PreTransLit(LongLiteral(v)) =>
            PreTransLit(DoubleLiteral(java.lang.Double.longBitsToDouble(v)))
          case _ =>
            default
        }

      // clz

      case Int_clz =>
        arg match {
          case PreTransLit(IntLiteral(v)) =>
            PreTransLit(IntLiteral(Integer.numberOfLeadingZeros(v)))
          case _ =>
            default
        }
      case Long_clz =>
        arg match {
          case PreTransLit(LongLiteral(v)) =>
            PreTransLit(IntLiteral(java.lang.Long.numberOfLeadingZeros(v)))
          case _ =>
            default
        }

      // Unsigned int to long

      case UnsignedIntToLong =>
        arg match {
          case PreTransLit(IntLiteral(v)) =>
            PreTransLit(LongLiteral(Integer.toUnsignedLong(v)))
          case _ =>
            default
        }

      case _ =>
        default
    }
  }

  private def constantClassNameOf(typeRef: TypeRef): String = {
    def mappedClassName(className: ClassName): String = {
      RuntimeClassNameMapperImpl.map(
          config.coreSpec.semantics.runtimeClassNameMapper,
          className.nameString)
    }

    typeRef match {
      case primRef: PrimRef =>
        primRef.displayName
      case ClassRef(className) =>
        mappedClassName(className)
      case ArrayTypeRef(primRef: PrimRef, dimensions) =>
        "[" * dimensions + primRef.charCode
      case ArrayTypeRef(ClassRef(className), dimensions) =>
        "[" * dimensions + "L" + mappedClassName(className) + ";"
      case typeRef: TransientTypeRef =>
        throw new IllegalArgumentException(typeRef.toString())
    }
  }

  /** Performs === for two literals.
   *  The result is always known statically.
   *
   *  Bytes, Shorts, Ints, Floats and Doubles all live in the same "space" for
   *  `===` comparison, since they all upcast as primitive numbers. They are
   *  compared with `equals()` instead of `==` so that `NaN === NaN` and
   *  `+0.0 !== -0.0`.
   *
   *  Chars and Longs, however, never compare as `===`, since they are
   *  boxed---unless we are using `BigInt`s for `Long`s, in which case Longs
   *  can be `===`.
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
      case (AnyNumLiteral(l), AnyNumLiteral(r))   => l.equals(r)
      case (LongLiteral(l), LongLiteral(r))       => l == r && !useRuntimeLong && !isWasm
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
        (lhs, rhs) match {
          case (PreTransLit(l), PreTransLit(r)) =>
            val isSame = literal_===(l, r)
            PreTransLit(BooleanLiteral(if (op == ===) isSame else !isSame))
          case (PreTransLit(_), _) =>
            foldBinaryOp(op, rhs, lhs)

          case (_, PreTransLit(Null())) if !lhs.tpe.isNullable =>
            Block(
                finishTransformStat(lhs),
                BooleanLiteral(op == !==)).toPreTransform

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
            else foldNot(rhs)

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

          // 1 + (-1 ^ x) == 1 + ~x == -x == 0 - x (this appears when optimizing a Range with step == -1)
          case (PreTransLit(IntLiteral(1)), PreTransBinaryOp(Int_^, PreTransLit(IntLiteral(-1)), x)) =>
            foldBinaryOp(Int_-, PreTransLit(IntLiteral(0)), x)

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

          // -1 - y == -1 + -y == -1 + (~y + 1) == ~y
          // (this appears when optimizing `a shift (31 - y)` -> `a shift (-1 - y)`, a common idiom)
          case (PreTransLit(IntLiteral(-1)), _) =>
            foldBinaryOp(Int_^, lhs, rhs) // -1 ^ y is the canonical form of ~y

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
              case _ if isUnsignedPowerOf2(x) =>
                // Interpret the multiplication as unsigned and turn it into a shift.
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

      case Int_unsigned_/ =>
        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(0))) =>
            default
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            intLit(java.lang.Integer.divideUnsigned(l, r))

          case (_, PreTransLit(IntLiteral(r))) if isUnsignedPowerOf2(r) =>
            foldBinaryOp(BinaryOp.Int_>>>, lhs,
                PreTransLit(IntLiteral(java.lang.Integer.numberOfTrailingZeros(r))))

          case _ => default
        }

      case Int_unsigned_% =>
        (lhs, rhs) match {
          case (_, PreTransLit(IntLiteral(0))) =>
            default
          case (PreTransLit(IntLiteral(l)), PreTransLit(IntLiteral(r))) =>
            intLit(java.lang.Integer.remainderUnsigned(l, r))

          case (_, PreTransLit(IntLiteral(r))) if isUnsignedPowerOf2(r) =>
            foldBinaryOp(BinaryOp.Int_&, PreTransLit(IntLiteral(r - 1)), lhs)

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

          case (PreTransLit(IntLiteral(x)), _) =>
            val rhs2 = simplifyOnlyInterestedInMask(rhs, ~x)
            if (rhs2 eq rhs)
              default
            else
              foldBinaryOp(Int_|, lhs, rhs2)

          // x | (~x & z)  -->  x | z  (appears in the inlining of 0L - b)
          case (PreTransLocalDef(x),
              PreTransBinaryOp(Int_&,
                  PreTransBinaryOp(Int_^, PreTransLit(IntLiteral(-1)), PreTransLocalDef(y)),
                  z)) if x eq y =>
            foldBinaryOp(Int_|, lhs, z)

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

          case (PreTransLit(IntLiteral(x)), _) =>
            val rhs2 = simplifyOnlyInterestedInMask(rhs, x)
            if (rhs2 eq rhs)
              default
            else
              foldBinaryOp(Int_&, lhs, rhs2)

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
            if (dist == 0) {
              lhs
            } else {
              val lhs2 = simplifyOnlyInterestedInMask(lhs, (-1) >>> dist)
              if (lhs2 eq lhs)
                PreTransBinaryOp(Int_<<, lhs, PreTransLit(IntLiteral(dist)))
              else
                foldBinaryOp(Int_<<, lhs2, PreTransLit(IntLiteral(dist)))
            }

          case _ =>
            val rhs2 = simplifyOnlyInterestedInMask(rhs, 31)
            if (rhs2 eq rhs)
              default
            else
              foldBinaryOp(Int_<<, lhs, rhs2)
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
              foldBinaryOp(Int_>>>, x, PreTransLit(IntLiteral(dist)))

          case (PreTransBinaryOp(op @ (Int_| | Int_& | Int_^),
              PreTransLit(IntLiteral(x)), y),
              z @ PreTransLit(IntLiteral(zValue))) =>
            foldBinaryOp(
                op,
                PreTransLit(IntLiteral(x >>> zValue)),
                foldBinaryOp(Int_>>>, y, z))

          case (_, PreTransLit(IntLiteral(y))) =>
            val dist = y & 31
            if (dist == 0) {
              lhs
            } else {
              val lhs2 = simplifyOnlyInterestedInMask(lhs, (-1) << dist)
              if (lhs2 eq lhs)
                PreTransBinaryOp(Int_>>>, lhs, PreTransLit(IntLiteral(dist)))
              else
                foldBinaryOp(Int_>>>, lhs2, PreTransLit(IntLiteral(dist)))
            }

          case _ =>
            val rhs2 = simplifyOnlyInterestedInMask(rhs, 31)
            if (rhs2 eq rhs)
              default
            else
              foldBinaryOp(Int_>>>, lhs, rhs2)
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
            foldBinaryOp(Int_>>, x, PreTransLit(IntLiteral(dist)))

          case (PreTransBinaryOp(Int_>>>, x, PreTransLit(IntLiteral(y))),
              PreTransLit(IntLiteral(_))) if (y & 31) != 0 =>
            foldBinaryOp(Int_>>>, lhs, rhs)

          case (PreTransBinaryOp(op @ (Int_| | Int_& | Int_^), PreTransLit(IntLiteral(x)), y),
              z @ PreTransLit(IntLiteral(zValue))) =>
            foldBinaryOp(
                op,
                PreTransLit(IntLiteral(x >> zValue)),
                foldBinaryOp(Int_>>, y, z))

          case (_, PreTransLit(IntLiteral(y))) =>
            val dist = y & 31
            if (dist == 0) {
              lhs
            } else {
              val lhs2 = simplifyOnlyInterestedInMask(lhs, (-1) << dist)
              if (lhs2 eq lhs)
                PreTransBinaryOp(Int_>>, lhs, PreTransLit(IntLiteral(dist)))
              else
                foldBinaryOp(Int_>>, lhs2, PreTransLit(IntLiteral(dist)))
            }

          case _ =>
            val rhs2 = simplifyOnlyInterestedInMask(rhs, 31)
            if (rhs2 eq rhs)
              default
            else
              foldBinaryOp(Int_>>, lhs, rhs2)
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
              case _ if isUnsignedPowerOf2(x) =>
                // Interpret the multiplication as unsigned and turn it into a shift.
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

          case (_, PreTransLit(LongLiteral(1L))) =>
            lhs
          case (_, PreTransLit(LongLiteral(-1L))) =>
            foldBinaryOp(Long_-, PreTransLit(LongLiteral(0L)), lhs)

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

      case Long_unsigned_/ =>
        (lhs, rhs) match {
          case (_, PreTransLit(LongLiteral(0L))) =>
            default
          case (PreTransLit(LongLiteral(l)), PreTransLit(LongLiteral(r))) =>
            longLit(java.lang.Long.divideUnsigned(l, r))

          case (_, PreTransLit(LongLiteral(r))) if isUnsignedPowerOf2(r) =>
            foldBinaryOp(BinaryOp.Long_>>>, lhs,
                PreTransLit(IntLiteral(java.lang.Long.numberOfTrailingZeros(r))))

          case _ => default
        }

      case Long_unsigned_% =>
        (lhs, rhs) match {
          case (_, PreTransLit(LongLiteral(0L))) =>
            default
          case (PreTransLit(LongLiteral(l)), PreTransLit(LongLiteral(r))) =>
            longLit(java.lang.Long.remainderUnsigned(l, r))

          case (_, PreTransLit(LongLiteral(r))) if isUnsignedPowerOf2(r) =>
            foldBinaryOp(BinaryOp.Long_&, PreTransLit(LongLiteral(r - 1L)), lhs)

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

          case (PreTransLit(LongLiteral(0xffffffffL)), LongFromInt(intRhs)) =>
            foldUnaryOp(UnaryOp.UnsignedIntToLong, intRhs)

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

      /* All the integer comparisons.
       * We avoid using the IntComparison extractor here to preserve the
       * @switch on the entire match.
       */
      case Int_== | Int_!= | Int_< | Int_<= | Int_> | Int_>= |
          Int_unsigned_< | Int_unsigned_<= | Int_unsigned_> | Int_unsigned_>= |
          Long_== | Long_!= | Long_< | Long_<= | Long_> | Long_>= |
          Long_unsigned_< | Long_unsigned_<= | Long_unsigned_> | Long_unsigned_>= =>
        foldCmp(IntComparison(op), lhs, rhs)

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

          /* ±0.0 + cast(a >>> b, DoubleType)  -->  cast(a >>> b, DoubleType)
           *
           * In general, `+0.0 + y -> y` is not a valid rewrite, because it does
           * not give the same result when y is -0.0. (Paradoxically, with -0.0
           * on the left it *is* a valid rewrite, though not a very useful one.)
           *
           * However, if y is the result of a JS `>>>` operator, we know it
           * cannot be -0.0, hence the rewrite is valid. That particular shape
           * appears in the inlining of `Integer.toUnsignedLong(x).toDouble`.
           */
          case (PreTransLit(DoubleLiteral(0.0)), // also matches -0.0
              PreTransTree(Transient(Cast(JSBinaryOp(JSBinaryOp.>>>, _, _), DoubleType)), _)) =>
            rhs

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

      case Class_isInstance | Class_isAssignableFrom =>
        /* TODO: Improve this, now that we have a knowledge query to
         * turn a TypeRef into a Type. Before that, we couldn't tell whether a
         * ClassRef must become an `AnyType` or a `ClassType`.
         */
        default

      case Class_cast =>
        if (semantics.asInstanceOfs == CheckedBehavior.Unchecked) {
          PreTransBlock(finishTransformStat(lhs), rhs)
        } else {
          // Same comment as in isInstance and isAssignableFrom
          default
        }

      case Class_newArray =>
        lhs match {
          case PreTransLit(ClassOf(elementTypeRef)) if elementTypeRef != VoidRef =>
            val arrayTypeRef = ArrayTypeRef.of(elementTypeRef)
            NewArray(arrayTypeRef, finishTransformExpr(rhs)).toPreTransform
          case _ =>
            default
        }
    }
  }

  /** Folds an integer comparison `lhs cmp rhs`.
   *
   *  We normalize literals to the right-hand-side of comparisons. Unlike for
   *  arithmetic operations¹, there is no objectively better direction.
   *  Subjectively, it seems easier to reason about comparisons with the
   *  constants on the right, so we normalize on the right.
   *
   *  ¹ For arithmetic operations, only literals on the left allow us to
   *    normalize non-commutative operations (notably -).
   */
  private def foldCmp(cmp: IntComparison, lhs: PreTransform, rhs: PreTransform)(
      implicit pos: Position): PreTransform = {
    import BinaryOp._
    import IntComparison._

    def constantResult(result: Boolean): PreTransform =
      Block(finishTransformStat(lhs), finishTransformStat(rhs), BooleanLiteral(result)).toPreTransform

    def default: PreTransform = {
      /* Only called after we have ruled out tautologies and contradictions,
       * so we can safely call `toBinaryOpCode`.
       */
      PreTransBinaryOp(cmp.toBinaryOpCode, lhs, rhs)
    }

    (lhs, rhs) match {
      /* Get tautologies and contradictions out of the way, so that we don't
       * have to think about them in the subsequent cases.
       */
      case _ if cmp.isAlwaysTrue =>
        constantResult(true)
      case _ if cmp.isAlwaysFalse =>
        constantResult(false)

      // Constant-folding and normalization
      case (PreTransLit(l), PreTransLit(r)) =>
        constantResult(cmp(cmp.extractLit(l), cmp.extractLit(r)))
      case (PreTransLit(_), _) =>
        foldCmp(cmp.flipped, rhs, lhs)

      // Comparing a value against itself
      case (PreTransLocalDef(l), PreTransLocalDef(r)) if l == r =>
        constantResult(cmp.hasEQ)

      // Turn ((SignBit ^ a) cmp (SignBit ^ b)) into (a cmp.otherSignedness b)
      case (PreTransBinaryOp(Int_^ | Long_^, PreTransLit(x), y), PreTransLit(z))
          if cmp.extractLit(x) == cmp.signBit =>
        foldCmp(cmp.otherSignedness, y, cmp.makeLit(cmp.extractLit(z) ^ cmp.signBit))
      case (PreTransBinaryOp(Int_^ | Long_^, PreTransLit(x), y),
          PreTransBinaryOp(Int_^ | Long_^, PreTransLit(z), w))
          if cmp.extractLit(x) == cmp.signBit && z == x =>
        foldCmp(cmp.otherSignedness, y, w)

      /* When cmp is an equality test, we can move +, - and ^ to the right,
       * which is useful if we can then constant-fold the right-hand-side.
       *   (x + y) == z  -->  y == (z - x)
       *   (x - y) == z  -->  -y == (z - x)  -->  y == (x - z)
       *   (x ^ y) == z  -->  y == (x ^ z)
       * We could also do that for inequalities with + and - if `z - x` does not overflow;
       * we currently don't.
       */
      case (PreTransBinaryOp(Int_+ | Long_+, PreTransLit(x), y), PreTransLit(z)) if cmp.isEQOrNE =>
        foldCmp(cmp, y, cmp.makeLit(cmp.extractLit(z) - cmp.extractLit(x)))
      case (PreTransBinaryOp(Int_- | Long_-, PreTransLit(x), y), PreTransLit(z)) if cmp.isEQOrNE =>
        foldCmp(cmp, y, cmp.makeLit(cmp.extractLit(x) - cmp.extractLit(z)))
      case (PreTransBinaryOp(Int_^ | Long_^, PreTransLit(x), y), PreTransLit(z)) if cmp.isEQOrNE =>
        foldCmp(cmp, y, cmp.makeLit(cmp.extractLit(x) ^ cmp.extractLit(z)))

      // When comparing Int's converted to Long's, we can simplify to comparing Int's
      case (LongFromInt(x), LongFromInt(y)) if cmp.isSigned =>
        foldCmp(cmp.toIntOp, x, y)
      case (LongFromInt(x), PreTransLit(LongLiteral(y))) if cmp.isSigned =>
        // If `y` were in the Int range, the previous `case` would have applied
        assert(y > Int.MaxValue || y < Int.MinValue)
        constantResult(if (y > Int.MaxValue) cmp.hasLT else cmp.hasGT)

      // Turn inequalities near the extreme values into equality tests
      case (_, PreTransLit(yLit)) if !cmp.isEQOrNE =>
        val y = cmp.extractLit(yLit)
        val minValue = cmp.minValue
        val maxValue = cmp.maxValue

        y match {
          case `minValue` =>
            // LT is irrelevant for `minValue`; simplify based on EQ and GT
            if (cmp.hasEQ) {
              if (cmp.hasGT)
                constantResult(true)
              else
                foldCmp(cmp.withRels(Rels_==), lhs, rhs)
            } else {
              if (cmp.hasGT)
                foldCmp(cmp.withRels(Rels_!=), lhs, rhs)
              else
                constantResult(false)
            }

          case `maxValue` =>
            // GT is irrelevant for `maxValue`; simplify based on EQ and LT
            if (cmp.hasEQ) {
              if (cmp.hasLT)
                constantResult(true)
              else
                foldCmp(cmp.withRels(Rels_==), lhs, rhs)
            } else {
              if (cmp.hasLT)
                foldCmp(cmp.withRels(Rels_!=), lhs, rhs)
              else
                constantResult(false)
            }

          /* x <  (minValue + 1)  -->  x == minValue
           * x >= (minValue + 1)  -->  x != minValue
           */
          case _ if y == minValue + 1L && (cmp.rels == Rels_< || cmp.rels == Rels_>=) =>
            foldCmp(cmp.withRels(if (cmp.rels == Rels_<) Rels_== else Rels_!=), lhs, cmp.makeLit(minValue))

          /* x >  (maxValue - 1)  -->  x == maxValue
           * x >= (maxValue - 1)  -->  x != maxValue
           */
          case _ if y == maxValue - 1L && (cmp.rels == Rels_> || cmp.rels == Rels_<=) =>
            foldCmp(cmp.withRels(if (cmp.rels == Rels_>) Rels_== else Rels_!=), lhs, cmp.makeLit(maxValue))

          case _ =>
            default
        }

      case _ =>
        default
    }
  }

  /** Can we combine the given `PreTransform` in folds like those done in
   *  `foldTwoIntComparisonsOr`?
   *
   *  Hard requirements are that it is side-effect-free and that the
   *  `PreTransform` has a meaningful `==` test.
   *
   *  Currently, we consider literals, local defs, and pure unary operations
   *  as combinable.
   */
  private def isCombinable(preTrans: PreTransform): Boolean = preTrans match {
    case PreTransLit(_) | PreTransLocalDef(_) => true
    case PreTransUnaryOp(op, lhs)             => UnaryOp.isPureOp(op) && isCombinable(lhs)
    case _                                    => false
  }

  /** Folds `(lhs1 op1 rhs1) || (lhs2 op2 rhs2)`.
   *
   *  All the operands must be combinable according to `isCombinable`.
   *  The operators must be such that `op1.isCompatibleWith(op2)`.
   *
   *  They must not be tautologies nor contradictions (typically ensured
   *  because they directly come from the `IntComparison` extractor).
   */
  private def foldTwoIntComparisonsOr(
      lhs1: PreTransform, op1: IntComparison, rhs1: PreTransform,
      lhs2: PreTransform, op2: IntComparison, rhs2: PreTransform)(
      implicit pos: Position): PreTransform = {

    import BinaryOp._
    import IntComparison._

    require(!op1.isAlwaysTrue && !op1.isAlwaysFalse, op1)
    require(!op2.isAlwaysTrue && !op2.isAlwaysFalse, op2)

    val combinedOp = op1 | op2 // mostly for the signedness and its properties

    def isLiteral(preTrans: PreTransform): Boolean = preTrans match {
      case PreTransLit(_) => true
      case _              => false
    }

    def combinedInequalitiesOrDefault(): PreTransform = {
      if (op1.rels == Rels_!= && op2.rels == Rels_!=) {
        // (lhs1 != rhs1) || (lhs2 != rhs2)  -->  (lhs1 ^ rhs1) | (lhs2 ^ rhs2) != 0
        val notEqualsCmp = op1 // by construction, it is the Rels_!= we want
        val xorOp = if (combinedOp.isLongOp) Long_^ else Int_^
        val orOp = if (combinedOp.isLongOp) Long_| else Int_|
        foldCmp(
          notEqualsCmp,
          foldBinaryOp(
            orOp,
            foldBinaryOp(xorOp, lhs1, rhs1),
            foldBinaryOp(xorOp, lhs2, rhs2)
          ),
          combinedOp.makeLit(0L)
        )
      } else {
        PreTransIf.orElse(
          makeBinaryOp(op1, lhs1, rhs1),
          makeBinaryOp(op2, lhs2, rhs2)
        )
      }
    }

    if (lhs1 != lhs2) {
      // Try to normalize so that lhs1 == lhs2
      if (lhs1 == rhs2) {
        // Flip lhs2 op2 rhs2
        foldTwoIntComparisonsOr(lhs1, op1, rhs1, rhs2, op2.flipped, lhs2)
      } else if (lhs2 == rhs1) {
        // Flip lhs1 op1 rhs1
        foldTwoIntComparisonsOr(rhs1, op1.flipped, lhs1, lhs2, op2, rhs2)
      } else if (rhs1 == rhs2 && !isLiteral(rhs1)) {
        // Flip both; don't do that if they are a literal, because it won't help
        foldTwoIntComparisonsOr(rhs1, op1.flipped, lhs1, rhs2, op2.flipped, lhs2)
      } else {
        combinedInequalitiesOrDefault()
      }
    } else if (rhs1 == rhs2) {
      makeBinaryOp(combinedOp, lhs1, rhs1)
    } else {
      // lhs1 == lhs2 but rhs1 != rhs2
      (rhs1, rhs2) match {
        case (PreTransLit(r1), PreTransLit(r2)) =>
          foldTwoIntComparisonsOrLiterals(lhs1,
              op1, combinedOp.extractLit(r1), op2, combinedOp.extractLit(r2))
        case _ =>
          combinedInequalitiesOrDefault()
      }
    }
  }

  /** Folds `(lhs op1 r1) || (lhs op2 r2)`, where the right-hand-sides are constants. */
  private def foldTwoIntComparisonsOrLiterals(lhs: PreTransform,
      op1: IntComparison, r1: Long, op2: IntComparison, r2: Long)(
      implicit pos: Position): PreTransform = {

    import BinaryOp._
    import IntComparison._

    val combinedOp = op1 | op2 // mostly for the signedness and its properties

    // Tests whether `x > y` according to the signedness of `combinedOp`
    def greaterThan(x: Long, y: Long): Boolean =
      if (combinedOp.isSigned) x > y
      else (x ^ Long.MinValue) > (y ^ Long.MinValue)

    /* Produce the equivalent of (lhs >= min && lhs <= max), possibly negated.
     * Assumes `min <= max` according to the same signedness.
     * Under that assumption, rewrite to `(lhs - min) unsigned_<= (max - min)`.
     * Note that the rewritten comparison is always unsigned, and it works
     * regardless of the signedness of the original range test.
     * See Hacker's Delight, Section 4-1.
     */
    def makeTestForInclusiveRange(min: Long, max: Long, negated: Boolean): PreTransform = {
      val minusOp = if (combinedOp.isLongOp) Long_- else Int_-
      val baseCmp = IntComparison(if (combinedOp.isLongOp) Long_unsigned_<= else Int_unsigned_<=)
      val cmp = if (negated) !baseCmp else baseCmp
      makeBinaryOp(cmp, foldBinaryOp(minusOp, lhs, cmp.makeLit(min)), max - min)
    }

    if (greaterThan(r1, r2)) {
      // Normalize r1 < r2 according to the chosen signedness
      foldTwoIntComparisonsOrLiterals(lhs, op2, r2, op1, r1)
    } else if (r1 == r2) {
      makeBinaryOp(combinedOp, lhs, r1)
    } else if (op1.rels == Rels_<=) {
      // Since r1 < r2, r1 + 1 won't overflow -- normalize <= r1 to <
      foldTwoIntComparisonsOrLiterals(lhs, op1.withRels(Rels_<), r1 + 1L, op2, r2)
    } else if (op2.rels == Rels_>=) {
      // Symmetric
      foldTwoIntComparisonsOrLiterals(lhs, op1, r1, op2.withRels(Rels_>), r2 - 1L)
    } else if (op1.hasGT) {
      if (op2.hasLT)
        PreTransLit(BooleanLiteral(true)) // tautology: x > r1 || x < r2 is always true when r1 < r2
      else
        makeBinaryOp(op1, lhs, r1) // x >= r2 implies x > r1, so we can drop the former
    } else if (op2.hasLT) {
      makeBinaryOp(op2, lhs, r2) // x <= r1 implies x < r2, so we can drop the former
    } else {
      /* Now we have normalized with
       * - r1 < r2
       * - op1 is not <= and has no >, so it can only be < or ==
       * - op2 is not >= and has no <, so it can only be > or ==
       */
      assert(op1.rels == Rels_< || op1.rels == Rels_==)
      assert(op2.rels == Rels_> || op2.rels == Rels_==)

      if ((op1.hasLT || r1 == combinedOp.minValue) && (op2.hasGT || r2 == combinedOp.maxValue)) {
        // x < r1 || x > r2  -->  !(x >= r1 && x <= r2)
        // we can consider x == minValue as x < minValue + 1; likewise for maxValue
        makeTestForInclusiveRange(
            if (op1.hasLT) r1 else r1 + 1L,
            if (op2.hasGT) r2 else r2 - 1L,
            negated = true)
      } else if (op1.hasEQ && op2.hasEQ && r1 + 1L == r2) {
        // x == r1 || x == r1 + 1  -->  x >= r1 && x <= r1 + 1
        makeTestForInclusiveRange(r1, r2, negated = false)
      } else {
        // Otherwise, there's nothing we can do
        PreTransIf.orElse(
          makeBinaryOp(op1, lhs, op1.makeLit(r1)),
          makeBinaryOp(op2, lhs, op2.makeLit(r2))
        )
      }
    }
  }

  /** Folds `(lhs1 op1 rhs1) && (lhs2 op2 rhs2)`.
   *
   *  All the operands must be combinable according to `isCombinable`.
   *  The operators must be such that `op1.isCompatibleWith(op2)`.
   */
  private def foldTwoIntComparisonsAnd(
      lhs1: PreTransform, op1: IntComparison, rhs1: PreTransform,
      lhs2: PreTransform, op2: IntComparison, rhs2: PreTransform)(
      implicit pos: Position): PreTransform = {

    // Delegate to the Or variant using De Morgan laws
    foldTwoIntComparisonsOr(lhs1, !op1, rhs1, lhs2, !op2, rhs2) match {
      case PreTransIf(x, PreTransLit(BooleanLiteral(true)), y, tpe) =>
        // undo the De Morgan transformation if we couldn't improve anything
        PreTransIf(foldNot(x), foldNot(y), PreTransLit(BooleanLiteral(false)), tpe)
      case negResult =>
        foldNot(negResult)
    }
  }

  /** Constructs a BinaryOp node for the given IntComparison.
   *
   *  Do not fold it, unless it is a tautology or a contradiction (because
   *  there is no way to create a PreTransBinaryOp for those).
   */
  private def makeBinaryOp(cmp: IntComparison, lhs: PreTransform, rhs: PreTransform)(
      implicit pos: Position): PreTransform = {
    if (cmp.isAlwaysTrue || cmp.isAlwaysFalse) {
      Block(finishTransformStat(lhs), finishTransformStat(rhs),
          BooleanLiteral(cmp.isAlwaysTrue)).toPreTransform
    } else {
      PreTransBinaryOp(cmp.toBinaryOpCode, lhs, rhs)
    }
  }

  /** Constructs a BinaryOp node for the given IntComparison, where the rhs is
   *  a literal value.
   *
   *  Convenience overload.
   */
  private def makeBinaryOp(cmp: IntComparison, lhs: PreTransform, rhs: Long)(
      implicit pos: Position): PreTransform = {
    makeBinaryOp(cmp, lhs, cmp.makeLit(rhs))
  }

  /** Simplifies the given `value` expression with the knowledge that only some
   *  of its resulting bits will be relevant.
   *
   *  The relevant bits are those that are 1 in `mask`. These bits must be
   *  preserved by the simplifications. Bits that are 0 in `mask` can be
   *  arbitrarily altered.
   *
   *  For an example of why this is useful, consider Long addition where `a`
   *  is a constant. The formula for the `hi` result contains the following
   *  subexpression:
   *  {{{
   *    ((alo & blo) | ((alo | blo) & ~lo)) >>> 31
   *  }}}
   *
   *  Since we are going to shift by >>> 31, only the most significant bit
   *  (msb) of the left-hand-side is relevant. We can alter the other ones.
   *  Since `a` is constant, `alo` is constant. If it were equal to 0, the
   *  leftmost `&` and the innermost `|` would fold away. It is unfortunately
   *  often not 0. The end result only depends on its msb, however, and that's
   *  where this simplification helps.
   *
   *  If the msb of `alo` is 0, we can replace `alo` in that subexpression by 0
   *  without altering the final result. That allows parts of the expression to
   *  fold away.
   *
   *  Likewise, if its msb is 1, we can replace `alo` by -1. That also allows
   *  to fold the leftmost `&` and the innermost `|` (in different ways).
   *
   *  The simplification performed in this method is capable of performing that
   *  rewrite. It pushes the relevant masking information down combinations of
   *  `&`, `|` and `^`, and rewrites constants in the way that allows the most
   *  folding without altering the end result.
   *
   *  If the mask is of the form `00...0011...11`, we can also push the mask
   *  down into combinations of `+`, `-` and `*`. These operations respect
   *  modular arithmetics, which is true for 2^k as much as for 2^32.
   *
   *  When we cannot improve a fold, we transform constants so that they are
   *  closer to 0. This is a code size improvement. Constants close to 0 use
   *  fewer bytes in the final encoding (textual in JS, signed LEB in Wasm).
   */
  private def simplifyOnlyInterestedInMask(value: PreTransform, mask: Int): PreTransform = {
    import BinaryOp._

    implicit val pos = value.pos

    def chooseSmallestAbs(a: Int, b: Int): Int =
      if (Integer.compareUnsigned(Math.abs(a), Math.abs(b)) <= 0) a
      else b

    def canPushIntoOp(op: BinaryOp.Code): Boolean = (op: @switch) match {
      case Int_& | Int_| | Int_^ => true
      case Int_+ | Int_- | Int_* => (mask & (mask + 1)) == 0 // i.e., mask = 00...0011...11
      case _                     => false
    }

    value match {
      case PreTransBinaryOp(op, lhs, rhs) if canPushIntoOp(op) =>
        def simplifyArg(arg: PreTransform): PreTransform = {
          simplifyOnlyInterestedInMask(arg, mask) match {
            case arg2 @ PreTransLit(IntLiteral(v)) =>
              val improvedV = (v & mask) match {
                case 0      => 0 // foldBinaryOp below will fold this away
                case `mask` => -1 // same, except for Int_^, in which case it becomes the ~z representation
                case masked => chooseSmallestAbs(masked, masked | ~mask)
              }
              if (improvedV == v)
                arg2
              else
                PreTransLit(IntLiteral(improvedV)(arg2.pos))
            case arg2 =>
              arg2
          }
        }

        val lhs2 = simplifyArg(lhs)
        val rhs2 = simplifyArg(rhs)

        if ((lhs2 eq lhs) && (rhs2 eq rhs))
          value
        else
          foldBinaryOp(op, lhs2, rhs2)

      case _ =>
        value
    }
  }


  private def foldAsInstanceOf(arg: PreTransform, tpe: Type)(
      implicit pos: Position): PreTransform = {
    def mayRequireUnboxing: Boolean =
      arg.tpe.isNullable && tpe.isInstanceOf[PrimType]

    if (semantics.asInstanceOfs == CheckedBehavior.Unchecked && !mayRequireUnboxing) {
      foldCast(arg, tpe)
    } else {
      def default: Tree =
        AsInstanceOf(finishTransformExpr(arg), tpe)

      typeTestResult(arg.tpe, tpe) match {
        case TypeTestResult.Subtype =>
          arg
        case TypeTestResult.Unknown =>
          default.toPreTransform
        case _ if mayRequireUnboxing =>
          default.toPreTransform
        case TypeTestResult.NotAnInstance =>
          // The AsInstanceOf will always fail, so we can cast its result to NothingType
          makeCast(default, NothingType).toPreTransform
        case TypeTestResult.NotAnInstanceUnlessNull =>
          Block(default, Null()).toPreTransform
        case TypeTestResult.SubtypeOrNull =>
          throw new AssertionError(s"Unreachable; arg.tpe was ${arg.tpe} and tpe was $tpe at $pos")
      }
    }
  }

  private def foldCast(arg: PreTransform, tpe: Type)(
      implicit pos: Position): PreTransform = {

    def default(arg: PreTransform, newTpe: RefinedType): PreTransform =
      PreTransTree(makeCast(finishTransformExpr(arg), newTpe.base), newTpe)

    def castLocalDef(arg: PreTransform, newTpe: RefinedType): PreTransform = arg match {
      case PreTransMaybeBlock(bindingsAndStats, PreTransLocalDef(localDef)) =>
        val refinedLocalDef = localDef.tryWithRefinedType(newTpe)
        if (refinedLocalDef ne localDef)
          PreTransBlock(bindingsAndStats, PreTransLocalDef(refinedLocalDef))
        else
          default(arg, newTpe)

      case _ =>
        default(arg, newTpe)
    }

    def doCast(tpe: Type): PreTransform = {
      val castTpe = RefinedType(tpe, isExact = false, arg.tpe.allocationSite)

      val isCastFreeAtRunTime = tpe match {
        case CharType => false
        case LongType => !useRuntimeLong
        case _        => true
      }

      if (isCastFreeAtRunTime) {
        // Try to push the cast down to usages of LocalDefs, in order to preserve aliases
        castLocalDef(arg, castTpe)
      } else {
        default(arg, castTpe)
      }
    }

    typeTestResult(arg.tpe, tpe) match {
      case TypeTestResult.Subtype =>
        arg
      case TypeTestResult.SubtypeOrNull =>
        // Only cast away nullability, but otherwise preserve the better type
        doCast(arg.tpe.base.toNonNullable)
      case TypeTestResult.Unknown =>
        // Full cast, but if the argument was non-nullable, we can cast to non-nullable
        doCast(if (arg.tpe.isNullable) tpe else tpe.toNonNullable)
      case TypeTestResult.NotAnInstance =>
        // The cast always fails, which is UB by construction
        Block(finishTransformStat(arg), Transient(Cast(Null(), NothingType))).toPreTransform
      case TypeTestResult.NotAnInstanceUnlessNull =>
        // UB if the argument is not null, so constant-fold to null
        Block(finishTransformStat(arg), Null()).toPreTransform
    }
  }

  private def transformMethodDefBody(optTarget: Option[MethodID], thisType: Type,
      params: List[ParamDef], jsClassCaptures: List[ParamDef], resultType: Type,
      body: Tree, isNoArgCtor: Boolean): (List[ParamDef], Tree) = {

    val jsClassCaptureLocalDefs = for {
      ParamDef(LocalIdent(name), _, ptpe, mutable) <- jsClassCaptures
    } yield {
      /* Reserve capture name: They have the same name for the whole class
       * definition, so we cannot rename them.
       */
      localNameAllocator.reserve(name)

      val replacement = ReplaceWithVarRef(name, newSimpleState(Unused))
      val localDef = LocalDef(RefinedType(ptpe), mutable, replacement)
      name -> localDef
    }

    val (paramLocalDefs, newParamDefs) = params.map(newParamReplacement(_)).unzip

    val thisLocalDef =
      if (thisType == VoidType) None
      else Some(newThisLocalDef(thisType))

    val env = OptEnv.Empty
      .withThisLocalDef(thisLocalDef)
      .withLocalDefs(paramLocalDefs)
      .withLocalDefs(jsClassCaptureLocalDefs)

    val scope = {
      val scope0 = Scope.Empty.withEnv(env)

      optTarget.fold(scope0) { target =>
        val allocationSiteCount =
          paramLocalDefs.size + (if (thisLocalDef.isDefined) 1 else 0)
        val allocationSites =
          List.fill(allocationSiteCount)(AllocationSite.Anonymous)

        scope0.inlining(allocationSites -> target)
      }
    }

    val newBody0 = transform(body, resultType == VoidType)(scope)

    val newBody =
      if (isNoArgCtor) tryElimStoreModule(newBody0)
      else newBody0

    (newParamDefs, newBody)
  }

  private def pretransformLabeled(oldLabelName: LabelName, resultType: Type,
      body: Tree, isStat: Boolean, usePreTransform: Boolean)(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = tailcall {
    val newLabel = freshLabelName(oldLabelName)

    def doMakeTree(newBody: Tree, newResultType: Type, returnCount: Int): Tree = {
      tryOptimizePatternMatch(oldLabelName, newLabel, newResultType,
          returnCount, newBody) getOrElse {
        Labeled(newLabel, newResultType, newBody)
      }
    }

    val info = new LabelInfo(newLabel, isStat, acceptRecords = usePreTransform,
        returnedTreeTypes = newSimpleState(Nil), returnedStructures = newSimpleState(Nil))
    val bodyScope = scope.withEnv(scope.env.withLabelInfo(oldLabelName, info))

    if (usePreTransform) {
      assert(!isStat, "Cannot use pretransform in statement position")
      tryOrRollback { cancelFun =>
        pretransformExpr(body) { tbody0 =>
          val returnedTypes = info.returnedTreeTypes.value
          val returnedStructures = info.returnedStructures.value

          if (returnedTypes.isEmpty && returnedStructures.isEmpty) {
            // no return to that label, we can eliminate it
            cont(tbody0)
          } else {
            // Extract the body, and take its result into account as well
            val tbody = resolvePreTransform(tbody0)
            val (newBody, resultTypes, resultStructures) = tbody match {
              case PreTransRecordTree(bodyTree, structure, _) =>
                (bodyTree, returnedTypes, structure :: returnedStructures)
              case PreTransTree(bodyTree, tpe) =>
                if (tpe.isNothingType)
                  (bodyTree, returnedTypes, returnedStructures)
                else
                  (bodyTree, tpe :: returnedTypes, returnedStructures)
            }

            if (resultStructures.isEmpty) {
              // implies returnedStructures.isEmpty, which implies !returnedTypes.isEmpty, which implies:
              assert(!resultTypes.isEmpty)

              // No records; compute constrained lub of refined types
              val refinedType = resultTypes.reduce(constrainedLub(_, _, resultType))
              cont(PreTransTree(
                  doMakeTree(newBody, refinedType.base, returnedTypes.size),
                  refinedType))
            } else {
              // At least one record -- they must all agree on being records of the same class

              if (resultTypes.nonEmpty)
                cancelFun()

              val structure = resultStructures.head
              if (resultStructures.tail.exists(!_.sameClassAs(structure)))
                cancelFun()

              cont(PreTransRecordTree(
                  doMakeTree(newBody, structure.recordType, returnedStructures.size),
                  structure,
                  cancelFun))
            }
          }
        } (bodyScope)
      } { () =>
        pretransformLabeled(oldLabelName, resultType, body, isStat,
            usePreTransform = false)(cont)
      }
    } else {
      assert(info.returnedStructures.value.isEmpty)

      val newBody = transform(body, isStat)(bodyScope)
      val returnedTypes = info.returnedTreeTypes.value
      if (returnedTypes.isEmpty) {
        // no return to that label, we can eliminate it
        cont(PreTransTree(newBody))
      } else {
        val refinedType =
          returnedTypes.fold(RefinedType(newBody.tpe))(constrainedLub(_, _, resultType))
        cont(PreTransTree(
            doMakeTree(newBody, refinedType.base, returnedTypes.size),
            refinedType))
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
              case BlockOrAlone(prep, Return(result, `newLabelName`)) =>
                val result1 =
                  if (refinedType == VoidType) keepOnlySideEffects(result)
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
                          If(cond, newBody, elsep)(refinedType)(cond.pos))

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

  private def arrayElemType(tpe: ArrayType): Type = {
    val ArrayTypeRef(base, dimensions) = tpe.arrayTypeRef

    if (dimensions == 1) {
      base match {
        case PrimRef(elemType)     => elemType
        case ClassRef(ObjectClass) => AnyType

        case ClassRef(className) =>
          if (isJSType(className)) AnyType
          else ClassType(className, nullable = true)
      }
    } else {
      ArrayType(ArrayTypeRef(base, dimensions - 1), nullable = true)
    }
  }

  private def checkNotNull(texpr: PreTransform)(implicit pos: Position): PreTransform = {
    if (!texpr.tpe.isNullable)
      texpr
    else if (semantics.nullPointers == CheckedBehavior.Unchecked)
      foldCast(texpr, texpr.tpe.base.toNonNullable)
    else
      PreTransUnaryOp(UnaryOp.CheckNotNull, texpr)
  }

  private def checkNotNull(expr: Tree)(implicit pos: Position): Tree = {
    if (!expr.tpe.isNullable)
      expr
    else if (semantics.nullPointers == CheckedBehavior.Unchecked)
      makeCast(expr, expr.tpe.toNonNullable)
    else
      UnaryOp(UnaryOp.CheckNotNull, expr)
  }

  private def checkNotNullStatement(texpr: PreTransform)(implicit pos: Position): Tree = {
    if (!texpr.tpe.isNullable || semantics.nullPointers == CheckedBehavior.Unchecked)
      finishTransformStat(texpr)
    else
      UnaryOp(UnaryOp.CheckNotNull, finishTransformExpr(texpr))
  }

  private def checkNotNullStatement(expr: Tree)(implicit pos: Position): Tree = {
    if (!expr.tpe.isNullable || semantics.nullPointers == CheckedBehavior.Unchecked)
      keepOnlySideEffects(expr)
    else
      UnaryOp(UnaryOp.CheckNotNull, expr)
  }

  private def newParamReplacement(paramDef: ParamDef): ((LocalName, LocalDef), ParamDef) = {
    val ParamDef(ident @ LocalIdent(name), originalName, ptpe, mutable) = paramDef

    val (newName, newOriginalName) = freshLocalName(name, originalName, mutable)

    val replacement = ReplaceWithVarRef(newName, newSimpleState(Unused))
    val localDef = LocalDef(RefinedType(ptpe), mutable, replacement)
    val localIdent = LocalIdent(newName)(ident.pos)
    val newParamDef = ParamDef(localIdent, newOriginalName, ptpe, mutable)(paramDef.pos)
    (name -> localDef, newParamDef)
  }

  private def newThisLocalDef(thisType: Type): LocalDef = {
    LocalDef(RefinedType(thisType), false,
        ReplaceWithVarRef(LocalName.This, new SimpleState(this, UsedAtLeastOnce)))
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

  private def withSplitLong(tlongExpr: PreTransform)(
      buildInner: (PreTransform, PreTransform, PreTransCont) => TailRec[Tree])(
      cont: PreTransCont)(
      implicit scope: Scope, pos: Position): TailRec[Tree] = {

    assert(useRuntimeLong)

    tlongExpr match {
      case PreTransLit(LongLiteral(longValue)) =>
        val (loValue, hiValue) = LongImpl.extractParts(longValue)
        val tlo = PreTransLit(IntLiteral(loValue)(tlongExpr.pos))
        val thi = PreTransLit(IntLiteral(hiValue)(tlongExpr.pos))
        buildInner(tlo, thi, cont)

      case _ =>
        // For other pretransforms, we need to evaluate them in a temporary.
        withNewTempLocalDef(tlongExpr) { (localDef, cont1) =>
          val (lo, hi) = localDef.replacement match {
            case LongPairReplacement(lo, hi) =>
              // Already split; directly access the underlying LocalDefs
              (lo, hi)
            case _ =>
              val lo = LocalDef(RefinedType(IntType), mutable = false,
                  LongPartReplacement(localDef, isHiPart = false))
              val hi = LocalDef(RefinedType(IntType), mutable = false,
                  LongPartReplacement(localDef, isHiPart = true))
              (lo, hi)
          }
          buildInner(lo.toPreTransform, hi.toPreTransform, cont1)
        } (cont)
    }
  }

  private def withNewTempLocalDef(texpr: PreTransform)(
      buildInner: (LocalDef, PreTransCont) => TailRec[Tree])(
      cont: PreTransCont)(
      implicit scope: Scope): TailRec[Tree] = {
    withNewLocalDef(Binding.temp(LocalName("x"), texpr))(buildInner)(cont)
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
    val Binding(bindingName, originalName, declaredType, mutable, value) = binding
    implicit val pos = value.pos

    def withDedicatedVar(tpe: RefinedType): TailRec[Tree] = {
      val (newName, newOriginalName) = freshLocalName(bindingName, originalName, mutable)

      val used = newSimpleState[IsUsed](Unused)

      val (replacement, refinedType) = resolveRecordStructure(value) match {
        case Some((structure, cancelFun)) =>
          (ReplaceWithRecordVarRef(newName, structure, used, cancelFun), value.tpe)

        case None =>
          (ReplaceWithVarRef(newName, used), tpe)
      }

      val localDef = LocalDef(refinedType, mutable, replacement)
      val preTransBinding = PreTransBinding(newOriginalName, localDef, value)

      buildInner(localDef, { tinner =>
        cont(addPreTransBinding(preTransBinding, tinner))
      })
    }

    if (value.tpe.isNothingType) {
      cont(value)
    } else if (mutable) {
      withDedicatedVar(RefinedType(declaredType))
    } else {
      value match {
        case PreTransBlock(bindingsAndStats, result) =>
          withNewLocalDef(binding.copy(value = result))(buildInner) { tresult =>
            cont(addPreTransBindings(bindingsAndStats, tresult))
          }

        case PreTransLocalDef(localDef) if !localDef.mutable =>
          /* Attention: the same-name optimization in transformCapturingBody
           * relies on immutable bindings to var refs not being renamed.
           */
          buildInner(localDef, cont)

        case PreTransTree(literal: Literal, _) =>
          buildInner(LocalDef(value.tpe, false,
              ReplaceWithConstant(literal)), cont)

        case PreTransTree(VarRef(refName), _)
            if !localIsMutable(refName) =>
          buildInner(LocalDef(value.tpe, false,
              ReplaceWithVarRef(refName, newSimpleState(UsedAtLeastOnce))), cont)

        case _ =>
          withDedicatedVar(value.tpe)
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
      case PreTransRecordTree(tree, structure, cancelFun) =>
        PreTransRecordTree(
            finishTransformBindings(bindingsAndStats, tree),
            structure, cancelFun)
      case PreTransTree(tree, tpe) =>
        PreTransTree(
            finishTransformBindings(bindingsAndStats, tree),
            tpe)
    }
  }

  /** Finds a type as precise as possible which is a supertype of lhs and rhs
   *  but still a subtype of upperBound.
   *  Requires that lhs and rhs be subtypes of upperBound, obviously.
   *
   *  The RefinedType version does not have an `isStat` flag, since RefinedTypes
   *  only exist in a PreTransform context, which is always an expression context.
   */
  private def constrainedLub(lhs: RefinedType, rhs: RefinedType,
      upperBound: Type): RefinedType = {
    if (upperBound == VoidType) RefinedType(VoidType)
    else if (lhs == rhs) lhs
    else if (lhs.isNothingType) rhs
    else if (rhs.isNothingType) lhs
    else RefinedType(constrainedLub(lhs.base, rhs.base, upperBound, isStat = false))
  }

  /** Finds a type as precise as possible which is a supertype of lhs and rhs
   *  but still a subtype of upperBound.
   *  Requires that lhs and rhs be subtypes of upperBound, obviously.
   */
  private def constrainedLub(lhs: Type, rhs: Type, upperBound: Type, isStat: Boolean): Type = {
    // TODO Improve this
    if (isStat || upperBound == VoidType) VoidType
    else if (lhs == rhs) lhs
    else if (lhs == NothingType) rhs
    else if (rhs == NothingType) lhs
    else if (lhs.toNonNullable == rhs) lhs
    else if (rhs.toNonNullable == lhs) rhs
    else if (!lhs.isNullable && !rhs.isNullable) upperBound.toNonNullable
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

  /** When creating a `freshName` based on a `LocalName.This`, use this name as
   *  base.
   */
  private val LocalThisNameForFresh = LocalName("this")

  private val LabelDiscardBase = LabelName("discard")

  private val thisOriginalName: OriginalName = OriginalName("this")

  private val ClassTagModuleClass = ClassName("scala.reflect.ClassTag$")
  private val JavaScriptExceptionClass = ClassName("scala.scalajs.js.JavaScriptException")
  private val JSWrappedArrayClass = ClassName("scala.scalajs.js.WrappedArray")
  private val WrappedVarArgsClass = ClassName("scala.scalajs.runtime.WrappedVarArgs")
  private val NilClass = ClassName("scala.collection.immutable.Nil$")
  private val Tuple2Class = ClassName("scala.Tuple2")

  private val JavaScriptExceptionClassType = ClassType(JavaScriptExceptionClass, nullable = true)
  private val ThrowableClassType = ClassType(ThrowableClass, nullable = true)

  private val exceptionFieldName =
    FieldName(JavaScriptExceptionClass, SimpleFieldName("exception"))

  private val AnyArgConstructorName = MethodName.constructor(List(ClassRef(ObjectClass)))

  private val TupleFirstMethodName = MethodName("_1", Nil, ClassRef(ObjectClass))
  private val TupleSecondMethodName = MethodName("_2", Nil, ClassRef(ObjectClass))
  private val ClassTagApplyMethodName =
    MethodName("apply", List(ClassRef(ClassClass)), ClassRef(ClassName("scala.reflect.ClassTag")))

  def isUnsignedPowerOf2(x: Int): Boolean =
    (x & (x - 1)) == 0 && x != 0

  def isUnsignedPowerOf2(x: Long): Boolean =
    (x & (x - 1L)) == 0L && x != 0L

  final class InlineableClassStructure(val className: ClassName, private val allFields: List[FieldDef]) {
    private[OptimizerCore] val refinedType: RefinedType =
      RefinedType(ClassType(className, nullable = false), isExact = true)

    private[OptimizerCore] val fieldNames: List[FieldName] =
      allFields.map(_.name.name)

    private[OptimizerCore] val recordType: RecordType = {
      val allocator = new FreshNameAllocator.Field
      val recordFields = for {
        f @ FieldDef(flags, FieldIdent(name), originalName, ftpe) <- allFields
      } yield {
        assert(!flags.namespace.isStatic,
            s"unexpected static field in InlineableClassStructure at ${f.pos}")
        RecordType.Field(allocator.freshName(name.simpleName), originalName, ftpe,
            flags.isMutable)
      }
      RecordType(recordFields)
    }

    private val recordFields: Map[FieldName, RecordType.Field] = {
      val elems = for ((fieldDef, recordField) <- allFields.zip(recordType.fields))
        yield fieldDef.name.name -> recordField
      elems.toMap
    }

    private[OptimizerCore] def findField(fieldName: FieldName): RecordType.Field =
      recordFields(fieldName)

    private[OptimizerCore] def fieldOriginalName(fieldName: FieldName): OriginalName =
      recordFields(fieldName).originalName

    def sameStructureAs(that: InlineableClassStructure): Boolean = {
      assert(this.sameClassAs(that))
      this.allFields == that.allFields
    }

    def sameClassAs(that: InlineableClassStructure): Boolean =
      this.className == that.className

    override def toString(): String = {
      allFields
        .map(f => s"${f.name.name.nameString}: ${f.ftpe}")
        .mkString(s"InlineableClassStructure(${className.nameString}, ", ", ", ")")
    }
  }

  final class InlineableFieldBodies(
    val fieldBodies: Map[FieldName, InlineableFieldBodies.FieldBody]
  ) {
    def isEmpty: Boolean = fieldBodies.isEmpty

    override def equals(that: Any): Boolean = that match {
      case that: InlineableFieldBodies =>
        this.fieldBodies == that.fieldBodies
      case _ =>
        false
    }

    override def hashCode(): Int = fieldBodies.##

    override def toString(): String = {
      fieldBodies
        .map(f => s"${f._1.nameString}: ${f._2}")
        .mkString("InlineableFieldBodies(", ", ", ")")
    }
  }

  object InlineableFieldBodies {
    /** The body of field that we can inline.
     *
     *  This hierarchy mirrors the small subset of `Tree`s that we need to
     *  represent field bodies that we can inline.
     *
     *  Unlike `Tree`, `FieldBody` guarantees a comprehensive equality test
     *  representing its full structure. It is generally not safe to compare
     *  `Tree`s for equality, but for `FieldBody` it is safe. We use equality
     *  in `IncOptimizer` to detect changes.
     *
     *  This is also why the members of the hierarchy contain an explicit
     *  `Position` in their primary parameter list.
     */
    sealed abstract class FieldBody {
      val pos: Position
    }

    object FieldBody {
      final case class Literal(literal: Trees.Literal, pos: Position) extends FieldBody {
        require(pos == literal.pos, s"TreeBody.Literal.pos must be the same as its Literal")
      }

      object Literal {
        def apply(literal: Trees.Literal): Literal =
          Literal(literal, literal.pos)
      }

      final case class LoadModule(moduleClassName: ClassName, pos: Position)
          extends FieldBody
      final case class ModuleSelect(qualifier: LoadModule,
          fieldName: FieldName, tpe: Type, pos: Position) extends FieldBody
      final case class ModuleGetter(qualifier: LoadModule,
          methodName: MethodName, tpe: Type, pos: Position) extends FieldBody
    }

    val Empty: InlineableFieldBodies = new InlineableFieldBodies(Map.empty)
  }

  private final val MaxRollbacksPerMethod = 256

  private final class TooManyRollbacksException
      extends scala.util.control.ControlThrowable

  private type CancelFun = () => Nothing
  private type PreTransCont = PreTransform => TailRec[Tree]

  private sealed abstract class TypeTestResult

  private object TypeTestResult {
    case object Subtype extends TypeTestResult
    case object SubtypeOrNull extends TypeTestResult
    case object NotAnInstance extends TypeTestResult
    case object NotAnInstanceUnlessNull extends TypeTestResult
    case object Unknown extends TypeTestResult
  }

  private final case class RefinedType private (base: Type, isExact: Boolean)(
      val allocationSite: AllocationSite, dummy: Int = 0) {

    def isNullable: Boolean = base.isNullable

    def isNothingType: Boolean = base == NothingType
  }

  private object RefinedType {
    def apply(base: Type, isExact: Boolean,
        allocationSite: AllocationSite): RefinedType =
      new RefinedType(base, isExact)(allocationSite)

    def apply(base: Type, isExact: Boolean): RefinedType =
      RefinedType(base, isExact, AllocationSite.Anonymous)

    def apply(tpe: Type): RefinedType =
      RefinedType(tpe, isExact = false)
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

  /** Virtual local definition.
   *
   *  `LocalDef` is a `case class` for efficient extraction in pattern matching.
   *  However, its equality is defined by reference.
   */
  private final case class LocalDef(
      tpe: RefinedType,
      mutable: Boolean,
      replacement: LocalDefReplacement) {

    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

    override def hashCode(): Int = System.identityHashCode(this)

    def toPreTransform(implicit pos: Position): PreTransform = {
      replacement match {
        case ReplaceWithConstant(value) => PreTransTree(value)
        case _                          => PreTransLocalDef(this)
      }
    }

    def newReplacement(implicit pos: Position): Tree = this.replacement match {
      case ReplaceWithVarRef(name, used) =>
        used.value = used.value.inc
        VarRef(name)(tpe.base)

      case ReplaceWithRecordVarRef(_, _, _, cancelFun) =>
        cancelFun()

      case ReplaceWithOtherLocalDef(localDef) =>
        /* A previous version would push down the `tpe` of this `LocalDef` to
         * use for the replacement. While that creates trees with narrower types,
         * it also creates inconsistent trees, with `VarRef`s that are not typed
         * as the corresponding VarDef / ParamDef / receiver type.
         *
         * Type based optimizations happen (mainly) in the optimizer so
         * consistent downstream types are more important than narrower types;
         * notably because it allows us to run the ClassDefChecker after the
         * optimizer.
         */
        val underlying = localDef.newReplacement
        if (underlying.tpe == tpe.base)
          underlying
        else
          makeCast(underlying, tpe.base)

      case ReplaceWithConstant(value) =>
        value

      case LongPairReplacement(lo, hi) =>
        Transient(PackLong(lo.newReplacement, hi.newReplacement))

      case LongPartReplacement(longLocalDef, isHiPart) =>
        val longTree = longLocalDef.newReplacement
        if (isHiPart)
          Transient(ExtractLongHi(longTree))
        else
          UnaryOp(UnaryOp.LongToInt, longTree)

      case TentativeClosureReplacement(_, _, _, _, _, _, _, cancelFun) =>
        cancelFun()

      case InlineClassBeingConstructedReplacement(_, _, cancelFun) =>
        cancelFun()

      case InlineClassInstanceReplacement(_, _, cancelFun) =>
        cancelFun()

      case InlineArrayReplacement(_, _, cancelFun) =>
        cancelFun()

      case InlineJSArrayReplacement(_, cancelFun) =>
        cancelFun()
    }

    def contains(that: LocalDef): Boolean = {
      (this eq that) || (replacement match {
        case ReplaceWithOtherLocalDef(localDef) =>
          localDef.contains(that)
        case LongPairReplacement(lo, hi) =>
          lo.contains(that) || hi.contains(that)
        case LongPartReplacement(longLocalDef, _) =>
          longLocalDef.contains(that)
        case TentativeClosureReplacement(_, _, _, _, _, captureLocalDefs, _, _) =>
          captureLocalDefs.exists(_.contains(that))
        case InlineClassBeingConstructedReplacement(_, fieldLocalDefs, _) =>
          fieldLocalDefs.valuesIterator.exists(_.contains(that))
        case InlineClassInstanceReplacement(_, fieldLocalDefs, _) =>
          fieldLocalDefs.valuesIterator.exists(_.contains(that))
        case InlineArrayReplacement(_, elemLocalDefs, _) =>
          elemLocalDefs.exists(_.contains(that))
        case InlineJSArrayReplacement(elemLocalDefs, _) =>
          elemLocalDefs.exists(_.contains(that))

        case _:ReplaceWithVarRef | _:ReplaceWithRecordVarRef |
             _:ReplaceWithConstant =>
          false
      })
    }

    def tryWithRefinedType(refinedType: RefinedType): LocalDef = {
      /* Only adjust if the replacement if ReplaceWithVarRef, because other
       * types have nothing to gain (e.g., ReplaceWithConstant) or we want to
       * keep them unwrapped because they are examined in optimizations
       * (notably all the types with virtualized objects).
       */
      replacement match {
        case _:ReplaceWithVarRef =>
          LocalDef(refinedType, mutable, ReplaceWithOtherLocalDef(this))
        case replacement: ReplaceWithOtherLocalDef =>
          LocalDef(refinedType, mutable, replacement)
        case _ =>
          this
      }
    }
  }

  private sealed abstract class LocalDefReplacement

  private final case class ReplaceWithVarRef(name: LocalName,
      used: SimpleState[IsUsed]) extends LocalDefReplacement

  private final case class ReplaceWithRecordVarRef(name: LocalName,
      structure: InlineableClassStructure,
      used: SimpleState[IsUsed],
      cancelFun: CancelFun) extends LocalDefReplacement

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

  private final case class LongPairReplacement(
      lo: LocalDef, hi: LocalDef)
      extends LocalDefReplacement

  private final case class LongPartReplacement(
      longLocalDef: LocalDef, isHiPart: Boolean)
      extends LocalDefReplacement

  private final case class TentativeClosureReplacement(
      flags: ClosureFlags, captureParams: List[ParamDef],
      params: List[ParamDef], resultType: Type, body: Tree,
      captureValues: List[LocalDef], alreadyUsed: SimpleState[IsUsed],
      cancelFun: CancelFun) extends LocalDefReplacement

  private final case class InlineClassBeingConstructedReplacement(
      structure: InlineableClassStructure,
      fieldLocalDefs: Map[FieldName, LocalDef],
      cancelFun: CancelFun) extends LocalDefReplacement

  private final case class InlineClassInstanceReplacement(
      structure: InlineableClassStructure,
      fieldLocalDefs: Map[FieldName, LocalDef],
      cancelFun: CancelFun) extends LocalDefReplacement

  private final case class InlineArrayReplacement(
      arrayTypeRef: ArrayTypeRef,
      elemLocalDefs: Vector[LocalDef],
      cancelFun: CancelFun)
      extends LocalDefReplacement

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
      val isStat: Boolean,
      val acceptRecords: Boolean,
      /** Types of normal trees that are returned; cannot contain `RecordType` nor `NothingType`. */
      val returnedTreeTypes: SimpleState[List[RefinedType]],
      /** Record structures that are returned. */
      val returnedStructures: SimpleState[List[InlineableClassStructure]])

  private class OptEnv(
      val localDefs: Map[LocalName, LocalDef],
      val labelInfos: Map[LabelName, LabelInfo]) {

    def withThisLocalDef(rep: LocalDef): OptEnv =
      withLocalDef(LocalName.This, rep)

    /** Optionally adds a binding for `this`.
     *
     *  If `rep.isEmpty`, returns this environment unchanged.
     */
    def withThisLocalDef(rep: Option[LocalDef]): OptEnv =
      if (rep.isEmpty) this
      else withThisLocalDef(rep.get)

    def withLocalDef(oldName: LocalName, rep: LocalDef): OptEnv =
      new OptEnv(localDefs + (oldName -> rep), labelInfos)

    def withLocalDefs(reps: List[(LocalName, LocalDef)]): OptEnv =
      new OptEnv(localDefs ++ reps, labelInfos)

    def withLabelInfo(oldName: LabelName, info: LabelInfo): OptEnv =
      new OptEnv(localDefs, labelInfos + (oldName -> info))

    override def toString(): String = {
      "localDefs:" + localDefs.mkString("\n  ", "\n  ", "\n") +
      "labelInfos:" + labelInfos.mkString("\n  ", "\n  ", "")
    }
  }

  private object OptEnv {
    val Empty: OptEnv = new OptEnv(Map.empty, Map.empty)
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
      case PreTransIf(cond, thenp, elsep, _) =>
        cond.contains(localDef) || thenp.contains(localDef) || elsep.contains(localDef)
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
      case ReplaceWithVarRef(_, used)             => used.value.isUsed
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
      PreTransRecordTree(Block(stat, result.tree)(result.pos),
          result.structure, result.cancelFun)
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
          case PreTransRecordTree(tree, structure, cancelFun) =>
            PreTransRecordTree(Block(stat, tree)(tree.pos), structure, cancelFun)
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

    val tpe: RefinedType = RefinedType(UnaryOp.resultTypeOf(op, lhs.tpe.base))
  }

  /** A `PreTransform` for a `BinaryOp`. */
  private final case class PreTransBinaryOp(op: BinaryOp.Code,
      lhs: PreTransform, rhs: PreTransform)(implicit val pos: Position)
      extends PreTransResult {

    val tpe: RefinedType = RefinedType(BinaryOp.resultTypeOf(op))
  }

  /** A `PreTransform` for an `If` that does not produce a record. */
  private final case class PreTransIf(cond: PreTransform,
      thenp: PreTransform, elsep: PreTransform, tpe: RefinedType)(
      implicit val pos: Position)
      extends PreTransResult

  private object PreTransIf {
    def orElse(lhs: PreTransform, rhs: PreTransform)(implicit pos: Position): PreTransIf =
      PreTransIf(lhs, PreTransLit(BooleanLiteral(true)), rhs, RefinedType(BooleanType))
  }

  /** A virtual reference to a `LocalDef`. */
  private final case class PreTransLocalDef(localDef: LocalDef)(
      implicit val pos: Position) extends PreTransResult {
    val tpe: RefinedType = localDef.tpe
  }

  /** Either a `PreTransTree` or a `PreTransRecordTree`.
   *
   *  This is the result type `resolvePreTransform`.
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
      structure: InlineableClassStructure, cancelFun: CancelFun)
      extends PreTransGenTree {

    def pos: Position = tree.pos

    val tpe: RefinedType = structure.refinedType

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
        case _:New | _:NewArray | _:ArrayValue | _:ClassOf =>
          RefinedType(tree.tpe, isExact = true)
        case Transient(Cast(LoadModule(_), ClassType(_, false))) =>
          /* If a LoadModule is cast to be non-nullable, we know it is exact.
           * If it is nullable, it cannot be exact since it could be `null` or
           * an actual instance.
           */
          RefinedType(tree.tpe, isExact = true)
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

  private final case class Binding(name: LocalName, originalName: OriginalName,
      declaredType: Type, mutable: Boolean, value: PreTransform)

  private object Binding {
    def apply(localIdent: LocalIdent, originalName: OriginalName,
        declaredType: Type, mutable: Boolean, value: PreTransform): Binding = {
      apply(localIdent.name, originalName, declaredType, mutable, value)
    }

    def forReceiver(declaredType: Type, value: PreTransform): Binding =
      apply(LocalName.This, NoOriginalName, declaredType, mutable = false, value)

    def temp(baseName: LocalName, declaredType: Type, mutable: Boolean,
        value: PreTransform): Binding = {
      apply(baseName, NoOriginalName, declaredType, mutable, value)
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

  private object IntFlipSign {
    def unapply(tree: PreTransform): Option[PreTransform] = tree match {
      case PreTransBinaryOp(BinaryOp.Int_^, PreTransLit(IntLiteral(Int.MinValue)), x) =>
        Some(x)
      case _ =>
        None
    }
  }

  private object LongFlipSign {
    def unapply(tree: PreTransform): Option[PreTransform] = tree match {
      case PreTransBinaryOp(BinaryOp.Long_^, PreTransLit(LongLiteral(Long.MinValue)), x) =>
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
          genThen(unitPromise,
              Closure(ClosureFlags.arrow, Nil, Nil, None, AnyType, require, Nil))
      }

      genThen(importTree, callback)
    }
  }

  /** Makes a `Transient(Cast(expr, tpe))` but collapses consecutive casts. */
  private def makeCast(expr: Tree, tpe: Type)(implicit pos: Position): Tree = {
    val innerExpr = expr match {
      case Transient(Cast(innerExpr, _)) => innerExpr
      case _                             => expr
    }

    /* We could refine the result type to be the intersection of `expr.tpe`
     * and `tpe`, but we do not have any infrastructure to do so. We always use
     * `tpe` instead.
     */
    Transient(Cast(innerExpr, tpe))
  }

  /** Tests whether `x + y` is valid without falling out of range. */
  private def canAddLongs(x: Long, y: Long): Boolean =
    if (y >= 0) x+y >= x
    else        x+y <  x

  /** Tests whether `x - y` is valid without falling out of range. */
  private def canSubtractLongs(x: Long, y: Long): Boolean =
    if (y >= 0) x-y <= x
    else        x-y >  x

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

    final val IntegerNTZ = ArrayLength + 1
    final val IntegerBitCount = IntegerNTZ + 1
    final val IntegerRotateLeft = IntegerBitCount + 1
    final val IntegerRotateRight = IntegerRotateLeft + 1

    final val LongNTZ = IntegerRotateRight + 1
    final val LongBitCount = LongNTZ + 1
    final val LongRotateLeft = LongBitCount + 1
    final val LongRotateRight = LongRotateLeft + 1
    final val LongToString = LongRotateRight + 1
    final val LongCompare = LongToString + 1

    final val CharacterCodePointToString = LongCompare + 1

    final val StringCodePointAt = CharacterCodePointToString + 1
    final val StringSubstringStart = StringCodePointAt + 1
    final val StringSubstringStartEnd = StringSubstringStart + 1

    final val MathAbsLong = StringSubstringStartEnd + 1
    final val MathAbsFloat = MathAbsLong + 1
    final val MathAbsDouble = MathAbsFloat + 1
    final val MathCeil = MathAbsDouble + 1
    final val MathFloor = MathCeil + 1
    final val MathRint = MathFloor + 1
    final val MathSqrt = MathRint + 1
    final val MathMinFloat = MathSqrt + 1
    final val MathMinDouble = MathMinFloat + 1
    final val MathMaxFloat = MathMinDouble + 1
    final val MathMaxDouble = MathMaxFloat + 1
    final val MathMultiplyFull = MathMaxDouble + 1

    final val ArrayBuilderZeroOf = MathMultiplyFull + 1
    final val GenericArrayBuilderResult = ArrayBuilderZeroOf + 1

    final val ClassGetName = GenericArrayBuilderResult + 1

    final val ArrayToJSArray = ClassGetName + 1

    final val ObjectLiteral = ArrayToJSArray + 1

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
    private val Z = BooleanRef
    private val C = CharRef
    private val B = ByteRef
    private val S = ShortRef
    private val I = IntRef
    private val J = LongRef
    private val F = FloatRef
    private val D = DoubleRef
    private val O = ClassRef(ObjectClass)
    private val ClassClassRef = ClassRef(ClassClass)
    private val StringClassRef = ClassRef(BoxedStringClass)
    private val SeqClassRef = ClassRef(ClassName("scala.collection.Seq"))
    private val ImmutableSeqClassRef = ClassRef(ClassName("scala.collection.immutable.Seq"))
    private val JSObjectClassRef = ClassRef(ClassName("scala.scalajs.js.Object"))
    private val JSArrayClassRef = ClassRef(ClassName("scala.scalajs.js.Array"))

    private def a(base: NonArrayTypeRef): ArrayTypeRef = ArrayTypeRef(base, 1)

    private def typedarrayClassRef(baseName: String): ClassRef =
      ClassRef(ClassName(s"scala.scalajs.js.typedarray.${baseName}Array"))

    // scalastyle:off line.size.limit
    // scalafmt: { maxColumn = 1000 }
    private val commonIntrinsics: List[(ClassName, List[(MethodName, Int)])] = List(
        ClassName("java.lang.System$") -> List(
            m("arraycopy", List(O, I, O, I, I), V) -> ArrayCopy
        ),
        ClassName("scala.runtime.ScalaRunTime$") -> List(
            m("array_apply", List(O, I), O) -> ArrayApply,
            m("array_update", List(O, I, O), V) -> ArrayUpdate,
            m("array_length", List(O), I) -> ArrayLength
        ),
        ClassName("java.lang.Class") -> List(
            m("getName", Nil, StringClassRef) -> ClassGetName
        ),
        ClassName("scala.scalajs.runtime.package$") -> List(
            m("genericArrayToJSArray", List(O), JSArrayClassRef) -> ArrayToJSArray,
            m("refArrayToJSArray", List(ArrayTypeRef(O, 1)), JSArrayClassRef) -> ArrayToJSArray,
            m("booleanArrayToJSArray", List(ArrayTypeRef(Z, 1)), JSArrayClassRef) -> ArrayToJSArray,
            m("charArrayToJSArray", List(ArrayTypeRef(C, 1)), JSArrayClassRef) -> ArrayToJSArray,
            m("byteArrayToJSArray", List(ArrayTypeRef(B, 1)), JSArrayClassRef) -> ArrayToJSArray,
            m("shortArrayToJSArray", List(ArrayTypeRef(S, 1)), JSArrayClassRef) -> ArrayToJSArray,
            m("intArrayToJSArray", List(ArrayTypeRef(I, 1)), JSArrayClassRef) -> ArrayToJSArray,
            m("longArrayToJSArray", List(ArrayTypeRef(J, 1)), JSArrayClassRef) -> ArrayToJSArray,
            m("floatArrayToJSArray", List(ArrayTypeRef(F, 1)), JSArrayClassRef) -> ArrayToJSArray,
            m("doubleArrayToJSArray", List(ArrayTypeRef(D, 1)), JSArrayClassRef) -> ArrayToJSArray
        ),
        ClassName("scala.scalajs.js.special.package$") -> List(
            m("objectLiteral", List(SeqClassRef), JSObjectClassRef) -> ObjectLiteral, // 2.12
            m("objectLiteral", List(ImmutableSeqClassRef), JSObjectClassRef) -> ObjectLiteral // 2.13
        )
    )

    private val baseJSIntrinsics: List[(ClassName, List[(MethodName, Int)])] = List(
        ClassName("scala.collection.mutable.ArrayBuilder$") -> List(
            m("scala$collection$mutable$ArrayBuilder$$zeroOf", List(ClassClassRef), O) -> ArrayBuilderZeroOf,
            m("scala$collection$mutable$ArrayBuilder$$genericArrayBuilderResult", List(ClassClassRef, JSArrayClassRef), O) -> GenericArrayBuilderResult
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
            m("compare", List(J, J), I) -> LongCompare
        ),
        ClassName("java.lang.Math$") -> List(
            m("abs", List(J), J) -> MathAbsLong,
            m("multiplyFull", List(I, I), J) -> MathMultiplyFull
        )
    )

    private val wasmIntrinsics: List[(ClassName, List[(MethodName, Int)])] = List(
        ClassName("java.lang.Integer$") -> List(
            m("numberOfTrailingZeros", List(I), I) -> IntegerNTZ,
            m("bitCount", List(I), I) -> IntegerBitCount,
            m("rotateLeft", List(I, I), I) -> IntegerRotateLeft,
            m("rotateRight", List(I, I), I) -> IntegerRotateRight
        ),
        ClassName("java.lang.Long$") -> List(
            m("numberOfTrailingZeros", List(J), I) -> LongNTZ,
            m("bitCount", List(J), I) -> LongBitCount,
            m("rotateLeft", List(J, I), J) -> LongRotateLeft,
            m("rotateRight", List(J, I), J) -> LongRotateRight
        ),
        ClassName("java.lang.Character$") -> List(
            m("toString", List(I), StringClassRef) -> CharacterCodePointToString
        ),
        ClassName("java.lang.String") -> List(
            m("codePointAt", List(I), I) -> StringCodePointAt,
            m("substring", List(I), StringClassRef) -> StringSubstringStart,
            m("substring", List(I, I), StringClassRef) -> StringSubstringStartEnd
        ),
        ClassName("java.lang.Math$") -> List(
            m("abs", List(F), F) -> MathAbsFloat,
            m("abs", List(D), D) -> MathAbsDouble,
            m("ceil", List(D), D) -> MathCeil,
            m("floor", List(D), D) -> MathFloor,
            m("rint", List(D), D) -> MathRint,
            m("sqrt", List(D), D) -> MathSqrt,
            m("min", List(F, F), F) -> MathMinFloat,
            m("min", List(D, D), D) -> MathMinDouble,
            m("max", List(F, F), F) -> MathMaxFloat,
            m("max", List(D, D), D) -> MathMaxDouble
        )
    )
    // scalafmt: {}
    // scalastyle:on line.size.limit

    def buildIntrinsics(esFeatures: ESFeatures, isWasm: Boolean): Intrinsics = {
      val allIntrinsics = if (isWasm) {
        commonIntrinsics ::: wasmIntrinsics
      } else {
        val baseIntrinsics = commonIntrinsics ::: baseJSIntrinsics
        if (esFeatures.allowBigIntsForLongs) baseIntrinsics
        else baseIntrinsics ++ runtimeLongIntrinsics
      }

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

  object MethodAttributes {
    def compute(enclosingClassName: ClassName, methodDef: MethodDef): MethodAttributes = {
      val MethodDef(_, MethodIdent(methodName), _, params, _, optBody) = methodDef
      val body = optBody getOrElse {
        throw new AssertionError("Methods in optimizer must be concrete")
      }

      val optimizerHints = methodDef.optimizerHints

      val isForwarder = body match {
        // Shape of forwards to default methods
        case ApplyStatically(_, This(), className, method, args) =>
          args.size == params.size &&
          args.zip(params).forall {
            case (VarRef(aname), ParamDef(LocalIdent(pname), _, _, _)) =>
              aname == pname
            case _ =>
              false
          }

        // Shape of bridges for generic methods
        case Apply(_, This(), method, args) =>
          !method.name.isReflectiveProxy &&
          (args.size == params.size) &&
          args.zip(params).forall {
            case (MaybeUnbox(VarRef(aname), _),
                ParamDef(LocalIdent(pname), _, _, _)) => aname == pname
            case _ => false
          }

        case _ => false
      }

      val inlineable = !optimizerHints.noinline

      val shouldInline = inlineable && {
        optimizerHints.inline || isForwarder || {
          body match {
            case _:Skip | _:VarRef | _:Literal =>
              true

            // Shape of accessors
            case Select(This(), _) if params.isEmpty =>
              true
            case Assign(Select(This(), _), VarRef(_)) if params.size == 1 =>
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

  private object MaybeCast {
    def unapply(tree: Tree): Some[Tree] = tree match {
      case Transient(Cast(inner, _)) => Some(inner)
      case _                         => Some(tree)
    }
  }

  private val TraitInitSimpleMethodName = SimpleMethodName("$init$")

  private def isTrivialConstructorStat(stat: Tree): Boolean = stat match {
    case _: VarRef =>
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
    case Select(This(), _)       => true
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
      case ApplyTypedClosure(_, fun, args)          => areSimpleArgs(fun :: args)
      case Select(qual, _)                          => isSimpleArg(qual)
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

      case ArraySelect(array, index) => isTrivialArg(array) && isTrivialArg(index)

      case AsInstanceOf(inner, _) => isSimpleArg(inner)
      case UnaryOp(_, inner)      => isSimpleArg(inner)

      case _ =>
        isTrivialArg(arg)
    }

    private def isTrivialArg(arg: Tree): Boolean = arg match {
      case _:VarRef | _:Literal | _:LoadModule =>
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

  /** An integer comparison operator.
   *
   *  Represents a comparison operator for `x cmp y` where `x` and `y` are of
   *  a given integer type (`int` or `long`).
   *
   *  An `IntComparison` has an integer type (`int` or `long`), a signedness
   *  (signed or unsigned) and a set of relationships (`LT`, `EQ` and `GT`).
   *  The latter forms a *disjunction*. `x cmp y` iff the relationship between
   *  `x` and `y` exists in the set.
   *
   *  For example, `<=` is represented by `LT | EQ`. Perhaps more surprisingly,
   *  `!=` is represented by `LT | GT`.
   *
   *  `==` and `!=` are sign-agnostic. They admit both signednesses. Their
   *  `isSigned` method returns `true`.
   *
   *  An `IntComparison` can represent a tautology (when all 3 of `LT`, `EQ`
   *  and `GT` are in the relationship set) or a contradiction (when the
   *  relationship set is empty).
   */
  private final class IntComparison(private val bits: Int) extends AnyVal {
    import IntComparison._
    import BinaryOp._

    override def toString(): String =
      s"IntComparison(${bits.toBinaryString})"

    private def hasAnyFlag(flags: Int): Boolean = (bits & flags) != 0
    private def hasAllFlags(flags: Int): Boolean = (bits & flags) == flags
    private def hasFlag(flag: Int): Boolean = hasAnyFlag(flag)

    def isCompatibleWith(that: IntComparison): Boolean = {
      (this.isSigned == that.isSigned || this.isSignAgnostic || that.isSignAgnostic) &&
      ((this.bits & LongOp) == (that.bits & LongOp))
    }

    def isLongOp: Boolean = hasFlag(LongOp)

    def toIntOp: IntComparison = new IntComparison(bits & ~LongOp)

    def isSignAgnostic: Boolean = hasLT == hasGT
    def isSigned: Boolean = !hasFlag(Unsigned)

    def signBit: Long =
      if (isLongOp) Long.MinValue
      else Int.MinValue.toLong

    /** The minimum value of the integer type according to the operator's signedness.
     *
     *  Calling this method for operators that can have both signednesses
     *  (typically for `this.isEQOrNE`) is not advisable, but is not forbidden.
     *  In that case, the *signed* minimum value is used.
     */
    def minValue: Long = {
      if (isSigned)
        if (isLongOp) Long.MinValue else Int.MinValue.toLong
      else
        0L
    }

    /** The maximum value of the integer type according to the operator's signedness.
     *
     *  Calling this method for operators that can have both signednesses
     *  (typically for `this.isEQOrNE`) is not advisable, but is not forbidden.
     *  In that case, the *signed* maximum value is used.
     */
    def maxValue: Long = {
      if (isSigned)
        if (isLongOp) Long.MaxValue else Int.MaxValue.toLong
      else
        if (isLongOp) -1L else Integer.toUnsignedLong(-1)
    }

    def rels: Int = bits & RelationshipsMask

    def withRels(rels: Int): IntComparison = {
      val result1 = new IntComparison((bits & ~RelationshipsMask) | rels)
      if (result1.isSignAgnostic)
        new IntComparison(result1.bits & ~Unsigned) // force sign-agnostic not to have the Unsigned bit
      else
        result1
    }

    def hasLT: Boolean = hasFlag(LT)
    def hasGT: Boolean = hasFlag(GT)
    def hasEQ: Boolean = hasFlag(EQ)

    def isEQOrNE: Boolean = rels == Rels_== || rels == Rels_!=

    def isAlwaysTrue: Boolean = hasAllFlags(RelationshipsMask)
    def isAlwaysFalse: Boolean = !hasAnyFlag(RelationshipsMask)

    /** Returns `negated` such that `x negated y` iff `!(x this y)`. */
    def unary_! : IntComparison = // scalastyle:ignore
      new IntComparison(this.bits ^ RelationshipsMask)

    /** Returns `combined` such that `x combined y` iff `x this y || x that y`.
     *
     *  Precondition: `this.isCompatibleWith(that)` must be `true`.
     */
    def |(that: IntComparison): IntComparison =
      new IntComparison(this.bits | that.bits)

    /** Returns `combined` such that `x combined y` iff `x this y && x that y`.
     *
     *  Precondition: `this.isCompatibleWith(that)` must be `true`.
     */
    def &(that: IntComparison): IntComparison =
      new IntComparison((this.bits & that.bits) | ((this.bits | that.bits) & Unsigned))

    /** Returns `flipped` such that `x flipped y` iff `y this x`. */
    def flipped: IntComparison = {
      val lt = (bits & GT) >>> (GTShift - LTShift)
      val gt = (bits & LT) << (GTShift - LTShift)
      new IntComparison((bits & ~(LT | GT)) | lt | gt)
    }

    /** Returns an operator with the same integer type and relationships as `this`,
     *  but of the opposite signedness.
     *
     *  If this operator is sign-agnostic, returns `this`.
     */
    def otherSignedness: IntComparison =
      if (isSignAgnostic) this
      else new IntComparison(bits ^ Unsigned)

    /** Applies this operator to the given constant values. */
    def apply(lhs: Long, rhs: Long): Boolean = {
      val cmp =
        if (isSigned) java.lang.Long.compare(lhs, rhs)
        else java.lang.Long.compareUnsigned(lhs, rhs)
      (hasFlag(LT) && cmp < 0) ||
      (hasFlag(GT) && cmp > 0) ||
      (hasFlag(EQ) && cmp == 0)
    }

    /** Returns the `BinaryOp.Code` implementing this comparison.
     *
     *  Precondition: this operator is neither a tautology nor a contradiction.
     */
    def toBinaryOpCode: BinaryOp.Code = {
      (rels: @switch) match {
        case Rels_== =>
          if (isLongOp) Long_== else Int_==
        case Rels_!= =>
          if (isLongOp) Long_!= else Int_!=
        case Rels_< =>
          if (isSigned)
            if (isLongOp) Long_< else Int_<
          else
            if (isLongOp) Long_unsigned_< else Int_unsigned_<
        case Rels_<= =>
          if (isSigned)
            if (isLongOp) Long_<= else Int_<=
          else
            if (isLongOp) Long_unsigned_<= else Int_unsigned_<=
        case Rels_> =>
          if (isSigned)
            if (isLongOp) Long_> else Int_>
          else
            if (isLongOp) Long_unsigned_> else Int_unsigned_>
        case Rels_>= =>
          if (isSigned)
            if (isLongOp) Long_>= else Int_>=
          else
            if (isLongOp) Long_unsigned_>= else Int_unsigned_>=
        case _ =>
          throw new IllegalArgumentException(
              s"Cannot create binary op for tautological or contradictory comparison ${this}")
      }
    }

    def makeLit(x: Long)(implicit pos: Position): PreTransTree =
      PreTransLit(if (isLongOp) LongLiteral(x) else IntLiteral(x.toInt))

    def extractLit(x: Literal): Long = (x: @unchecked) match {
      case IntLiteral(value)  => if (isSigned) value.toLong else Integer.toUnsignedLong(value)
      case LongLiteral(value) => value
    }
  }

  private object IntComparison {
    import BinaryOp._

    private final val EQShift = 0
    private final val EQ = 1 << EQShift

    private final val LTShift = 1
    private final val LT = 1 << LTShift

    private final val GTShift = 2 // note that `flipped` relies on `GTShift > LTShift`
    private final val GT = 1 << GTShift

    private final val LongOpShift = 3
    private final val LongOp = 1 << LongOpShift

    private final val UnsignedShift = 4
    private final val Unsigned = 1 << UnsignedShift

    private final val RelationshipsMask = EQ | LT | GT

    final val Rels_== = EQ
    final val Rels_!= = LT | GT
    final val Rels_< = LT
    final val Rels_<= = LT | EQ
    final val Rels_> = GT
    final val Rels_>= = GT | EQ

    def apply(op: BinaryOp.Code): IntComparison = unapply(op).get

    def unapply(op: BinaryOp.Code): Option[IntComparison] = (op: @switch) match {
      case Int_== => Some(new IntComparison(Rels_==))
      case Int_!= => Some(new IntComparison(Rels_!=))

      case Int_<  => Some(new IntComparison(Rels_<))
      case Int_<= => Some(new IntComparison(Rels_<=))
      case Int_>  => Some(new IntComparison(Rels_>))
      case Int_>= => Some(new IntComparison(Rels_>=))

      case Long_== => Some(new IntComparison(LongOp | Rels_==))
      case Long_!= => Some(new IntComparison(LongOp | Rels_!=))

      case Long_<  => Some(new IntComparison(LongOp | Rels_<))
      case Long_<= => Some(new IntComparison(LongOp | Rels_<=))
      case Long_>  => Some(new IntComparison(LongOp | Rels_>))
      case Long_>= => Some(new IntComparison(LongOp | Rels_>=))

      case Int_unsigned_<  => Some(new IntComparison(Unsigned | Rels_<))
      case Int_unsigned_<= => Some(new IntComparison(Unsigned | Rels_<=))
      case Int_unsigned_>  => Some(new IntComparison(Unsigned | Rels_>))
      case Int_unsigned_>= => Some(new IntComparison(Unsigned | Rels_>=))

      case Long_unsigned_<  => Some(new IntComparison(LongOp | Unsigned | Rels_<))
      case Long_unsigned_<= => Some(new IntComparison(LongOp | Unsigned | Rels_<=))
      case Long_unsigned_>  => Some(new IntComparison(LongOp | Unsigned | Rels_>))
      case Long_unsigned_>= => Some(new IntComparison(LongOp | Unsigned | Rels_>=))

      case _ => None
    }
  }

  private def exceptionMsg(debugID: String,
      attemptedInlining: List[AbstractMethodID], cause: Throwable) = {
    val buf = new StringBuilder()

    buf.append(s"The Scala.js optimizer crashed while optimizing $debugID: $cause")
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

  class OptimizeException(val debugID: String,
      val attemptedInlining: List[AbstractMethodID], cause: Throwable
  ) extends Exception(exceptionMsg(debugID, attemptedInlining, cause), cause)

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

    /** Reserves the provided name to not be allocated.
     *
     *  May only be called on a "cleared" instance (i.e. [[freshName]] has not
     *  been called yet or clear has just been called).
     */
    def reserve(name: N): Unit =
      usedNamesToNextCounter = usedNamesToNextCounter.updated(name, 1)

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
      override def freshName(base: LocalName): LocalName =
        super.freshName(if (base.isThis) LocalThisNameForFresh else base)

      protected def nameWithSuffix(name: LocalName, suffix: String): LocalName =
        name.withSuffix(suffix)
    }

    private val InitialLabelMap: Map[LabelName, Int] =
      EmitterReservedJSIdentifiers.map(i => LabelName(i) -> 1).toMap

    final class Label extends FreshNameAllocator[LabelName](InitialLabelMap) {
      protected def nameWithSuffix(name: LabelName, suffix: String): LabelName =
        name.withSuffix(suffix)
    }

    private val InitialFieldMap: Map[SimpleFieldName, Int] =
      Map.empty

    final class Field extends FreshNameAllocator[SimpleFieldName](InitialFieldMap) {
      protected def nameWithSuffix(name: SimpleFieldName, suffix: String): SimpleFieldName =
        name.withSuffix(suffix)
    }

    final class Snapshot[N <: Name] private[FreshNameAllocator] (
        private[FreshNameAllocator] val usedNamesToNextCounter: Map[N, Int])
  }

  def originalNameForFresh(base: Name, originalName: OriginalName,
      freshName: Name): OriginalName = {
    if (originalName.isDefined || (freshName eq base)) originalName
    else if (base eq LocalName.This) thisOriginalName
    else OriginalName(base)
  }

  private final case class IsUsed(count: Int) {
    def isUsed: Boolean = count > 0

    lazy val inc: IsUsed = IsUsed(count + 1)
  }

  private val Unused: IsUsed = IsUsed(count = 0)
  private val UsedAtLeastOnce: IsUsed = Unused.inc

  private sealed abstract class EvalContextInsertion[+A] {
    import EvalContextInsertion._

    def mapOrKeepGoing[B](f: A => B): EvalContextInsertion[B] = this match {
      case Success(a)        => Success(f(a))
      case NotFoundPureSoFar => NotFoundPureSoFar
      case Failed            => Failed
    }

    def mapOrKeepGoingIf[B](f: A => B)(keepGoingIf: => Boolean): EvalContextInsertion[B] = this match {
      case Success(a)        => Success(f(a))
      case NotFoundPureSoFar => if (keepGoingIf) NotFoundPureSoFar else Failed
      case Failed            => Failed
    }

    def mapOrFailed[B](f: A => B): EvalContextInsertion[B] = this match {
      case Success(a) => Success(f(a))
      case _          => Failed
    }
  }

  private object EvalContextInsertion {
    final case class Success[+A](result: A) extends EvalContextInsertion[A]
    case object Failed extends EvalContextInsertion[Nothing]
    case object NotFoundPureSoFar extends EvalContextInsertion[Nothing]
  }

}
