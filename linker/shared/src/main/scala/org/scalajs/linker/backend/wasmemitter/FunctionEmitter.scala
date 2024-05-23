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

package org.scalajs.linker.backend.wasmemitter

import scala.annotation.switch

import scala.collection.mutable

import org.scalajs.ir.{ClassKind, OriginalName, Position, UTF8String}
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Identitities => wanme}
import org.scalajs.linker.backend.webassembly.{Types => watpe}
import org.scalajs.linker.backend.webassembly.Types.{FunctionType => Sig}

import EmbeddedConstants._
import SWasmGen._
import VarGen._
import TypeTransformer._

object FunctionEmitter {

  /** Whether to use the legacy `try` instruction to implement `TryCatch`.
   *
   *  Support for catching JS exceptions was only added to `try_table` in V8 12.5 from April 2024.
   *  While waiting for Node.js to catch up with V8, we use `try` to implement our `TryCatch`.
   *
   *  We use this "fixed configuration option" to keep the code that implements `TryCatch` using
   *  `try_table` in the codebase, as code that is actually compiled, so that refactorings apply to
   *  it as well. It also makes it easier to manually experiment with the new `try_table` encoding,
   *  which is available in Chrome since v125.
   *
   *  Note that we use `try_table` regardless to implement `TryFinally`. Its `catch_all_ref` handler
   *  is perfectly happy to catch and rethrow JavaScript exception in Node.js 22. Duplicating that
   *  implementation for `try` would be a nightmare, given how complex it is already.
   */
  private final val UseLegacyExceptionsForTryCatch = true

  def emitFunction(
      functionID: wanme.FunctionID,
      originalName: OriginalName,
      enclosingClassName: Option[ClassName],
      captureParamDefs: Option[List[ParamDef]],
      receiverType: Option[watpe.Type],
      paramDefs: List[ParamDef],
      restParam: Option[ParamDef],
      body: Tree,
      resultType: Type
  )(implicit ctx: WasmContext, pos: Position): Unit = {
    val emitter = prepareEmitter(
      functionID,
      originalName,
      enclosingClassName,
      captureParamDefs,
      preSuperVarDefs = None,
      hasNewTarget = false,
      receiverType,
      paramDefs ::: restParam.toList,
      transformResultType(resultType)
    )
    emitter.genBody(body, resultType)
    emitter.fb.buildAndAddToModule()
  }

  def emitJSConstructorFunctions(
      preSuperStatsFunctionID: wanme.FunctionID,
      superArgsFunctionID: wanme.FunctionID,
      postSuperStatsFunctionID: wanme.FunctionID,
      enclosingClassName: ClassName,
      jsClassCaptures: List[ParamDef],
      ctor: JSConstructorDef
  )(implicit ctx: WasmContext): Unit = {
    implicit val pos = ctor.pos

    val allCtorParams = ctor.args ::: ctor.restParam.toList
    val ctorBody = ctor.body

    // Compute the pre-super environment
    val preSuperDecls = ctorBody.beforeSuper.collect { case varDef: VarDef =>
      varDef
    }

    // Build the `preSuperStats` function
    locally {
      val preSuperEnvStructTypeID = ctx.getClosureDataStructType(preSuperDecls.map(_.vtpe))
      val preSuperEnvType = watpe.RefType(preSuperEnvStructTypeID)

      val emitter = prepareEmitter(
        preSuperStatsFunctionID,
        OriginalName(UTF8String("preSuperStats.") ++ enclosingClassName.encoded),
        Some(enclosingClassName),
        Some(jsClassCaptures),
        preSuperVarDefs = None,
        hasNewTarget = true,
        receiverType = None,
        allCtorParams,
        List(preSuperEnvType)
      )

      emitter.genBlockStats(ctorBody.beforeSuper) {
        // Build and return the preSuperEnv struct
        for (varDef <- preSuperDecls)
          emitter.fb += wa.LocalGet(emitter.lookupLocalAssertLocalStorage(varDef.name.name))
        emitter.fb += wa.StructNew(preSuperEnvStructTypeID)
      }

      emitter.fb.buildAndAddToModule()
    }

    // Build the `superArgs` function
    locally {
      val emitter = prepareEmitter(
        superArgsFunctionID,
        OriginalName(UTF8String("superArgs.") ++ enclosingClassName.encoded),
        Some(enclosingClassName),
        Some(jsClassCaptures),
        Some(preSuperDecls),
        hasNewTarget = true,
        receiverType = None,
        allCtorParams,
        List(watpe.RefType.anyref) // a js.Array
      )
      emitter.genBody(JSArrayConstr(ctorBody.superCall.args), AnyType)
      emitter.fb.buildAndAddToModule()
    }

    // Build the `postSuperStats` function
    locally {
      val emitter = prepareEmitter(
        postSuperStatsFunctionID,
        OriginalName(UTF8String("postSuperStats.") ++ enclosingClassName.encoded),
        Some(enclosingClassName),
        Some(jsClassCaptures),
        Some(preSuperDecls),
        hasNewTarget = true,
        receiverType = Some(watpe.RefType.anyref),
        allCtorParams,
        List(watpe.RefType.anyref)
      )
      emitter.genBody(Block(ctorBody.afterSuper), AnyType)
      emitter.fb.buildAndAddToModule()
    }
  }

  private def prepareEmitter(
      functionID: wanme.FunctionID,
      originalName: OriginalName,
      enclosingClassName: Option[ClassName],
      captureParamDefs: Option[List[ParamDef]],
      preSuperVarDefs: Option[List[VarDef]],
      hasNewTarget: Boolean,
      receiverType: Option[watpe.Type],
      paramDefs: List[ParamDef],
      resultTypes: List[watpe.Type]
  )(implicit ctx: WasmContext, pos: Position): FunctionEmitter = {
    val fb = new FunctionBuilder(ctx.moduleBuilder, functionID, originalName, pos)

    def addCaptureLikeParamListAndMakeEnv(
        captureParamName: String,
        captureLikes: Option[List[(LocalName, Type)]]
    ): Env = {
      captureLikes match {
        case None =>
          Map.empty

        case Some(captureLikes) =>
          val dataStructTypeID = ctx.getClosureDataStructType(captureLikes.map(_._2))
          val param = fb.addParam(captureParamName, watpe.RefType(dataStructTypeID))
          val env: Env = captureLikes.zipWithIndex.map { case (captureLike, idx) =>
            val storage = VarStorage.StructField(
              param,
              dataStructTypeID,
              genFieldID.captureParam(idx)
            )
            captureLike._1 -> storage
          }.toMap
          env
      }
    }

    val captureParamsEnv = addCaptureLikeParamListAndMakeEnv(
      "__captureData",
      captureParamDefs.map(_.map(p => p.name.name -> p.ptpe))
    )

    val preSuperEnvEnv = addCaptureLikeParamListAndMakeEnv(
      "__preSuperEnv",
      preSuperVarDefs.map(_.map(p => p.name.name -> p.vtpe))
    )

    val newTargetStorage = if (!hasNewTarget) {
      None
    } else {
      val newTargetParam = fb.addParam(newTargetOriginalName, watpe.RefType.anyref)
      Some(VarStorage.Local(newTargetParam))
    }

    val receiverStorage = receiverType.map { tpe =>
      val receiverParam = fb.addParam(receiverOriginalName, tpe)
      VarStorage.Local(receiverParam)
    }

    val normalParamsEnv = paramDefs.map { paramDef =>
      val param = fb.addParam(
        paramDef.originalName.orElse(paramDef.name.name),
        transformLocalType(paramDef.ptpe)
      )
      paramDef.name.name -> VarStorage.Local(param)
    }

    val fullEnv = captureParamsEnv ++ preSuperEnvEnv ++ normalParamsEnv

    fb.setResultTypes(resultTypes)

    new FunctionEmitter(
      fb,
      enclosingClassName,
      newTargetStorage,
      receiverStorage,
      fullEnv
    )
  }

  private val ObjectRef = ClassRef(ObjectClass)
  private val BoxedStringRef = ClassRef(BoxedStringClass)
  private val toStringMethodName = MethodName("toString", Nil, BoxedStringRef)
  private val equalsMethodName = MethodName("equals", List(ObjectRef), BooleanRef)
  private val compareToMethodName = MethodName("compareTo", List(ObjectRef), IntRef)

  private val CharSequenceClass = ClassName("java.lang.CharSequence")
  private val ComparableClass = ClassName("java.lang.Comparable")
  private val JLNumberClass = ClassName("java.lang.Number")

  private val newTargetOriginalName = OriginalName("new.target")
  private val receiverOriginalName = OriginalName("this")

  private sealed abstract class VarStorage

  private object VarStorage {
    final case class Local(localID: wanme.LocalID) extends VarStorage

    final case class StructField(structLocalID: wanme.LocalID,
        structTypeID: wanme.TypeID, fieldID: wanme.FieldID)
        extends VarStorage
  }

  private type Env = Map[LocalName, VarStorage]

  private final class ClosureFunctionID(debugName: OriginalName) extends wanme.FunctionID {
    override def toString(): String = s"ClosureFunctionID(${debugName.toString()})"
  }
}

private class FunctionEmitter private (
    val fb: FunctionBuilder,
    enclosingClassName: Option[ClassName],
    _newTargetStorage: Option[FunctionEmitter.VarStorage.Local],
    _receiverStorage: Option[FunctionEmitter.VarStorage.Local],
    paramsEnv: FunctionEmitter.Env
)(implicit ctx: WasmContext) {
  import FunctionEmitter._

  private var innerFuncIdx: Int = 0
  private var currentEnv: Env = paramsEnv

  private def newTargetStorage: VarStorage.Local =
    _newTargetStorage.getOrElse(throw new Error("Cannot access new.target in this context."))

  private def receiverStorage: VarStorage.Local =
    _receiverStorage.getOrElse(throw new Error("Cannot access to the receiver in this context."))

  private def withNewLocal[A](name: LocalName, originalName: OriginalName, tpe: watpe.Type)(
      body: wanme.LocalID => A
  ): A = {
    val savedEnv = currentEnv
    val local = fb.addLocal(originalName.orElse(name), tpe)
    currentEnv = currentEnv.updated(name, VarStorage.Local(local))
    try body(local)
    finally currentEnv = savedEnv
  }

  private def lookupLocal(name: LocalName): VarStorage = {
    currentEnv.getOrElse(
      name, {
        throw new AssertionError(s"Cannot find binding for '${name.nameString}'")
      }
    )
  }

  private def lookupLocalAssertLocalStorage(name: LocalName): wanme.LocalID = {
    (lookupLocal(name): @unchecked) match {
      case VarStorage.Local(local) => local
    }
  }

  private def addSyntheticLocal(tpe: watpe.Type): wanme.LocalID =
    fb.addLocal(NoOriginalName, tpe)

  private def genInnerFuncOriginalName(): OriginalName = {
    if (fb.functionOriginalName.isEmpty) {
      NoOriginalName
    } else {
      val innerName = OriginalName(fb.functionOriginalName.get ++ UTF8String("__c" + innerFuncIdx))
      innerFuncIdx += 1
      innerName
    }
  }

  private def markPosition(tree: Tree): Unit =
    fb += wa.PositionMark(tree.pos)

  def genBody(tree: Tree, expectedType: Type): Unit =
    genTree(tree, expectedType)

  def genTrees(trees: List[Tree], expectedTypes: List[Type]): Unit = {
    for ((tree, expectedType) <- trees.zip(expectedTypes))
      genTree(tree, expectedType)
  }

  def genTreeAuto(tree: Tree): Unit =
    genTree(tree, tree.tpe)

  def genTree(tree: Tree, expectedType: Type): Unit = {
    val generatedType: Type = tree match {
      case t: Literal             => genLiteral(t, expectedType)
      case t: UnaryOp             => genUnaryOp(t)
      case t: BinaryOp            => genBinaryOp(t)
      case t: VarRef              => genVarRef(t)
      case t: LoadModule          => genLoadModule(t)
      case t: StoreModule         => genStoreModule(t)
      case t: This                => genThis(t)
      case t: ApplyStatically     => genApplyStatically(t)
      case t: Apply               => genApply(t)
      case t: ApplyStatic         => genApplyStatic(t)
      case t: ApplyDynamicImport  => genApplyDynamicImport(t)
      case t: IsInstanceOf        => genIsInstanceOf(t)
      case t: AsInstanceOf        => genAsInstanceOf(t)
      case t: GetClass            => genGetClass(t)
      case t: Block               => genBlock(t, expectedType)
      case t: Labeled             => unwinding.genLabeled(t, expectedType)
      case t: Return              => unwinding.genReturn(t)
      case t: Select              => genSelect(t)
      case t: SelectStatic        => genSelectStatic(t)
      case t: Assign              => genAssign(t)
      case t: VarDef              => genVarDef(t)
      case t: New                 => genNew(t)
      case t: If                  => genIf(t, expectedType)
      case t: While               => genWhile(t)
      case t: ForIn               => genForIn(t)
      case t: TryCatch            => genTryCatch(t, expectedType)
      case t: TryFinally          => unwinding.genTryFinally(t, expectedType)
      case t: Throw               => genThrow(t)
      case t: Match               => genMatch(t, expectedType)
      case t: Debugger            => NoType // ignore
      case t: Skip                => NoType
      case t: Clone               => genClone(t)
      case t: IdentityHashCode    => genIdentityHashCode(t)
      case t: WrapAsThrowable     => genWrapAsThrowable(t)
      case t: UnwrapFromThrowable => genUnwrapFromThrowable(t)

      // JavaScript expressions
      case t: JSNew                => genJSNew(t)
      case t: JSSelect             => genJSSelect(t)
      case t: JSFunctionApply      => genJSFunctionApply(t)
      case t: JSMethodApply        => genJSMethodApply(t)
      case t: JSImportCall         => genJSImportCall(t)
      case t: JSImportMeta         => genJSImportMeta(t)
      case t: LoadJSConstructor    => genLoadJSConstructor(t)
      case t: LoadJSModule         => genLoadJSModule(t)
      case t: SelectJSNativeMember => genSelectJSNativeMember(t)
      case t: JSDelete             => genJSDelete(t)
      case t: JSUnaryOp            => genJSUnaryOp(t)
      case t: JSBinaryOp           => genJSBinaryOp(t)
      case t: JSArrayConstr        => genJSArrayConstr(t)
      case t: JSObjectConstr       => genJSObjectConstr(t)
      case t: JSGlobalRef          => genJSGlobalRef(t)
      case t: JSTypeOfGlobalRef    => genJSTypeOfGlobalRef(t)
      case t: JSLinkingInfo        => genJSLinkingInfo(t)
      case t: Closure              => genClosure(t)

      // array
      case t: ArrayLength => genArrayLength(t)
      case t: NewArray    => genNewArray(t)
      case t: ArraySelect => genArraySelect(t)
      case t: ArrayValue  => genArrayValue(t)

      // Non-native JS classes
      case t: CreateJSClass     => genCreateJSClass(t)
      case t: JSPrivateSelect   => genJSPrivateSelect(t)
      case t: JSSuperSelect     => genJSSuperSelect(t)
      case t: JSSuperMethodCall => genJSSuperMethodCall(t)
      case t: JSNewTarget       => genJSNewTarget(t)

      case _: RecordSelect | _: RecordValue | _: Transient | _: JSSuperConstructorCall =>
        throw new AssertionError(s"Invalid tree: $tree")
    }

    genAdapt(generatedType, expectedType)
  }

  private def genAdapt(generatedType: Type, expectedType: Type): Unit = {
    (generatedType, expectedType) match {
      case _ if generatedType == expectedType =>
        ()
      case (NothingType, _) =>
        ()
      case (_, NoType) =>
        fb += wa.Drop
      case (primType: PrimTypeWithRef, _) =>
        // box
        primType match {
          case NullType =>
            ()
          case CharType =>
            /* `char` and `long` are opaque to JS in the Scala.js semantics.
             * We implement them with real Wasm classes following the correct
             * vtable. Upcasting wraps a primitive into the corresponding class.
             */
            genBox(watpe.Int32, SpecialNames.CharBoxClass)
          case LongType =>
            genBox(watpe.Int64, SpecialNames.LongBoxClass)
          case NoType | NothingType =>
            throw new AssertionError(s"Unexpected adaptation from $primType to $expectedType")
          case _ =>
            /* Calls a `bX` helper. Most of them are of the form
             *   bX: (x) => x
             * at the JavaScript level, but with a primType->anyref Wasm type.
             * For example, for `IntType`, `bI` has type `i32 -> anyref`. This
             * asks the JS host to turn a primitive `i32` into its generic
             * representation, which we can store in an `anyref`.
             */
            fb += wa.Call(genFunctionID.box(primType.primRef))
        }
      case _ =>
        ()
    }
  }

  private def genAssign(t: Assign): Type = {
    t.lhs match {
      case sel: Select =>
        val className = sel.field.name.className
        val classInfo = ctx.getClassInfo(className)

        // For Select, the receiver can never be a hijacked class, so we can use genTreeAuto
        genTreeAuto(sel.qualifier)

        if (!classInfo.hasInstances) {
          /* The field may not exist in that case, and we cannot look it up.
           * However we necessarily have a `null` receiver if we reach this
           * point, so we can trap as NPE.
           */
          fb += wa.Unreachable
        } else {
          genTree(t.rhs, t.lhs.tpe)
          fb += wa.StructSet(
            genTypeID.forClass(className),
            genFieldID.forClassInstanceField(sel.field.name)
          )
        }

      case sel: SelectStatic =>
        val fieldName = sel.field.name
        val globalID = genGlobalID.forStaticField(fieldName)

        genTree(t.rhs, sel.tpe)
        fb += wa.GlobalSet(globalID)

        // Update top-level export mirrors
        val classInfo = ctx.getClassInfo(fieldName.className)
        val mirrors = classInfo.staticFieldMirrors.getOrElse(fieldName, Nil)
        for (exportedName <- mirrors) {
          fb += wa.GlobalGet(globalID)
          fb += wa.Call(genFunctionID.forTopLevelExportSetter(exportedName))
        }

      case sel: ArraySelect =>
        genTreeAuto(sel.array)
        sel.array.tpe match {
          case ArrayType(arrayTypeRef) =>
            // Get the underlying array; implicit trap on null
            fb += wa.StructGet(
              genTypeID.forArrayClass(arrayTypeRef),
              genFieldID.objStruct.arrayUnderlying
            )
            genTree(sel.index, IntType)
            genTree(t.rhs, sel.tpe)
            fb += wa.ArraySet(genTypeID.underlyingOf(arrayTypeRef))
          case NothingType =>
            // unreachable
            ()
          case NullType =>
            fb += wa.Unreachable
          case _ =>
            throw new IllegalArgumentException(
              s"ArraySelect.array must be an array type, but has type ${sel.array.tpe}"
            )
        }

      case sel: JSPrivateSelect =>
        genTree(sel.qualifier, AnyType)
        fb += wa.GlobalGet(genGlobalID.forJSPrivateField(sel.field.name))
        genTree(t.rhs, AnyType)
        fb += wa.Call(genFunctionID.jsSelectSet)

      case assign: JSSelect =>
        genTree(assign.qualifier, AnyType)
        genTree(assign.item, AnyType)
        genTree(t.rhs, AnyType)
        fb += wa.Call(genFunctionID.jsSelectSet)

      case assign: JSSuperSelect =>
        genTree(assign.superClass, AnyType)
        genTree(assign.receiver, AnyType)
        genTree(assign.item, AnyType)
        genTree(t.rhs, AnyType)
        fb += wa.Call(genFunctionID.jsSuperSet)

      case assign: JSGlobalRef =>
        fb ++= ctx.getConstantStringInstr(assign.name)
        genTree(t.rhs, AnyType)
        fb += wa.Call(genFunctionID.jsGlobalRefSet)

      case ref: VarRef =>
        lookupLocal(ref.ident.name) match {
          case VarStorage.Local(local) =>
            genTree(t.rhs, t.lhs.tpe)
            fb += wa.LocalSet(local)
          case VarStorage.StructField(structLocal, structTypeID, fieldID) =>
            fb += wa.LocalGet(structLocal)
            genTree(t.rhs, t.lhs.tpe)
            fb += wa.StructSet(structTypeID, fieldID)
        }

      case assign: RecordSelect =>
        throw new AssertionError(s"Invalid tree: $t")
    }

    NoType
  }

  private def genApply(t: Apply): Type = {
    t.receiver.tpe match {
      case NothingType =>
        genTree(t.receiver, NothingType)
        // nothing else to do; this is unreachable
        NothingType

      case NullType =>
        genTree(t.receiver, NullType)
        fb += wa.Unreachable // trap
        NothingType

      case _ if t.method.name.isReflectiveProxy =>
        genReflectiveCall(t)

      case _ =>
        val receiverClassName = t.receiver.tpe match {
          case prim: PrimType  => PrimTypeToBoxedClass(prim)
          case ClassType(cls)  => cls
          case AnyType         => ObjectClass
          case ArrayType(_)    => ObjectClass
          case tpe: RecordType => throw new AssertionError(s"Invalid receiver type $tpe")
        }
        val receiverClassInfo = ctx.getClassInfo(receiverClassName)

        val canUseStaticallyResolved = {
          receiverClassInfo.kind == ClassKind.HijackedClass ||
          t.receiver.tpe.isInstanceOf[ArrayType] ||
          receiverClassInfo.resolvedMethodInfos.get(t.method.name).exists(_.isEffectivelyFinal)
        }
        if (canUseStaticallyResolved) {
          genApplyStatically(
            ApplyStatically(t.flags, t.receiver, receiverClassName, t.method, t.args)(
              t.tpe
            )(
              t.pos
            )
          )
        } else {
          genApplyWithDispatch(t, receiverClassInfo)
        }
    }
  }

  private def genReflectiveCall(t: Apply): Type = {
    assert(t.method.name.isReflectiveProxy)
    val receiverLocalForDispatch =
      addSyntheticLocal(watpe.RefType.any)

    val proxyId = ctx.getReflectiveProxyId(t.method.name)
    val funcTypeID = ctx.tableFunctionType(t.method.name)

    fb.block(watpe.RefType.anyref) { done =>
      fb.block(watpe.RefType.any) { labelNotOurObject =>
        // arguments
        genTree(t.receiver, AnyType)
        fb += wa.RefAsNotNull
        fb += wa.LocalTee(receiverLocalForDispatch)
        genArgs(t.args, t.method.name)

        // Looks up the method to be (reflectively) called
        fb += wa.LocalGet(receiverLocalForDispatch)
        fb += wa.BrOnCastFail(
          labelNotOurObject,
          watpe.RefType.any,
          watpe.RefType(genTypeID.ObjectStruct)
        )
        fb += wa.StructGet(
          genTypeID.forClass(ObjectClass),
          genFieldID.objStruct.vtable
        )
        fb += wa.I32Const(proxyId)
        // `searchReflectiveProxy`: [typeData, i32] -> [(ref func)]
        fb += wa.Call(genFunctionID.searchReflectiveProxy)

        fb += wa.RefCast(watpe.RefType(watpe.HeapType(funcTypeID)))
        fb += wa.CallRef(funcTypeID)
        fb += wa.Br(done)
      } // labelNotFound
      fb += wa.Unreachable
      // TODO? reflective call on primitive types
      t.tpe
    }
    // done
  }

  /** Generates the code an `Apply` call that requires dynamic dispatch.
   *
   *  In that case, there is always at least a vtable/itable-based dispatch. It may also contain
   *  primitive-based dispatch if the receiver's type is an ancestor of a hijacked class.
   */
  private def genApplyWithDispatch(t: Apply,
      receiverClassInfo: WasmContext.ClassInfo): Type = {
    implicit val pos: Position = t.pos

    val receiverClassName = receiverClassInfo.name

    /* Similar to transformType(t.receiver.tpe), but:
     * - it is non-null,
     * - ancestors of hijacked classes are not treated specially,
     * - array types are treated as j.l.Object.
     *
     * This is used in the code paths where we have already ruled out `null`
     * values and primitive values (that implement hijacked classes).
     */
    val refTypeForDispatch: watpe.RefType = {
      if (receiverClassInfo.isInterface)
        watpe.RefType(genTypeID.ObjectStruct)
      else
        watpe.RefType(genTypeID.forClass(receiverClassName))
    }

    // A local for a copy of the receiver that we will use to resolve dispatch
    val receiverLocalForDispatch = addSyntheticLocal(refTypeForDispatch)

    /* Gen loading of the receiver and check that it is non-null.
     * After this codegen, the non-null receiver is on the stack.
     */
    def genReceiverNotNull(): Unit = {
      genTreeAuto(t.receiver)
      fb += wa.RefAsNotNull
    }

    /* Generates a resolved call to a method of a hijacked class.
     * Before this code gen, the stack must contain the receiver and the args.
     * After this code gen, the stack contains the result.
     */
    def genHijackedClassCall(hijackedClass: ClassName): Unit = {
      val funcID = genFunctionID.forMethod(MemberNamespace.Public, hijackedClass, t.method.name)
      fb += wa.Call(funcID)
    }

    if (!receiverClassInfo.hasInstances) {
      /* If the target class info does not have any instance, the only possible
       * for the receiver is `null`. We can therefore immediately trap for an
       * NPE. It is important to short-cut this path because the reachability
       * analysis may have dead-code eliminated the target method method
       * entirely, which means we do not know its signature and therefore
       * cannot emit the corresponding vtable/itable calls.
       */
      genTreeAuto(t.receiver)
      fb += wa.Unreachable // NPE
    } else if (!receiverClassInfo.isAncestorOfHijackedClass) {
      // Standard dispatch codegen
      genReceiverNotNull()
      fb += wa.LocalTee(receiverLocalForDispatch)
      genArgs(t.args, t.method.name)
      genTableDispatch(receiverClassInfo, t.method.name, receiverLocalForDispatch)
    } else {
      /* Here the receiver's type is an ancestor of a hijacked class (or `any`,
       * which is treated as `jl.Object`).
       *
       * We must emit additional dispatch for the possible primitive values.
       *
       * The overall structure of the generated code is as follows:
       *
       * block resultType $done
       *   block (ref any) $notOurObject
       *     load non-null receiver and args and store into locals
       *     reload copy of receiver
       *     br_on_cast_fail (ref any) (ref $targetRealClass) $notOurObject
       *     reload args
       *     generate standard table-based dispatch
       *     br $done
       *   end $notOurObject
       *   choose an implementation of a single hijacked class, or a JS helper
       *   reload args
       *   call the chosen implementation
       * end $done
       */

      assert(receiverClassInfo.kind != ClassKind.HijackedClass, receiverClassName)

      val resultType = transformResultType(t.tpe)

      fb.block(resultType) { labelDone =>
        def pushArgs(argsLocals: List[wanme.LocalID]): Unit =
          argsLocals.foreach(argLocal => fb += wa.LocalGet(argLocal))

        // First try the case where the value is one of our objects
        val argsLocals = fb.block(watpe.RefType.any) { labelNotOurObject =>
          // Load receiver and arguments and store them in temporary variables
          genReceiverNotNull()
          val argsLocals = if (t.args.isEmpty) {
            /* When there are no arguments, we can leave the receiver directly on
             * the stack instead of going through a local. We will still need a
             * local for the table-based dispatch, though.
             */
            Nil
          } else {
            val receiverLocal = addSyntheticLocal(watpe.RefType.any)

            fb += wa.LocalSet(receiverLocal)
            val argsLocals: List[wanme.LocalID] =
              for ((arg, typeRef) <- t.args.zip(t.method.name.paramTypeRefs)) yield {
                val tpe = ctx.inferTypeFromTypeRef(typeRef)
                genTree(arg, tpe)
                val localID = addSyntheticLocal(transformLocalType(tpe))
                fb += wa.LocalSet(localID)
                localID
              }
            fb += wa.LocalGet(receiverLocal)
            argsLocals
          }

          fb += wa.BrOnCastFail(labelNotOurObject, watpe.RefType.any, refTypeForDispatch)
          fb += wa.LocalTee(receiverLocalForDispatch)
          pushArgs(argsLocals)
          genTableDispatch(receiverClassInfo, t.method.name, receiverLocalForDispatch)
          fb += wa.Br(labelDone)

          argsLocals
        } // end block labelNotOurObject

        /* Now we have a value that is not one of our objects, so it must be
         * a JavaScript value whose representative class extends/implements the
         * receiver class. It may be a primitive instance of a hijacked class, or
         * any other value (whose representative class is therefore `jl.Object`).
         *
         * It is also *not* `char` or `long`, since those would reach
         * `genApplyNonPrim` in their boxed form, and therefore they are
         * "ourObject".
         *
         * The (ref any) is still on the stack.
         */

        if (t.method.name == toStringMethodName) {
          // By spec, toString() is special
          assert(argsLocals.isEmpty)
          fb += wa.Call(genFunctionID.jsValueToString)
        } else if (receiverClassName == JLNumberClass) {
          // the value must be a `number`, hence we can unbox to `double`
          genUnbox(DoubleType)
          pushArgs(argsLocals)
          genHijackedClassCall(BoxedDoubleClass)
        } else if (receiverClassName == CharSequenceClass) {
          // the value must be a `string`; it already has the right type
          pushArgs(argsLocals)
          genHijackedClassCall(BoxedStringClass)
        } else if (t.method.name == compareToMethodName) {
          /* The only method of jl.Comparable. Here the value can be a boolean,
           * a number or a string. We use `jsValueType` to dispatch to Wasm-side
           * implementations because they have to perform casts on their arguments.
           */
          assert(argsLocals.size == 1)

          val receiverLocal = addSyntheticLocal(watpe.RefType.any)
          fb += wa.LocalTee(receiverLocal)

          val jsValueTypeLocal = addSyntheticLocal(watpe.Int32)
          fb += wa.Call(genFunctionID.jsValueType)
          fb += wa.LocalTee(jsValueTypeLocal)

          fb.switch(Sig(List(watpe.Int32), Nil), Sig(Nil, List(watpe.Int32))) { () =>
            // scrutinee is already on the stack
          }(
            // case JSValueTypeFalse | JSValueTypeTrue =>
            List(JSValueTypeFalse, JSValueTypeTrue) -> { () =>
              // the jsValueTypeLocal is the boolean value, thanks to the chosen encoding
              fb += wa.LocalGet(jsValueTypeLocal)
              pushArgs(argsLocals)
              genHijackedClassCall(BoxedBooleanClass)
            },
            // case JSValueTypeString =>
            List(JSValueTypeString) -> { () =>
              fb += wa.LocalGet(receiverLocal)
              // no need to unbox for string
              pushArgs(argsLocals)
              genHijackedClassCall(BoxedStringClass)
            }
          ) { () =>
            // case _ (JSValueTypeNumber) =>
            fb += wa.LocalGet(receiverLocal)
            genUnbox(DoubleType)
            pushArgs(argsLocals)
            genHijackedClassCall(BoxedDoubleClass)
          }
        } else {
          /* It must be a method of j.l.Object and it can be any value.
           * hashCode() and equals() are overridden in all hijacked classes.
           * We use `identityHashCode` for `hashCode` and `Object.is` for `equals`,
           * as they coincide with the respective specifications (on purpose).
           * The other methods are never overridden and can be statically
           * resolved to j.l.Object.
           */
          pushArgs(argsLocals)
          t.method.name match {
            case SpecialNames.hashCodeMethodName =>
              fb += wa.Call(genFunctionID.identityHashCode)
            case `equalsMethodName` =>
              fb += wa.Call(genFunctionID.is)
            case _ =>
              genHijackedClassCall(ObjectClass)
          }
        }
      } // end block labelDone
    }

    if (t.tpe == NothingType)
      fb += wa.Unreachable

    t.tpe
  }

  /** Generates a vtable- or itable-based dispatch.
   *
   *  Before this code gen, the stack must contain the receiver and the args of the target method.
   *  In addition, the receiver must be available in the local `receiverLocalForDispatch`. The two
   *  occurrences of the receiver must have the type for dispatch.
   *
   *  After this code gen, the stack contains the result. If the result type is `NothingType`,
   *  `genTableDispatch` leaves the stack in an arbitrary state. It is up to the caller to insert an
   *  `unreachable` instruction when appropriate.
   */
  def genTableDispatch(receiverClassInfo: WasmContext.ClassInfo,
      methodName: MethodName, receiverLocalForDispatch: wanme.LocalID): Unit = {
    // Generates an itable-based dispatch.
    def genITableDispatch(): Unit = {
      fb += wa.LocalGet(receiverLocalForDispatch)
      fb += wa.StructGet(
        genTypeID.forClass(ObjectClass),
        genFieldID.objStruct.itables
      )
      fb += wa.I32Const(receiverClassInfo.itableIdx)
      fb += wa.ArrayGet(genTypeID.itables)
      fb += wa.RefCast(watpe.RefType(genTypeID.forITable(receiverClassInfo.name)))
      fb += wa.StructGet(
        genTypeID.forITable(receiverClassInfo.name),
        genFieldID.forMethodTableEntry(methodName)
      )
      fb += wa.CallRef(ctx.tableFunctionType(methodName))
    }

    // Generates a vtable-based dispatch.
    def genVTableDispatch(): Unit = {
      val receiverClassName = receiverClassInfo.name

      fb += wa.LocalGet(receiverLocalForDispatch)
      fb += wa.RefCast(watpe.RefType(genTypeID.forClass(receiverClassName)))
      fb += wa.StructGet(
        genTypeID.forClass(receiverClassName),
        genFieldID.objStruct.vtable
      )
      fb += wa.StructGet(
        genTypeID.forVTable(receiverClassName),
        genFieldID.forMethodTableEntry(methodName)
      )
      fb += wa.CallRef(ctx.tableFunctionType(methodName))
    }

    if (receiverClassInfo.isInterface)
      genITableDispatch()
    else
      genVTableDispatch()
  }

  private def genApplyStatically(t: ApplyStatically): Type = {
    t.receiver.tpe match {
      case NothingType =>
        genTree(t.receiver, NothingType)
        // nothing else to do; this is unreachable
        NothingType

      case NullType =>
        genTree(t.receiver, NullType)
        fb += wa.Unreachable // trap
        NothingType

      case _ =>
        val namespace = MemberNamespace.forNonStaticCall(t.flags)
        val targetClassName = {
          val classInfo = ctx.getClassInfo(t.className)
          if (!classInfo.isInterface && namespace == MemberNamespace.Public)
            classInfo.resolvedMethodInfos(t.method.name).ownerClass
          else
            t.className
        }

        BoxedClassToPrimType.get(targetClassName) match {
          case None =>
            genTree(t.receiver, ClassType(targetClassName))
            fb += wa.RefAsNotNull

          case Some(primReceiverType) =>
            if (t.receiver.tpe == primReceiverType) {
              genTreeAuto(t.receiver)
            } else {
              genTree(t.receiver, AnyType)
              fb += wa.RefAsNotNull
              genUnbox(primReceiverType)(t.pos)
            }
        }

        genArgs(t.args, t.method.name)

        val funcID = genFunctionID.forMethod(namespace, targetClassName, t.method.name)
        fb += wa.Call(funcID)
        if (t.tpe == NothingType)
          fb += wa.Unreachable
        t.tpe
    }
  }

  private def genApplyStatic(tree: ApplyStatic): Type = {
    genArgs(tree.args, tree.method.name)
    val namespace = MemberNamespace.forStaticCall(tree.flags)
    val funcID = genFunctionID.forMethod(namespace, tree.className, tree.method.name)
    fb += wa.Call(funcID)
    if (tree.tpe == NothingType)
      fb += wa.Unreachable
    tree.tpe
  }

  private def genApplyDynamicImport(tree: ApplyDynamicImport): Type = {
    // As long as we do not support multiple modules, this cannot happen
    throw new AssertionError(
        s"Unexpected $tree at ${tree.pos}; multiple modules are not supported yet")
  }

  private def genArgs(args: List[Tree], methodName: MethodName): Unit = {
    for ((arg, paramTypeRef) <- args.zip(methodName.paramTypeRefs)) {
      val paramType = ctx.inferTypeFromTypeRef(paramTypeRef)
      genTree(arg, paramType)
    }
  }

  private def genLiteral(l: Literal, expectedType: Type): Type = {
    if (expectedType == NoType) {
      /* Since all primitives are pure, we can always get rid of them.
       * This is mostly useful for the argument of `Return` nodes that target a
       * `Labeled` in statement position, since they must have a non-`void`
       * type in the IR but they get a `void` expected type.
       */
      expectedType
    } else {
      markPosition(l)

      l match {
        case BooleanLiteral(v) => fb += wa.I32Const(if (v) 1 else 0)
        case ByteLiteral(v)    => fb += wa.I32Const(v)
        case ShortLiteral(v)   => fb += wa.I32Const(v)
        case IntLiteral(v)     => fb += wa.I32Const(v)
        case CharLiteral(v)    => fb += wa.I32Const(v)
        case LongLiteral(v)    => fb += wa.I64Const(v)
        case FloatLiteral(v)   => fb += wa.F32Const(v)
        case DoubleLiteral(v)  => fb += wa.F64Const(v)

        case Undefined() =>
          fb += wa.GlobalGet(genGlobalID.undef)
        case Null() =>
          fb += wa.RefNull(watpe.HeapType.None)

        case StringLiteral(v) =>
          fb ++= ctx.getConstantStringInstr(v)

        case ClassOf(typeRef) =>
          typeRef match {
            case typeRef: NonArrayTypeRef =>
              genClassOfFromTypeData(getNonArrayTypeDataInstr(typeRef))

            case typeRef: ArrayTypeRef =>
              val typeDataType = watpe.RefType(genTypeID.typeData)
              val typeDataLocal = addSyntheticLocal(typeDataType)

              genLoadArrayTypeData(typeRef)
              fb += wa.LocalSet(typeDataLocal)
              genClassOfFromTypeData(wa.LocalGet(typeDataLocal))
          }
      }

      l.tpe
    }
  }

  private def getNonArrayTypeDataInstr(typeRef: NonArrayTypeRef): wa.Instr =
    wa.GlobalGet(genGlobalID.forVTable(typeRef))

  private def genLoadArrayTypeData(arrayTypeRef: ArrayTypeRef): Unit = {
    fb += getNonArrayTypeDataInstr(arrayTypeRef.base)
    fb += wa.I32Const(arrayTypeRef.dimensions)
    fb += wa.Call(genFunctionID.arrayTypeData)
  }

  private def genClassOfFromTypeData(loadTypeDataInstr: wa.Instr): Unit = {
    fb.block(watpe.RefType(genTypeID.ClassStruct)) { nonNullLabel =>
      // fast path first
      fb += loadTypeDataInstr
      fb += wa.StructGet(genTypeID.typeData, genFieldID.typeData.classOfValue)
      fb += wa.BrOnNonNull(nonNullLabel)
      // slow path
      fb += loadTypeDataInstr
      fb += wa.Call(genFunctionID.createClassOf)
    }
  }

  private def genSelect(sel: Select): Type = {
    val className = sel.field.name.className
    val classInfo = ctx.getClassInfo(className)

    // For Select, the receiver can never be a hijacked class, so we can use genTreeAuto
    genTreeAuto(sel.qualifier)

    markPosition(sel)

    if (!classInfo.hasInstances) {
      /* The field may not exist in that case, and we cannot look it up.
       * However we necessarily have a `null` receiver if we reach this point,
       * so we can trap as NPE.
       */
      fb += wa.Unreachable
    } else {
      fb += wa.StructGet(
        genTypeID.forClass(className),
        genFieldID.forClassInstanceField(sel.field.name)
      )
    }

    sel.tpe
  }

  private def genSelectStatic(tree: SelectStatic): Type = {
    markPosition(tree)
    fb += wa.GlobalGet(genGlobalID.forStaticField(tree.field.name))
    tree.tpe
  }

  private def genStoreModule(t: StoreModule): Type = {
    val className = enclosingClassName.getOrElse {
      throw new AssertionError(s"Cannot emit $t at ${t.pos} without enclosing class name")
    }

    genTreeAuto(This()(ClassType(className))(t.pos))

    markPosition(t)
    fb += wa.GlobalSet(genGlobalID.forModuleInstance(className))
    NoType
  }

  private def genLoadModule(t: LoadModule): Type = {
    markPosition(t)
    fb += wa.Call(genFunctionID.loadModule(t.className))
    t.tpe
  }

  private def genUnaryOp(unary: UnaryOp): Type = {
    import UnaryOp._

    genTreeAuto(unary.lhs)

    markPosition(unary)

    (unary.op: @switch) match {
      case Boolean_! =>
        fb += wa.I32Const(1)
        fb += wa.I32Xor

      // Widening conversions
      case CharToInt | ByteToInt | ShortToInt =>
        /* These are no-ops because they are all represented as i32's with the
         * right mathematical value.
         */
        ()
      case IntToLong =>
        fb += wa.I64ExtendI32S
      case IntToDouble =>
        fb += wa.F64ConvertI32S
      case FloatToDouble =>
        fb += wa.F64PromoteF32

      // Narrowing conversions
      case IntToChar =>
        fb += wa.I32Const(0xFFFF)
        fb += wa.I32And
      case IntToByte =>
        fb += wa.I32Extend8S
      case IntToShort =>
        fb += wa.I32Extend16S
      case LongToInt =>
        fb += wa.I32WrapI64
      case DoubleToInt =>
        fb += wa.I32TruncSatF64S
      case DoubleToFloat =>
        fb += wa.F32DemoteF64

      // Long <-> Double (neither widening nor narrowing)
      case LongToDouble =>
        fb += wa.F64ConvertI64S
      case DoubleToLong =>
        fb += wa.I64TruncSatF64S

      // Long -> Float (neither widening nor narrowing), introduced in 1.6
      case LongToFloat =>
        fb += wa.F32ConvertI64S

      // String.length, introduced in 1.11
      case String_length =>
        fb += wa.Call(genFunctionID.stringLength)
    }

    unary.tpe
  }

  private def genBinaryOp(binary: BinaryOp): Type = {
    def genLongShiftOp(shiftInstr: wa.Instr): Type = {
      genTree(binary.lhs, LongType)
      genTree(binary.rhs, IntType)
      markPosition(binary)
      fb += wa.I64ExtendI32S
      fb += shiftInstr
      LongType
    }

    def genThrowArithmeticException(): Unit = {
      implicit val pos = binary.pos
      val divisionByZeroEx = Throw(
        New(
          ArithmeticExceptionClass,
          MethodIdent(
            MethodName.constructor(List(ClassRef(BoxedStringClass)))
          ),
          List(StringLiteral("/ by zero"))
        )
      )
      genThrow(divisionByZeroEx)
    }

    def genDivModByConstant[T](isDiv: Boolean, rhsValue: T,
        const: T => wa.Instr, sub: wa.Instr, mainOp: wa.Instr)(
        implicit num: Numeric[T]): Type = {
      /* When we statically know the value of the rhs, we can avoid the
       * dynamic tests for division by zero and overflow. This is quite
       * common in practice.
       */

      val tpe = binary.tpe

      if (rhsValue == num.zero) {
        genTree(binary.lhs, tpe)
        markPosition(binary)
        genThrowArithmeticException()
        NothingType
      } else if (isDiv && rhsValue == num.fromInt(-1)) {
        /* MinValue / -1 overflows; it traps in Wasm but we need to wrap.
         * We rewrite as `0 - lhs` so that we do not need any test.
         */
        markPosition(binary)
        fb += const(num.zero)
        genTree(binary.lhs, tpe)
        markPosition(binary)
        fb += sub
        tpe
      } else {
        genTree(binary.lhs, tpe)
        markPosition(binary.rhs)
        fb += const(rhsValue)
        markPosition(binary)
        fb += mainOp
        tpe
      }
    }

    def genDivMod[T](isDiv: Boolean, const: T => wa.Instr, eqz: wa.Instr,
        eq: wa.Instr, sub: wa.Instr, mainOp: wa.Instr)(
        implicit num: Numeric[T]): Type = {
      /* Here we perform the same steps as in the static case, but using
       * value tests at run-time.
       */

      val tpe = binary.tpe
      val wasmType = transformType(tpe)

      val lhsLocal = addSyntheticLocal(wasmType)
      val rhsLocal = addSyntheticLocal(wasmType)
      genTree(binary.lhs, tpe)
      fb += wa.LocalSet(lhsLocal)
      genTree(binary.rhs, tpe)
      fb += wa.LocalTee(rhsLocal)

      markPosition(binary)

      fb += eqz
      fb.ifThen() {
        genThrowArithmeticException()
      }
      if (isDiv) {
        // Handle the MinValue / -1 corner case
        fb += wa.LocalGet(rhsLocal)
        fb += const(num.fromInt(-1))
        fb += eq
        fb.ifThenElse(wasmType) {
          // 0 - lhs
          fb += const(num.zero)
          fb += wa.LocalGet(lhsLocal)
          fb += sub
        } {
          // lhs / rhs
          fb += wa.LocalGet(lhsLocal)
          fb += wa.LocalGet(rhsLocal)
          fb += mainOp
        }
      } else {
        // lhs % rhs
        fb += wa.LocalGet(lhsLocal)
        fb += wa.LocalGet(rhsLocal)
        fb += mainOp
      }

      tpe
    }

    (binary.op: @switch) match {
      case BinaryOp.=== | BinaryOp.!== =>
        genEq(binary)

      case BinaryOp.String_+ =>
        genStringConcat(binary)

      case BinaryOp.Int_/ =>
        binary.rhs match {
          case IntLiteral(rhsValue) =>
            genDivModByConstant(isDiv = true, rhsValue, wa.I32Const(_), wa.I32Sub, wa.I32DivS)
          case _ =>
            genDivMod(isDiv = true, wa.I32Const(_), wa.I32Eqz, wa.I32Eq, wa.I32Sub, wa.I32DivS)
        }
      case BinaryOp.Int_% =>
        binary.rhs match {
          case IntLiteral(rhsValue) =>
            genDivModByConstant(isDiv = false, rhsValue, wa.I32Const(_), wa.I32Sub, wa.I32RemS)
          case _ =>
            genDivMod(isDiv = false, wa.I32Const(_), wa.I32Eqz, wa.I32Eq, wa.I32Sub, wa.I32RemS)
        }
      case BinaryOp.Long_/ =>
        binary.rhs match {
          case LongLiteral(rhsValue) =>
            genDivModByConstant(isDiv = true, rhsValue, wa.I64Const(_), wa.I64Sub, wa.I64DivS)
          case _ =>
            genDivMod(isDiv = true, wa.I64Const(_), wa.I64Eqz, wa.I64Eq, wa.I64Sub, wa.I64DivS)
        }
      case BinaryOp.Long_% =>
        binary.rhs match {
          case LongLiteral(rhsValue) =>
            genDivModByConstant(isDiv = false, rhsValue, wa.I64Const(_), wa.I64Sub, wa.I64RemS)
          case _ =>
            genDivMod(isDiv = false, wa.I64Const(_), wa.I64Eqz, wa.I64Eq, wa.I64Sub, wa.I64RemS)
        }

      case BinaryOp.Long_<< =>
        genLongShiftOp(wa.I64Shl)
      case BinaryOp.Long_>>> =>
        genLongShiftOp(wa.I64ShrU)
      case BinaryOp.Long_>> =>
        genLongShiftOp(wa.I64ShrS)

      /* Floating point remainders are specified by
       * https://262.ecma-international.org/#sec-numeric-types-number-remainder
       * which says that it is equivalent to the C library function `fmod`.
       * For `Float`s, we promote and demote to `Double`s.
       * `fmod` seems quite hard to correctly implement, so we delegate to a
       * JavaScript Helper.
       * (The naive function `x - trunc(x / y) * y` that we can find on the
       * Web does not work.)
       */
      case BinaryOp.Float_% =>
        genTree(binary.lhs, FloatType)
        fb += wa.F64PromoteF32
        genTree(binary.rhs, FloatType)
        fb += wa.F64PromoteF32
        markPosition(binary)
        fb += wa.Call(genFunctionID.fmod)
        fb += wa.F32DemoteF64
        FloatType
      case BinaryOp.Double_% =>
        genTree(binary.lhs, DoubleType)
        genTree(binary.rhs, DoubleType)
        markPosition(binary)
        fb += wa.Call(genFunctionID.fmod)
        DoubleType

      // New in 1.11
      case BinaryOp.String_charAt =>
        genTree(binary.lhs, StringType) // push the string
        genTree(binary.rhs, IntType) // push the index
        markPosition(binary)
        fb += wa.Call(genFunctionID.stringCharAt)
        CharType

      case _ =>
        genElementaryBinaryOp(binary)
    }
  }

  private def genEq(binary: BinaryOp): Type = {
    // TODO Optimize this when the operands have a better type than `any`
    genTree(binary.lhs, AnyType)
    genTree(binary.rhs, AnyType)

    markPosition(binary)

    fb += wa.Call(genFunctionID.is)

    if (binary.op == BinaryOp.!==) {
      fb += wa.I32Const(1)
      fb += wa.I32Xor
    }

    BooleanType
  }

  private def genElementaryBinaryOp(binary: BinaryOp): Type = {
    genTreeAuto(binary.lhs)
    genTreeAuto(binary.rhs)

    markPosition(binary)

    val operation = (binary.op: @switch) match {
      case BinaryOp.Boolean_== => wa.I32Eq
      case BinaryOp.Boolean_!= => wa.I32Ne
      case BinaryOp.Boolean_|  => wa.I32Or
      case BinaryOp.Boolean_&  => wa.I32And

      case BinaryOp.Int_+   => wa.I32Add
      case BinaryOp.Int_-   => wa.I32Sub
      case BinaryOp.Int_*   => wa.I32Mul
      case BinaryOp.Int_|   => wa.I32Or
      case BinaryOp.Int_&   => wa.I32And
      case BinaryOp.Int_^   => wa.I32Xor
      case BinaryOp.Int_<<  => wa.I32Shl
      case BinaryOp.Int_>>> => wa.I32ShrU
      case BinaryOp.Int_>>  => wa.I32ShrS
      case BinaryOp.Int_==  => wa.I32Eq
      case BinaryOp.Int_!=  => wa.I32Ne
      case BinaryOp.Int_<   => wa.I32LtS
      case BinaryOp.Int_<=  => wa.I32LeS
      case BinaryOp.Int_>   => wa.I32GtS
      case BinaryOp.Int_>=  => wa.I32GeS

      case BinaryOp.Long_+ => wa.I64Add
      case BinaryOp.Long_- => wa.I64Sub
      case BinaryOp.Long_* => wa.I64Mul
      case BinaryOp.Long_| => wa.I64Or
      case BinaryOp.Long_& => wa.I64And
      case BinaryOp.Long_^ => wa.I64Xor

      case BinaryOp.Long_== => wa.I64Eq
      case BinaryOp.Long_!= => wa.I64Ne
      case BinaryOp.Long_<  => wa.I64LtS
      case BinaryOp.Long_<= => wa.I64LeS
      case BinaryOp.Long_>  => wa.I64GtS
      case BinaryOp.Long_>= => wa.I64GeS

      case BinaryOp.Float_+ => wa.F32Add
      case BinaryOp.Float_- => wa.F32Sub
      case BinaryOp.Float_* => wa.F32Mul
      case BinaryOp.Float_/ => wa.F32Div

      case BinaryOp.Double_+ => wa.F64Add
      case BinaryOp.Double_- => wa.F64Sub
      case BinaryOp.Double_* => wa.F64Mul
      case BinaryOp.Double_/ => wa.F64Div

      case BinaryOp.Double_== => wa.F64Eq
      case BinaryOp.Double_!= => wa.F64Ne
      case BinaryOp.Double_<  => wa.F64Lt
      case BinaryOp.Double_<= => wa.F64Le
      case BinaryOp.Double_>  => wa.F64Gt
      case BinaryOp.Double_>= => wa.F64Ge
    }

    fb += operation
    binary.tpe
  }

  private def genStringConcat(binary: BinaryOp): Type = {
    val wasmStringType = watpe.RefType.any

    def genToString(tree: Tree): Unit = {
      def genWithDispatch(isAncestorOfHijackedClass: Boolean): Unit = {
        /* Somewhat duplicated from genApplyNonPrim, but specialized for
         * `toString`, and where the handling of `null` is different.
         *
         * We need to return the `"null"` string in two special cases:
         * - if the value itself is `null`, or
         * - if the value's `toString(): String` method returns `null`!
         */

        // A local for a copy of the receiver that we will use to resolve dispatch
        val receiverLocalForDispatch =
          addSyntheticLocal(watpe.RefType(genTypeID.ObjectStruct))

        val objectClassInfo = ctx.getClassInfo(ObjectClass)

        if (!isAncestorOfHijackedClass) {
          /* Standard dispatch codegen, with dedicated null handling.
           *
           * The overall structure of the generated code is as follows:
           *
           * block (ref any) $done
           *   block $isNull
           *     load receiver as (ref null java.lang.Object)
           *     br_on_null $isNull
           *     generate standard table-based dispatch
           *     br_on_non_null $done
           *   end $isNull
           *   gen "null"
           * end $done
           */

          fb.block(watpe.RefType.any) { labelDone =>
            fb.block() { labelIsNull =>
              genTreeAuto(tree)
              markPosition(binary)
              fb += wa.BrOnNull(labelIsNull)
              fb += wa.LocalTee(receiverLocalForDispatch)
              genTableDispatch(objectClassInfo, toStringMethodName, receiverLocalForDispatch)
              fb += wa.BrOnNonNull(labelDone)
            }

            fb ++= ctx.getConstantStringInstr("null")
          }
        } else {
          /* Dispatch where the receiver can be a JS value.
           *
           * The overall structure of the generated code is as follows:
           *
           * block (ref any) $done
           *   block anyref $notOurObject
           *     load receiver
           *     br_on_cast_fail anyref (ref $java.lang.Object) $notOurObject
           *     generate standard table-based dispatch
           *     br_on_non_null $done
           *     ref.null any
           *   end $notOurObject
           *   call the JS helper, also handles `null`
           * end $done
           */

          fb.block(watpe.RefType.any) { labelDone =>
            // First try the case where the value is one of our objects
            fb.block(watpe.RefType.anyref) { labelNotOurObject =>
              // Load receiver
              genTreeAuto(tree)

              markPosition(binary)

              fb += wa.BrOnCastFail(
                labelNotOurObject,
                watpe.RefType.anyref,
                watpe.RefType(genTypeID.ObjectStruct)
              )
              fb += wa.LocalTee(receiverLocalForDispatch)
              genTableDispatch(objectClassInfo, toStringMethodName, receiverLocalForDispatch)
              fb += wa.BrOnNonNull(labelDone)
              fb += wa.RefNull(watpe.HeapType.Any)
            } // end block labelNotOurObject

            // Now we have a value that is not one of our objects; the anyref is still on the stack
            fb += wa.Call(genFunctionID.jsValueToStringForConcat)
          } // end block labelDone
        }
      }

      tree.tpe match {
        case primType: PrimType =>
          genTreeAuto(tree)

          markPosition(binary)

          primType match {
            case StringType =>
              () // no-op
            case BooleanType =>
              fb += wa.Call(genFunctionID.booleanToString)
            case CharType =>
              fb += wa.Call(genFunctionID.charToString)
            case ByteType | ShortType | IntType =>
              fb += wa.Call(genFunctionID.intToString)
            case LongType =>
              fb += wa.Call(genFunctionID.longToString)
            case FloatType =>
              fb += wa.F64PromoteF32
              fb += wa.Call(genFunctionID.doubleToString)
            case DoubleType =>
              fb += wa.Call(genFunctionID.doubleToString)
            case NullType | UndefType =>
              fb += wa.Call(genFunctionID.jsValueToStringForConcat)
            case NothingType =>
              () // unreachable
            case NoType =>
              throw new AssertionError(
                  s"Found expression of type void in String_+ at ${tree.pos}: $tree")
          }

        case ClassType(BoxedStringClass) =>
          // Common case for which we want to avoid the hijacked class dispatch
          genTreeAuto(tree)
          markPosition(binary)
          fb += wa.Call(genFunctionID.jsValueToStringForConcat) // for `null`

        case ClassType(className) =>
          genWithDispatch(ctx.getClassInfo(className).isAncestorOfHijackedClass)

        case AnyType =>
          genWithDispatch(isAncestorOfHijackedClass = true)

        case ArrayType(_) =>
          genWithDispatch(isAncestorOfHijackedClass = false)

        case tpe: RecordType =>
          throw new AssertionError(
              s"Invalid type $tpe for String_+ at ${tree.pos}: $tree")
      }
    }

    binary.lhs match {
      case StringLiteral("") =>
        // Common case where we don't actually need a concatenation
        genToString(binary.rhs)

      case _ =>
        genToString(binary.lhs)
        genToString(binary.rhs)
        markPosition(binary)
        fb += wa.Call(genFunctionID.stringConcat)
    }

    StringType
  }

  private def genIsInstanceOf(tree: IsInstanceOf): Type = {
    genTree(tree.expr, AnyType)

    markPosition(tree)

    def genIsPrimType(testType: PrimType): Unit = {
      testType match {
        case UndefType =>
          fb += wa.Call(genFunctionID.isUndef)
        case StringType =>
          fb += wa.Call(genFunctionID.isString)

        case testType: PrimTypeWithRef =>
          testType match {
            case CharType =>
              val structTypeID = genTypeID.forClass(SpecialNames.CharBoxClass)
              fb += wa.RefTest(watpe.RefType(structTypeID))
            case LongType =>
              val structTypeID = genTypeID.forClass(SpecialNames.LongBoxClass)
              fb += wa.RefTest(watpe.RefType(structTypeID))
            case NoType | NothingType | NullType =>
              throw new AssertionError(s"Illegal isInstanceOf[$testType]")
            case _ =>
              fb += wa.Call(genFunctionID.typeTest(testType.primRef))
          }
      }
    }

    tree.testType match {
      case testType: PrimType =>
        genIsPrimType(testType)

      case AnyType | ClassType(ObjectClass) =>
        fb += wa.RefIsNull
        fb += wa.I32Const(1)
        fb += wa.I32Xor

      case ClassType(JLNumberClass) =>
        /* Special case: the only non-Object *class* that is an ancestor of a
         * hijacked class. We need to accept `number` primitives here.
         */
        val tempLocal = addSyntheticLocal(watpe.RefType.anyref)
        fb += wa.LocalTee(tempLocal)
        fb += wa.RefTest(watpe.RefType(genTypeID.forClass(JLNumberClass)))
        fb.ifThenElse(watpe.Int32) {
          fb += wa.I32Const(1)
        } {
          fb += wa.LocalGet(tempLocal)
          fb += wa.Call(genFunctionID.typeTest(DoubleRef))
        }

      case ClassType(testClassName) =>
        BoxedClassToPrimType.get(testClassName) match {
          case Some(primType) =>
            genIsPrimType(primType)
          case None =>
            if (ctx.getClassInfo(testClassName).isInterface)
              fb += wa.Call(genFunctionID.instanceTest(testClassName))
            else
              fb += wa.RefTest(watpe.RefType(genTypeID.forClass(testClassName)))
        }

      case ArrayType(arrayTypeRef) =>
        arrayTypeRef match {
          case ArrayTypeRef(ClassRef(ObjectClass) | _: PrimRef, 1) =>
            // For primitive arrays and exactly Array[Object], a wa.RefTest is enough
            val structTypeID = genTypeID.forArrayClass(arrayTypeRef)
            fb += wa.RefTest(watpe.RefType(structTypeID))

          case _ =>
            /* Non-Object reference array types need a sophisticated type test
             * based on assignability of component types.
             */
            import watpe.RefType.anyref

            fb.block(Sig(List(anyref), List(watpe.Int32))) { doneLabel =>
              fb.block(Sig(List(anyref), List(anyref))) { notARefArrayLabel =>
                // Try and cast to the generic representation first
                val refArrayStructTypeID = genTypeID.forArrayClass(arrayTypeRef)
                fb += wa.BrOnCastFail(
                  notARefArrayLabel,
                  watpe.RefType.anyref,
                  watpe.RefType(refArrayStructTypeID)
                )

                // refArrayValue := the generic representation
                val refArrayValueLocal =
                  addSyntheticLocal(watpe.RefType(refArrayStructTypeID))
                fb += wa.LocalSet(refArrayValueLocal)

                // Load typeDataOf(arrayTypeRef)
                genLoadArrayTypeData(arrayTypeRef)

                // Load refArrayValue.vtable
                fb += wa.LocalGet(refArrayValueLocal)
                fb += wa.StructGet(refArrayStructTypeID, genFieldID.objStruct.vtable)

                // Call isAssignableFrom and return its result
                fb += wa.Call(genFunctionID.isAssignableFrom)
                fb += wa.Br(doneLabel)
              }

              // Here, the value is not a reference array type, so return false
              fb += wa.Drop
              fb += wa.I32Const(0)
            }
        }

      case testType: RecordType =>
        throw new AssertionError(s"Illegal type in IsInstanceOf: $testType")
    }

    BooleanType
  }

  private def genAsInstanceOf(tree: AsInstanceOf): Type = {
    val sourceTpe = tree.expr.tpe
    val targetTpe = tree.tpe

    if (sourceTpe == NothingType) {
      // We cannot call transformType for NothingType, so we have to handle this case separately.
      genTree(tree.expr, NothingType)
      NothingType
    } else {
      // By IR checker rules, targetTpe is none of NothingType, NullType, NoType or RecordType

      val sourceWasmType = transformType(sourceTpe)
      val targetWasmType = transformType(targetTpe)

      if (sourceWasmType == targetWasmType) {
        /* Common case where no cast is necessary at the Wasm level.
         * Note that this is not *obviously* correct. It is only correct
         * because, under our choices of representation and type translation
         * rules, there is no pair `(sourceTpe, targetTpe)` for which the Wasm
         * types are equal but a valid cast would require a *conversion*.
         */
        genTreeAuto(tree.expr)
      } else {
        genTree(tree.expr, AnyType)

        markPosition(tree)

        targetTpe match {
          case targetTpe: PrimType =>
            // TODO Opt: We could do something better for things like double.asInstanceOf[int]
            genUnbox(targetTpe)(tree.pos)

          case _ =>
            targetWasmType match {
              case watpe.RefType(true, watpe.HeapType.Any) =>
                () // nothing to do
              case targetWasmType: watpe.RefType =>
                fb += wa.RefCast(targetWasmType)
              case _ =>
                throw new AssertionError(s"Unexpected type in AsInstanceOf: $targetTpe")
            }
        }
      }

      targetTpe
    }
  }

  /** Unbox the `anyref` on the stack to the target `PrimType`.
   *
   *  `targetTpe` must not be `NothingType`, `NullType` nor `NoType`.
   *
   *  The type left on the stack is non-nullable.
   */
  private def genUnbox(targetTpe: PrimType)(implicit pos: Position): Unit = {
    targetTpe match {
      case UndefType =>
        fb += wa.Drop
        fb += wa.GlobalGet(genGlobalID.undef)

      case StringType =>
        fb += wa.RefAsNotNull

      case targetTpe: PrimTypeWithRef =>
        targetTpe match {
          case CharType | LongType =>
            // Extract the `value` field (the only field) out of the box class.

            val boxClass =
              if (targetTpe == CharType) SpecialNames.CharBoxClass
              else SpecialNames.LongBoxClass
            val fieldName = FieldName(boxClass, SpecialNames.valueFieldSimpleName)
            val resultType = transformType(targetTpe)

            fb.block(Sig(List(watpe.RefType.anyref), List(resultType))) { doneLabel =>
              fb.block(Sig(List(watpe.RefType.anyref), Nil)) { isNullLabel =>
                fb += wa.BrOnNull(isNullLabel)
                val structTypeID = genTypeID.forClass(boxClass)
                fb += wa.RefCast(watpe.RefType(structTypeID))
                fb += wa.StructGet(
                  structTypeID,
                  genFieldID.forClassInstanceField(fieldName)
                )
                fb += wa.Br(doneLabel)
              }
              genTree(zeroOf(targetTpe), targetTpe)
            }

          case NothingType | NullType | NoType =>
            throw new IllegalArgumentException(s"Illegal type in genUnbox: $targetTpe")
          case _ =>
            fb += wa.Call(genFunctionID.unbox(targetTpe.primRef))
        }
    }
  }

  private def genGetClass(tree: GetClass): Type = {
    /* Unlike in `genApply` or `genStringConcat`, here we make no effort to
     * optimize known-primitive receivers. In practice, such cases would be
     * useless.
     */

    val needHijackedClassDispatch = tree.expr.tpe match {
      case ClassType(className) =>
        ctx.getClassInfo(className).isAncestorOfHijackedClass
      case ArrayType(_) | NothingType | NullType =>
        false
      case _ =>
        true
    }

    if (!needHijackedClassDispatch) {
      val typeDataType = watpe.RefType(genTypeID.typeData)
      val objectTypeIdx = genTypeID.forClass(ObjectClass)

      val typeDataLocal = addSyntheticLocal(typeDataType)

      genTreeAuto(tree.expr)
      markPosition(tree)
      fb += wa.StructGet(objectTypeIdx, genFieldID.objStruct.vtable) // implicit trap on null
      fb += wa.LocalSet(typeDataLocal)
      genClassOfFromTypeData(wa.LocalGet(typeDataLocal))
    } else {
      genTree(tree.expr, AnyType)
      markPosition(tree)
      fb += wa.RefAsNotNull
      fb += wa.Call(genFunctionID.anyGetClass)
    }

    tree.tpe
  }

  private def genReadStorage(storage: VarStorage): Unit = {
    storage match {
      case VarStorage.Local(localID) =>
        fb += wa.LocalGet(localID)
      case VarStorage.StructField(structLocal, structTypeID, fieldID) =>
        fb += wa.LocalGet(structLocal)
        fb += wa.StructGet(structTypeID, fieldID)
    }
  }

  private def genVarRef(r: VarRef): Type = {
    markPosition(r)
    if (r.tpe == NothingType)
      fb += wa.Unreachable
    else
      genReadStorage(lookupLocal(r.ident.name))
    r.tpe
  }

  private def genThis(t: This): Type = {
    markPosition(t)
    genReadStorage(receiverStorage)
    t.tpe
  }

  private def genVarDef(r: VarDef): Type = {
    /* This is an isolated VarDef that is not in a Block.
     * Its scope is empty by construction, and therefore it need not be stored.
     */
    genTree(r.rhs, NoType)
    NoType
  }

  private def genIf(t: If, expectedType: Type): Type = {
    val ty = transformResultType(expectedType)
    genTree(t.cond, BooleanType)

    markPosition(t)

    t.elsep match {
      case Skip() =>
        assert(expectedType == NoType)
        fb.ifThen() {
          genTree(t.thenp, expectedType)
        }
      case _ =>
        fb.ifThenElse(ty) {
          genTree(t.thenp, expectedType)
        } {
          genTree(t.elsep, expectedType)
        }
    }

    if (expectedType == NothingType)
      fb += wa.Unreachable

    expectedType
  }

  private def genWhile(t: While): Type = {
    t.cond match {
      case BooleanLiteral(true) =>
        // infinite loop that must be typed as `nothing`, i.e., unreachable
        markPosition(t)
        fb.loop() { label =>
          genTree(t.body, NoType)
          markPosition(t)
          fb += wa.Br(label)
        }
        fb += wa.Unreachable
        NothingType

      case _ =>
        // normal loop typed as `void`
        markPosition(t)
        fb.loop() { label =>
          genTree(t.cond, BooleanType)
          markPosition(t)
          fb.ifThen() {
            genTree(t.body, NoType)
            markPosition(t)
            fb += wa.Br(label)
          }
        }
        NoType
    }
  }

  private def genForIn(t: ForIn): Type = {
    /* This is tricky. In general, the body of a ForIn can be an arbitrary
     * statement, which can refer to the enclosing scope and its locals,
     * including for mutations. Unfortunately, there is no way to implement a
     * ForIn other than actually doing a JS `for (var key in obj) { body }`
     * loop. That means we need to pass the `body` as a JS closure.
     *
     * That is problematic for our backend because we basically need to perform
     * lambda lifting: identifying captures ourselves, and turn references to
     * local variables into accessing the captured environment.
     *
     * We side-step this issue for now by exploiting the known shape of `ForIn`
     * generated by the Scala.js compiler. This is fine as long as we do not
     * support the Scala.js optimizer. We will have to revisit this code when
     * we add that support.
     */

    t.body match {
      case JSFunctionApply(fVarRef: VarRef, List(VarRef(argIdent)))
          if fVarRef.ident.name != t.keyVar.name && argIdent.name == t.keyVar.name =>
        genTree(t.obj, AnyType)
        genTree(fVarRef, AnyType)
        markPosition(t)
        fb += wa.Call(genFunctionID.jsForInSimple)

      case _ =>
        throw new NotImplementedError(s"Unsupported shape of ForIn node at ${t.pos}: $t")
    }

    NoType
  }

  private def genTryCatch(t: TryCatch, expectedType: Type): Type = {
    val resultType = transformResultType(expectedType)

    if (UseLegacyExceptionsForTryCatch) {
      markPosition(t)
      fb += wa.Try(fb.sigToBlockType(Sig(Nil, resultType)))
      genTree(t.block, expectedType)
      markPosition(t)
      fb += wa.Catch(genTagID.exception)
      withNewLocal(t.errVar.name, t.errVarOriginalName, watpe.RefType.anyref) { exceptionLocal =>
        fb += wa.AnyConvertExtern
        fb += wa.LocalSet(exceptionLocal)
        genTree(t.handler, expectedType)
      }
      fb += wa.End
    } else {
      markPosition(t)
      fb.block(resultType) { doneLabel =>
        fb.block(watpe.RefType.externref) { catchLabel =>
          /* We used to have `resultType` as result of the try_table, with the
           * `wa.BR(doneLabel)` outside of the try_table. Unfortunately it seems
           * V8 cannot handle try_table with a result type that is `(ref ...)`.
           * The current encoding with `externref` as result type (to match the
           * enclosing block) and the `br` *inside* the `try_table` works.
           */
          fb.tryTable(watpe.RefType.externref)(
            List(wa.CatchClause.Catch(genTagID.exception, catchLabel))
          ) {
            genTree(t.block, expectedType)
            markPosition(t)
            fb += wa.Br(doneLabel)
          }
        } // end block $catch
        withNewLocal(t.errVar.name, t.errVarOriginalName, watpe.RefType.anyref) { exceptionLocal =>
          fb += wa.AnyConvertExtern
          fb += wa.LocalSet(exceptionLocal)
          genTree(t.handler, expectedType)
        }
      } // end block $done
    }

    if (expectedType == NothingType)
      fb += wa.Unreachable

    expectedType
  }

  private def genThrow(tree: Throw): Type = {
    genTree(tree.expr, AnyType)
    markPosition(tree)
    fb += wa.ExternConvertAny
    fb += wa.Throw(genTagID.exception)

    NothingType
  }

  private def genBlock(t: Block, expectedType: Type): Type = {
    genBlockStats(t.stats.init) {
      genTree(t.stats.last, expectedType)
    }
    expectedType
  }

  final def genBlockStats(stats: List[Tree])(inner: => Unit): Unit = {
    stats match {
      case (stat @ VarDef(name, originalName, vtpe, _, rhs)) :: rest =>
        genTree(rhs, vtpe)
        markPosition(stat)
        withNewLocal(name.name, originalName, transformLocalType(vtpe)) { local =>
          fb += wa.LocalSet(local)
          genBlockStats(rest)(inner)
        }
      case stat :: rest =>
        genTree(stat, NoType)
        genBlockStats(rest)(inner)
      case Nil =>
        inner
    }
  }

  private def genNew(n: New): Type = {
    /* Do not use transformType here, because we must get the struct type even
     * if the given class is an ancestor of hijacked classes (which in practice
     * is only the case for j.l.Object).
     */
    val instanceType = watpe.RefType(genTypeID.forClass(n.className))
    val localInstance = addSyntheticLocal(instanceType)

    markPosition(n)
    fb += wa.Call(genFunctionID.newDefault(n.className))
    fb += wa.LocalTee(localInstance)

    genArgs(n.args, n.ctor.name)

    markPosition(n)

    fb += wa.Call(
      genFunctionID.forMethod(
        MemberNamespace.Constructor,
        n.className,
        n.ctor.name
      )
    )
    fb += wa.LocalGet(localInstance)
    n.tpe
  }

  /** Codegen to box a primitive `char`/`long` into a `CharacterBox`/`LongBox`. */
  private def genBox(primType: watpe.SimpleType, boxClassName: ClassName): Type = {
    val primLocal = addSyntheticLocal(primType)

    /* We use a direct `StructNew` instead of the logical call to `newDefault`
     * plus constructor call. We can do this because we know that this is
     * what the constructor would do anyway (so we're basically inlining it).
     */

    fb += wa.LocalSet(primLocal)
    fb += wa.GlobalGet(genGlobalID.forVTable(boxClassName))
    fb += wa.GlobalGet(genGlobalID.forITable(boxClassName))
    fb += wa.LocalGet(primLocal)
    fb += wa.StructNew(genTypeID.forClass(boxClassName))

    ClassType(boxClassName)
  }

  private def genIdentityHashCode(tree: IdentityHashCode): Type = {
    // TODO Avoid dispatch when we know a more precise type than any
    genTree(tree.expr, AnyType)

    markPosition(tree)
    fb += wa.Call(genFunctionID.identityHashCode)

    IntType
  }

  private def genWrapAsThrowable(tree: WrapAsThrowable): Type = {
    val throwableClassType = ClassType(ThrowableClass)
    val nonNullThrowableType = watpe.RefType(genTypeID.ThrowableStruct)

    val jsExceptionType =
      transformClassType(SpecialNames.JSExceptionClass).toNonNullable

    fb.block(nonNullThrowableType) { doneLabel =>
      genTree(tree.expr, AnyType)

      markPosition(tree)

      // if expr.isInstanceOf[Throwable], then br $done
      fb += wa.BrOnCast(
        doneLabel,
        watpe.RefType.anyref,
        nonNullThrowableType
      )

      // otherwise, wrap in a new JavaScriptException

      val exprLocal = addSyntheticLocal(watpe.RefType.anyref)
      val instanceLocal = addSyntheticLocal(jsExceptionType)

      fb += wa.LocalSet(exprLocal)
      fb += wa.Call(genFunctionID.newDefault(SpecialNames.JSExceptionClass))
      fb += wa.LocalTee(instanceLocal)
      fb += wa.LocalGet(exprLocal)
      fb += wa.Call(
        genFunctionID.forMethod(
          MemberNamespace.Constructor,
          SpecialNames.JSExceptionClass,
          SpecialNames.JSExceptionCtor
        )
      )
      fb += wa.LocalGet(instanceLocal)
    }

    throwableClassType
  }

  private def genUnwrapFromThrowable(tree: UnwrapFromThrowable): Type = {
    fb.block(watpe.RefType.anyref) { doneLabel =>
      genTree(tree.expr, ClassType(ThrowableClass))

      markPosition(tree)

      fb += wa.RefAsNotNull

      // if !expr.isInstanceOf[js.JavaScriptException], then br $done
      fb += wa.BrOnCastFail(
        doneLabel,
        watpe.RefType(genTypeID.ThrowableStruct),
        watpe.RefType(genTypeID.JSExceptionStruct)
      )

      // otherwise, unwrap the JavaScriptException by reading its field
      fb += wa.StructGet(
        genTypeID.forClass(SpecialNames.JSExceptionClass),
        genFieldID.forClassInstanceField(SpecialNames.JSExceptionField)
      )
    }

    AnyType
  }

  private def genJSNew(tree: JSNew): Type = {
    genTree(tree.ctor, AnyType)
    genJSArgsArray(tree.args)
    markPosition(tree)
    fb += wa.Call(genFunctionID.jsNew)
    AnyType
  }

  private def genJSSelect(tree: JSSelect): Type = {
    genTree(tree.qualifier, AnyType)
    genTree(tree.item, AnyType)
    markPosition(tree)
    fb += wa.Call(genFunctionID.jsSelect)
    AnyType
  }

  private def genJSFunctionApply(tree: JSFunctionApply): Type = {
    genTree(tree.fun, AnyType)
    genJSArgsArray(tree.args)
    markPosition(tree)
    fb += wa.Call(genFunctionID.jsFunctionApply)
    AnyType
  }

  private def genJSMethodApply(tree: JSMethodApply): Type = {
    genTree(tree.receiver, AnyType)
    genTree(tree.method, AnyType)
    genJSArgsArray(tree.args)
    markPosition(tree)
    fb += wa.Call(genFunctionID.jsMethodApply)
    AnyType
  }

  private def genJSImportCall(tree: JSImportCall): Type = {
    genTree(tree.arg, AnyType)
    markPosition(tree)
    fb += wa.Call(genFunctionID.jsImportCall)
    AnyType
  }

  private def genJSImportMeta(tree: JSImportMeta): Type = {
    markPosition(tree)
    fb += wa.Call(genFunctionID.jsImportMeta)
    AnyType
  }

  private def genLoadJSConstructor(tree: LoadJSConstructor): Type = {
    markPosition(tree)
    SWasmGen.genLoadJSConstructor(fb, tree.className)
    AnyType
  }

  private def genLoadJSModule(tree: LoadJSModule): Type = {
    markPosition(tree)

    ctx.getClassInfo(tree.className).jsNativeLoadSpec match {
      case Some(loadSpec) =>
        genLoadJSFromSpec(fb, loadSpec)
      case None =>
        // This is a non-native JS module
        fb += wa.Call(genFunctionID.loadModule(tree.className))
    }

    AnyType
  }

  private def genSelectJSNativeMember(tree: SelectJSNativeMember): Type = {
    val info = ctx.getClassInfo(tree.className)
    val jsNativeLoadSpec = info.jsNativeMembers.getOrElse(tree.member.name, {
      throw new AssertionError(
          s"Found $tree for non-existing JS native member at ${tree.pos}")
    })
    genLoadJSFromSpec(fb, jsNativeLoadSpec)
    AnyType
  }

  private def genJSDelete(tree: JSDelete): Type = {
    genTree(tree.qualifier, AnyType)
    genTree(tree.item, AnyType)
    markPosition(tree)
    fb += wa.Call(genFunctionID.jsDelete)
    NoType
  }

  private def genJSUnaryOp(tree: JSUnaryOp): Type = {
    genTree(tree.lhs, AnyType)
    markPosition(tree)
    fb += wa.Call(genFunctionID.jsUnaryOps(tree.op))
    AnyType
  }

  private def genJSBinaryOp(tree: JSBinaryOp): Type = {
    tree.op match {
      case JSBinaryOp.|| | JSBinaryOp.&& =>
        /* Here we need to implement the short-circuiting behavior, with a
         * condition based on the truthy value of the left-hand-side.
         */
        val lhsLocal = addSyntheticLocal(watpe.RefType.anyref)
        genTree(tree.lhs, AnyType)
        markPosition(tree)
        fb += wa.LocalTee(lhsLocal)
        fb += wa.Call(genFunctionID.jsIsTruthy)
        fb += wa.If(wa.BlockType.ValueType(watpe.RefType.anyref))
        if (tree.op == JSBinaryOp.||) {
          fb += wa.LocalGet(lhsLocal)
          fb += wa.Else
          genTree(tree.rhs, AnyType)
          markPosition(tree)
        } else {
          genTree(tree.rhs, AnyType)
          markPosition(tree)
          fb += wa.Else
          fb += wa.LocalGet(lhsLocal)
        }
        fb += wa.End

      case _ =>
        genTree(tree.lhs, AnyType)
        genTree(tree.rhs, AnyType)
        markPosition(tree)
        fb += wa.Call(genFunctionID.jsBinaryOps(tree.op))
    }

    tree.tpe
  }

  private def genJSArrayConstr(tree: JSArrayConstr): Type = {
    genJSArgsArray(tree.items)
    AnyType
  }

  private def genJSObjectConstr(tree: JSObjectConstr): Type = {
    markPosition(tree)
    fb += wa.Call(genFunctionID.jsNewObject)
    for ((prop, value) <- tree.fields) {
      genTree(prop, AnyType)
      genTree(value, AnyType)
      fb += wa.Call(genFunctionID.jsObjectPush)
    }
    AnyType
  }

  private def genJSGlobalRef(tree: JSGlobalRef): Type = {
    markPosition(tree)
    fb ++= ctx.getConstantStringInstr(tree.name)
    fb += wa.Call(genFunctionID.jsGlobalRefGet)
    AnyType
  }

  private def genJSTypeOfGlobalRef(tree: JSTypeOfGlobalRef): Type = {
    markPosition(tree)
    fb ++= ctx.getConstantStringInstr(tree.globalRef.name)
    fb += wa.Call(genFunctionID.jsGlobalRefTypeof)
    AnyType
  }

  private def genJSArgsArray(args: List[TreeOrJSSpread]): Unit = {
    fb += wa.Call(genFunctionID.jsNewArray)
    for (arg <- args) {
      arg match {
        case arg: Tree =>
          genTree(arg, AnyType)
          fb += wa.Call(genFunctionID.jsArrayPush)
        case JSSpread(items) =>
          genTree(items, AnyType)
          fb += wa.Call(genFunctionID.jsArraySpreadPush)
      }
    }
  }

  private def genJSLinkingInfo(tree: JSLinkingInfo): Type = {
    markPosition(tree)
    fb += wa.Call(genFunctionID.jsLinkingInfo)
    AnyType
  }

  private def genArrayLength(t: ArrayLength): Type = {
    genTreeAuto(t.array)

    markPosition(t)

    t.array.tpe match {
      case ArrayType(arrayTypeRef) =>
        // Get the underlying array; implicit trap on null
        fb += wa.StructGet(
          genTypeID.forArrayClass(arrayTypeRef),
          genFieldID.objStruct.arrayUnderlying
        )
        // Get the length
        fb += wa.ArrayLen
        IntType

      case NothingType =>
        // unreachable
        NothingType
      case NullType =>
        fb += wa.Unreachable
        NothingType
      case _ =>
        throw new IllegalArgumentException(
            s"ArraySelect.array must be an array type, but has type ${t.array.tpe}")
    }
  }

  private def genNewArray(t: NewArray): Type = {
    val arrayTypeRef = t.typeRef

    if (t.lengths.isEmpty || t.lengths.size > arrayTypeRef.dimensions) {
      throw new AssertionError(
          s"invalid lengths ${t.lengths} for array type ${arrayTypeRef.displayName}")
    }

    markPosition(t)

    if (t.lengths.size == 1) {
      genLoadVTableAndITableForArray(arrayTypeRef)

      // Create the underlying array
      genTree(t.lengths.head, IntType)
      markPosition(t)

      val underlyingArrayType = genTypeID.underlyingOf(arrayTypeRef)
      fb += wa.ArrayNewDefault(underlyingArrayType)

      // Create the array object
      fb += wa.StructNew(genTypeID.forArrayClass(arrayTypeRef))
    } else {
      /* There is no Scala source code that produces `NewArray` with more than
       * one specified dimension, so this branch is not tested.
       * (The underlying function `newArrayObject` is tested as part of
       * reflective array instantiations, though.)
       */

      // First arg to `newArrayObject`: the typeData of the array to create
      genLoadArrayTypeData(arrayTypeRef)

      // Second arg: an array of the lengths
      for (length <- t.lengths)
        genTree(length, IntType)
      markPosition(t)
      fb += wa.ArrayNewFixed(genTypeID.i32Array, t.lengths.size)

      // Third arg: constant 0 (start index inside the array of lengths)
      fb += wa.I32Const(0)

      fb += wa.Call(genFunctionID.newArrayObject)
    }

    t.tpe
  }

  /** Gen code to load the vtable and the itable of the given array type. */
  private def genLoadVTableAndITableForArray(arrayTypeRef: ArrayTypeRef): Unit = {
    // Load the typeData of the resulting array type. It is the vtable of the resulting object.
    genLoadArrayTypeData(arrayTypeRef)

    // Load the itables for the array type
    fb += wa.GlobalGet(genGlobalID.arrayClassITable)
  }

  private def genArraySelect(t: ArraySelect): Type = {
    genTreeAuto(t.array)

    markPosition(t)

    t.array.tpe match {
      case ArrayType(arrayTypeRef) =>
        // Get the underlying array; implicit trap on null
        fb += wa.StructGet(
          genTypeID.forArrayClass(arrayTypeRef),
          genFieldID.objStruct.arrayUnderlying
        )

        // Load the index
        genTree(t.index, IntType)

        markPosition(t)

        // Use the appropriate variant of array.get for sign extension
        val typeIdx = genTypeID.underlyingOf(arrayTypeRef)
        arrayTypeRef match {
          case ArrayTypeRef(BooleanRef | CharRef, 1) =>
            fb += wa.ArrayGetU(typeIdx)
          case ArrayTypeRef(ByteRef | ShortRef, 1) =>
            fb += wa.ArrayGetS(typeIdx)
          case _ =>
            fb += wa.ArrayGet(typeIdx)
        }

        /* If it is a reference array type whose element type does not translate
         * to `anyref`, we must cast down the result.
         */
        arrayTypeRef match {
          case ArrayTypeRef(_: PrimRef, 1) =>
            // a primitive array type always has the correct
            ()
          case _ =>
            transformType(t.tpe) match {
              case watpe.RefType.anyref =>
                // nothing to do
                ()
              case refType: watpe.RefType =>
                fb += wa.RefCast(refType)
              case otherType =>
                throw new AssertionError(s"Unexpected result type for reference array: $otherType")
            }
        }

        t.tpe

      case NothingType =>
        // unreachable
        NothingType
      case NullType =>
        fb += wa.Unreachable
        NothingType
      case _ =>
        throw new IllegalArgumentException(
          s"ArraySelect.array must be an array type, but has type ${t.array.tpe}"
        )
    }
  }

  private def genArrayValue(t: ArrayValue): Type = {
    val arrayTypeRef = t.typeRef

    markPosition(t)

    genLoadVTableAndITableForArray(arrayTypeRef)

    val expectedElemType = arrayTypeRef match {
      case ArrayTypeRef(base: PrimRef, 1) => base.tpe
      case _                              => AnyType
    }

    // Create the underlying array
    t.elems.foreach(genTree(_, expectedElemType))
    markPosition(t)
    val underlyingArrayType = genTypeID.underlyingOf(arrayTypeRef)
    fb += wa.ArrayNewFixed(underlyingArrayType, t.elems.size)

    // Create the array object
    fb += wa.StructNew(genTypeID.forArrayClass(arrayTypeRef))

    t.tpe
  }

  private def genClosure(tree: Closure): Type = {
    implicit val pos = tree.pos
    implicit val ctx = this.ctx

    val hasThis = !tree.arrow
    val hasRestParam = tree.restParam.isDefined
    val dataStructTypeID = ctx.getClosureDataStructType(tree.captureParams.map(_.ptpe))

    // Define the function where captures are reified as a `__captureData` argument.
    val closureFuncOrigName = genInnerFuncOriginalName()
    val closureFuncID = new ClosureFunctionID(closureFuncOrigName)
    emitFunction(
      closureFuncID,
      closureFuncOrigName,
      enclosingClassName = None,
      Some(tree.captureParams),
      receiverType = if (!hasThis) None else Some(watpe.RefType.anyref),
      tree.params,
      tree.restParam,
      tree.body,
      resultType = AnyType
    )

    markPosition(tree)

    // Put a reference to the function on the stack
    fb += ctx.refFuncWithDeclaration(closureFuncID)

    // Evaluate the capture values and instantiate the capture data struct
    for ((param, value) <- tree.captureParams.zip(tree.captureValues))
      genTree(value, param.ptpe)
    markPosition(tree)
    fb += wa.StructNew(dataStructTypeID)

    /* If there is a ...rest param, the helper requires as third argument the
     * number of regular arguments.
     */
    if (hasRestParam)
      fb += wa.I32Const(tree.params.size)

    // Call the appropriate helper
    val helper = (hasThis, hasRestParam) match {
      case (false, false) => genFunctionID.closure
      case (true, false)  => genFunctionID.closureThis
      case (false, true)  => genFunctionID.closureRest
      case (true, true)   => genFunctionID.closureThisRest
    }
    fb += wa.Call(helper)

    AnyType
  }

  private def genClone(t: Clone): Type = {
    val expr = addSyntheticLocal(transformType(t.expr.tpe))

    genTree(t.expr, ClassType(CloneableClass))

    markPosition(t)

    fb += wa.RefCast(watpe.RefType(genTypeID.ObjectStruct))
    fb += wa.LocalTee(expr)
    fb += wa.RefAsNotNull // cloneFunction argument is not nullable

    fb += wa.LocalGet(expr)
    fb += wa.StructGet(genTypeID.forClass(ObjectClass), genFieldID.objStruct.vtable)
    fb += wa.StructGet(genTypeID.typeData, genFieldID.typeData.cloneFunction)
    // cloneFunction: (ref j.l.Object) -> ref j.l.Object
    fb += wa.CallRef(genTypeID.cloneFunctionType)

    t.tpe match {
      case ClassType(className) =>
        val info = ctx.getClassInfo(className)
        if (!info.isInterface) // if it's interface, no need to cast from j.l.Object
          fb += wa.RefCast(watpe.RefType(genTypeID.forClass(className)))
      case _ =>
        throw new IllegalArgumentException(
            s"Clone result type must be a class type, but is ${t.tpe}")
    }
    t.tpe
  }

  private def genMatch(tree: Match, expectedType: Type): Type = {
    val Match(selector, cases, defaultBody) = tree
    val selectorLocal = addSyntheticLocal(transformType(selector.tpe))

    genTreeAuto(selector)

    markPosition(tree)

    fb += wa.LocalSet(selectorLocal)

    fb.block(transformResultType(expectedType)) { doneLabel =>
      fb.block() { defaultLabel =>
        val caseLabels = cases.map(c => c._1 -> fb.genLabel())
        for (caseLabel <- caseLabels)
          fb += wa.Block(wa.BlockType.ValueType(), Some(caseLabel._2))

        for {
          caseLabel <- caseLabels
          matchableLiteral <- caseLabel._1
        } {
          markPosition(matchableLiteral)
          val label = caseLabel._2
          fb += wa.LocalGet(selectorLocal)
          matchableLiteral match {
            case IntLiteral(value) =>
              fb += wa.I32Const(value)
              fb += wa.I32Eq
              fb += wa.BrIf(label)
            case StringLiteral(value) =>
              fb ++= ctx.getConstantStringInstr(value)
              fb += wa.Call(genFunctionID.is)
              fb += wa.BrIf(label)
            case Null() =>
              fb += wa.RefIsNull
              fb += wa.BrIf(label)
          }
        }
        fb += wa.Br(defaultLabel)

        for ((caseLabel, caze) <- caseLabels.zip(cases).reverse) {
          markPosition(caze._2)
          fb += wa.End
          genTree(caze._2, expectedType)
          fb += wa.Br(doneLabel)
        }
      }
      genTree(defaultBody, expectedType)
    }

    if (expectedType == NothingType)
      fb += wa.Unreachable

    expectedType
  }

  private def genCreateJSClass(tree: CreateJSClass): Type = {
    val classInfo = ctx.getClassInfo(tree.className)
    val jsClassCaptures = classInfo.jsClassCaptures.getOrElse {
      throw new AssertionError(
          s"Illegal CreateJSClass of top-level class ${tree.className.nameString}")
    }

    for ((captureValue, captureParam) <- tree.captureValues.zip(jsClassCaptures))
      genTree(captureValue, captureParam.ptpe)

    markPosition(tree)

    fb += wa.Call(genFunctionID.createJSClassOf(tree.className))

    AnyType
  }

  private def genJSPrivateSelect(tree: JSPrivateSelect): Type = {
    genTree(tree.qualifier, AnyType)

    markPosition(tree)

    fb += wa.GlobalGet(genGlobalID.forJSPrivateField(tree.field.name))
    fb += wa.Call(genFunctionID.jsSelect)

    AnyType
  }

  private def genJSSuperSelect(tree: JSSuperSelect): Type = {
    genTree(tree.superClass, AnyType)
    genTree(tree.receiver, AnyType)
    genTree(tree.item, AnyType)

    markPosition(tree)

    fb += wa.Call(genFunctionID.jsSuperGet)

    AnyType
  }

  private def genJSSuperMethodCall(tree: JSSuperMethodCall): Type = {
    genTree(tree.superClass, AnyType)
    genTree(tree.receiver, AnyType)
    genTree(tree.method, AnyType)
    genJSArgsArray(tree.args)

    markPosition(tree)

    fb += wa.Call(genFunctionID.jsSuperCall)

    AnyType
  }

  private def genJSNewTarget(tree: JSNewTarget): Type = {
    markPosition(tree)

    genReadStorage(newTargetStorage)

    AnyType
  }

  /*--------------------------------------------------------------------*
   * HERE BE DRAGONS --- Handling of TryFinally, Labeled and Return --- *
   *--------------------------------------------------------------------*/

  /* From this point onwards, and until the end of the file, you will find
   * the infrastructure required to handle TryFinally and Labeled/Return pairs.
   *
   * Independently, TryFinally and Labeled/Return are not very difficult to
   * handle. The dragons come when they interact, and in particular when a
   * TryFinally stands in the middle of a Labeled/Return pair.
   *
   * For example:
   *
   * val foo: int = alpha[int]: {
   *   val bar: string = try {
   *     if (somethingHappens)
   *       return@alpha 5
   *     "bar"
   *   } finally {
   *     doTheFinally()
   *   }
   *   someOtherThings(bar)
   * }
   *
   * In that situation, if we naively translate the `return@alpha` into
   * `br $alpha`, we bypass the `finally` block, which goes against the spec.
   *
   * Instead, we must stash the result 5 in a local and jump to the finally
   * block. The issue is that, at the end of `doTheFinally()`, we need to keep
   * propagating further up (instead of executing `someOtherThings()`).
   *
   * That means that there are 3 possible outcomes after the `finally` block:
   *
   * - Rethrow the exception if we caught one.
   * - Reload the stashed result and branch further to `alpha`.
   * - Otherwise keep going to do `someOtherThings()`.
   *
   * Now what if there are *several* labels for which we cross that
   * `try..finally`? Well we need to deal with all the possible labels. This
   * means that, in general, we in fact have `2 + n` possible outcomes, where
   * `n` is the number of labels for which we found a `Return` that crosses the
   * boundary.
   *
   * In order to know whether we need to rethrow, we look at a nullable
   * `exnref`. For the remaining cases, we use a separate `destinationTag`
   * local. Every label gets assigned a distinct tag > 0. Fall-through is
   * always represented by 0. Before branching to a `finally` block, we set the
   * appropriate value to the `destinationTag` value.
   *
   * Since the various labels can have different result types, and since they
   * can be different from the result of the regular flow of the `try` block,
   * we have to normalize to `void` for the `try_table` itself. Each label has
   * a dedicated local for its result if it comes from such a crossing
   * `return`.
   *
   * Two more complications:
   *
   * - If the `finally` block itself contains another `try..finally`, they may
   *   need a `destinationTag` concurrently. Therefore, every `try..finally`
   *   gets its own `destinationTag` local.
   * - If the `try` block contains another `try..finally`, so that there are
   *   two (or more) `try..finally` in the way between a `Return` and a
   *   `Labeled`, we must forward to the next `finally` in line (and its own
   *   `destinationTag` local) so that the whole chain gets executed before
   *   reaching the `Labeled`.
   *
   * ---
   *
   * As an evil example of everything that can happen, consider:
   *
   * alpha[double]: { // allocated destinationTag = 1
   *   val foo: int = try { // uses the local destinationTagOuter
   *     beta[int]: { // allocated destinationTag = 2
   *       val bar: int = try { // uses the local destinationTagInner
   *         if (A) return@alpha 5
   *         if (B) return@beta 10
   *         56
   *       } finally {
   *         doTheFinally()
   *         // not shown: there is another try..finally here
   *         // its destinationTagLocal must be different than destinationTag
   *         // since both are live at the same time.
   *       }
   *       someOtherThings(bar)
   *     }
   *   } finally {
   *     doTheOuterFinally()
   *   }
   *   moreOtherThings(foo)
   * }
   *
   * The whole compiled code is too overwhelming to be useful, so we show the
   * important aspects piecemiel, from the bottom up.
   *
   * First, the compiled code for `return@alpha 5`:
   *
   * i32.const 5                    ; eval the argument of the return
   * local.set $alphaResult         ; store it in $alphaResult because we are cross a try..finally
   * i32.const 1                    ; the destination tag of alpha
   * local.set $destinationTagInner ; store it in the destinationTag local of the inner try..finally
   * br $innerCross                 ; branch to the cross label of the inner try..finally
   *
   * Second, we look at the shape generated for the inner try..finally:
   *
   * block $innerDone (result i32)
   *   block $innerCatch (result exnref)
   *     block $innerCross
   *       try_table (catch_all_ref $innerCatch)
   *         ; [...] body of the try
   *
   *         local.set $innerTryResult
   *       end ; try_table
   *
   *       ; set destinationTagInner := 0 to mean fall-through
   *       i32.const 0
   *       local.set $destinationTagInner
   *     end ; block $innerCross
   *
   *     ; no exception thrown
   *     ref.null exn
   *   end ; block $innerCatch
   *
   *   ; now we have the common code with the finally
   *
   *   ; [...] body of the finally
   *
   *   ; maybe re-throw
   *   block $innerExnIsNull (param exnref)
   *     br_on_null $innerExnIsNull
   *     throw_ref
   *   end
   *
   *   ; re-dispatch after the outer finally based on $destinationTagInner
   *
   *   ; first transfer our destination tag to the outer try's destination tag
   *   local.get $destinationTagInner
   *   local.set $destinationTagOuter
   *
   *   ; now use a br_table to jump to the appropriate destination
   *   ; if 0, fall-through
   *   ; if 1, go the outer try's cross label because it is still on the way to alpha
   *   ; if 2, go to beta's cross label
   *   ; default to fall-through (never used but br_table needs a default)
   *   br_table $innerDone $outerCross $betaCross $innerDone
   * end ; block $innerDone
   *
   * We omit the shape of beta and of the outer try. There are similar to the
   * shape of alpha and inner try, respectively.
   *
   * We conclude with the shape of the alpha block:
   *
   * block $alpha (result f64)
   *   block $alphaCross
   *     ; begin body of alpha
   *
   *     ; [...]              ; the try..finally
   *     local.set $foo       ; val foo =
   *     moreOtherThings(foo)
   *
   *     ; end body of alpha
   *
   *     br $alpha ; if alpha finished normally, jump over `local.get $alphaResult`
   *   end ; block $alphaCross
   *
   *   ; if we returned from alpha across a try..finally, fetch the result from the local
   *   local.get $alphaResult
   * end ; block $alpha
   */

  /** This object namespaces everything related to unwinding, so that we don't pollute too much the
   *  overall internal scope of `FunctionEmitter`.
   */
  private object unwinding {

    /** The number of enclosing `Labeled` and `TryFinally` blocks.
     *
     *  For `TryFinally`, it is only enclosing if we are in the `try` branch, not the `finally`
     *  branch.
     *
     *  Invariant:
     *  {{{
     *  currentUnwindingStackDepth == enclosingTryFinallyStack.size + enclosingLabeledBlocks.size
     *  }}}
     */
    private var currentUnwindingStackDepth: Int = 0

    private var enclosingTryFinallyStack: List[TryFinallyEntry] = Nil

    private var enclosingLabeledBlocks: Map[LabelName, LabeledEntry] = Map.empty

    private def innermostTryFinally: Option[TryFinallyEntry] =
      enclosingTryFinallyStack.headOption

    private def enterTryFinally(entry: TryFinallyEntry)(body: => Unit): Unit = {
      assert(entry.depth == currentUnwindingStackDepth)
      enclosingTryFinallyStack ::= entry
      currentUnwindingStackDepth += 1
      try {
        body
      } finally {
        currentUnwindingStackDepth -= 1
        enclosingTryFinallyStack = enclosingTryFinallyStack.tail
      }
    }

    private def enterLabeled(entry: LabeledEntry)(body: => Unit): Unit = {
      assert(entry.depth == currentUnwindingStackDepth)
      val savedLabeledBlocks = enclosingLabeledBlocks
      enclosingLabeledBlocks = enclosingLabeledBlocks.updated(entry.irLabelName, entry)
      currentUnwindingStackDepth += 1
      try {
        body
      } finally {
        currentUnwindingStackDepth -= 1
        enclosingLabeledBlocks = savedLabeledBlocks
      }
    }

    /** The last destination tag that was allocated to a LabeledEntry. */
    private var lastDestinationTag: Int = 0

    private def allocateDestinationTag(): Int = {
      lastDestinationTag += 1
      lastDestinationTag
    }

    /** Information about an enclosing `TryFinally` block. */
    private final class TryFinallyEntry(val depth: Int) {
      private var _crossInfo: Option[(wanme.LocalID, wanme.LabelID)] = None

      def isInside(labeledEntry: LabeledEntry): Boolean =
        this.depth > labeledEntry.depth

      def wasCrossed: Boolean = _crossInfo.isDefined

      def requireCrossInfo(): (wanme.LocalID, wanme.LabelID) = {
        _crossInfo.getOrElse {
          val info = (addSyntheticLocal(watpe.Int32), fb.genLabel())
          _crossInfo = Some(info)
          info
        }
      }
    }

    /** Information about an enclosing `Labeled` block. */
    private final class LabeledEntry(val depth: Int,
        val irLabelName: LabelName, val expectedType: Type) {

      /** The regular label for this `Labeled` block, used for `Return`s that do not cross a
       *  `TryFinally`.
       */
      val regularWasmLabel: wanme.LabelID = fb.genLabel()

      /** The destination tag allocated to this label, used by the `finally` blocks to keep
       *  propagating to the right destination.
       *
       *  Destination tags are always `> 0`. The value `0` is reserved for fall-through.
       */
      private var destinationTag: Int = 0

      /** The locals in which to store the result of the label if we have to cross a `try..finally`. */
      private var resultLocals: List[wanme.LocalID] = null

      /** An additional Wasm label that has a `[]` result, and which will get its result from the
       *  `resultLocal` instead of expecting it on the stack.
       */
      private var crossLabel: wanme.LabelID = null

      def wasCrossUsed: Boolean = destinationTag != 0

      def requireCrossInfo(): (Int, List[wanme.LocalID], wanme.LabelID) = {
        if (destinationTag == 0) {
          destinationTag = allocateDestinationTag()
          val resultTypes = transformResultType(expectedType)
          resultLocals = resultTypes.map(addSyntheticLocal(_))
          crossLabel = fb.genLabel()
        }

        (destinationTag, resultLocals, crossLabel)
      }
    }

    def genLabeled(t: Labeled, expectedType: Type): Type = {
      val entry = new LabeledEntry(currentUnwindingStackDepth, t.label.name, expectedType)

      val ty = transformResultType(expectedType)

      markPosition(t)

      // Manual wa.Block here because we have a specific `label`
      fb += wa.Block(
        fb.sigToBlockType(Sig(Nil, ty)),
        Some(entry.regularWasmLabel)
      )

      /* Remember the position in the instruction stream, in case we need to
       * come back and insert the wa.Block for the cross handling.
       */
      val instrsBlockBeginIndex = fb.markCurrentInstructionIndex()

      // Emit the body
      enterLabeled(entry) {
        genTree(t.body, expectedType)
      }

      markPosition(t)

      // Deal with crossing behavior
      if (entry.wasCrossUsed) {
        assert(
          expectedType != NothingType,
          "The tryFinallyCrossLabel should not have been used for label " +
            s"${t.label.name.nameString} of type nothing"
        )

        /* In this case we need to handle situations where we receive the value
         * from the label's `result` local, branching out of the label's
         * `crossLabel`.
         *
         * Instead of the standard shape
         *
         * block $labeled (result t)
         *   body
         * end
         *
         * We need to amend the shape to become
         *
         * block $labeled (result t)
         *   block $crossLabel
         *     body            ; inside the body, jumps to this label after a
         *                     ; `finally` are compiled as `br $crossLabel`
         *     br $labeled
         *   end
         *   local.get $label.resultLocals ; (0 to many)
         * end
         */

        val (_, resultLocals, crossLabel) = entry.requireCrossInfo()

        // Go back and insert the `block $crossLabel` right after `block $labeled`
        fb.insert(instrsBlockBeginIndex, wa.Block(wa.BlockType.ValueType(), Some(crossLabel)))

        // Add the `br`, `end` and `local.get` at the current position, as usual
        fb += wa.Br(entry.regularWasmLabel)
        fb += wa.End
        for (local <- resultLocals)
          fb += wa.LocalGet(local)
      }

      fb += wa.End

      if (expectedType == NothingType)
        fb += wa.Unreachable

      expectedType
    }

    def genTryFinally(t: TryFinally, expectedType: Type): Type = {
      val entry = new TryFinallyEntry(currentUnwindingStackDepth)

      val resultType = transformResultType(expectedType)
      val resultLocals = resultType.map(addSyntheticLocal(_))

      markPosition(t)

      fb.block() { doneLabel =>
        fb.block(watpe.RefType.exnref) { catchLabel =>
          /* Remember the position in the instruction stream, in case we need
           * to come back and insert the wa.BLOCK for the cross handling.
           */
          val instrsBlockBeginIndex = fb.markCurrentInstructionIndex()

          fb.tryTable()(List(wa.CatchClause.CatchAllRef(catchLabel))) {
            // try block
            enterTryFinally(entry) {
              genTree(t.block, expectedType)
            }

            markPosition(t)

            // store the result in locals during the finally block
            for (resultLocal <- resultLocals.reverse)
              fb += wa.LocalSet(resultLocal)
          }

          /* If this try..finally was crossed by a `Return`, we need to amend
           * the shape of our try part to
           *
           * block $catch (result exnref)
           *   block $cross
           *     try_table (catch_all_ref $catch)
           *       body
           *       set_local $results ; 0 to many
           *     end
           *     i32.const 0 ; 0 always means fall-through
           *     local.set $destinationTag
           *   end
           *   ref.null exn
           * end
           */
          if (entry.wasCrossed) {
            val (destinationTagLocal, crossLabel) = entry.requireCrossInfo()

            // Go back and insert the `block $cross` right after `block $catch`
            fb.insert(
              instrsBlockBeginIndex,
              wa.Block(wa.BlockType.ValueType(), Some(crossLabel))
            )

            // And the other amendments normally
            fb += wa.I32Const(0)
            fb += wa.LocalSet(destinationTagLocal)
            fb += wa.End // of the inserted wa.BLOCK
          }

          // on success, push a `null_ref exn` on the stack
          fb += wa.RefNull(watpe.HeapType.Exn)
        } // end block $catch

        // finally block (during which we leave the `(ref null exn)` on the stack)
        genTree(t.finalizer, NoType)

        markPosition(t)

        if (!entry.wasCrossed) {
          // If the `exnref` is non-null, rethrow it
          fb += wa.BrOnNull(doneLabel)
          fb += wa.ThrowRef
        } else {
          /* If the `exnref` is non-null, rethrow it.
           * Otherwise, stay within the `$done` block.
           */
          fb.block(Sig(List(watpe.RefType.exnref), Nil)) { exnrefIsNullLabel =>
            fb += wa.BrOnNull(exnrefIsNullLabel)
            fb += wa.ThrowRef
          }

          /* Otherwise, use a br_table to dispatch to the right destination
           * based on the value of the try..finally's destinationTagLocal,
           * which is set by `Return` or to 0 for fall-through.
           */

          // The order does not matter here because they will be "re-sorted" by emitwa.BRTable
          val possibleTargetEntries =
            enclosingLabeledBlocks.valuesIterator.filter(_.wasCrossUsed).toList

          val nextTryFinallyEntry = innermostTryFinally // note that we're out of ourselves already
            .filter(nextTry => possibleTargetEntries.exists(nextTry.isInside(_)))

          /* Build the destination table for `br_table`. Target Labeled's that
           * are outside of the next try..finally in line go to the latter;
           * for other `Labeled`'s, we go to their cross label.
           */
          val brTableDests: List[(Int, wanme.LabelID)] = possibleTargetEntries.map { targetEntry =>
            val (destinationTag, _, crossLabel) = targetEntry.requireCrossInfo()
            val label = nextTryFinallyEntry.filter(_.isInside(targetEntry)) match {
              case None          => crossLabel
              case Some(nextTry) => nextTry.requireCrossInfo()._2
            }
            destinationTag -> label
          }

          fb += wa.LocalGet(entry.requireCrossInfo()._1)
          for (nextTry <- nextTryFinallyEntry) {
            // Transfer the destinationTag to the next try..finally in line
            fb += wa.LocalTee(nextTry.requireCrossInfo()._1)
          }
          emitBRTable(brTableDests, doneLabel)
        }
      } // end block $done

      // reload the result onto the stack
      for (resultLocal <- resultLocals)
        fb += wa.LocalGet(resultLocal)

      if (expectedType == NothingType)
        fb += wa.Unreachable

      expectedType
    }

    private def emitBRTable(
        dests: List[(Int, wanme.LabelID)],
        defaultLabel: wanme.LabelID
    ): Unit = {
      dests match {
        case Nil =>
          fb += wa.Drop
          fb += wa.Br(defaultLabel)

        case (singleDestValue, singleDestLabel) :: Nil =>
          /* Common case (as far as getting here in the first place is concerned):
           * All the `Return`s that cross the current `TryFinally` have the same
           * target destination (namely the enclosing `def` in the original program).
           */
          fb += wa.I32Const(singleDestValue)
          fb += wa.I32Eq
          fb += wa.BrIf(singleDestLabel)
          fb += wa.Br(defaultLabel)

        case _ :: _ =>
          // `max` is safe here because the list is non-empty
          val table = Array.fill(dests.map(_._1).max + 1)(defaultLabel)
          for (dest <- dests)
            table(dest._1) = dest._2
          fb += wa.BrTable(table.toList, defaultLabel)
      }
    }

    def genReturn(t: Return): Type = {
      val targetEntry = enclosingLabeledBlocks(t.label.name)

      genTree(t.expr, targetEntry.expectedType)

      markPosition(t)

      if (targetEntry.expectedType != NothingType) {
        innermostTryFinally.filter(_.isInside(targetEntry)) match {
          case None =>
            // Easy case: directly branch out of the block
            fb += wa.Br(targetEntry.regularWasmLabel)

          case Some(tryFinallyEntry) =>
            /* Here we need to branch to the innermost enclosing `finally` block,
             * while remembering the destination label and the result value.
             */
            val (destinationTag, resultLocals, _) = targetEntry.requireCrossInfo()
            val (destinationTagLocal, crossLabel) = tryFinallyEntry.requireCrossInfo()

            // 1. Store the result in the label's result locals.
            for (local <- resultLocals.reverse)
              fb += wa.LocalSet(local)

            // 2. Store the label's destination tag into the try..finally's destination local.
            fb += wa.I32Const(destinationTag)
            fb += wa.LocalSet(destinationTagLocal)

            // 3. Branch to the enclosing `finally` block's cross label.
            fb += wa.Br(crossLabel)
        }
      }

      NothingType
    }
  }
}
