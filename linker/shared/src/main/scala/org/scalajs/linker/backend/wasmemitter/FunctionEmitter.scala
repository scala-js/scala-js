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

import scala.annotation.{switch, tailrec}

import scala.collection.mutable

import org.scalajs.ir.{ClassKind, OriginalName, Position, UTF8String}
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.CheckedBehavior
import org.scalajs.linker.backend.emitter.Transients

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Identitities => wanme}
import org.scalajs.linker.backend.webassembly.{Types => watpe}
import org.scalajs.linker.backend.webassembly.Types.{FunctionType => Sig}

import org.scalajs.linker.backend.javascript.{Trees => js}

import EmbeddedConstants._
import SWasmGen._
import VarGen._
import TypeTransformer._

object FunctionEmitter {

  private val dotUTF8String = UTF8String(".")

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

      emitter.returnWithNPEScope() {
        emitter.genBlockStats(ctorBody.beforeSuper) {
          // Build and return the preSuperEnv struct
          for (varDef <- preSuperDecls) {
            val localID = (emitter.lookupLocal(varDef.name.name): @unchecked) match {
              case VarStorage.Local(localID) => localID
            }
            emitter.fb += wa.LocalGet(localID)
          }
          emitter.fb += wa.StructNew(preSuperEnvStructTypeID)
        }
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
        captureLikes: List[(LocalName, Type)]
    ): Env = {
      val dataStructTypeID = ctx.getClosureDataStructType(captureLikes.map(_._2))
      val param = fb.addParam(captureParamName, watpe.RefType(dataStructTypeID))
      val env: List[(LocalName, VarStorage)] = for {
        ((name, _), idx) <- captureLikes.zipWithIndex
      } yield {
        val storage = VarStorage.StructField(
          param,
          dataStructTypeID,
          genFieldID.captureParam(idx)
        )
        name -> storage
      }
      env.toMap
    }

    val captureParamsEnv: Env = captureParamDefs match {
      case None =>
        Map.empty
      case Some(defs) =>
        addCaptureLikeParamListAndMakeEnv("__captureData",
            defs.map(p => p.name.name -> p.ptpe))
    }

    val preSuperEnvEnv: Env = preSuperVarDefs match {
      case None =>
        Map.empty
      case Some(defs) =>
        addCaptureLikeParamListAndMakeEnv("__preSuperEnv",
            defs.map(p => p.name.name -> p.vtpe))
    }

    val newTargetStorage = if (!hasNewTarget) {
      None
    } else {
      val newTargetParam = fb.addParam(newTargetOriginalName, watpe.RefType.anyref)
      Some(VarStorage.Local(newTargetParam))
    }

    val receiverEnv: Env = receiverType match {
      case None =>
        Map.empty
      case Some(tpe) =>
        val receiverParam = fb.addParam(receiverOriginalName, tpe)
        Map(LocalName.This -> VarStorage.Local(receiverParam))
    }

    val normalParamsEnv: Env = paramDefs.map { paramDef =>
      val param = fb.addParam(
        paramDef.originalName.orElse(paramDef.name.name),
        transformParamType(paramDef.ptpe)
      )
      paramDef.name.name -> VarStorage.Local(param)
    }.toMap

    val fullEnv: Env = captureParamsEnv ++ preSuperEnvEnv ++ receiverEnv ++ normalParamsEnv

    fb.setResultTypes(resultTypes)

    new FunctionEmitter(
      fb,
      enclosingClassName,
      newTargetStorage,
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
    sealed abstract class NonStructStorage extends VarStorage

    final case class Local(localID: wanme.LocalID) extends NonStructStorage

    // We use Vector here because we want a decent reverseIterator
    final case class LocalRecord(fields: Vector[(SimpleFieldName, NonStructStorage)])
        extends NonStructStorage

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
    paramsEnv: FunctionEmitter.Env
)(implicit ctx: WasmContext) {
  import FunctionEmitter._

  private val coreSpec = ctx.coreSpec
  import coreSpec.semantics

  private var currentNPELabel: Option[wanme.LabelID] = null
  private var closureIdx: Int = 0
  private var currentEnv: Env = paramsEnv

  private def newTargetStorage: VarStorage.Local =
    _newTargetStorage.getOrElse(throw new Error("Cannot access new.target in this context."))

  /** Opens a new scope in which NPEs can be thrown by jumping to the NPE label.
   *
   *  When NPEs are unchecked this is a no-op (other than calling `body`).
   *
   *  Otherwise, this method logically generates the following code:
   *
   *  {{{
   *  block resultType $noNPELabel
   *    block $npeLabel
   *      body
   *      br $noNPELabel
   *    end
   *    call $throwNullPointerException
   *    unreachable
   *  end
   *  }}}
   *
   *  Inside the `body`, it is therefore possible to throw an NPE by jumping to
   *  the `npeLabel`. This is typically done through a `br_on_null $npeLabel`
   *  instruction.
   *
   *  If the `npeLabel` is not actually requested while generated `body`, the
   *  surrounding code is skipped.
   */
  private def withNPEScope[A](resultType: List[watpe.Type])(body: => A): A = {
    if (semantics.nullPointers == CheckedBehavior.Unchecked) {
      body
    } else {
      val savedNPELabel = currentNPELabel

      currentNPELabel = None
      val startIndex = fb.markCurrentInstructionIndex()

      val result = body

      for (npeLabel <- currentNPELabel) {
        val noNPELabel = fb.genLabel()

        // Go back and open the two blocks
        val blockType = fb.sigToBlockType(watpe.FunctionType(Nil, resultType))
        fb.insertAll(
          startIndex,
          List(
            wa.Block(blockType, Some(noNPELabel)),
            wa.Block(wa.BlockType.ValueType(), Some(npeLabel))
          )
        )

        // Add the code after the body in the normal way
        fb += wa.Br(noNPELabel)
        fb += wa.End // npeLabel
        fb += wa.Call(genFunctionID.throwNullPointerException)
        fb += wa.Unreachable
        fb += wa.End // noNPELabel
      }

      currentNPELabel = savedNPELabel
      result
    }
  }

  /** Like `withNPEScope`, but `return` for the success path.
   *
   *  This alternative can be used instead of `withNPEScope` when the `body`
   *  is what gets returned from the current function. It generates better code
   *  for that common case, namely:
   *
   *  {{{
   *  block $npeLabel
   *    body
   *    return
   *  end
   *  call $throwNullPointerException
   *  unreachable
   *  }}}
   */
  private def returnWithNPEScope[A]()(body: => A): A = {
    if (semantics.nullPointers == CheckedBehavior.Unchecked) {
      body
    } else {
      val savedNPELabel = currentNPELabel

      currentNPELabel = None
      val startIndex = fb.markCurrentInstructionIndex()

      val result = body

      for (npeLabel <- currentNPELabel) {
        // Go back and open the block
        fb.insert(startIndex, wa.Block(wa.BlockType.ValueType(), Some(npeLabel)))

        // Add the code after the body in the normal way
        fb += wa.Return
        fb += wa.End // npeLabel
        fb += wa.Call(genFunctionID.throwNullPointerException)
        fb += wa.Unreachable
      }

      currentNPELabel = savedNPELabel
      result
    }
  }

  private def getNPELabel(): wanme.LabelID = {
    assert(semantics.nullPointers != CheckedBehavior.Unchecked)
    currentNPELabel.getOrElse {
      val label = fb.genLabel()
      currentNPELabel = Some(label)
      label
    }
  }

  /** Emits a `ref.as_non_null` or an NPE check if required for the given `Tree`.
   *
   *  This method does not emit `tree`. It only uses it to determine whether
   *  a check is required.
   */
  private def genAsNonNullOrNPEFor(tree: Tree): Unit = {
    if (tree.tpe.isNullable) {
      if (tree.tpe == NullType)
        genNPE()
      else if (semantics.nullPointers != CheckedBehavior.Unchecked)
        fb += wa.BrOnNull(getNPELabel())
      else
        fb += wa.RefAsNonNull
    }
  }

  /** Emits an NPE check if required for the given `Tree`, otherwise nothing.
   *
   *  This method does not emit `tree`. It only uses it to determine whether
   *  a check is required.
   *
   *  Unlike `genAsNonNullOrNPE`, after this codegen the value on the stack is
   *  still statically typed as nullable at the Wasm level.
   */
  private def genCheckNonNullFor(tree: Tree): Unit = {
    if (tree.tpe.isNullable && semantics.nullPointers != CheckedBehavior.Unchecked)
      fb += wa.BrOnNull(getNPELabel())
  }

  /** Emits an unconditional NPE. */
  private def genNPE(): Unit = {
    if (semantics.nullPointers == CheckedBehavior.Unchecked)
      fb += wa.Unreachable
    else
      fb += wa.Br(getNPELabel())
  }

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

  private def lookupRecordSelect(tree: RecordSelect): VarStorage.NonStructStorage = {
    val RecordSelect(record, field) = tree

    val recordStorage = record match {
      case VarRef(name) =>
        lookupLocal(name)
      case record: RecordSelect =>
        lookupRecordSelect(record)
      case _ =>
        throw new AssertionError(s"Unexpected record tree: $record")
    }

    recordStorage match {
      case VarStorage.LocalRecord(fields) =>
        fields.find(_._1 == field.name).getOrElse {
          throw new AssertionError(s"Unknown field ${field.name} of $record")
        }._2
      case other =>
        throw new AssertionError(s"Unexpected storage $other for record $record")
    }
  }

  @tailrec
  private def canLookupRecordSelect(tree: RecordSelect): Boolean = {
    tree.record match {
      case _: VarRef            => true
      case record: RecordSelect => canLookupRecordSelect(record)
      case _                    => false
    }
  }

  private def addSyntheticLocal(tpe: watpe.Type): wanme.LocalID =
    fb.addLocal(NoOriginalName, tpe)

  private def genClosureFuncOriginalName(): OriginalName = {
    if (fb.functionOriginalName.isEmpty) {
      NoOriginalName
    } else {
      val innerName = OriginalName(fb.functionOriginalName.get ++ UTF8String("__c" + closureIdx))
      closureIdx += 1
      innerName
    }
  }

  private def markPosition(pos: Position): Unit =
    fb += wa.PositionMark(pos)

  private def markPosition(tree: Tree): Unit =
    markPosition(tree.pos)

  def genBody(tree: Tree, expectedType: Type): Unit = {
    returnWithNPEScope() {
      genTree(tree, expectedType)
    }
  }

  def genTreeAuto(tree: Tree): Unit =
    genTree(tree, tree.tpe)

  def genTreeToAny(tree: Tree): Unit =
    genTree(tree, if (tree.tpe.isNullable) AnyType else AnyNotNullType)

  def genTree(tree: Tree, expectedType: Type): Unit = {
    val generatedType: Type = tree match {
      case t: Literal             => genLiteral(t, expectedType)
      case t: UnaryOp             => genUnaryOp(t)
      case t: BinaryOp            => genBinaryOp(t)
      case t: VarRef              => genVarRef(t)
      case t: LoadModule          => genLoadModule(t)
      case t: StoreModule         => genStoreModule(t)
      case t: ApplyStatically     => genApplyStatically(t)
      case t: Apply               => genApply(t)
      case t: ApplyStatic         => genApplyStatic(t)
      case t: ApplyDynamicImport  => genApplyDynamicImport(t)
      case t: IsInstanceOf        => genIsInstanceOf(t)
      case t: AsInstanceOf        => genAsInstanceOf(t)
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
      case t: Match               => genMatch(t, expectedType)
      case t: Debugger            => VoidType // ignore
      case t: Skip                => VoidType

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
      case t: Closure              => genClosure(t)

      // array
      case t: NewArray    => genNewArray(t)
      case t: ArraySelect => genArraySelect(t)
      case t: ArrayValue  => genArrayValue(t)

      // Non-native JS classes
      case t: CreateJSClass     => genCreateJSClass(t)
      case t: JSPrivateSelect   => genJSPrivateSelect(t)
      case t: JSSuperSelect     => genJSSuperSelect(t)
      case t: JSSuperMethodCall => genJSSuperMethodCall(t)
      case t: JSNewTarget       => genJSNewTarget(t)

      // Records (only generated by the optimizer)
      case t: RecordSelect => genRecordSelect(t)
      case t: RecordValue  => genRecordValue(t)

      // Transients (only generated by the optimizer)
      case t: Transient => genTransient(t)

      case _:JSSuperConstructorCall | _:LinkTimeProperty =>
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
      case (_, VoidType) =>
        fb += wa.Drop
      case (primType: PrimTypeWithRef, _) =>
        // box
        primType match {
          case NullType =>
            expectedType match {
              case ClassType(BoxedStringClass, true) => fb += wa.ExternConvertAny
              case _                                 => ()
            }
          case ByteType | ShortType =>
            fb += wa.RefI31
          case CharType =>
            /* `char` and `long` are opaque to JS in the Scala.js semantics.
             * We implement them with real Wasm classes following the correct
             * vtable. Upcasting wraps a primitive into the corresponding class.
             */
            genBox(watpe.Int32, SpecialNames.CharBoxClass)
          case LongType =>
            genBox(watpe.Int64, SpecialNames.LongBoxClass)
          case VoidType | NothingType =>
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
      case (StringType | ClassType(BoxedStringClass, _), _) =>
        expectedType match {
          case ClassType(BoxedStringClass, _) => ()
          case _                              => fb += wa.AnyConvertExtern
        }
      case _ =>
        ()
    }
  }

  private def genAssign(tree: Assign): Type = {
    val Assign(lhs, rhs) = tree

    implicit val pos = tree.pos

    lhs match {
      case Select(qualifier, field) =>
        val className = field.name.className
        val classInfo = ctx.getClassInfo(className)

        // For Select, the receiver can never be a hijacked class, so we can use genTreeAuto
        genTreeAuto(qualifier)

        if (!classInfo.hasInstances) {
          /* The field may not exist in that case, and we cannot look it up.
           * However we necessarily have a `null` receiver if we reach this
           * point, so we can trap as NPE.
           */
          markPosition(tree)
          genNPE()
        } else {
          genCheckNonNullFor(qualifier)
          genTree(rhs, lhs.tpe)
          markPosition(tree)
          fb += wa.StructSet(
            genTypeID.forClass(className),
            genFieldID.forClassInstanceField(field.name)
          )
        }

      case SelectStatic(field) =>
        val fieldName = field.name
        val globalID = genGlobalID.forStaticField(fieldName)

        genTree(rhs, lhs.tpe)
        markPosition(tree)
        fb += wa.GlobalSet(globalID)

        // Update top-level export mirrors
        val classInfo = ctx.getClassInfo(fieldName.className)
        val mirrors = classInfo.staticFieldMirrors.getOrElse(fieldName, Nil)
        for (exportedName <- mirrors) {
          fb += wa.GlobalGet(globalID)
          fb += wa.Call(genFunctionID.forTopLevelExportSetter(exportedName))
        }

      case ArraySelect(array, index) =>
        genTreeAuto(array)
        array.tpe match {
          case ArrayType(arrayTypeRef, _) =>
            def isPrimArray = arrayTypeRef match {
              case ArrayTypeRef(_: PrimRef, 1) => true
              case _                           => false
            }

            genCheckNonNullFor(array)

            def genRhs(): Unit = {
              genTree(rhs, lhs.tpe)
              lhs.tpe match {
                case ClassType(BoxedStringClass, _) => fb += wa.AnyConvertExtern
                case _                              => ()
              }
            }

            if (semantics.arrayIndexOutOfBounds == CheckedBehavior.Unchecked &&
                (semantics.arrayStores == CheckedBehavior.Unchecked || isPrimArray)) {
              // Get the underlying array
              fb += wa.StructGet(
                genTypeID.forArrayClass(arrayTypeRef),
                genFieldID.objStruct.arrayUnderlying
              )
              genTree(index, IntType)
              genRhs()
              markPosition(tree)
              fb += wa.ArraySet(genTypeID.underlyingOf(arrayTypeRef))
            } else {
              genTree(index, IntType)
              genRhs()
              markPosition(tree)
              fb += wa.Call(genFunctionID.arraySetFor(arrayTypeRef))
            }
          case NothingType =>
            // unreachable
            ()
          case NullType =>
            markPosition(tree)
            genNPE()
          case _ =>
            throw new IllegalArgumentException(
                s"ArraySelect.array must be an array type, but has type ${array.tpe}")
        }

      case JSPrivateSelect(qualifier, field) =>
        genTree(qualifier, AnyType)
        fb += wa.GlobalGet(genGlobalID.forJSPrivateField(field.name))
        genTree(rhs, AnyType)
        markPosition(tree)
        fb += wa.Call(genFunctionID.jsSelectSet)

      case JSSelect(qualifier, item) =>
        genThroughCustomJSHelper(List(qualifier, item, rhs), VoidType) { allJSArgs =>
          val List(jsQualifier, jsItem, jsRhs) = allJSArgs
          js.Assign(js.BracketSelect.makeOptimized(jsQualifier, jsItem), jsRhs)
        }

      case JSSuperSelect(superClass, receiver, item) =>
        genTree(superClass, AnyType)
        genTree(receiver, AnyType)
        genTree(item, AnyType)
        genTree(rhs, AnyType)
        markPosition(tree)
        fb += wa.Call(genFunctionID.jsSuperSelectSet)

      case lhs @ JSGlobalRef(name) =>
        val builder = new CustomJSHelperBuilderWithTreeSupport()
        val rhsRef = builder.addInput(rhs)
        val helperID = builder.build(VoidType) {
          js.Assign(builder.genGlobalRef(name), rhsRef)
        }
        markPosition(tree)
        fb += wa.Call(helperID)

      case VarRef(name) =>
        genTree(rhs, lhs.tpe)
        markPosition(tree)
        genWriteToStorage(lookupLocal(name))

      case lhs: RecordSelect =>
        genTree(rhs, lhs.tpe)
        markPosition(tree)
        genWriteToStorage(lookupRecordSelect(lhs))
    }

    VoidType
  }

  private def genWriteToStorage(storage: VarStorage): Unit = {
    storage match {
      case VarStorage.Local(local) =>
        fb += wa.LocalSet(local)

      case VarStorage.LocalRecord(fields) =>
        fields.reverseIterator.foreach(field => genWriteToStorage(field._2))

      case storage: VarStorage.StructField =>
        throw new AssertionError(s"Unexpected write to capture storage $storage")
    }
  }

  private def genApply(tree: Apply): Type = {
    val Apply(flags, receiver, method, args) = tree

    receiver.tpe match {
      case NothingType =>
        genTree(receiver, NothingType)
        // nothing else to do; this is unreachable
        NothingType

      case NullType =>
        genTree(receiver, NullType)
        genNPE()
        NothingType

      case _ if method.name.isReflectiveProxy =>
        genReflectiveCall(tree)

      case _ =>
        val receiverClassName = receiver.tpe match {
          case prim: PrimType =>
            PrimTypeToBoxedClass(prim)
          case ClassType(cls, _) =>
            cls
          case AnyType | AnyNotNullType | ArrayType(_, _) =>
            ObjectClass
          case tpe: RecordType =>
            throw new AssertionError(s"Invalid receiver type $tpe")
        }
        val receiverClassInfo = ctx.getClassInfo(receiverClassName)

        /* Hijacked classes do not receive tables at all, and `Apply`s on array
         * types are considered to be statically resolved by the `Analyzer`.
         * Therefore, if the receiver's static type is a prim type, hijacked
         * class or array type, we must use static dispatch instead.
         *
         * This never happens when we use the optimizer, since it already turns
         * any such `Apply` into an `ApplyStatically` (when it does not inline
         * it altogether).
         */
        val useStaticDispatch = {
          receiverClassInfo.kind == ClassKind.HijackedClass ||
          receiver.tpe.isInstanceOf[ArrayType]
        }
        if (useStaticDispatch) {
          genApplyStatically(ApplyStatically(
              flags, receiver, receiverClassName, method, args)(tree.tpe)(tree.pos))
        } else {
          genApplyWithDispatch(tree, receiverClassInfo)
        }
    }
  }

  private def genReflectiveCall(tree: Apply): Type = {
    val Apply(flags, receiver, MethodIdent(methodName), args) = tree

    assert(methodName.isReflectiveProxy)

    val receiverLocalForDispatch =
      addSyntheticLocal(watpe.RefType.any)

    val proxyId = ctx.getReflectiveProxyId(methodName)
    val funcTypeID = ctx.tableFunctionType(methodName)

    /* We only need to handle calls on non-hijacked classes. For hijacked
     * classes, the compiler already emits the appropriate dispatch at the IR
     * level.
     */

    // Load receiver and arguments
    genTreeToAny(receiver)
    genAsNonNullOrNPEFor(receiver)
    fb += wa.LocalTee(receiverLocalForDispatch)
    genArgs(args, methodName)

    // Looks up the method to be (reflectively) called
    markPosition(tree)
    fb += wa.LocalGet(receiverLocalForDispatch)
    fb += wa.RefCast(watpe.RefType(genTypeID.ObjectStruct)) // see above: cannot be a hijacked class
    fb += wa.StructGet(genTypeID.ObjectStruct, genFieldID.objStruct.vtable)
    fb += wa.I32Const(proxyId)
    // `searchReflectiveProxy`: [typeData, i32] -> [(ref func)]
    fb += wa.Call(genFunctionID.searchReflectiveProxy)

    fb += wa.RefCast(watpe.RefType(watpe.HeapType(funcTypeID)))
    fb += wa.CallRef(funcTypeID)

    tree.tpe
  }

  /** Generates the code for an `Apply` tree that requires dynamic dispatch.
   *
   *  In that case, there is always at least a vtable/itable-based dispatch. It may also contain
   *  primitive-based dispatch if the receiver's type is an ancestor of a hijacked class.
   *
   *  This method must not be used if the receiver's type is a primitive, a
   *  hijacked class or an array type. Hijacked classes do not have dispatch
   *  tables, so the methods that are not available in any superclass/interface
   *  cannot be called through a table dispatch. Array types share their vtable
   *  with jl.Object, but methods called directly on an array type are not
   *  registered as called on jl.Object by the Analyzer. In all these cases,
   *  we must use a statically resolved call instead.
   */
  private def genApplyWithDispatch(tree: Apply,
      receiverClassInfo: WasmContext.ClassInfo): Type = {

    val Apply(flags, receiver, MethodIdent(methodName), args) = tree

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
      genTreeAuto(receiver)
      genAsNonNullOrNPEFor(receiver)
    }

    /* Generates a resolved call to a method of a hijacked class.
     * Before this code gen, the stack must contain the receiver and the args.
     * After this code gen, the stack contains the result.
     */
    def genHijackedClassCall(hijackedClass: ClassName): Unit = {
      val funcID = genFunctionID.forMethod(MemberNamespace.Public, hijackedClass, methodName)
      fb += wa.Call(funcID)
    }

    if (!receiverClassInfo.hasInstances) {
      /* If the target class info does not have any instance, the only possible
       * value for the receiver is `null`. We can therefore immediately trap for
       * an NPE. It is important to short-cut this path because the reachability
       * analysis may have entirely dead-code eliminated the target method,
       * which means we do not know its signature and therefore cannot emit the
       * corresponding vtable/itable calls.
       */
      genTreeAuto(receiver)
      markPosition(tree)
      genNPE()
    } else if (!receiverClassInfo.isAncestorOfHijackedClass) {
      // Standard dispatch codegen
      genReceiverNotNull()
      fb += wa.LocalTee(receiverLocalForDispatch)
      genArgs(args, methodName)

      markPosition(tree)
      genTableDispatch(receiverClassInfo, methodName, receiverLocalForDispatch)
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

      val resultType = transformResultType(tree.tpe)

      fb.block(resultType) { labelDone =>
        def pushArgs(argsLocals: List[wanme.LocalID]): Unit =
          argsLocals.foreach(argLocal => fb += wa.LocalGet(argLocal))

        /* First try the case where the value is one of our objects.
         * We load the receiver and arguments inside the block `notOurObject`.
         * This helps producing good code for the no-args case, in which we do
         * not need to store the receiver in a local at all.
         * For the case with the args, it does not hurt either way. We could
         * move it out, but that would make for a less consistent codegen.
         */
        val argsLocals = fb.block(watpe.RefType.any) { labelNotOurObject =>
          // Load receiver and arguments and store them in temporary variables
          genReceiverNotNull()
          val argsLocals = if (args.isEmpty) {
            /* When there are no arguments, we can leave the receiver directly on
             * the stack instead of going through a local. We will still need a
             * local for the table-based dispatch, though.
             */
            Nil
          } else {
            /* When there are arguments, we need to store them in temporary
             * variables. This is not required for correctness of the evaluation
             * order. It is only necessary so that we do not duplicate the
             * codegen of the arguments. If the arguments are complex, doing so
             * could lead to exponential blow-up of the generated code.
             */
            val receiverLocal = addSyntheticLocal(watpe.RefType.any)

            fb += wa.LocalSet(receiverLocal)
            val argsLocals: List[wanme.LocalID] =
              for ((arg, typeRef) <- args.zip(methodName.paramTypeRefs)) yield {
                val tpe = ctx.inferTypeFromTypeRef(typeRef)
                genTree(arg, tpe)
                val localID = addSyntheticLocal(transformParamType(tpe))
                fb += wa.LocalSet(localID)
                localID
              }
            fb += wa.LocalGet(receiverLocal)
            argsLocals
          }

          markPosition(tree) // main position marker for the entire hijacked class dispatch branch

          fb += wa.BrOnCastFail(labelNotOurObject, watpe.RefType.any, refTypeForDispatch)
          fb += wa.LocalTee(receiverLocalForDispatch)
          pushArgs(argsLocals)
          genTableDispatch(receiverClassInfo, methodName, receiverLocalForDispatch)
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

        if (methodName == toStringMethodName) {
          // By spec, toString() is special
          assert(argsLocals.isEmpty)
          fb += wa.Call(genFunctionID.jsValueToString)
        } else if (receiverClassName == JLNumberClass) {
          // the value must be a `number`, hence we can unbox to `double`
          genUnbox(DoubleType)
          pushArgs(argsLocals)
          genHijackedClassCall(BoxedDoubleClass)
        } else if (receiverClassName == CharSequenceClass) {
          // the value must be a `string`
          fb += wa.ExternConvertAny
          pushArgs(argsLocals)
          genHijackedClassCall(BoxedStringClass)
        } else if (methodName == compareToMethodName) {
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
              /* The jsValueTypeLocal is the boolean value, thanks to the chosen encoding.
               * This trick avoids an additional unbox.
               */
              fb += wa.LocalGet(jsValueTypeLocal)
              pushArgs(argsLocals)
              genHijackedClassCall(BoxedBooleanClass)
            },
            // case JSValueTypeString =>
            List(JSValueTypeString) -> { () =>
              fb += wa.LocalGet(receiverLocal)
              fb += wa.ExternConvertAny
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
          methodName match {
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

    if (tree.tpe == NothingType)
      fb += wa.Unreachable

    tree.tpe
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
      fb += wa.StructGet(genTypeID.ObjectStruct, genFieldID.objStruct.itables)
      fb += wa.StructGet(
        genTypeID.itables,
        genFieldID.itablesStruct.itableSlot(receiverClassInfo.itableIdx)
      )
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

  private def genApplyStatically(tree: ApplyStatically): Type = {
    val ApplyStatically(flags, receiver, className, MethodIdent(methodName), args) = tree

    receiver.tpe match {
      case NothingType =>
        genTree(receiver, NothingType)
        // nothing else to do; this is unreachable
        NothingType

      case NullType =>
        genTree(receiver, NullType)
        markPosition(tree)
        genNPE()
        NothingType

      case _ =>
        val namespace = MemberNamespace.forNonStaticCall(flags)
        val targetClassName = {
          val classInfo = ctx.getClassInfo(className)
          if (!classInfo.isInterface && namespace == MemberNamespace.Public)
            classInfo.resolvedMethodInfos(methodName).ownerClass
          else
            className
        }

        BoxedClassToPrimType.get(targetClassName) match {
          case None =>
            genTree(receiver, ClassType(targetClassName, nullable = receiver.tpe.isNullable))
            genAsNonNullOrNPEFor(receiver)

          case Some(primReceiverType) =>
            if (receiver.tpe == primReceiverType) {
              genTreeAuto(receiver)
            } else {
              genTreeToAny(receiver)
              genAsNonNullOrNPEFor(receiver)
              genUnbox(primReceiverType)
            }
        }

        genArgs(args, methodName)

        markPosition(tree)
        val funcID = genFunctionID.forMethod(namespace, targetClassName, methodName)
        fb += wa.Call(funcID)
        if (tree.tpe == NothingType)
          fb += wa.Unreachable
        tree.tpe
    }
  }

  private def genApplyStatic(tree: ApplyStatic): Type = {
    val ApplyStatic(flags, className, MethodIdent(methodName), args) = tree

    genArgs(args, methodName)
    val namespace = MemberNamespace.forStaticCall(flags)
    val funcID = genFunctionID.forMethod(namespace, className, methodName)
    markPosition(tree)
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

  private def genLiteral(tree: Literal, expectedType: Type): Type = {
    if (expectedType == VoidType) {
      /* Since all literals are pure, we can always get rid of them.
       * This is mostly useful for the argument of `Return` nodes that target a
       * `Labeled` in statement position, since they must have a non-`void`
       * type in the IR but they get a `void` expected type.
       */
      expectedType
    } else if (tree.isInstanceOf[Null] && expectedType == ClassType(BoxedStringClass, true)) {
      /* Directly emit a `ref.null noextern` instead of requiring an
       * `extern.convert_from_any` in `genAdapt`.
       */
      markPosition(tree)
      fb += wa.RefNull(watpe.HeapType.NoExtern)
      expectedType
    } else {
      markPosition(tree)

      tree match {
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
          fb ++= ctx.stringPool.getConstantStringInstr(v)

        case ClassOf(typeRef) =>
          genLoadTypeData(fb, typeRef)
          fb += wa.Call(genFunctionID.getClassOf)
      }

      tree.tpe
    }
  }

  private def genSelect(tree: Select): Type = {
    val Select(qualifier, FieldIdent(fieldName)) = tree

    val className = fieldName.className
    val classInfo = ctx.getClassInfo(className)

    // For Select, the receiver can never be a hijacked class, so we can use genTreeAuto
    genTreeAuto(qualifier)

    markPosition(tree)

    if (!classInfo.hasInstances) {
      /* The field may not exist in that case, and we cannot look it up.
       * However we necessarily have a `null` receiver if we reach this point,
       * so we can trap as NPE.
       */
      genNPE()
    } else {
      genCheckNonNullFor(qualifier)
      fb += wa.StructGet(
        genTypeID.forClass(className),
        genFieldID.forClassInstanceField(fieldName)
      )
    }

    tree.tpe
  }

  private def genSelectStatic(tree: SelectStatic): Type = {
    val SelectStatic(FieldIdent(fieldName)) = tree

    markPosition(tree)
    fb += wa.GlobalGet(genGlobalID.forStaticField(fieldName))
    tree.tpe
  }

  private def genStoreModule(tree: StoreModule): Type = {
    val className = enclosingClassName.getOrElse {
      throw new AssertionError(s"Cannot emit $tree at ${tree.pos} without enclosing class name")
    }

    markPosition(tree)
    genReadStorage(lookupLocal(LocalName.This))
    fb += wa.GlobalSet(genGlobalID.forModuleInstance(className))
    VoidType
  }

  private def genLoadModule(tree: LoadModule): Type = {
    val LoadModule(className) = tree

    markPosition(tree)
    fb += wa.Call(genFunctionID.loadModule(className))
    tree.tpe
  }

  private def genUnaryOp(tree: UnaryOp): Type = {
    // scalastyle:off return

    import UnaryOp._

    val UnaryOp(op, lhs) = tree

    /* Touch of peephole optimization; useful so that the various operators can
     * assume the NothingType case away (e.g., `Array_length`, `Clone`).
     */
    if (lhs.tpe == NothingType) {
      genTreeAuto(lhs)
      return NothingType
    }

    // scalastyle:on return

    genTree(lhs, op match {
      case IdentityHashCode        => AnyNotNullType
      case WrapAsThrowable | Throw => AnyType
      case _                       => lhs.tpe
    })

    markPosition(tree)

    (op: @switch) match {
      case Boolean_! =>
        fb += wa.I32Eqz

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

      // Long -> Float (neither widening nor narrowing)
      case LongToFloat =>
        fb += wa.F32ConvertI64S

      // String.length
      case String_length =>
        fb += wa.Call(genFunctionID.stringBuiltins.length)

      // Null check
      case CheckNotNull =>
        genAsNonNullOrNPEFor(lhs)

      // Class operations
      case Class_name =>
        fb += wa.StructGet(genTypeID.ClassStruct, genFieldID.classData)
        fb += wa.Call(genFunctionID.typeDataName)
      case Class_isPrimitive =>
        fb += wa.StructGet(genTypeID.ClassStruct, genFieldID.classData)
        fb += wa.StructGet(genTypeID.typeData, genFieldID.typeData.kind)
        fb += wa.I32Const(KindLastPrimitive)
        fb += wa.I32LeU
      case Class_isInterface =>
        fb += wa.StructGet(genTypeID.ClassStruct, genFieldID.classData)
        fb += wa.StructGet(genTypeID.typeData, genFieldID.typeData.kind)
        fb += wa.I32Const(KindInterface)
        fb += wa.I32Eq
      case Class_isArray =>
        fb += wa.StructGet(genTypeID.ClassStruct, genFieldID.classData)
        fb += wa.StructGet(genTypeID.typeData, genFieldID.typeData.kind)
        fb += wa.I32Const(KindArray)
        fb += wa.I32Eq
      case Class_componentType =>
        fb += wa.Call(genFunctionID.getComponentType)
      case Class_superClass =>
        fb += wa.Call(genFunctionID.getSuperClass)

      case Array_length =>
        val ArrayType(arrayTypeRef, _) = lhs.tpe: @unchecked
        fb += wa.StructGet(
          genTypeID.forArrayClass(arrayTypeRef),
          genFieldID.objStruct.arrayUnderlying
        )
        fb += wa.ArrayLen

      case GetClass =>
        val needHijackedClassDispatch = lhs.tpe match {
          case ClassType(className, _) =>
            ctx.getClassInfo(className).isAncestorOfHijackedClass
          case ArrayType(_, _) =>
            false
          case _ =>
            true
        }

        if (!needHijackedClassDispatch) {
          fb += wa.StructGet(genTypeID.ObjectStruct, genFieldID.objStruct.vtable)
          fb += wa.Call(genFunctionID.getClassOf)
        } else {
          genAdapt(lhs.tpe, AnyNotNullType) // no-op when the optimizer is enabled
          fb += wa.Call(genFunctionID.anyGetClass)
        }

      case Clone =>
        val lhsLocal = addSyntheticLocal(watpe.RefType(genTypeID.ObjectStruct))
        fb += wa.LocalTee(lhsLocal)
        fb += wa.LocalGet(lhsLocal)
        fb += wa.StructGet(genTypeID.ObjectStruct, genFieldID.objStruct.vtable)
        fb += wa.StructGet(genTypeID.typeData, genFieldID.typeData.cloneFunction)
        // cloneFunction: (ref jl.Object) -> (ref jl.Object)
        fb += wa.CallRef(genTypeID.cloneFunctionType)

        // cast the (ref jl.Object) back down to the result type
        transformSingleType(lhs.tpe) match {
          case watpe.RefType(_, watpe.HeapType.Type(genTypeID.ObjectStruct)) =>
            // no need to cast to (ref null? jl.Object)
          case wasmType: watpe.RefType =>
            fb += wa.RefCast(wasmType.toNonNullable)
          case wasmType =>
            // Since no hijacked class extends jl.Cloneable, this case cannot happen
            throw new AssertionError(
                s"Unexpected type for Clone: ${lhs.tpe} (Wasm: $wasmType)")
        }

      case IdentityHashCode =>
        // TODO Avoid dispatch when we know a more precise type than any
        fb += wa.Call(genFunctionID.identityHashCode)

      case WrapAsThrowable =>
        val nonNullThrowableType = watpe.RefType(genTypeID.ThrowableStruct)
        val jsExceptionType = watpe.RefType(genTypeID.JSExceptionStruct)

        val anyRefToNonNullThrowable =
          Sig(List(watpe.RefType.anyref), List(nonNullThrowableType))
        fb.block(anyRefToNonNullThrowable) { doneLabel =>
          // if expr.isInstanceOf[Throwable], then br $done
          fb += wa.BrOnCast(doneLabel, watpe.RefType.anyref, nonNullThrowableType)

          // otherwise, wrap in a new JavaScriptException

          val lhsLocal = addSyntheticLocal(watpe.RefType.anyref)
          val instanceLocal = addSyntheticLocal(jsExceptionType)

          fb += wa.LocalSet(lhsLocal)
          fb += wa.Call(genFunctionID.newDefault(SpecialNames.JSExceptionClass))
          fb += wa.LocalTee(instanceLocal)
          fb += wa.LocalGet(lhsLocal)
          fb += wa.Call(
            genFunctionID.forMethod(
              MemberNamespace.Constructor,
              SpecialNames.JSExceptionClass,
              SpecialNames.AnyArgConstructorName
            )
          )
          fb += wa.LocalGet(instanceLocal)
        }

      case UnwrapFromThrowable =>
        val nonNullThrowableToAnyRef =
          Sig(List(watpe.RefType(genTypeID.ThrowableStruct)), List(watpe.RefType.anyref))
        fb.block(nonNullThrowableToAnyRef) { doneLabel =>
          // if !expr.isInstanceOf[js.JavaScriptException], then br $done
          fb += wa.BrOnCastFail(
            doneLabel,
            watpe.RefType(genTypeID.ThrowableStruct),
            watpe.RefType(genTypeID.JSExceptionStruct)
          )
          // otherwise, unwrap the JavaScriptException by reading its field
          fb += wa.StructGet(
            genTypeID.JSExceptionStruct,
            genFieldID.forClassInstanceField(SpecialNames.exceptionFieldName)
          )
        }

      case Throw =>
        fb += wa.ExternConvertAny
        fb += wa.Throw(genTagID.exception)
    }

    tree.tpe
  }

  private def genBinaryOp(tree: BinaryOp): Type = {
    import BinaryOp._

    val BinaryOp(op, lhs, rhs) = tree

    def genLongShiftOp(shiftInstr: wa.Instr): Type = {
      genTree(lhs, LongType)
      genTree(rhs, IntType)
      markPosition(tree)
      fb += wa.I64ExtendI32S
      fb += shiftInstr
      LongType
    }

    /* Gen the given tree of type `jl.Class!` then extract its `classData`.
     * If the arg is a literal `ClassOf`, we directly generate its type data.
     */
    def genTreeClassData(arg: Tree): Unit = {
      arg match {
        case ClassOf(typeRef) =>
          markPosition(arg)
          genLoadTypeData(fb, typeRef)
        case _ =>
          genTreeAuto(arg)
          fb += wa.StructGet(genTypeID.ClassStruct, genFieldID.classData)
      }
    }

    (op: @switch) match {
      case === | !== =>
        genEq(tree)

      case String_+ =>
        genStringConcat(tree)

      case Int_/ =>
        rhs match {
          case IntLiteral(rhsValue) =>
            genDivModByConstant(tree, isDiv = true, rhsValue, wa.I32Const(_), wa.I32Sub, wa.I32DivS)
          case _ =>
            genDivMod(tree, isDiv = true, wa.I32Const(_), wa.I32Eqz, wa.I32Eq, wa.I32Sub, wa.I32DivS)
        }
      case Int_% =>
        rhs match {
          case IntLiteral(rhsValue) =>
            genDivModByConstant(tree, isDiv = false, rhsValue, wa.I32Const(_), wa.I32Sub, wa.I32RemS)
          case _ =>
            genDivMod(tree, isDiv = false, wa.I32Const(_), wa.I32Eqz, wa.I32Eq, wa.I32Sub, wa.I32RemS)
        }
      case Long_/ =>
        rhs match {
          case LongLiteral(rhsValue) =>
            genDivModByConstant(tree, isDiv = true, rhsValue, wa.I64Const(_), wa.I64Sub, wa.I64DivS)
          case _ =>
            genDivMod(tree, isDiv = true, wa.I64Const(_), wa.I64Eqz, wa.I64Eq, wa.I64Sub, wa.I64DivS)
        }
      case Long_% =>
        rhs match {
          case LongLiteral(rhsValue) =>
            genDivModByConstant(tree, isDiv = false, rhsValue, wa.I64Const(_), wa.I64Sub, wa.I64RemS)
          case _ =>
            genDivMod(tree, isDiv = false, wa.I64Const(_), wa.I64Eqz, wa.I64Eq, wa.I64Sub, wa.I64RemS)
        }

      case Long_<< =>
        genLongShiftOp(wa.I64Shl)
      case Long_>>> =>
        genLongShiftOp(wa.I64ShrU)
      case Long_>> =>
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
      case Float_% =>
        genTree(lhs, FloatType)
        fb += wa.F64PromoteF32
        genTree(rhs, FloatType)
        fb += wa.F64PromoteF32
        markPosition(tree)
        fb += wa.Call(genFunctionID.fmod)
        fb += wa.F32DemoteF64
        FloatType
      case Double_% =>
        genTree(lhs, DoubleType)
        genTree(rhs, DoubleType)
        markPosition(tree)
        fb += wa.Call(genFunctionID.fmod)
        DoubleType

      case String_charAt =>
        genTree(lhs, StringType)
        genTree(rhs, IntType)
        markPosition(tree)
        if (semantics.stringIndexOutOfBounds == CheckedBehavior.Unchecked)
          fb += wa.Call(genFunctionID.stringBuiltins.charCodeAt)
        else
          fb += wa.Call(genFunctionID.checkedStringCharAt)
        CharType

      // Class operations for which genTreeAuto would not do the right thing
      case Class_isInstance =>
        genTreeClassData(lhs)
        genTree(rhs, AnyType)
        markPosition(tree)
        fb += wa.Call(genFunctionID.isInstance)
        BooleanType
      case Class_isAssignableFrom =>
        genTreeClassData(lhs)
        genTreeClassData(rhs)
        markPosition(tree)
        fb += wa.Call(genFunctionID.isAssignableFrom)
        BooleanType
      case Class_cast =>
        if (semantics.asInstanceOfs != CheckedBehavior.Unchecked) {
          genTreeAuto(lhs)
          genTree(rhs, AnyType)
          markPosition(tree)
          fb += wa.Call(genFunctionID.cast)
          AnyType
        } else {
          genTree(lhs, VoidType)
          genTreeAuto(rhs)
          rhs.tpe
        }

      case _ =>
        genTreeAuto(lhs)
        genTreeAuto(rhs)
        markPosition(tree)
        fb += getElementaryBinaryOpInstr(op)
        tree.tpe
    }
  }

  private def genEq(tree: BinaryOp): Type = {
    import BinaryOp.{===, !==}

    val BinaryOp(op, lhs, rhs) = tree
    assert(op == === || op == !==)

    def maybeGenInvert(): Unit = {
      if (op == BinaryOp.!==)
        fb += wa.I32Eqz
    }

    /* Can we use `ref.eq` for the given type?
     *
     * This is the case if the Wasm encoding of the given type a subtype of
     * `eqref`, i.e., `(ref null eq)`.
     *
     * This requires that it be a ref type of the form `(ref null? heapType)`
     * and that the `heapType` be a sub-heap-type of `eq`.
     *
     * Note that all the `HeapType.Type(_)`s returned by `transformSingleType`
     * point to struct types, which are sub-heap-types of `eq`. (In general
     * this is not true, since they could also point to `func` heap types,
     * which are not sub-heap-types of `eq`.)
     *
     * Therefore, in practice, the only heap types we can observe and that are
     * *not* sub-heap-types of `eq` are `any` and `extern`.
     */
    def canUseRefEq(tpe: Type): Boolean = transformSingleType(tpe) match {
      case watpe.RefType(_, heapType) =>
        heapType != watpe.HeapType.Any && heapType != watpe.HeapType.Extern
      case _ =>
        false
    }

    def isStringType(tpe: Type): Boolean = tpe match {
      case StringType                     => true
      case ClassType(BoxedStringClass, _) => true
      case _                              => false
    }

    val lhsType = lhs.tpe
    val rhsType = rhs.tpe

    if (lhsType == NothingType) {
      genTree(lhs, NothingType)
      NothingType
    } else if (rhsType == NothingType) {
      genTree(lhs, VoidType)
      genTree(rhs, NothingType)
      NothingType
    } else if (rhsType == NullType) {
      /* Note that the optimizer normalizes Literals on the right of `===`,
       * so testing for the `lhsType == NullType` is not as useful.
       */
      genTree(lhs, AnyType)
      genTree(rhs, VoidType) // no-op if it is actually a Null() literal
      markPosition(tree)
      fb += wa.RefIsNull
      maybeGenInvert()
      BooleanType
    } else if (canUseRefEq(lhsType) && canUseRefEq(rhsType)) {
      /* When both types translate to Wasm types that are subtypes of `eqref`,
       * we can use `ref.eq`. Note that for all possible `eqref`s (in all of
       * Wasm, not just the subset that we use), `Object.is` coincides with
       * `ref.eq`. So this is a sound optimization over using `Object.is`.
       */
      genTree(lhs, lhsType)
      genTree(rhs, rhsType)
      markPosition(tree)
      fb += wa.RefEq
      maybeGenInvert()
      BooleanType
    } else if (isStringType(lhsType) && isStringType(rhsType)) {
      genTreeAuto(lhs)
      genTreeAuto(rhs)
      markPosition(tree)
      fb += wa.Call(genFunctionID.stringBuiltins.equals)
      maybeGenInvert()
      BooleanType
    } else {
      // Otherwise, fall back on the `Object.is` helper
      genTree(lhs, AnyType)
      genTree(rhs, AnyType)
      markPosition(tree)
      fb += wa.Call(genFunctionID.is)
      maybeGenInvert()
      BooleanType
    }
  }

  private def getElementaryBinaryOpInstr(op: BinaryOp.Code): wa.Instr = {
    import BinaryOp._

    (op: @switch) match {
      case Boolean_== => wa.I32Eq
      case Boolean_!= => wa.I32Ne
      case Boolean_|  => wa.I32Or
      case Boolean_&  => wa.I32And

      case Int_+   => wa.I32Add
      case Int_-   => wa.I32Sub
      case Int_*   => wa.I32Mul
      case Int_|   => wa.I32Or
      case Int_&   => wa.I32And
      case Int_^   => wa.I32Xor
      case Int_<<  => wa.I32Shl
      case Int_>>> => wa.I32ShrU
      case Int_>>  => wa.I32ShrS
      case Int_==  => wa.I32Eq
      case Int_!=  => wa.I32Ne
      case Int_<   => wa.I32LtS
      case Int_<=  => wa.I32LeS
      case Int_>   => wa.I32GtS
      case Int_>=  => wa.I32GeS

      case Long_+ => wa.I64Add
      case Long_- => wa.I64Sub
      case Long_* => wa.I64Mul
      case Long_| => wa.I64Or
      case Long_& => wa.I64And
      case Long_^ => wa.I64Xor

      case Long_== => wa.I64Eq
      case Long_!= => wa.I64Ne
      case Long_<  => wa.I64LtS
      case Long_<= => wa.I64LeS
      case Long_>  => wa.I64GtS
      case Long_>= => wa.I64GeS

      case Float_+ => wa.F32Add
      case Float_- => wa.F32Sub
      case Float_* => wa.F32Mul
      case Float_/ => wa.F32Div

      case Double_+ => wa.F64Add
      case Double_- => wa.F64Sub
      case Double_* => wa.F64Mul
      case Double_/ => wa.F64Div

      case Double_== => wa.F64Eq
      case Double_!= => wa.F64Ne
      case Double_<  => wa.F64Lt
      case Double_<= => wa.F64Le
      case Double_>  => wa.F64Gt
      case Double_>= => wa.F64Ge

      case Class_newArray => wa.Call(genFunctionID.newArray)
    }
  }

  private def genStringConcat(tree: BinaryOp): Type = {
    val BinaryOp(op, lhs, rhs) = tree
    assert(op == BinaryOp.String_+)

    lhs match {
      case StringLiteral("") =>
        // Common case where we don't actually need a concatenation
        genToStringForConcat(rhs)

      case _ =>
        genToStringForConcat(lhs)
        genToStringForConcat(rhs)
        markPosition(tree)
        fb += wa.Call(genFunctionID.stringBuiltins.concat)
    }

    StringType
  }

  private def genToStringForConcat(tree: Tree): Unit = {
    def genWithDispatch(isAncestorOfHijackedClass: Boolean): Unit = {
      // TODO Better codegen when non-nullable

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
         * block (ref extern) $done
         *   block $isNull
         *     load receiver as (ref null java.lang.Object)
         *     br_on_null $isNull
         *     generate standard table-based dispatch
         *     br_on_non_null $done
         *   end $isNull
         *   gen "null"
         * end $done
         */

        fb.block(watpe.RefType.extern) { labelDone =>
          fb.block() { labelIsNull =>
            genTreeAuto(tree)
            markPosition(tree)
            fb += wa.BrOnNull(labelIsNull)
            fb += wa.LocalTee(receiverLocalForDispatch)
            genTableDispatch(objectClassInfo, toStringMethodName, receiverLocalForDispatch)
            fb += wa.BrOnNonNull(labelDone)
          }

          fb ++= ctx.stringPool.getConstantStringInstr("null")
        }
      } else {
        /* Dispatch where the receiver can be a JS value.
         *
         * The overall structure of the generated code is as follows:
         *
         * block (ref extern) $done
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

        fb.block(watpe.RefType.extern) { labelDone =>
          // First try the case where the value is one of our objects
          fb.block(watpe.RefType.anyref) { labelNotOurObject =>
            // Load receiver
            genTreeAuto(tree)

            markPosition(tree)

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

        markPosition(tree)

        primType match {
          case StringType =>
            () // no-op
          case BooleanType =>
            fb += wa.Call(genFunctionID.booleanToString)
          case CharType =>
            fb += wa.Call(genFunctionID.stringBuiltins.fromCharCode)
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
          case VoidType =>
            throw new AssertionError(
                s"Found expression of type void in String_+ at ${tree.pos}: $tree")
        }

      case ClassType(BoxedStringClass, nullable) =>
        // Common case for which we want to avoid the hijacked class dispatch
        if (nullable) {
          fb.block(watpe.RefType.extern) { notNullLabel =>
            genTreeAuto(tree)
            markPosition(tree)
            fb += wa.BrOnNonNull(notNullLabel)
            fb ++= ctx.stringPool.getConstantStringInstr("null")
          }
        } else {
          genTreeAuto(tree)
        }

      case ClassType(className, _) =>
        genWithDispatch(ctx.getClassInfo(className).isAncestorOfHijackedClass)

      case AnyType | AnyNotNullType =>
        genWithDispatch(isAncestorOfHijackedClass = true)

      case ArrayType(_, _) =>
        genWithDispatch(isAncestorOfHijackedClass = false)

      case tpe: RecordType =>
        throw new AssertionError(
            s"Invalid type $tpe for String_+ at ${tree.pos}: $tree")
    }
  }

  private def genDivModByConstant[T](tree: BinaryOp, isDiv: Boolean,
      rhsValue: T, const: T => wa.Instr, sub: wa.Instr, mainOp: wa.Instr)(
      implicit num: Numeric[T]): Type = {
    /* When we statically know the value of the rhs, we can avoid the
     * dynamic tests for division by zero and overflow. This is quite
     * common in practice.
     */

    import BinaryOp._

    val BinaryOp(op, lhs, rhs) = tree
    assert(op == Int_/ || op == Int_% || op == Long_/ || op == Long_%)

    val tpe = tree.tpe

    if (rhsValue == num.zero) {
      genTree(lhs, tpe)
      markPosition(tree)
      genThrowArithmeticException()(tree.pos)
      NothingType
    } else if (isDiv && rhsValue == num.fromInt(-1)) {
      /* MinValue / -1 overflows; it traps in Wasm but we need to wrap.
       * We rewrite as `0 - lhs` so that we do not need any test.
       */
      markPosition(tree)
      fb += const(num.zero)
      genTree(lhs, tpe)
      markPosition(tree)
      fb += sub
      tpe
    } else {
      genTree(lhs, tpe)
      markPosition(rhs)
      fb += const(rhsValue)
      markPosition(tree)
      fb += mainOp
      tpe
    }
  }

  private def genDivMod[T](tree: BinaryOp, isDiv: Boolean, const: T => wa.Instr,
      eqz: wa.Instr, eqInstr: wa.Instr, sub: wa.Instr, mainOp: wa.Instr)(
      implicit num: Numeric[T]): Type = {
    /* Here we perform the same steps as in the static case, but using
     * value tests at run-time.
     */

    import BinaryOp._

    val BinaryOp(op, lhs, rhs) = tree
    assert(op == Int_/ || op == Int_% || op == Long_/ || op == Long_%)

    val tpe = tree.tpe.asInstanceOf[PrimType]
    val wasmType = transformPrimType(tpe)

    val lhsLocal = addSyntheticLocal(wasmType)
    val rhsLocal = addSyntheticLocal(wasmType)
    genTree(lhs, tpe)
    fb += wa.LocalSet(lhsLocal)
    genTree(rhs, tpe)
    fb += wa.LocalTee(rhsLocal)

    markPosition(tree)

    fb += eqz
    fb.ifThen() {
      genThrowArithmeticException()(tree.pos)
    }
    if (isDiv) {
      // Handle the MinValue / -1 corner case
      fb += wa.LocalGet(rhsLocal)
      fb += const(num.fromInt(-1))
      fb += eqInstr
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

  private def genThrowArithmeticException()(implicit pos: Position): Unit = {
    val ctorName = MethodName.constructor(List(ClassRef(BoxedStringClass)))
    genNewScalaClass(ArithmeticExceptionClass, ctorName) {
      fb ++= ctx.stringPool.getConstantStringInstr("/ by zero")
    }
    fb += wa.ExternConvertAny
    fb += wa.Throw(genTagID.exception)
  }

  private def genIsInstanceOf(tree: IsInstanceOf): Type = {
    val IsInstanceOf(expr, testType) = tree

    genTree(expr, AnyType)

    markPosition(tree)

    def genIsPrimType(testType: PrimType): Unit = testType match {
      case UndefType =>
        fb += wa.Call(genFunctionID.isUndef)
      case StringType =>
        fb += wa.ExternConvertAny
        fb += wa.Call(genFunctionID.stringBuiltins.test)
      case CharType =>
        val structTypeID = genTypeID.forClass(SpecialNames.CharBoxClass)
        fb += wa.RefTest(watpe.RefType(structTypeID))
      case LongType =>
        val structTypeID = genTypeID.forClass(SpecialNames.LongBoxClass)
        fb += wa.RefTest(watpe.RefType(structTypeID))
      case VoidType | NothingType | NullType =>
        throw new AssertionError(s"Illegal isInstanceOf[$testType]")
      case testType: PrimTypeWithRef =>
        fb += wa.Call(genFunctionID.typeTest(testType.primRef))
    }

    testType match {
      case testType: PrimType =>
        genIsPrimType(testType)

      case AnyNotNullType | ClassType(ObjectClass, false) =>
        fb += wa.RefIsNull
        fb += wa.I32Eqz

      case ClassType(JLNumberClass, false) =>
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

      case ClassType(testClassName, false) =>
        BoxedClassToPrimType.get(testClassName) match {
          case Some(primType) =>
            genIsPrimType(primType)
          case None =>
            if (ctx.getClassInfo(testClassName).isInterface)
              fb += wa.Call(genFunctionID.instanceTest(testClassName))
            else
              fb += wa.RefTest(watpe.RefType(genTypeID.forClass(testClassName)))
        }

      case ArrayType(arrayTypeRef, false) =>
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
                genLoadArrayTypeData(fb, arrayTypeRef)

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

      case AnyType | ClassType(_, true) | ArrayType(_, true) | _:RecordType =>
        throw new AssertionError(s"Illegal type in IsInstanceOf: $testType")
    }

    BooleanType
  }

  private def genAsInstanceOf(tree: AsInstanceOf): Type = {
    val AsInstanceOf(expr, targetTpe) = tree

    if (semantics.asInstanceOfs == CheckedBehavior.Unchecked)
      genCast(expr, targetTpe, tree.pos)
    else
      genCheckedCast(expr, targetTpe, tree.pos)
  }

  private def genCheckedCast(expr: Tree, targetTpe: Type, pos: Position): Type = {
    genTree(expr, AnyType)

    markPosition(pos)

    targetTpe match {
      case AnyType | ClassType(ObjectClass, true) =>
        // no-op
        ()

      case ArrayType(arrayTypeRef, true) =>
        arrayTypeRef match {
          case ArrayTypeRef(ClassRef(ObjectClass) | _: PrimRef, 1) =>
            // For primitive arrays and exactly Array[Object], we have a dedicated function
            fb += wa.Call(genFunctionID.asInstance(targetTpe))
          case _ =>
            // For other array types, we must use the generic function
            genLoadArrayTypeData(fb, arrayTypeRef)
            fb += wa.Call(genFunctionID.asSpecificRefArray)
        }

      case _ =>
        fb += wa.Call(genFunctionID.asInstance(targetTpe))
    }

    targetTpe
  }

  private def genCast(expr: Tree, targetTpe: Type, pos: Position): Type = {
    val sourceTpe = expr.tpe

    /* We cannot call `transformSingleType` for NothingType, so we have to
     * handle these cases separately.
     */

    if (sourceTpe == NothingType) {
      genTree(expr, NothingType)
      NothingType
    } else if (targetTpe == NothingType) {
      genTree(expr, VoidType)
      fb += wa.Unreachable
      NothingType
    } else {
      /* At this point, neither sourceTpe nor targetTpe can be NothingType,
       * VoidType or RecordType, so we can use `transformSingleType`.
       */

      val sourceWasmType = transformSingleType(sourceTpe)
      val targetWasmType = transformSingleType(targetTpe)

      (sourceWasmType, targetWasmType) match {
        case _ if sourceWasmType == targetWasmType =>
          /* Common case where no cast is necessary at the Wasm level.
           * Note that this is not *obviously* correct. It is only correct
           * because, under our choices of representation and type translation
           * rules, there is no pair `(sourceTpe, targetTpe)` for which the Wasm
           * types are equal but a valid cast would require a *conversion*.
           */
          genTreeAuto(expr)

        case (watpe.RefType(true, sourceHeapType), watpe.RefType(false, targetHeapType))
            if sourceHeapType == targetHeapType =>
          /* Similar but here we need to cast away nullability. This shape of
           * Cast is a common case for checkNotNull's inserted by the optimizer
           * when null pointers are unchecked.
           */
          genTreeAuto(expr)
          markPosition(pos)
          fb += wa.RefAsNonNull

        case _ =>
          genTree(expr, AnyType)

          markPosition(pos)

          targetTpe match {
            case targetTpe: PrimType =>
              // TODO Opt: We could do something better for things like double.asInstanceOf[int]
              genUnbox(targetTpe)

            case _ =>
              targetWasmType match {
                case watpe.RefType(true, watpe.HeapType.Any) =>
                  () // nothing to do
                case watpe.RefType(targetNullable, watpe.HeapType.Extern) =>
                  fb += wa.ExternConvertAny
                  if (!targetNullable)
                    fb += wa.RefAsNonNull
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
   *  `targetTpe` must not be `NothingType`, `NullType` nor `VoidType`.
   *
   *  The type left on the stack is non-nullable.
   */
  private def genUnbox(targetTpe: PrimType): Unit = {
    targetTpe match {
      case UndefType =>
        fb += wa.Drop
        fb += wa.GlobalGet(genGlobalID.undef)

      case StringType =>
        fb += wa.ExternConvertAny
        val sig = watpe.FunctionType(List(watpe.RefType.externref), List(watpe.RefType.extern))
        fb.block(sig) { nonNullLabel =>
          fb += wa.BrOnNonNull(nonNullLabel)
          fb += wa.GlobalGet(genGlobalID.emptyString)
        }

      case CharType | LongType =>
        // Extract the `value` field (the only field) out of the box class.

        val boxClass =
          if (targetTpe == CharType) SpecialNames.CharBoxClass
          else SpecialNames.LongBoxClass
        val fieldName = FieldName(boxClass, SpecialNames.valueFieldSimpleName)
        val resultType = transformPrimType(targetTpe)

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
          fb += genZeroOf(targetTpe)
        }

      case NothingType | NullType | VoidType =>
        throw new IllegalArgumentException(s"Illegal type in genUnbox: $targetTpe")

      case targetTpe: PrimTypeWithRef =>
        fb += wa.Call(genFunctionID.unbox(targetTpe.primRef))
    }
  }

  private def genReadStorage(storage: VarStorage): Unit = {
    storage match {
      case VarStorage.Local(localID) =>
        fb += wa.LocalGet(localID)
      case VarStorage.LocalRecord(fields) =>
        for ((_, fieldStorage) <- fields)
          genReadStorage(fieldStorage)
      case VarStorage.StructField(structLocal, structTypeID, fieldID) =>
        fb += wa.LocalGet(structLocal)
        fb += wa.StructGet(structTypeID, fieldID)
    }
  }

  private def genVarRef(tree: VarRef): Type = {
    val VarRef(name) = tree

    markPosition(tree)
    if (tree.tpe == NothingType)
      fb += wa.Unreachable
    else
      genReadStorage(lookupLocal(name))
    tree.tpe
  }

  private def genVarDef(tree: VarDef): Type = {
    /* This is an isolated VarDef that is not in a Block.
     * Its scope is empty by construction, and therefore it need not be stored.
     */
    val VarDef(_, _, _, _, rhs) = tree
    genTree(rhs, VoidType)
    VoidType
  }

  private def genIf(tree: If, expectedType: Type): Type = {
    val If(cond, thenp, elsep) = tree

    val ty = transformResultType(expectedType)
    genTree(cond, BooleanType)

    markPosition(tree)

    elsep match {
      case Skip() =>
        assert(expectedType == VoidType)
        fb.ifThen() {
          genTree(thenp, expectedType)
        }
      case _ =>
        fb.ifThenElse(ty) {
          genTree(thenp, expectedType)
        } {
          genTree(elsep, expectedType)
        }
    }

    if (expectedType == NothingType)
      fb += wa.Unreachable

    expectedType
  }

  private def genWhile(tree: While): Type = {
    val While(cond, body) = tree

    cond match {
      case BooleanLiteral(true) =>
        // infinite loop that must be typed as `nothing`, i.e., unreachable
        markPosition(tree)
        fb.loop() { label =>
          genTree(body, VoidType)
          markPosition(tree)
          fb += wa.Br(label)
        }
        fb += wa.Unreachable
        NothingType

      case _ =>
        // normal loop typed as `void`
        markPosition(tree)
        fb.loop() { label =>
          genTree(cond, BooleanType)
          markPosition(tree)
          fb.ifThen() {
            genTree(body, VoidType)
            markPosition(tree)
            fb += wa.Br(label)
          }
        }
        VoidType
    }
  }

  private def genForIn(tree: ForIn): Type = {
    val ForIn(obj, LocalIdent(keyVarName), keyVarOrigName, body) = tree

    val generatorLocal = addSyntheticLocal(watpe.RefType.anyref)

    // Evaluate the obj and create the for..in generator
    genTree(obj, AnyType)
    markPosition(tree)
    fb += wa.Call(genFunctionID.jsForInStart)
    fb += wa.LocalSet(generatorLocal)

    // Loop over the keys returned by the generator
    fb.block() { doneLabel =>
      fb.loop() { loopLabel =>
        fb += wa.LocalGet(generatorLocal)
        fb += wa.Call(genFunctionID.jsForInNext) // returns [value, done]

        // if done, jump to done
        fb += wa.BrIf(doneLabel)

        // otherwise, store the value and execute the body
        withNewLocal(keyVarName, keyVarOrigName, watpe.RefType.anyref) { keyLocal =>
          fb += wa.LocalSet(keyLocal)
          genTree(body, VoidType)
        }

        // and loop back
        fb += wa.Br(loopLabel)
      }
    }

    VoidType
  }

  private def genTryCatch(tree: TryCatch, expectedType: Type): Type = {
    val TryCatch(block, LocalIdent(errVarName), errVarOrigName, handler) = tree

    val resultType = transformResultType(expectedType)

    markPosition(tree)
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
          withNPEScope(resultType) {
            genTree(block, expectedType)
          }
          markPosition(tree)
          fb += wa.Br(doneLabel)
        }
      } // end block $catch
      withNewLocal(errVarName, errVarOrigName, watpe.RefType.anyref) { exceptionLocal =>
        fb += wa.AnyConvertExtern
        fb += wa.LocalSet(exceptionLocal)
        genTree(handler, expectedType)
      }
    } // end block $done

    if (expectedType == NothingType)
      fb += wa.Unreachable

    expectedType
  }

  private def genBlock(tree: Block, expectedType: Type): Type = {
    val Block(stats) = tree

    genBlockStats(stats.init) {
      genTree(stats.last, expectedType)
    }
    expectedType
  }

  final def genBlockStats(stats: List[Tree])(inner: => Unit): Unit = {
    val savedEnv = currentEnv

    def buildStorage(origName: UTF8String, vtpe: Type): VarStorage.NonStructStorage = vtpe match {
      case RecordType(fields) =>
        val fieldStorages = fields.map { field =>
          val fieldOrigName =
            origName ++ dotUTF8String ++ field.originalName.getOrElse(field.name)
          field.name -> buildStorage(fieldOrigName, field.tpe)
        }
        VarStorage.LocalRecord(fieldStorages.toVector)
      case _ =>
        val wasmType =
          if (vtpe == NothingType) watpe.Int32
          else transformSingleType(vtpe)
        val local = fb.addLocal(OriginalName(origName), wasmType)
        VarStorage.Local(local)
    }

    for (stat <- stats) {
      stat match {
        case VarDef(LocalIdent(name), originalName, vtpe, _, rhs) =>
          genTree(rhs, vtpe)
          markPosition(stat)
          val storage = buildStorage(originalName.getOrElse(name), vtpe)
          currentEnv = currentEnv.updated(name, storage)
          genWriteToStorage(storage)

        case _ =>
          genTree(stat, VoidType)
      }
    }

    inner

    currentEnv = savedEnv
  }

  private def genNew(tree: New): Type = {
    val New(className, MethodIdent(ctorName), args) = tree

    genNewScalaClass(className, ctorName) {
      genArgs(args, ctorName)
    } (tree.pos)

    tree.tpe
  }

  private def genNewScalaClass(cls: ClassName, ctor: MethodName)(
      genCtorArgs: => Unit)(implicit pos: Position): Unit = {

    /* Do not use transformType here, because we must get the struct type even
     * if the given class is an ancestor of hijacked classes (which in practice
     * is only the case for j.l.Object).
     */
    val instanceLocal = addSyntheticLocal(watpe.RefType(genTypeID.forClass(cls)))

    markPosition(pos)
    fb += wa.Call(genFunctionID.newDefault(cls))
    fb += wa.LocalTee(instanceLocal)
    genCtorArgs
    markPosition(pos)
    fb += wa.Call(genFunctionID.forMethod(MemberNamespace.Constructor, cls, ctor))
    fb += wa.LocalGet(instanceLocal)
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

    ClassType(boxClassName, nullable = false)
  }

  private def genJSNew(tree: JSNew): Type = {
    val JSNew(ctor, args) = tree

    implicit val pos = tree.pos

    genThroughCustomJSHelper(ctor :: args) { allJSArgs =>
      val jsCtor :: jsArgs = allJSArgs
      js.Return(js.New(jsCtor, jsArgs))
    }
  }

  private def genJSSelect(tree: JSSelect): Type = {
    val JSSelect(qualifier, item) = tree

    implicit val pos = tree.pos

    genThroughCustomJSHelper(List(qualifier, item)) { allJSArgs =>
      val List(jsQualifier, jsItem) = allJSArgs
      js.Return(js.BracketSelect.makeOptimized(jsQualifier, jsItem))
    }
  }

  private def genJSFunctionApply(tree: JSFunctionApply): Type = {
    val JSFunctionApply(fun, args) = tree

    implicit val pos = tree.pos

    genThroughCustomJSHelper(fun :: args) { allJSArgs =>
      val jsFun :: jsArgs = allJSArgs
      js.Return(js.Apply.makeProtected(jsFun, jsArgs))
    }
  }

  private def genJSMethodApply(tree: JSMethodApply): Type = {
    val JSMethodApply(receiver, method, args) = tree

    implicit val pos = tree.pos

    genThroughCustomJSHelper(receiver :: method :: args) { allJSArgs =>
      val jsReceiver :: jsMethod :: jsArgs = allJSArgs
      js.Return(js.Apply(js.BracketSelect.makeOptimized(jsReceiver, jsMethod), jsArgs))
    }
  }

  private def genJSImportCall(tree: JSImportCall): Type = {
    val JSImportCall(arg) = tree

    genTree(arg, AnyType)
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
    val LoadJSConstructor(className) = tree

    markPosition(tree)

    ctx.getClassInfo(className).jsNativeLoadSpec match {
      case Some(loadSpec) =>
        genLoadJSFromSpec(loadSpec)(tree.pos)
      case None =>
        // This is a non-native JS class
        fb += wa.Call(genFunctionID.loadJSClass(className))
    }

    AnyType
  }

  private def genLoadJSModule(tree: LoadJSModule): Type = {
    val LoadJSModule(className) = tree

    markPosition(tree)

    ctx.getClassInfo(className).jsNativeLoadSpec match {
      case Some(loadSpec) =>
        genLoadJSFromSpec(loadSpec)(tree.pos)
      case None =>
        // This is a non-native JS module
        fb += wa.Call(genFunctionID.loadModule(className))
    }

    AnyType
  }

  private def genSelectJSNativeMember(tree: SelectJSNativeMember): Type = {
    val SelectJSNativeMember(className, MethodIdent(memberName)) = tree

    val info = ctx.getClassInfo(className)
    val jsNativeLoadSpec = info.jsNativeMembers.getOrElse(memberName, {
      throw new AssertionError(
          s"Found $tree for non-existing JS native member at ${tree.pos}")
    })
    genLoadJSFromSpec(jsNativeLoadSpec)(tree.pos)
    AnyType
  }

  private def genLoadJSFromSpec(jsNativeLoadSpec: JSNativeLoadSpec)(
      implicit pos: Position): Unit = {

    val builder = new CustomJSHelperBuilder()
    val result = builder.genJSNativeLoadSpec(jsNativeLoadSpec)
    val helperID = builder.build(AnyType) {
      js.Return(result)
    }

    markPosition(pos)
    fb += wa.Call(helperID)
  }

  private def genJSDelete(tree: JSDelete): Type = {
    val JSDelete(qualifier, item) = tree

    genTree(qualifier, AnyType)
    genTree(item, AnyType)
    markPosition(tree)
    fb += wa.Call(genFunctionID.jsDelete)
    VoidType
  }

  private def genJSUnaryOp(tree: JSUnaryOp): Type = {
    val JSUnaryOp(op, lhs) = tree

    implicit val pos = tree.pos

    genThroughCustomJSHelper(List(lhs), tree.tpe) { allJSArgs =>
      val List(jsLhs) = allJSArgs

      val protectedLhs = if (op == JSUnaryOp.typeof && lhs.isInstanceOf[JSGlobalRef]) {
        /* #3822 We protect the argument so that it throws a ReferenceError
         * if the global variable is not defined at all, as specified.
         */
        js.Block(js.IntLiteral(0), jsLhs)
      } else {
        jsLhs
      }

      js.Return(js.UnaryOp(op, protectedLhs))
    }

    tree.tpe
  }

  private def genJSBinaryOp(tree: JSBinaryOp): Type = {
    val JSBinaryOp(op, lhs, rhs) = tree

    op match {
      case JSBinaryOp.|| | JSBinaryOp.&& =>
        /* Here we need to implement the short-circuiting behavior, with a
         * condition based on the truthy value of the left-hand-side.
         */
        val lhsLocal = addSyntheticLocal(watpe.RefType.anyref)
        genTree(lhs, AnyType)
        markPosition(tree)
        fb += wa.LocalTee(lhsLocal)
        fb += wa.Call(genFunctionID.jsIsTruthy)
        if (op == JSBinaryOp.||) {
          fb.ifThenElse(watpe.RefType.anyref) {
            fb += wa.LocalGet(lhsLocal)
          } {
            genTree(rhs, AnyType)
            markPosition(tree)
          }
        } else {
          fb.ifThenElse(watpe.RefType.anyref) {
            genTree(rhs, AnyType)
            markPosition(tree)
          } {
            fb += wa.LocalGet(lhsLocal)
          }
        }

      case _ =>
        implicit val pos = tree.pos

        genThroughCustomJSHelper(List(lhs, rhs), tree.tpe) { allJSArgs =>
          val List(jsLhs, jsRhs) = allJSArgs
          js.Return(js.BinaryOp(op, jsLhs, jsRhs))
        }
    }

    tree.tpe
  }

  private def genJSArrayConstr(tree: JSArrayConstr): Type = {
    val JSArrayConstr(items) = tree

    implicit val pos = tree.pos

    if (items.isEmpty) {
      markPosition(tree)
      fb += wa.Call(genFunctionID.jsNewArray)
      AnyType
    } else {
      genThroughCustomJSHelper(items, AnyNotNullType) { jsItems =>
        js.Return(js.ArrayConstr(jsItems))
      }
    }
  }

  private def genJSObjectConstr(tree: JSObjectConstr): Type = {
    val JSObjectConstr(fields) = tree

    implicit val pos = tree.pos

    if (fields.isEmpty) {
      markPosition(tree)
      fb += wa.Call(genFunctionID.jsNewObject)
      AnyType
    } else {
      val flatPropValues = fields.flatMap(pv => List(pv._1, pv._2))

      genThroughCustomJSHelper(flatPropValues, AnyNotNullType) { jsFlatPropValues =>
        val jsPropValuesIter = jsFlatPropValues.grouped(2).map { pvList =>
          val List(jsPropTree, jsValue) = pvList
          val jsProp: js.PropertyName = jsPropTree match {
            case jsPropTree: js.StringLiteral => jsPropTree
            case _                            => js.ComputedName(jsPropTree)
          }
          jsProp -> jsValue
        }
        js.Return(js.ObjectConstr(jsPropValuesIter.toList))
      }
    }
  }

  private def genJSGlobalRef(tree: JSGlobalRef): Type = {
    val JSGlobalRef(name) = tree

    implicit val pos = tree.pos
    markPosition(pos)

    if (name == JSGlobalRef.FileLevelThis) {
      // In ES modules global this is undefined, and Wasm backend only supports `ESModule`
      fb += wa.GlobalGet(genGlobalID.undef)
    } else {
      val builder = new CustomJSHelperBuilder()
      val helperID = builder.build(AnyType) {
        js.Return(builder.genGlobalRef(name))
      }

      fb += wa.Call(helperID)
    }
    AnyType
  }

  private def genJSTypeOfGlobalRef(tree: JSTypeOfGlobalRef): Type = {
    val JSTypeOfGlobalRef(JSGlobalRef(name)) = tree

    implicit val pos = tree.pos

    val builder = new CustomJSHelperBuilder()
    val helperID = builder.build(AnyType) {
      js.Return(js.UnaryOp(JSUnaryOp.typeof, builder.genGlobalRef(name)))
    }

    markPosition(pos)
    fb += wa.Call(helperID)
    AnyType
  }

  private def genNewArray(tree: NewArray): Type = {
    val NewArray(arrayTypeRef, length) = tree

    markPosition(tree)

    genLoadVTableAndITableForArray(fb, arrayTypeRef)

    // Create the underlying array
    genTree(length, IntType)
    markPosition(tree)

    if (semantics.negativeArraySizes != CheckedBehavior.Unchecked) {
      length match {
        case IntLiteral(lengthValue) if lengthValue >= 0 =>
          () // always good
        case _ =>
          // if length < 0
          val lengthLocal = addSyntheticLocal(watpe.Int32)
          fb += wa.LocalTee(lengthLocal)
          fb += wa.I32Const(0)
          fb += wa.I32LtS
          fb.ifThen() {
            // then throw NegativeArraySizeException
            fb += wa.LocalGet(lengthLocal)
            fb += wa.Call(genFunctionID.throwNegativeArraySizeException)
            fb += wa.Unreachable
          }
          fb += wa.LocalGet(lengthLocal)
      }
    }

    val underlyingArrayType = genTypeID.underlyingOf(arrayTypeRef)
    fb += wa.ArrayNewDefault(underlyingArrayType)

    // Create the array object
    fb += wa.StructNew(genTypeID.forArrayClass(arrayTypeRef))

    tree.tpe
  }

  private def genArraySelect(tree: ArraySelect): Type = {
    val ArraySelect(array, index) = tree

    genTreeAuto(array)

    array.tpe match {
      case ArrayType(arrayTypeRef, _) =>
        genCheckNonNullFor(array)

        if (semantics.arrayIndexOutOfBounds == CheckedBehavior.Unchecked) {
          // Get the underlying array
          fb += wa.StructGet(
            genTypeID.forArrayClass(arrayTypeRef),
            genFieldID.objStruct.arrayUnderlying
          )

          // Load the index
          genTree(index, IntType)

          markPosition(tree)

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
        } else {
          genTree(index, IntType)
          markPosition(tree)
          fb += wa.Call(genFunctionID.arrayGetFor(arrayTypeRef))
        }

        /* If it is a reference array type whose element type does not translate
         * to `anyref`, we must cast down the result.
         */
        arrayTypeRef match {
          case ArrayTypeRef(_: PrimRef, 1) =>
            // a primitive array always has the correct type
            ()
          case _ =>
            transformSingleType(tree.tpe) match {
              case watpe.RefType.anyref =>
                // nothing to do
                ()
              case watpe.RefType(nullable, watpe.HeapType.Extern) =>
                fb += wa.ExternConvertAny
                if (!nullable)
                  fb += wa.RefAsNonNull
              case refType: watpe.RefType =>
                fb += wa.RefCast(refType)
              case otherType =>
                throw new AssertionError(s"Unexpected result type for reference array: $otherType")
            }
        }

        tree.tpe

      case NothingType =>
        // unreachable
        NothingType
      case NullType =>
        genNPE()
        NothingType
      case _ =>
        throw new IllegalArgumentException(
            s"ArraySelect.array must be an array type, but has type ${array.tpe}")
    }
  }

  private def genArrayValue(tree: ArrayValue): Type = {
    val ArrayValue(arrayTypeRef, elems) = tree

    val expectedElemType = arrayTypeRef match {
      case ArrayTypeRef(base: PrimRef, 1) => base.tpe
      case _                              => AnyType
    }

    // Mark the position for the header of `genArrayValue`
    markPosition(tree)

    SWasmGen.genArrayValue(fb, arrayTypeRef, elems.size) {
      // Create the underlying array
      elems.foreach(genTree(_, expectedElemType))

      // Re-mark the position for the footer of `genArrayValue`
      markPosition(tree)
    }

    tree.tpe
  }

  private def genClosure(tree: Closure): Type = {
    val Closure(arrow, captureParams, params, restParam, body, captureValues) = tree

    implicit val pos = tree.pos

    val dataStructTypeID = ctx.getClosureDataStructType(captureParams.map(_.ptpe))

    // Define the function where captures are reified as a `__captureData` argument.
    val closureFuncOrigName = genClosureFuncOriginalName()
    val closureFuncID = new ClosureFunctionID(closureFuncOrigName)
    emitFunction(
      closureFuncID,
      closureFuncOrigName,
      enclosingClassName = None,
      Some(captureParams),
      receiverType = if (arrow) None else Some(watpe.RefType.anyref),
      params,
      restParam,
      body,
      resultType = AnyType
    )

    val builder = new CustomJSHelperBuilder()

    val fRef = builder.addWasmInput("f", watpe.RefType.func) {
      markPosition(tree)
      fb += ctx.refFuncWithDeclaration(closureFuncID)
    }
    val dataRef = builder.addWasmInput("d", watpe.RefType(dataStructTypeID)) {
      for ((param, value) <- captureParams.zip(captureValues))
        genTree(value, param.ptpe)
      markPosition(tree)
      fb += wa.StructNew(dataStructTypeID)
    }

    val helperID = builder.build(AnyNotNullType) {
      js.Return {
        val (argsParamDefs, restParamDef) = builder.genJSParamDefs(params, restParam)
        js.Function(arrow, argsParamDefs, restParamDef, {
          js.Return(js.Apply(
              fRef,
              dataRef ::
              (if (arrow) Nil else List(js.This())) :::
              argsParamDefs.map(_.ref) :::
              restParamDef.map(_.ref).toList
          ))
        })
      }
    }

    markPosition(tree)
    fb += wa.Call(helperID)

    AnyNotNullType
  }

  private def genMatch(tree: Match, expectedType: Type): Type = {
    val Match(selector, cases, defaultBody) = tree

    val selectorLocal = addSyntheticLocal(transformSingleType(selector.tpe))

    genTreeAuto(selector)

    markPosition(tree)

    fb += wa.LocalSet(selectorLocal)

    fb.block(transformResultType(expectedType)) { doneLabel =>
      fb.block() { defaultLabel =>
        val caseLabels = cases.map(c => c._1 -> fb.genLabel())
        for (caseLabel <- caseLabels)
          fb += wa.Block(wa.BlockType.ValueType(), Some(caseLabel._2))

        for {
          (matchableLiterals, label) <- caseLabels
          matchableLiteral <- matchableLiterals
        } {
          markPosition(matchableLiteral)
          fb += wa.LocalGet(selectorLocal)
          matchableLiteral match {
            case IntLiteral(value) =>
              fb += wa.I32Const(value)
              fb += wa.I32Eq
              fb += wa.BrIf(label)
            case StringLiteral(value) =>
              fb ++= ctx.stringPool.getConstantStringInstr(value)
              fb += wa.Call(genFunctionID.stringBuiltins.equals)
              fb += wa.BrIf(label)
            case Null() =>
              fb += wa.RefIsNull
              fb += wa.BrIf(label)
          }
        }
        fb += wa.Br(defaultLabel)

        for {
          (caseLabel, (_, caseBody)) <- caseLabels.zip(cases).reverse
        } {
          markPosition(caseBody)
          fb += wa.End
          genTree(caseBody, expectedType)
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
    val CreateJSClass(className, captureValues) = tree

    val classInfo = ctx.getClassInfo(className)
    val jsClassCaptures = classInfo.jsClassCaptures.getOrElse {
      throw new AssertionError(
          s"Illegal CreateJSClass of top-level class ${className.nameString}")
    }

    for ((captureValue, captureParam) <- captureValues.zip(jsClassCaptures))
      genTree(captureValue, captureParam.ptpe)

    markPosition(tree)

    fb += wa.Call(genFunctionID.createJSClassOf(className))

    AnyType
  }

  private def genJSPrivateSelect(tree: JSPrivateSelect): Type = {
    val JSPrivateSelect(qualifier, FieldIdent(fieldName)) = tree

    genTree(qualifier, AnyType)

    markPosition(tree)

    fb += wa.GlobalGet(genGlobalID.forJSPrivateField(fieldName))
    fb += wa.Call(genFunctionID.jsSelect)

    AnyType
  }

  private def genJSSuperSelect(tree: JSSuperSelect): Type = {
    val JSSuperSelect(superClass, receiver, item) = tree

    genTree(superClass, AnyType)
    genTree(receiver, AnyType)
    genTree(item, AnyType)

    markPosition(tree)

    fb += wa.Call(genFunctionID.jsSuperSelect)

    AnyType
  }

  private def genJSSuperMethodCall(tree: JSSuperMethodCall): Type = {
    val JSSuperMethodCall(superClass, receiver, method, args) = tree

    implicit val pos = tree.pos

    genThroughCustomJSHelper(superClass :: receiver :: method :: args) { allJSArgs =>
      val jsSuperClass :: jsReceiver :: jsMethod :: jsArgs = allJSArgs

      // return superClass.prototype[method].call(receiver, ...args);
      js.Return(
        js.Apply(
          js.DotSelect(
            js.BracketSelect.makeOptimized(
              js.DotSelect(jsSuperClass, js.Ident("prototype")),
              jsMethod
            ),
            js.Ident("call")
          ),
          jsReceiver :: jsArgs
        )
      )
    }
  }

  private def genJSNewTarget(tree: JSNewTarget): Type = {
    markPosition(tree)

    genReadStorage(newTargetStorage)

    AnyType
  }

  private def genRecordSelect(tree: RecordSelect): Type = {
    if (canLookupRecordSelect(tree)) {
      markPosition(tree)
      genReadStorage(lookupRecordSelect(tree))
    } else {
      /* We have a record select that we cannot simplify to a direct storage,
       * because its `record` part is neither a `VarRef` nor (recursively) a
       * `RecordSelect`. For example, it could be an `If` whose two branches
       * both return a different `VarRef`/`Record` of the same record type:
       *   (if (cond) record1 else record2).recordField
       * In that case, we must evaluate the `record` in full, then discard all
       * the fields that are not the one we're selecting.
       *
       * (The JS backend avoids this situation by construction because of its
       * unnesting logic. It always creates a temporary `VarRef` of the record
       * type. In Wasm we can use multiple values on the stack instead.)
       */

      genTreeAuto(tree.record)

      markPosition(tree)

      val tempTypes = transformResultType(tree.tpe)
      val tempLocals = tempTypes.map(addSyntheticLocal(_))

      val recordType = tree.record.tpe.asInstanceOf[RecordType]
      for (recordField <- recordType.fields.reverseIterator) {
        if (recordField.name == tree.field.name) {
          // Store this one in our temp locals
          for (tempLocal <- tempLocals.reverseIterator)
            fb += wa.LocalSet(tempLocal)
        } else {
          // Discard this field
          for (_ <- transformResultType(recordField.tpe))
            fb += wa.Drop
        }
      }

      // Read back our locals
      for (tempLocal <- tempLocals)
        fb += wa.LocalGet(tempLocal)
    }

    tree.tpe
  }

  private def genRecordValue(tree: RecordValue): Type = {
    for ((elem, field) <- tree.elems.zip(tree.tpe.fields))
      genTree(elem, field.tpe)

    tree.tpe
  }

  private def genTransient(tree: Transient): Type = {
    tree.value match {
      case Transients.Cast(expr, tpe) =>
        genCast(expr, tpe, tree.pos)

      case value: Transients.SystemArrayCopy =>
        genSystemArrayCopy(tree, value)

      case Transients.ObjectClassName(obj) =>
        genTreeToAny(obj)
        markPosition(tree)
        fb += wa.Call(genFunctionID.anyGetClassName)
        StringType

      case value @ WasmTransients.WasmUnaryOp(_, lhs) =>
        genTreeAuto(lhs)
        markPosition(tree)
        fb += value.wasmInstr
        value.tpe

      case value @ WasmTransients.WasmBinaryOp(_, lhs, rhs) =>
        genTreeAuto(lhs)
        genTreeAuto(rhs)
        markPosition(tree)
        fb += value.wasmInstr
        value.tpe

      case value @ WasmTransients.WasmStringFromCodePoint(codePoint) =>
        genTree(codePoint, IntType)
        markPosition(tree)
        fb += wa.Call(genFunctionID.stringBuiltins.fromCodePoint)
        value.tpe

      case value @ WasmTransients.WasmCodePointAt(string, index) =>
        genTree(string, ClassType(BoxedStringClass, nullable = string.tpe.isNullable))
        if (semantics.stringIndexOutOfBounds == CheckedBehavior.Unchecked)
          genCheckNonNullFor(string)
        else
          genAsNonNullOrNPEFor(string)
        genTree(index, IntType)
        markPosition(tree)
        if (semantics.stringIndexOutOfBounds == CheckedBehavior.Unchecked)
          fb += wa.Call(genFunctionID.stringBuiltins.codePointAt)
        else
          fb += wa.Call(genFunctionID.checkedStringCodePointAt)
        value.tpe

      case value @ WasmTransients.WasmSubstring(string, start, optEnd) =>
        genTree(string, ClassType(BoxedStringClass, nullable = string.tpe.isNullable))
        if (semantics.stringIndexOutOfBounds == CheckedBehavior.Unchecked)
          genCheckNonNullFor(string)
        else
          genAsNonNullOrNPEFor(string)
        genTree(start, IntType)
        optEnd.foreach(genTree(_, IntType))
        markPosition(tree)
        if (semantics.stringIndexOutOfBounds == CheckedBehavior.Unchecked) {
          if (optEnd.isEmpty)
            fb += wa.I32Const(-1) // unsigned max value
          fb += wa.Call(genFunctionID.stringBuiltins.substring)
        } else {
          if (optEnd.isEmpty)
            fb += wa.Call(genFunctionID.checkedSubstringStart)
          else
            fb += wa.Call(genFunctionID.checkedSubstringStartEnd)
        }
        value.tpe

      case other =>
        throw new AssertionError(s"Unknown transient: $other")
    }
  }

  private def genSystemArrayCopy(tree: Transient,
      transientValue: Transients.SystemArrayCopy): Type = {
    val Transients.SystemArrayCopy(src, srcPos, dest, destPos, length) = transientValue

    genTreeAuto(src)
    genTree(srcPos, IntType)
    genTreeAuto(dest)
    genTree(destPos, IntType)
    genTree(length, IntType)

    markPosition(tree)

    (src.tpe, dest.tpe) match {
      case (ArrayType(srcArrayTypeRef, _), ArrayType(destArrayTypeRef, _))
          if genTypeID.forArrayClass(srcArrayTypeRef) == genTypeID.forArrayClass(destArrayTypeRef) =>
        // Generate a specialized arrayCopyT call
        fb += wa.Call(genFunctionID.specializedArrayCopy(srcArrayTypeRef))

      case _ =>
        // Generate a generic arrayCopy call
        fb += wa.Call(genFunctionID.genericArrayCopy)
    }

    VoidType
  }

  // Helpers to generate code going through custom JS helpers

  /** Generates code with a custom JS helper based on Tree inputs.
   *
   *  Logically, the following steps happen:
   *
   *  1. Generates Wasm code to evaluate the `args`, in order.
   *  2. Passes the values, converted for use by JavaScript, to a custom JS
   *     helper whose body is provided by `makeJSHelperBody`.
   *     `makeJSHelperBody` receives a list of JS trees corresponding to the JS
   *     evaluated values of `args`.
   *  3. Leaves the result of `makeJSHelperBody` on the stack, typed as
   *     `resultType`.
   *
   *  Some `args` may be evaluated on the JS side when possible, notably for
   *  most literals, JS global refs and imports.
   *
   *  @see [[CustomJSHelperBuilder]]
   */
  private def genThroughCustomJSHelper(args: List[TreeOrJSSpread],
      resultType: Type = AnyType)(
      makeJSHelperBody: List[js.Tree] => js.Tree)(
      implicit pos: Position): Type = {

    val builder = new CustomJSHelperBuilderWithTreeSupport()
    val jsArgs = args.map(builder.addInput(_))
    val helperID = builder.build(resultType) {
      makeJSHelperBody(jsArgs)
    }

    markPosition(pos)
    fb += wa.Call(helperID)

    resultType
  }

  private final class CustomJSHelperBuilderWithTreeSupport()(implicit pos: Position)
      extends CustomJSHelperBuilder.WithTreeEval {
    protected def evalTreeAtCallSite(tree: Tree, expectedType: Type): Unit =
      genTree(tree, expectedType)
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
   * we cannot use the stack for the `try_table` itself: each label has a
   * dedicated local for its result if it comes from such a crossing `return`.
   *
   * Two more complications:
   *
   * - If the `finally` block itself contains another `try..finally`, they may
   *   need a `destinationTag` concurrently. Therefore, every `try..finally`
   *   gets its own `destinationTag` local. We do not need this for another
   *   `try..finally` inside a `try` (or elsewhere in the function), so this is
   *   not an optimal allocation; we do it this way not to complicate this
   *   further.
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
   *   val foo: int = try { // declare local destinationTagOuter
   *     beta[int]: { // allocated destinationTag = 2
   *       val bar: int = try { // declare local destinationTagInner
   *         if (A) return@alpha 5
   *         if (B) return@beta 10
   *         56
   *       } finally {
   *         doTheFinally()
   *         // not shown: there is another try..finally here using a third
   *         // local destinationTagThird, since destinationTagOuter and
   *         // destinationTagInner are alive at the same time.
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
      import TryFinallyEntry._

      private var _crossInfo: Option[CrossInfo] = None

      def isInside(labeledEntry: LabeledEntry): Boolean =
        this.depth > labeledEntry.depth

      def wasCrossed: Boolean = _crossInfo.isDefined

      def requireCrossInfo(): CrossInfo = {
        _crossInfo.getOrElse {
          val info = CrossInfo(addSyntheticLocal(watpe.Int32), fb.genLabel())
          _crossInfo = Some(info)
          info
        }
      }
    }

    private object TryFinallyEntry {
      /** Cross info for a `TryFinally` entry.
       *
       *  @param destinationTagLocal
       *    The destinationTag local variable for this `TryFinally`.
       *  @param crossLabel
       *    The cross label for this `TryFinally`.
       */
      sealed case class CrossInfo(
        val destinationTagLocal: wanme.LocalID,
        val crossLabel: wanme.LabelID
      )
    }

    /** Information about an enclosing `Labeled` block. */
    private final class LabeledEntry(val depth: Int,
        val irLabelName: LabelName, val expectedType: Type) {

      import LabeledEntry._

      /** The regular label for this `Labeled` block, used for `Return`s that
       *  do not cross a `TryFinally`.
       */
      val regularWasmLabel: wanme.LabelID = fb.genLabel()

      private var _crossInfo: Option[CrossInfo] = None

      def wasCrossUsed: Boolean = _crossInfo.isDefined

      def requireCrossInfo(): CrossInfo = {
        _crossInfo.getOrElse {
          val destinationTag = allocateDestinationTag()
          val resultTypes = transformResultType(expectedType)
          val resultLocals = resultTypes.map(addSyntheticLocal(_))
          val crossLabel = fb.genLabel()
          val info = CrossInfo(destinationTag, resultLocals, crossLabel)
          _crossInfo = Some(info)
          info
        }
      }
    }

    private object LabeledEntry {
      /** Cross info for a `LabeledEntry`.
       *
       *  @param destinationTag
       *    The destination tag allocated to this label, used by the `finally`
       *    blocks to keep propagating to the right destination. Destination
       *    tags are always `> 0`. The value `0` is reserved for fall-through.
       *  @param resultLocals
       *    The locals in which to store the result of the label if we have to
       *    cross a `try..finally`.
       *  @param crossLabel
       *    An additional Wasm label that has a `[]` result, and which will get
       *    its result from the `resultLocal` instead of expecting it on the stack.
       */
      sealed case class CrossInfo(
        destinationTag: Int,
        resultLocals: List[wanme.LocalID],
        crossLabel: wanme.LabelID
      )
    }

    def genLabeled(tree: Labeled, expectedType: Type): Type = {
      val Labeled(labelName, tpe, body) = tree

      val entry = new LabeledEntry(currentUnwindingStackDepth, labelName, expectedType)

      val ty = transformResultType(expectedType)

      markPosition(tree)

      // Manual wa.Block here because we have a specific `label`
      fb += wa.Block(fb.sigToBlockType(Sig(Nil, ty)), Some(entry.regularWasmLabel))

      /* Remember the position in the instruction stream, in case we need to
       * come back and insert the wa.Block for the cross handling.
       */
      val instrsBlockBeginIndex = fb.markCurrentInstructionIndex()

      // Emit the body
      enterLabeled(entry) {
        genTree(body, expectedType)
      }

      markPosition(tree)

      // Deal with crossing behavior
      if (entry.wasCrossUsed) {
        assert(expectedType != NothingType,
            "The tryFinallyCrossLabel should not have been used for label " +
            s"${labelName.nameString} of type nothing")

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

        val LabeledEntry.CrossInfo(_, resultLocals, crossLabel) =
          entry.requireCrossInfo()

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

    def genTryFinally(tree: TryFinally, expectedType: Type): Type = {
      val TryFinally(tryBlock, finalizer) = tree

      val entry = new TryFinallyEntry(currentUnwindingStackDepth)

      val resultType = transformResultType(expectedType)
      val resultLocals = resultType.map(addSyntheticLocal(_))

      markPosition(tree)

      fb.block() { doneLabel =>
        fb.block(watpe.RefType.exnref) { catchLabel =>
          /* Remember the position in the instruction stream, in case we need
           * to come back and insert the wa.BLOCK for the cross handling.
           */
          val instrsBlockBeginIndex = fb.markCurrentInstructionIndex()

          fb.tryTable()(List(wa.CatchClause.CatchAllRef(catchLabel))) {
            // try block
            enterTryFinally(entry) {
              withNPEScope(resultType) {
                genTree(tryBlock, expectedType)
              }
            }

            markPosition(tree)

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
            val TryFinallyEntry.CrossInfo(destinationTagLocal, crossLabel) =
              entry.requireCrossInfo()

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
        genTree(finalizer, VoidType)

        markPosition(tree)

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

          // The order does not matter here because they will be "re-sorted" by emitBRTable
          val possibleTargetEntries =
            enclosingLabeledBlocks.valuesIterator.filter(_.wasCrossUsed).toList

          val nextTryFinallyEntry = innermostTryFinally // note that we're out of ourselves already
            .filter(nextTry => possibleTargetEntries.exists(nextTry.isInside(_)))

          /* Build the destination table for `br_table`. Target Labeled's that
           * are outside of the next try..finally in line go to the latter;
           * for other `Labeled`'s, we go to their cross label.
           */
          val brTableDests: List[(Int, wanme.LabelID)] = possibleTargetEntries.map { targetEntry =>
            val LabeledEntry.CrossInfo(destinationTag, _, crossLabel) =
              targetEntry.requireCrossInfo()
            val label = nextTryFinallyEntry.filter(_.isInside(targetEntry)) match {
              case None          => crossLabel
              case Some(nextTry) => nextTry.requireCrossInfo().crossLabel
            }
            destinationTag -> label
          }

          fb += wa.LocalGet(entry.requireCrossInfo().destinationTagLocal)
          for (nextTry <- nextTryFinallyEntry) {
            // Transfer the destinationTag to the next try..finally in line
            fb += wa.LocalTee(nextTry.requireCrossInfo().destinationTagLocal)
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

    private def emitBRTable(dests: List[(Int, wanme.LabelID)],
        defaultLabel: wanme.LabelID): Unit = {
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

    def genReturn(tree: Return): Type = {
      val Return(expr, labelName) = tree

      val targetEntry = enclosingLabeledBlocks(labelName)

      genTree(expr, targetEntry.expectedType)

      markPosition(tree)

      if (targetEntry.expectedType != NothingType) {
        innermostTryFinally.filter(_.isInside(targetEntry)) match {
          case None =>
            // Easy case: directly branch out of the block
            fb += wa.Br(targetEntry.regularWasmLabel)

          case Some(tryFinallyEntry) =>
            /* Here we need to branch to the innermost enclosing `finally` block,
             * while remembering the destination label and the result value.
             */
            val LabeledEntry.CrossInfo(destinationTag, resultLocals, _) =
              targetEntry.requireCrossInfo()
            val TryFinallyEntry.CrossInfo(destinationTagLocal, crossLabel) =
              tryFinallyEntry.requireCrossInfo()

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
