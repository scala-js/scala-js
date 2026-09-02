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

import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees.{JSUnaryOp, JSBinaryOp, MemberNamespace}
import org.scalajs.ir.Types.{Type => _, ArrayType => _, _}
import org.scalajs.ir.{OriginalName, Position, Types => irtpe}
import org.scalajs.ir.WellKnownNames._

import org.scalajs.linker.interface.{CheckedBehavior, ModuleKind}
import org.scalajs.linker.standard.{CoreSpec, LinkedGlobalInfo}

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.Instructions._
import org.scalajs.linker.backend.webassembly.Identitities._
import org.scalajs.linker.backend.webassembly.Modules._
import org.scalajs.linker.backend.webassembly.Types._

import EmbeddedConstants._
import VarGen._
import SpecialNames._
import SWasmGen._
import TypeTransformer._

object CoreWasmLib {
  val arrayBaseRefs: List[NonArrayTypeRef] = List(
    BooleanRef,
    CharRef,
    ByteRef,
    ShortRef,
    IntRef,
    LongRef,
    FloatRef,
    DoubleRef,
    ObjectRef
  )
}

final class CoreWasmLib(coreSpec: CoreSpec, globalInfo: LinkedGlobalInfo) {
  import CoreWasmLib._
  import RefType.anyref
  import coreSpec.semantics

  private implicit val noPos: Position = Position.NoPosition

  private val useCustomDescriptors = coreSpec.wasmFeatures.experimentalUseCustomDescriptors
  private val hasJSInterop = coreSpec.moduleKind == ModuleKind.ESModule

  private val specificArrayVTableTypeID: TypeID =
    if (useCustomDescriptors) genTypeID.ObjectArrayVTable
    else genTypeID.ObjectVTable

  private val specificArrayVTableHeapType: HeapType =
    HeapType(specificArrayVTableTypeID, exact = useCustomDescriptors)

  private val stringType: RefType =
    if (hasJSInterop) RefType.extern
    else RefType(genTypeID.wasmString)

  private val noStringHeapType: HeapType =
    if (hasJSInterop) HeapType.NoExtern
    else HeapType.None

  private val nullableStringType: RefType = stringType.toNullable

  private val primRefsWithKinds = List(
    VoidRef -> KindVoid,
    BooleanRef -> KindBoolean,
    CharRef -> KindChar,
    ByteRef -> KindByte,
    ShortRef -> KindShort,
    IntRef -> KindInt,
    LongRef -> KindLong,
    FloatRef -> KindFloat,
    DoubleRef -> KindDouble
  )

  private def charCodeForOriginalName(baseRef: NonArrayTypeRef): Char = baseRef match {
    case baseRef: PrimRef => baseRef.charCode
    case _: ClassRef      => 'O'
  }

  /** Fields of the `typeData` struct definition.
   *
   *  They are accessible as a public list because they must be repeated in every vtable type
   *  definition.
   *
   *  @see
   *    [[VarGen.genFieldID.typeData]], which contains documentation of what is in each field.
   */
  val typeDataStructFields: List[StructField] = {
    import genFieldID.typeData._
    import RefType.nullable

    def make(id: FieldID, tpe: Type, isMutable: Boolean): StructField =
      StructField(id, OriginalName(id.toString()), tpe, isMutable)

    val jsPrototypeFieldOpt =
      if (!useCustomDescriptors || !hasJSInterop) Nil
      else List(make(jsPrototype, RefType.externref, isMutable = false))

    val nameFields = if (hasJSInterop) {
      List(make(name, RefType.externref, isMutable = true))
    } else {
      List(
        make(nameOffset, Int32, isMutable = false),
        make(nameSize, Int32, isMutable = false),
        make(nameStringIndex, Int32, isMutable = false),
        make(name, RefType.nullable(genTypeID.wasmString), isMutable = true)
      )
    }

    val allButJSPrototypeAndName = List(
      make(kind, Int32, isMutable = false),
      make(specialInstanceTypes, Int32, isMutable = false),
      make(strictAncestors, nullable(genTypeID.typeDataArray), isMutable = false),
      make(componentType, nullable(genTypeID.typeData), isMutable = false),
      make(classOfValue, nullable(HeapType(genTypeID.ClassStruct, exact = useCustomDescriptors)),
          isMutable = true),
      make(arrayOf, nullable(specificArrayVTableHeapType), isMutable = true),
      make(cloneFunction, nullable(genTypeID.cloneFunctionType), isMutable = false),
      make(
        isJSClassInstance,
        nullable(genTypeID.isJSClassInstanceFuncType),
        isMutable = false
      ),
      make(
        reflectiveProxies,
        RefType(genTypeID.reflectiveProxies),
        isMutable = false
      )
    )

    jsPrototypeFieldOpt ::: nameFields ::: allButJSPrototypeAndName
  }

  /** Generates definitions that must come *before* the code generated for regular classes.
   *
   *  This notably includes the `typeData` definitions, since the vtable of `jl.Object` is a subtype
   *  of `typeData`.
   */
  def genPreClasses()(implicit ctx: WasmContext): Unit = {
    genPreMainRecTypeDefinitions()
    ctx.moduleBuilder.addRecTypeBuilder(ctx.mainRecType)
    genCoreTypesInRecType()

    if (hasJSInterop)
      genJSImports()

    genPrimitiveTypeDataGlobals()

    genHelperDefinitions()
  }

  /** Generates definitions that must come *after* the code generated for regular classes. */
  def genPostClasses()(implicit ctx: WasmContext): Unit = {
    if (!hasJSInterop)
      genBoxedBooleanGlobals()
  }

  // --- Type definitions ---

  private def genPreMainRecTypeDefinitions()(implicit ctx: WasmContext): Unit = {
    val b = ctx.moduleBuilder

    def genUnderlyingArrayType(id: TypeID, elemType: StorageType): Unit =
      b.addRecType(id, OriginalName(id.toString()), ArrayType(FieldType(elemType, true)))

    genUnderlyingArrayType(genTypeID.i8Array, Int8)
    genUnderlyingArrayType(genTypeID.i16Array, Int16)
    genUnderlyingArrayType(genTypeID.i32Array, Int32)
    genUnderlyingArrayType(genTypeID.i64Array, Int64)
    genUnderlyingArrayType(genTypeID.f32Array, Float32)
    genUnderlyingArrayType(genTypeID.f64Array, Float64)
    genUnderlyingArrayType(genTypeID.anyArray, anyref)

    genUnderlyingArrayType(genTypeID.externrefArray, RefType.externref)
  }

  private def genCoreTypesInRecType()(implicit ctx: WasmContext): Unit = {
    def genCoreType(id: TypeID, compositeType: CompositeType): Unit =
      ctx.mainRecType.addSubType(id, OriginalName(id.toString()), compositeType)

    genCoreType(
      genTypeID.cloneFunctionType,
      FunctionType(
        List(RefType(genTypeID.ObjectStruct)),
        List(RefType(genTypeID.ObjectStruct))
      )
    )

    genCoreType(
      genTypeID.isJSClassInstanceFuncType,
      FunctionType(List(RefType.anyref), List(Int32))
    )

    genCoreType(
      genTypeID.typeDataArray,
      ArrayType(FieldType(RefType(genTypeID.typeData), isMutable = false))
    )

    genCoreType(
      genTypeID.reflectiveProxies,
      ArrayType(FieldType(RefType(genTypeID.reflectiveProxy), isMutable = false))
    )

    if (useCustomDescriptors) {
      ctx.mainRecType.addSubType(
        SubType(
          genTypeID.typeDataDescribed,
          OriginalName(genTypeID.typeDataDescribed.toString()),
          isFinal = false,
          superType = None,
          describes = None,
          descriptor = Some(genTypeID.typeData),
          StructType(Nil)
        )
      )
    }

    ctx.mainRecType.addSubType(
      SubType(
        genTypeID.typeData,
        OriginalName(genTypeID.typeData.toString()),
        isFinal = false,
        superType = None,
        describes = if (useCustomDescriptors) Some(genTypeID.typeDataDescribed) else None,
        descriptor = None,
        StructType(typeDataStructFields)
      )
    )

    genCoreType(
      genTypeID.reflectiveProxy,
      StructType(
        List(
          StructField(
            genFieldID.reflectiveProxy.methodID,
            OriginalName(genFieldID.reflectiveProxy.methodID.toString()),
            Int32,
            isMutable = false
          ),
          StructField(
            genFieldID.reflectiveProxy.funcRef,
            OriginalName(genFieldID.reflectiveProxy.funcRef.toString()),
            RefType(HeapType.Func),
            isMutable = false
          )
        )
      )
    )

    if (!hasJSInterop) {
      genCoreType(
        genTypeID.undefined,
        StructType(Nil)
      )
      genCoreType(
        genTypeID.wasmString,
        StructType(
          List(
            StructField(
              genFieldID.wasmString.chars,
              OriginalName(genFieldID.wasmString.chars.toString()),
              RefType(genTypeID.i16Array),
              isMutable = true
            ),
            StructField(
              genFieldID.wasmString.length,
              OriginalName(genFieldID.wasmString.length.toString()),
              Int32,
              isMutable = false
            ),
            StructField(
              genFieldID.wasmString.left,
              OriginalName(genFieldID.wasmString.left.toString()),
              RefType.nullable(genTypeID.wasmString),
              isMutable = true
            )
          )
        )
      )
      genCoreType(
        genTypeID.wasmStringArray,
        ArrayType(FieldType(RefType.nullable(genTypeID.wasmString), isMutable = true))
      )
    }
  }

  // --- Imports ---

  private def genJSImports()(implicit ctx: WasmContext): Unit = {
    genTagImports()
    genGlobalImports()
    genStringBuiltinImports()
    genHelperImports()
  }

  private def genTagImports()(implicit ctx: WasmContext): Unit = {
    val exceptionSig = FunctionType(List(RefType.externref), Nil)
    val typeID = ctx.moduleBuilder.functionTypeToTypeID(exceptionSig)
    ctx.moduleBuilder.addImport(
      Import(
        CoreHelpersModule,
        "JSTag",
        ImportDesc.Tag(
          genTagID.exception,
          OriginalName(genTagID.exception.toString()),
          typeID
        )
      )
    )
  }

  private def genGlobalImports()(implicit ctx: WasmContext): Unit = {
    def addGlobalHelperImport(id: genGlobalID.JSHelperGlobalID, tpe: Type): Unit = {
      ctx.moduleBuilder.addImport(
        Import(
          CoreHelpersModule,
          id.toString(), // import name, guaranteed by JSHelperGlobalID
          ImportDesc.Global(id, OriginalName(id.toString()), isMutable = false, tpe)
        )
      )
    }

    addGlobalHelperImport(genGlobalID.undef, RefType.any)
    addGlobalHelperImport(genGlobalID.bFalse, RefType.any)
    addGlobalHelperImport(genGlobalID.bTrue, RefType.any)
    addGlobalHelperImport(genGlobalID.idHashCodeMap, RefType.extern)
  }

  private def genStringBuiltinImports()(implicit ctx: WasmContext): Unit = {
    import RefType.{extern, externref}

    def addHelperImport(id: genFunctionID.JSHelperFunctionID,
        params: List[Type], results: List[Type]): Unit = {
      val sig = FunctionType(params, results)
      val typeID = ctx.moduleBuilder.functionTypeToTypeID(sig)
      ctx.moduleBuilder.addImport(
        Import(
          JSStringBuiltinsModule,
          id.toString(), // import name, guaranteed by JSHelperFunctionID
          ImportDesc.Func(id, OriginalName(id.toString()), typeID)
        )
      )
    }

    addHelperImport(genFunctionID.stringBuiltins.test, List(externref), List(Int32))
    addHelperImport(genFunctionID.stringBuiltins.fromCharCode, List(Int32), List(extern))
    addHelperImport(genFunctionID.stringBuiltins.fromCodePoint, List(Int32), List(extern))
    addHelperImport(genFunctionID.stringBuiltins.charCodeAt, List(externref, Int32), List(Int32))
    addHelperImport(genFunctionID.stringBuiltins.codePointAt, List(externref, Int32), List(Int32))
    addHelperImport(genFunctionID.stringBuiltins.length, List(externref), List(Int32))
    addHelperImport(genFunctionID.stringBuiltins.concat, List(externref, externref), List(extern))
    addHelperImport(
        genFunctionID.stringBuiltins.substring, List(externref, Int32, Int32), List(extern))
    addHelperImport(genFunctionID.stringBuiltins.equals, List(externref, externref), List(Int32))
  }

  private def genHelperImports()(implicit ctx: WasmContext): Unit = {
    def addHelperImport(id: genFunctionID.JSHelperFunctionID,
        params: List[Type], results: List[Type]): Unit = {
      val sig = FunctionType(params, results)
      val typeID = ctx.moduleBuilder.functionTypeToTypeID(sig)
      ctx.moduleBuilder.addImport(
        Import(
          CoreHelpersModule,
          id.toString(), // import name, guaranteed by JSHelperFunctionID
          ImportDesc.Func(id, OriginalName(id.toString()), typeID)
        )
      )
    }

    addHelperImport(genFunctionID.is, List(anyref, anyref), List(Int32))

    addHelperImport(genFunctionID.isUndef, List(anyref), List(Int32))

    for (primType <- List(BooleanType, FloatType, DoubleType)) {
      val primRef = primType.primRef
      val wasmType = transformPrimType(primType)
      if (primType != BooleanType)
        addHelperImport(genFunctionID.box(primRef), List(wasmType), List(RefType.any))
      addHelperImport(genFunctionID.unbox(primRef), List(anyref), List(wasmType))
      addHelperImport(genFunctionID.typeTest(primRef), List(anyref), List(Int32))
    }

    addHelperImport(genFunctionID.bIFallback, List(Int32), List(RefType.any))
    addHelperImport(genFunctionID.uIFallback, List(anyref), List(Int32))
    addHelperImport(genFunctionID.typeTest(IntRef), List(anyref), List(Int32))

    addHelperImport(genFunctionID.jsValueToString, List(RefType.any), List(RefType.extern))
    addHelperImport(genFunctionID.jsValueToStringForConcat, List(anyref), List(RefType.extern))
    addHelperImport(genFunctionID.booleanToString, List(Int32), List(RefType.extern))
    addHelperImport(genFunctionID.intToString, List(Int32), List(RefType.extern))
    addHelperImport(genFunctionID.longToString, List(Int64), List(RefType.extern))
    addHelperImport(genFunctionID.doubleToString, List(Float64), List(RefType.extern))

    addHelperImport(genFunctionID.jsValueType, List(RefType.any), List(Int32))
    addHelperImport(genFunctionID.jsValueDescription, List(anyref), List(RefType.extern))
    addHelperImport(genFunctionID.bigintHashCode, List(RefType.any), List(Int32))
    addHelperImport(
      genFunctionID.symbolDescription,
      List(RefType.any),
      List(RefType.externref)
    )
    addHelperImport(
      genFunctionID.idHashCodeGet,
      List(RefType.extern, RefType.any),
      List(Int32)
    )
    addHelperImport(
      genFunctionID.idHashCodeSet,
      List(RefType.extern, RefType.any, Int32),
      Nil
    )

    addHelperImport(genFunctionID.makeTypeError, List(RefType.extern), List(RefType.extern))

    addHelperImport(genFunctionID.jsNewArray, Nil, List(RefType.any))
    addHelperImport(genFunctionID.jsNewObject, Nil, List(RefType.any))
    addHelperImport(genFunctionID.jsNewNoArg, List(anyref), List(anyref))
    addHelperImport(genFunctionID.jsImportMeta, Nil, List(anyref))
    if (coreSpec.wasmFeatures.useJSPI)
      addHelperImport(genFunctionID.jsAwait, List(anyref), List(anyref))
    addHelperImport(genFunctionID.jsDelete, List(anyref, anyref), Nil)
    addHelperImport(genFunctionID.jsForInStart, List(anyref), List(anyref))
    addHelperImport(genFunctionID.jsForInNext, List(anyref), List(anyref, Int32))
    addHelperImport(genFunctionID.jsIsTruthy, List(anyref), List(Int32))

    addHelperImport(
      genFunctionID.jsSuperSelect,
      List(anyref, anyref, anyref),
      List(anyref)
    )
    addHelperImport(
      genFunctionID.jsSuperSelectSet,
      List(anyref, anyref, anyref, anyref),
      Nil
    )
  }

  // --- Global definitions ---

  private def genPrimitiveTypeDataGlobals()(implicit ctx: WasmContext): Unit = {
    import genFieldID.typeData._

    val typeDataTypeID = genTypeID.typeData

    val jsPrototypeOpt: List[Instr] =
      if (!useCustomDescriptors || !hasJSInterop) Nil
      else List(RefNull(HeapType.NoExtern))

    // Other than `name` and `kind`, all the fields have the same value for all primitives
    val commonFieldValues = List(
      // specialInstanceTypes
      I32Const(0),
      // strictAncestors
      RefNull(HeapType.None),
      // componentType
      RefNull(HeapType.None),
      // the classOf instance - initially `null`; filled in by the `createClassOf` helper
      RefNull(HeapType.None),
      // arrayOf, the typeData of an array of this type - always `null` for primitive types
      RefNull(HeapType.None),
      // cloneFunction
      RefNull(HeapType.NoFunc),
      // isJSClassInstance
      RefNull(HeapType.NoFunc),
      // reflectiveProxies
      ArrayNewFixed(genTypeID.reflectiveProxies, 0)
    )

    for ((primRef, kind) <- primRefsWithKinds) {
      val nameValue = ctx.stringPool.getConstantStringDataForTypeData(primRef.displayName)

      val instrs: List[Instr] = {
        jsPrototypeOpt ::: nameValue ::: I32Const(kind) :: commonFieldValues :::
        StructNew(genTypeID.typeData) :: Nil
      }

      ctx.addGlobal(
        Global(
          genGlobalID.forVTable(primRef),
          OriginalName("d." + primRef.charCode),
          isMutable = false,
          RefType(genTypeID.typeData),
          Expr(instrs)
        )
      )
    }
  }

  private def genBoxedBooleanGlobals()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    for ((globalID, value) <- List((genGlobalID.bFalse, false), (genGlobalID.bTrue, true))) {
      val structTypeID = genTypeID.forClass(BoxedBooleanClass)
      val vtableGlobalID = genGlobalID.forVTable(BoxedBooleanClass)

      val instructions = if (!ctx.useCustomDescriptors) {
        List(
          GlobalGet(vtableGlobalID),
          I32Const(value.##),
          I32Const(if (value) 1 else 0),
          StructNew(structTypeID)
        )
      } else {
        List(
          I32Const(value.##),
          I32Const(if (value) 1 else 0),
          GlobalGet(vtableGlobalID),
          StructNewDesc(structTypeID)
        )
      }

      ctx.addGlobal(
        Global(
          globalID,
          OriginalName(globalID.toString()),
          isMutable = false,
          RefType(structTypeID),
          Expr(instructions)
        )
      )
    }
  }

  // --- Function definitions ---

  /** Generates all the helper function definitions of the core Wasm lib. */
  private def genHelperDefinitions()(implicit ctx: WasmContext): Unit = {
    genBoxBoolean()
    genBoxInt()
    genUnboxInt()
    genUnboxByteOrShort(ByteRef)
    genUnboxByteOrShort(ShortRef)
    genTestByteOrShort(ByteRef, I32Extend8S)
    genTestByteOrShort(ShortRef, I32Extend16S)
    if (!hasJSInterop)
      genWasmWithoutJSHelpers()
    genTypeDataName()
    genCreateClassOf()
    genGetClassOf()
    genSpecificArrayTypeData()

    if (semantics.asInstanceOfs != CheckedBehavior.Unchecked ||
        semantics.arrayStores != CheckedBehavior.Unchecked) {
      genValueDescription()
    }

    if (semantics.asInstanceOfs != CheckedBehavior.Unchecked) {
      genClassCastException()
      genPrimitiveAsInstances()
      genArrayAsInstances()
    }

    if (semantics.arrayStores != CheckedBehavior.Unchecked)
      genThrowArrayStoreException()

    if (semantics.arrayIndexOutOfBounds != CheckedBehavior.Unchecked) {
      genThrowArrayIndexOutOfBoundsException()
      genArrayGets()
      genArraySets()
    } else if (semantics.arrayStores != CheckedBehavior.Unchecked) {
      genArraySet(ClassRef(ObjectClass))
    }

    if (semantics.negativeArraySizes != CheckedBehavior.Unchecked) {
      genThrowNegativeArraySizeException()
    }

    if (semantics.stringIndexOutOfBounds != CheckedBehavior.Unchecked) {
      if (hasJSInterop) {
        genCheckedStringCharAtOrCodePointAt(
            genFunctionID.checkedStringCharAt, genFunctionID.stringBuiltins.charCodeAt)
        genCheckedStringCharAtOrCodePointAt(
            genFunctionID.checkedStringCodePointAt, genFunctionID.stringBuiltins.codePointAt)
        genCheckedSubstringStart()
        genCheckedSubstringStartEnd()
      } else {
        // `checkedStringCodePointAt`, `checkedSubstringStart`, and `checkedSubstringStartEnd` wouldn't be
        // generated because optimizer won't create transient node for no-JS module kinds.
        genCheckedStringCharAtOrCodePointAt(
            genFunctionID.checkedStringCharAt, genFunctionID.wasmString.charCodeAt)
      }
    }

    if (semantics.nullPointers != CheckedBehavior.Unchecked) {
      genThrowNullPointerException()
    }

    if (semantics.moduleInit == CheckedBehavior.Fatal) {
      genThrowModuleInitError()
    }

    genIsInstance()
    genIsAssignableFrom()
    if (semantics.asInstanceOfs != CheckedBehavior.Unchecked)
      genCast()
    genGetComponentType()
    if (globalInfo.isClassSuperClassUsed)
      genGetSuperClass()
    genNewArray()
    genAnyGetClass()
    genAnyGetClassName()
    genIntGetTypeData()
    genDoubleGetTypeData()
    genAnyGetTypeData()
    genIdentityHashCode()
    genSearchReflectiveProxy()
    genArrayCloneFunctions()
    genArrayCopyFunctions()
  }

  private def newFunctionBuilder(functionID: FunctionID, originalName: OriginalName)(
      implicit ctx: WasmContext): FunctionBuilder = {
    new FunctionBuilder(ctx.moduleBuilder, functionID, originalName, noPos)
  }

  private def newFunctionBuilder(functionID: FunctionID)(
      implicit ctx: WasmContext): FunctionBuilder = {
    newFunctionBuilder(functionID, OriginalName(functionID.toString()))
  }

  private def genBoxBoolean()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.box(BooleanRef))
    val xParam = fb.addParam("x", Int32)
    fb.setResultType(RefType.any)

    fb += GlobalGet(genGlobalID.bTrue)
    fb += GlobalGet(genGlobalID.bFalse)
    fb += LocalGet(xParam)
    fb += Select(List(RefType.any))

    fb.buildAndAddToModule()
  }

  private def genBoxInt()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.box(IntRef))
    val xParam = fb.addParam("x", Int32)
    fb.setResultType(RefType.any)

    /* Test if the two most significant bits are different: (x ^ (x << 1)) & 0x80000000
     * If they are, we cannot box as i31 since sign extension on unbox will
     * duplicate the second most significant bit (and JS would see the wrong
     * number value as well).
     */
    fb += LocalGet(xParam)
    fb += LocalGet(xParam)
    fb += I32Const(1)
    fb += I32Shl
    fb += I32Xor
    fb += I32Const(0x80000000)
    fb += I32And

    // If non-zero,
    fb.ifThenElse(RefType.any) {
      // then call the fallback JS helper
      fb += LocalGet(xParam)
      fb += Call(genFunctionID.bIFallback)
    } {
      // else use ref.i31
      fb += LocalGet(xParam)
      fb += RefI31
    }

    fb.buildAndAddToModule()
  }

  private def genUnboxInt()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.unbox(IntRef))
    val xParam = fb.addParam("x", RefType.anyref)
    fb.setResultType(Int32)

    // If x is a (ref i31), extract it
    fb.block(RefType.anyref) { xIsNotI31Label =>
      fb += LocalGet(xParam)
      fb += BrOnCastFail(xIsNotI31Label, RefType.anyref, RefType.i31)
      fb += I31GetS
      fb += Return
    }

    // Otherwise, use the fallback helper
    fb += Call(genFunctionID.uIFallback)

    fb.buildAndAddToModule()
  }

  private def genUnboxByteOrShort(typeRef: PrimRef)(implicit ctx: WasmContext): Unit = {
    /* The unboxing functions for Byte and Short actually do exactly the same thing.
     * We keep them separate so that the rest of the codebase is clearer.
     * Note that *checked* unboxing goes through `genFunctionID.asInstance` instead.
     */

    val fb = newFunctionBuilder(genFunctionID.unbox(typeRef))
    val xParam = fb.addParam("x", RefType.anyref)
    fb.setResultType(Int32)

    // If x is a (ref i31), extract it
    fb.block(RefType.anyref) { xIsNotI31Label =>
      fb += LocalGet(xParam)
      fb += BrOnCastFail(xIsNotI31Label, RefType.anyref, RefType.i31)
      fb += I31GetS
      fb += Return
    }
    fb += Drop

    /* For Wasm-without-JS, it could also be a DoubleBox.
     * With JS interop, all JS `number`s in the correct range are guaranteed to be i31ref's.
     */
    if (!hasJSInterop) {
      val DoubleBoxTypeID = genTypeID.forClass(BoxedDoubleClass)
      fb.block(RefType.anyref) { xIsNotDoubleBox =>
        fb += LocalGet(xParam)
        fb += BrOnCastFail(xIsNotDoubleBox, RefType.anyref, RefType(DoubleBoxTypeID))
        fb += StructGet(DoubleBoxTypeID, genFieldID.boxValue)
        fb += I32TruncSatF64S
        fb += Return
      }
      fb += Drop
    }

    // Otherwise, it must be null, so return 0
    fb += I32Const(0)

    fb.buildAndAddToModule()
  }

  private def genTestByteOrShort(typeRef: PrimRef, signExtend: SimpleInstr)(
      implicit ctx: WasmContext): Unit = {

    val fb = newFunctionBuilder(genFunctionID.typeTest(typeRef))
    val xParam = fb.addParam("x", RefType.anyref)
    fb.setResultType(Int32)

    val intValueLocal = fb.addLocal("intValue", Int32)
    val doubleValueLocal = fb.addLocal("doubleValue", Float64)

    // If x is a (ref i31), extract it and test whether it sign-extends to itself
    fb.block(RefType.anyref) { xIsNotI31Label =>
      fb += LocalGet(xParam)
      fb += BrOnCastFail(xIsNotI31Label, RefType.anyref, RefType.i31)
      fb += I31GetS
      fb += LocalTee(intValueLocal)
      fb += LocalGet(intValueLocal)
      fb += signExtend
      fb += I32Eq
      fb += Return
    }
    fb += Drop

    /* For Wasm-without-JS, it could also be a DoubleBox.
     * With JS interop, all JS `number`s in the correct range are guaranteed to be i31ref's.
     */
    if (!hasJSInterop) {
      val doubleBoxTypeID = genTypeID.forClass(BoxedDoubleClass)
      fb.block(RefType.anyref) { xIsNotDoubleBox =>
        fb += LocalGet(xParam)
        fb += BrOnCastFail(xIsNotDoubleBox, RefType.anyref, RefType(doubleBoxTypeID))
        fb += StructGet(doubleBoxTypeID, genFieldID.boxValue)
        fb += LocalTee(doubleValueLocal)
        fb += I32TruncSatF64S
        fb += signExtend

        /* Convert back to double and test whether it's the same value.
         * Use a bitwise cast to rule out -0.0.
         */
        fb += F64ConvertI32S
        fb += I64ReinterpretF64
        fb += LocalGet(doubleValueLocal)
        fb += I64ReinterpretF64
        fb += I64Eq
        fb += Return
      }
      fb += Drop
    }

    // Otherwise, return false
    fb += I32Const(0)

    fb.buildAndAddToModule()
  }

  /** `typeDataName: (ref typeData) -> string`.
   *
   *  Initializes the `name` field of the given `typeData` if that was not done yet, and returns its
   *  value.
   *
   *  The computed value is specified by `java.lang.Class.getName()`. See also the documentation on
   *  [[Names.StructFieldIdx.typeData.name]] for details.
   *
   *  @see
   *    [[https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Class.html#getName()]]
   */
  private def genTypeDataName()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.typeDataName)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(stringType)

    val componentTypeDataLocal = fb.addLocal("componentTypeData", typeDataType)
    val nameLocal = fb.addLocal("name", stringType)

    def genArrayTypeDataName(): Unit = {
      // componentTypeData := ref_as_non_null(typeData.componentType)
      fb += LocalGet(typeDataParam)
      fb += StructGet(
        genTypeID.typeData,
        genFieldID.typeData.componentType
      )
      fb += RefAsNonNull
      fb += LocalTee(componentTypeDataLocal)

      // if (componentTypeData.kind == KindArray)
      fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
      fb += I32Const(KindArray)
      fb += I32Eq
      fb.ifThenElse(stringType) {
        // the component type is an array; concatenate "[" + <its own name>
        fb ++= ctx.stringPool.getConstantStringInstr("[")
        fb += LocalGet(componentTypeDataLocal)
        fb += Call(genFunctionID.typeDataName)
        genStringConcat(fb)
      } {
        // the component type is not array (nor a primitive);
        // concatenate "[L" + <its own name> + ";"
        fb ++= ctx.stringPool.getConstantStringInstr("[L")
        fb += LocalGet(componentTypeDataLocal)
        fb += Call(genFunctionID.typeDataName)
        genStringConcat(fb)
        fb ++= ctx.stringPool.getConstantStringInstr(";")
        genStringConcat(fb)
      }
    }

    fb.block(stringType) { alreadyInitializedLabel =>
      // br_on_non_null $alreadyInitialized typeData.name
      fb += LocalGet(typeDataParam)
      fb += StructGet(genTypeID.typeData, genFieldID.typeData.name)
      fb += BrOnNonNull(alreadyInitializedLabel)

      // for the StructSet typeData.name near the end
      fb += LocalGet(typeDataParam)

      /* In JS-Wasm, if it was null, the typeData must represent an
       * array type, and its component type cannot be a primitive type; compute
       * its name from the component type name.
       *
       * In Wasm-without-JS, `name` is initialized with null for both array and
       * primitive types.
       * If it is an array, compute the name from the component type name.
       * If not, initialize `name` from the data segment.
       */
      if (hasJSInterop) {
        genArrayTypeDataName()
      } else {
        fb += LocalGet(typeDataParam)
        fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
        fb += I32Const(KindArray)
        fb += I32Eq
        fb.ifThenElse(stringType) {
          genArrayTypeDataName()
        } {
          for (idx <- List(
                  genFieldID.typeData.nameOffset,
                  genFieldID.typeData.nameSize,
                  genFieldID.typeData.nameStringIndex)) {
            fb += LocalGet(typeDataParam)
            fb += StructGet(genTypeID.typeData, idx)
          }
          fb += Call(genFunctionID.stringLiteral)
        }
      }

      // typeData.name := <top of stack> ; leave it on the stack
      fb += LocalTee(nameLocal)
      fb += StructSet(genTypeID.typeData, genFieldID.typeData.name)
      fb += LocalGet(nameLocal)
    }

    fb.buildAndAddToModule()
  }

  private def genStringConcat(fb: FunctionBuilder): Unit = {
    if (hasJSInterop)
      fb += Call(genFunctionID.stringBuiltins.concat)
    else
      fb += Call(genFunctionID.wasmString.stringConcat)
  }

  /** `createClassOf: (ref typeData) -> (ref jlClass)`.
   *
   *  Creates the unique `java.lang.Class` instance associated with the given `typeData`, stores it
   *  in its `classOfValue` field, and returns it.
   *
   *  Must be called only if the `classOfValue` of the typeData is null. All call sites must deal
   *  with the non-null case as a fast-path.
   */
  private def genCreateClassOf()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)
    val resultType = RefType(HeapType(genTypeID.ClassStruct, exact = useCustomDescriptors))

    val fb = newFunctionBuilder(genFunctionID.createClassOf)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(resultType)

    val classInstanceLocal = fb.addLocal("classInstance", resultType)

    // classInstance := newDefault$java.lang.Class(typeData)
    // leave it on the stack for the constructor call
    fb += LocalGet(typeDataParam)
    fb += Call(genFunctionID.newDefault(ClassClass))
    fb += LocalTee(classInstanceLocal)

    // Call java.lang.Class::<init>()
    fb += Call(
      genFunctionID.forMethod(
        MemberNamespace.Constructor,
        ClassClass,
        NoArgConstructorName
      )
    )

    // typeData.classOfValue := classInstance
    fb += LocalGet(typeDataParam)
    fb += LocalGet(classInstanceLocal)
    fb += StructSet(genTypeID.typeData, genFieldID.typeData.classOfValue)

    // <top-of-stack> := classInstance for the implicit return
    fb += LocalGet(classInstanceLocal)

    fb.buildAndAddToModule()
  }

  /** `getClassOf: (ref typeData) -> (ref jlClass)`.
   *
   *  Initializes the `java.lang.Class` instance associated with the given `typeData` if not already
   *  done, and returns it.
   */
  private def genGetClassOf()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)
    val resultType = RefType(HeapType(genTypeID.ClassStruct, exact = useCustomDescriptors))

    val fb = newFunctionBuilder(genFunctionID.getClassOf)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(resultType)

    fb.block(resultType) { alreadyInitializedLabel =>
      // fast path
      fb += LocalGet(typeDataParam)
      fb += StructGet(genTypeID.typeData, genFieldID.typeData.classOfValue)
      fb += BrOnNonNull(alreadyInitializedLabel)
      // slow path
      fb += LocalGet(typeDataParam)
      fb += Call(genFunctionID.createClassOf)
    } // end bock alreadyInitializedLabel

    fb.buildAndAddToModule()
  }

  /** `valueDescription: anyref -> (ref extern)` (a string).
   *
   *  Returns a safe string description of a value. This helper is never called
   *  for `value === null`. As implemented, it would return `"object"` if it were.
   */
  private def genValueDescription()(implicit ctx: WasmContext): Unit = {
    if (hasJSInterop)
      genValueDescriptionWithJS()
    else
      genValueDescriptionWithoutJS()
  }

  private def genValueDescriptionWithJS()(implicit ctx: WasmContext): Unit = {
    val objectType = RefType(genTypeID.ObjectStruct)

    val fb = newFunctionBuilder(genFunctionID.valueDescription)
    val valueParam = fb.addParam("value", anyref)
    fb.setResultType(stringType)

    fb.block(anyref) { notOurObjectLabel =>
      fb.block(objectType) { isCharLabel =>
        fb.block(objectType) { isLongLabel =>
          // If it not our object, jump out of notOurObject
          fb += LocalGet(valueParam)
          fb += BrOnCastFail(notOurObjectLabel, anyref, objectType)

          // If is a long or char box, jump out to the appropriate label
          fb += BrOnCast(isLongLabel, objectType, RefType(genTypeID.forClass(BoxedLongClass)))
          fb += BrOnCast(isCharLabel, objectType, RefType(genTypeID.forClass(BoxedCharacterClass)))

          // Get and return the class name
          fb += ctx.getVTableInstr(genTypeID.ObjectStruct)
          fb += ReturnCall(genFunctionID.typeDataName)
        }

        // Return the constant string "long"
        fb ++= ctx.stringPool.getConstantStringInstr("long")
        fb += Return
      }

      // Return the constant string "char"
      fb ++= ctx.stringPool.getConstantStringInstr("char")
      fb += Return
    }

    // When it is not one of our objects, use the JS helper
    fb += Call(genFunctionID.jsValueDescription)

    fb.buildAndAddToModule()
  }

  private def genValueDescriptionWithoutJS()(implicit ctx: WasmContext): Unit = {
    import SpecialNames._

    val longBoxTypeID = genTypeID.forClass(BoxedLongClass)
    val charBoxTypeID = genTypeID.forClass(BoxedCharacterClass)
    val booleanBoxTypeID = genTypeID.forClass(BoxedBooleanClass)
    val integerBoxTypeID = genTypeID.forClass(BoxedIntegerClass)
    val doubleBoxTypeID = genTypeID.forClass(BoxedDoubleClass)

    val objectType = RefType(genTypeID.ObjectStruct)

    val fb = newFunctionBuilder(genFunctionID.valueDescription)
    val valueParam = fb.addParam("value", anyref)
    fb.setResultType(stringType)

    val doubleValue = fb.addLocal("doubleValue", Float64)

    fb.block(Float64) { numberLabel =>
      fb.block(anyref) { notOurObjectLabel =>
        fb.block(RefType(doubleBoxTypeID)) { isDoubleLabel =>
          fb.block(RefType(integerBoxTypeID)) { isIntegerLabel =>
            fb.block(objectType) { isBooleanLabel =>
              fb.block(objectType) { isCharLabel =>
                fb.block(objectType) { isLongLabel =>
                  // If it not our object, jump out of notOurObject
                  fb += LocalGet(valueParam)
                  fb += BrOnCastFail(notOurObjectLabel, anyref, objectType)

                  // If is one of the box classes, jump out to the appropriate label
                  fb += BrOnCast(isLongLabel, objectType, RefType(longBoxTypeID))
                  fb += BrOnCast(isCharLabel, objectType, RefType(charBoxTypeID))
                  fb += BrOnCast(isBooleanLabel, objectType, RefType(booleanBoxTypeID))
                  fb += BrOnCast(isIntegerLabel, objectType, RefType(integerBoxTypeID))
                  fb += BrOnCast(isDoubleLabel, objectType, RefType(doubleBoxTypeID))

                  // Get and return the class name
                  fb += ctx.getVTableInstr(genTypeID.ObjectStruct)
                  fb += ReturnCall(genFunctionID.typeDataName)
                }

                // LongBox
                fb ++= ctx.stringPool.getConstantStringInstr("long")
                fb += Return
              }

              // CharBox
              fb ++= ctx.stringPool.getConstantStringInstr("char")
              fb += Return
            }

            // BooleanBox
            fb ++= ctx.stringPool.getConstantStringInstr("boolean")
            fb += Return
          }

          // IntegerBox
          fb += StructGet(integerBoxTypeID, genFieldID.boxValue)
          fb += F64ConvertI32S
          fb += Br(numberLabel)
        }

        // DoubleBox
        fb += StructGet(doubleBoxTypeID, genFieldID.boxValue)
        fb += Br(numberLabel)
      }

      // When it is not one of our objects, it is an i31ref, a wasmString or undefined
      fb += Drop
      fb.block(RefType.i31) { isI31Label =>
        fb.block(RefType(genTypeID.wasmString)) { isStringLabel =>
          fb += LocalGet(valueParam)
          fb += BrOnCast(isI31Label, RefType.anyref, RefType.i31)
          fb += BrOnCast(isStringLabel, RefType.anyref, RefType(genTypeID.wasmString))

          // undefined
          fb ++= ctx.stringPool.getConstantStringInstr("undefined")
          fb += Return
        }

        // string
        fb ++= ctx.stringPool.getConstantStringInstr("string")
        fb += Return
      }
      fb += I31GetS
      fb += F64ConvertI32S
    } // end of numberLabel

    fb += LocalTee(doubleValue)

    // Test for -0.0
    fb += I64ReinterpretF64
    fb += I64Const(java.lang.Double.doubleToLongBits(-0.0))
    fb += I64Eq
    fb.ifThen() {
      fb ++= ctx.stringPool.getConstantStringInstr("number(-0)")
      fb += Return
    }

    // Emit "number(" + doubleValue + ")"
    fb ++= ctx.stringPool.getConstantStringInstr("number(")
    fb += LocalGet(doubleValue)
    fb += Call(genFunctionID.forMethod(MemberNamespace.PublicStatic,
        RyuDoubleClass, doubleToStringMethodName))
    fb += RefAsNonNull
    fb += Call(genFunctionID.wasmString.stringConcat)
    fb ++= ctx.stringPool.getConstantStringInstr(")")
    fb += Call(genFunctionID.wasmString.stringConcat)

    fb.buildAndAddToModule()
  }

  /** `classCastException: [anyref, (ref typeData)] -> void`.
   *
   *  This function always throws. It should be followed by an `unreachable`
   *  statement.
   */
  private def genClassCastException()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.classCastException)
    val objParam = fb.addParam("obj", anyref)
    val typeDataParam = fb.addParam("typeData", typeDataType)

    maybeWrapInUBE(fb, semantics.asInstanceOfs) {
      genNewScalaClass(fb, ClassCastExceptionClass, StringArgConstructorName) {
        fb += LocalGet(objParam)
        fb += Call(genFunctionID.valueDescription)

        fb ++= ctx.stringPool.getConstantStringInstr(" cannot be cast to ")
        genStringConcat(fb)

        fb += LocalGet(typeDataParam)
        fb += Call(genFunctionID.typeDataName)
        genStringConcat(fb)
      }
    }

    fb += ExternConvertAny
    fb += Throw(genTagID.exception)

    fb.buildAndAddToModule()
  }

  /** Generates the `asInstance` functions for primitive types. */
  private def genPrimitiveAsInstances()(implicit ctx: WasmContext): Unit = {
    val primTypesWithAsInstances: List[PrimType] = List(
      UndefType,
      BooleanType,
      CharType,
      ByteType,
      ShortType,
      IntType,
      LongType,
      FloatType,
      DoubleType,
      StringType
    )

    for (primType <- primTypesWithAsInstances) {
      // asInstanceOf[PrimType]
      genPrimitiveOrBoxedClassAsInstance(primType, targetTpe = primType, isUnbox = true)

      // asInstanceOf[BoxedClass]
      val boxedClassType = ClassType(PrimTypeToBoxedClass(primType), nullable = true, exact = false)
      genPrimitiveOrBoxedClassAsInstance(primType, targetTpe = boxedClassType, isUnbox = false)
    }
  }

  /** Common logic for primitives and boxed classes in `genPrimitiveAsInstances`. */
  private def genPrimitiveOrBoxedClassAsInstance(primType: PrimType,
      targetTpe: irtpe.Type, isUnbox: Boolean)(
      implicit ctx: WasmContext): Unit = {

    val origName = OriginalName("as." + targetTpe.show())

    val resultType = TypeTransformer.transformSingleType(targetTpe)

    val fb = newFunctionBuilder(genFunctionID.asInstance(targetTpe), origName)
    val objParam = fb.addParam("obj", RefType.anyref)
    fb.setResultType(resultType)

    def withJSInterop(objIsNullLabel: LabelID): Unit = {
      primType match {
        // For byte and short, use br_on_cast_fail with i31 then check the value
        case ByteType | ShortType =>
          val intValueLocal = fb.addLocal("intValue", Int32)

          fb.block(RefType.anyref) { castFailLabel =>
            fb += LocalGet(objParam)
            fb += BrOnNull(objIsNullLabel)
            fb += BrOnCastFail(castFailLabel, RefType.any, RefType.i31)

            // Extract the i31 value
            fb += I31GetS
            fb += LocalTee(intValueLocal)

            // if it sign-extends to itself
            fb += LocalGet(intValueLocal)
            if (primType == ByteType)
              fb += I32Extend8S
            else
              fb += I32Extend16S
            fb += I32Eq
            fb.ifThen() {
              // then success
              if (isUnbox)
                fb += LocalGet(intValueLocal)
              else
                fb += LocalGet(objParam)
              fb += Return
            }

            // Fall through to fallback
            fb += LocalGet(objParam)
          }

        // For char and long, use br_on_cast_fail to test+cast to the box class
        case CharType | LongType =>
          val boxClass = PrimTypeToBoxedClass(primType)
          val structTypeID = genTypeID.forClass(boxClass)

          fb.block(RefType.any) { castFailLabel =>
            fb += LocalGet(objParam)
            fb += BrOnNull(objIsNullLabel)
            fb += BrOnCastFail(castFailLabel, RefType.any, RefType(structTypeID))

            // Extract the `value` field if unboxing
            if (isUnbox)
              fb += StructGet(structTypeID, genFieldID.boxValue)

            fb += Return
          }

        // For all other types, use type test, and separately unbox if required
        case _ =>
          // For Int, include a fast path for values that fit in i31
          if (primType == IntType) {
            fb.block(RefType.any) { notI31Label =>
              fb += LocalGet(objParam)
              fb += BrOnNull(objIsNullLabel)
              fb += BrOnCastFail(notI31Label, RefType.any, RefType.i31)
              if (isUnbox)
                fb += I31GetS
              fb += Return
            }
          } else {
            fb += LocalGet(objParam)
            fb += BrOnNull(objIsNullLabel)
          }

          // if obj.isInstanceOf[primType]
          primType match {
            case UndefType =>
              fb += Call(genFunctionID.isUndef)
            case StringType =>
              SWasmGen.genStringTest(fb)
            case primType: PrimTypeWithRef =>
              fb += Call(genFunctionID.typeTest(primType.primRef))
          }
          fb.ifThen() {
            // then, unbox if required then return
            if (isUnbox) {
              primType match {
                case UndefType =>
                  fb += GlobalGet(genGlobalID.undef)
                case StringType =>
                  fb += LocalGet(objParam)
                  fb += RefAsNonNull
                  SWasmGen.genStringCast(fb)
                case primType: PrimTypeWithRef =>
                  fb += LocalGet(objParam)
                  fb += Call(genFunctionID.unbox(primType.primRef))
              }
            } else {
              fb += LocalGet(objParam)
              if (primType == StringType)
                genStringCast(fb, nullable = true)
            }

            fb += Return
          }

          // Fall through for CCE
          fb += LocalGet(objParam)
      }
    }

    def withoutJSInterop(objIsNullLabel: LabelID): Unit = {
      primType match {
        // Numbers, which can be in i31ref's or DoubleBox'es
        case ByteType | ShortType | IntType | FloatType | DoubleType =>
          val intValueLocal = fb.addLocal("intValue", Int32)
          val floatValueLocal = fb.addLocal("floatValue", Float32)
          val doubleValueLocal = fb.addLocal("doubleValue", Float64)

          val integerBoxTypeID = genTypeID.forClass(BoxedIntegerClass)
          val doubleBoxTypeID = genTypeID.forClass(BoxedDoubleClass)

          fb.block(RefType.anyref) { castFailLabel =>
            fb.block(RefType(integerBoxTypeID)) { integerBoxLabel =>
              fb.block(RefType(doubleBoxTypeID)) { doubleBoxLabel =>
                fb.block(RefType.i31) { i31Label =>
                  fb += LocalGet(objParam)
                  fb += BrOnCast(i31Label, RefType.anyref, RefType.i31)
                  fb += BrOnNull(objIsNullLabel)
                  fb += BrOnCast(doubleBoxLabel, RefType.any, RefType(doubleBoxTypeID))
                  if (primType != ByteType && primType != ShortType)
                    fb += BrOnCast(integerBoxLabel, RefType.any, RefType(integerBoxTypeID))
                  fb += Br(castFailLabel)
                } // end of i31Label

                // i31

                primType match {
                  case IntType =>
                    if (isUnbox)
                      fb += I31GetS
                    fb += Return
                  case DoubleType =>
                    if (isUnbox) {
                      fb += I31GetS
                      fb += F64ConvertI32S
                    }
                    fb += Return
                  case _ =>
                    // For other types, we need to check that they fit in the target type
                    fb += I31GetS
                    fb += LocalTee(intValueLocal)
                    val unboxedValueLocal = (primType: @unchecked) match {
                      case ByteType =>
                        fb += I32Extend8S
                        fb += LocalGet(intValueLocal)
                        fb += I32Eq
                        intValueLocal
                      case ShortType =>
                        fb += I32Extend16S
                        fb += LocalGet(intValueLocal)
                        fb += I32Eq
                        intValueLocal
                      case FloatType =>
                        fb += F32ConvertI32S
                        fb += LocalTee(floatValueLocal)
                        fb += F64PromoteF32
                        fb += LocalGet(intValueLocal)
                        fb += F64ConvertI32S
                        fb += F64Eq
                        floatValueLocal
                    }
                    fb.ifThen() {
                      fb += LocalGet(if (isUnbox) unboxedValueLocal else objParam)
                      fb += Return
                    }
                    fb += LocalGet(objParam)
                    fb += Br(castFailLabel)
                }
              } // end of doubleBoxLabel

              // DoubleBox

              if (primType == DoubleType && !isUnbox) {
                fb += Return
              } else {
                fb += StructGet(doubleBoxTypeID, genFieldID.boxValue)

                if (primType == DoubleType) {
                  fb += Return
                } else {
                  fb += LocalTee(doubleValueLocal)

                  /* Extract the unboxed value into `unboxedValueLocal`, and
                   * leave the re-extended/promoted f64 value on the stack.
                   */
                  val unboxedValueLocal = (primType: @unchecked) match {
                    case ByteType =>
                      fb += I32TruncSatF64S
                      fb += I32Extend8S
                      fb += LocalTee(intValueLocal)
                      fb += F64ConvertI32S
                      intValueLocal
                    case ShortType =>
                      fb += I32TruncSatF64S
                      fb += I32Extend16S
                      fb += LocalTee(intValueLocal)
                      fb += F64ConvertI32S
                      intValueLocal
                    case IntType =>
                      fb += I32TruncSatF64S
                      fb += LocalTee(intValueLocal)
                      fb += F64ConvertI32S
                      intValueLocal
                    case FloatType =>
                      /* Canonicalize NaN's so they compare equal bit-wise.
                       * The Wasm spec for f32.demote_f64 and f64.promote_f32
                       * guarantees that a canonical NaN becomes a canonical
                       * NaN, so this is safe.
                       */
                      fb += LocalGet(doubleValueLocal)
                      fb += F64Ne
                      fb.ifThen() {
                        // doubleValue is a NaN; replace it with a canonical NaN
                        fb += F64Const(Double.NaN)
                        fb += LocalSet(doubleValueLocal)
                      }
                      fb += LocalGet(doubleValueLocal)
                      fb += F32DemoteF64
                      fb += LocalTee(floatValueLocal)
                      fb += F64PromoteF32
                      floatValueLocal
                  }

                  // Check that the re-extended f64 value is exactly the same as doubleValue
                  fb += I64ReinterpretF64
                  fb += LocalGet(doubleValueLocal)
                  fb += I64ReinterpretF64
                  fb += I64Eq
                  fb.ifThen() {
                    // It's a match
                    fb += LocalGet(if (isUnbox) unboxedValueLocal else objParam)
                    fb += Return
                  }
                  fb += LocalGet(objParam)
                  fb += Br(castFailLabel)
                }
              }
            } // end of integerBoxLabel

            /* Assert: we never reach here if primType is ByteType or ShortType.
             * We still need to generate code that typechecks, so treat them like IntType.
             */

            (primType: @unchecked) match {
              case ByteType | ShortType | IntType =>
                if (isUnbox)
                  fb += StructGet(integerBoxTypeID, genFieldID.boxValue)
                fb += Return

              case DoubleType =>
                if (isUnbox) {
                  fb += StructGet(integerBoxTypeID, genFieldID.boxValue)
                  fb += F64ConvertI32S
                }
                fb += Return

              case FloatType =>
                // Here we need to check that the integer fits in an f32
                fb += StructGet(integerBoxTypeID, genFieldID.boxValue)
                fb += LocalTee(intValueLocal)
                fb += F32ConvertI32S
                fb += LocalTee(floatValueLocal)
                fb += F64PromoteF32
                fb += LocalGet(intValueLocal)
                fb += F64ConvertI32S
                fb += F64Eq
                fb.ifThen() {
                  // It fits
                  fb += LocalGet(if (isUnbox) floatValueLocal else objParam)
                  fb += Return
                }
                fb += LocalGet(objParam)
                fb += Br(castFailLabel)
            }
          }

        // For other types, use br_on_cast_fail to the appropriate struct type
        case CharType | LongType | BooleanType | StringType | UndefType =>
          val structTypeID: TypeID = (primType: @unchecked) match {
            case StringType => genTypeID.wasmString
            case UndefType  => genTypeID.undefined
            case _          => genTypeID.forClass(PrimTypeToBoxedClass(primType))
          }

          fb.block(RefType.any) { castFailLabel =>
            fb += LocalGet(objParam)
            fb += BrOnNull(objIsNullLabel)
            fb += BrOnCastFail(castFailLabel, RefType.any, RefType(structTypeID))

            // Extract the `value` field if unboxing a Box class
            structTypeID match {
              case genTypeID.forClass(boxClass) if isUnbox =>
                fb += StructGet(structTypeID, genFieldID.boxValue)
              case _ =>
                ()
            }

            fb += Return
          }

        case NothingType | NullType | VoidType =>
          throw new AssertionError(s"Unexpected primType $primType")
      }
    }

    fb.block() { objIsNullLabel =>
      if (hasJSInterop)
        withJSInterop(objIsNullLabel)
      else
        withoutJSInterop(objIsNullLabel)

      // If we get here, it is a CCE
      fb += GlobalGet(genGlobalID.forVTable(PrimTypeToBoxedClass(primType)))
      fb += Call(genFunctionID.classCastException)
      fb += Unreachable
    }

    // obj is null -- load the zero of the target type (which is `null` for boxed classes)
    fb += SWasmGen.genZeroOf(targetTpe)

    fb.buildAndAddToModule()
  }

  private def genArrayAsInstances()(implicit ctx: WasmContext): Unit = {
    for (baseRef <- arrayBaseRefs)
      genBaseArrayAsInstance(baseRef)

    genAsSpecificRefArray()
  }

  private def genBaseArrayAsInstance(baseRef: NonArrayTypeRef)(implicit ctx: WasmContext): Unit = {
    val arrayTypeRef = ArrayTypeRef(baseRef, 1)

    val wasmTypeID = genTypeID.forArrayClass(arrayTypeRef)
    val resultType = RefType.nullable(wasmTypeID)

    val fb = newFunctionBuilder(
      genFunctionID.asInstance(irtpe.ArrayType(arrayTypeRef, nullable = true, exact = false)),
      OriginalName("asArray." + baseRef.displayName)
    )
    val objParam = fb.addParam("obj", anyref)
    fb.setResultType(resultType)

    fb.block(resultType) { successLabel =>
      fb += LocalGet(objParam)
      fb += BrOnCast(successLabel, anyref, resultType)

      // If we get here, it's a CCE -- `obj` is still on the stack
      fb += GlobalGet(genGlobalID.forArrayVTable(baseRef))
      fb += Call(genFunctionID.classCastException)
      fb += Unreachable
    }

    fb.buildAndAddToModule()
  }

  private def genAsSpecificRefArray()(implicit ctx: WasmContext): Unit = {
    val refArrayStructTypeID = genTypeID.forArrayClass(ArrayTypeRef(ClassRef(ObjectClass), 1))
    val resultType = RefType.nullable(refArrayStructTypeID)

    val fb = newFunctionBuilder(genFunctionID.asSpecificRefArray)
    val objParam = fb.addParam("obj", anyref)
    val arrayTypeDataParam = fb.addParam("arrayTypeData", RefType(genTypeID.typeData))
    fb.setResultType(resultType)

    val refArrayLocal = fb.addLocal("refArray", RefType(refArrayStructTypeID))

    fb.block(resultType) { successLabel =>
      fb.block() { isNullLabel =>
        fb.block(anyref) { failureLabel =>
          // If obj is null, return null
          fb += LocalGet(objParam)
          fb += BrOnNull(isNullLabel)

          // Otherwise, if we cannot cast to ObjectArray, fail
          fb += BrOnCastFail(failureLabel, RefType.any, RefType(refArrayStructTypeID))
          fb += LocalTee(refArrayLocal) // leave it on the stack for BrIf or for fall through to CCE

          // Otherwise, test assignability of the array type
          fb += LocalGet(arrayTypeDataParam)
          fb += LocalGet(refArrayLocal)
          fb += ctx.getVTableInstr(genTypeID.ObjectStruct)
          fb += Call(genFunctionID.isAssignableFrom)

          // If true, jump to success
          fb += BrIf(successLabel)
        }

        // If we get here, it's a CCE -- `obj` is still on the stack
        fb += LocalGet(arrayTypeDataParam)
        fb += Call(genFunctionID.classCastException)
        fb += Unreachable // for clarity; technically redundant since the stacks align
      }

      fb += RefNull(HeapType.None)
    }

    fb.buildAndAddToModule()
  }

  /** `throwArrayStoreException: anyref -> void`.
   *
   *  This function always throws. It should be followed by an `unreachable`
   *  statement.
   */
  private def genThrowArrayStoreException()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.throwArrayStoreException)
    val valueParam = fb.addParam("value", anyref)

    maybeWrapInUBE(fb, semantics.arrayStores) {
      genNewScalaClass(fb, ArrayStoreExceptionClass, StringArgConstructorName) {
        fb += LocalGet(valueParam)
        fb += Call(genFunctionID.valueDescription)
      }
    }
    fb += ExternConvertAny
    fb += Throw(genTagID.exception)

    fb.buildAndAddToModule()
  }

  /** `throwArrayIndexOutOfBoundsException: i32 -> void`.
   *
   *  This function always throws. It should be followed by an `unreachable`
   *  statement.
   */
  private def genThrowArrayIndexOutOfBoundsException()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.throwArrayIndexOutOfBoundsException)
    val indexParam = fb.addParam("index", Int32)

    maybeWrapInUBE(fb, semantics.arrayIndexOutOfBounds) {
      genNewScalaClass(fb, ArrayIndexOutOfBoundsExceptionClass, StringArgConstructorName) {
        fb += LocalGet(indexParam)
        fb += Call(genFunctionID.intToString)
      }
    }
    fb += ExternConvertAny
    fb += Throw(genTagID.exception)

    fb.buildAndAddToModule()
  }

  /** `throwNegativeArraySizeException: i32 -> void`.
   *
   *  This function always throws. It should be followed by an `unreachable`
   *  statement.
   */
  private def genThrowNegativeArraySizeException()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.throwNegativeArraySizeException)
    val sizeParam = fb.addParam("size", Int32)

    maybeWrapInUBE(fb, semantics.negativeArraySizes) {
      genNewScalaClass(fb, NegativeArraySizeExceptionClass, StringArgConstructorName) {
        fb += LocalGet(sizeParam)
        fb += Call(genFunctionID.intToString)
      }
    }
    fb += ExternConvertAny
    fb += Throw(genTagID.exception)

    fb.buildAndAddToModule()
  }

  /** `throwNullPointerException: void -> void`.
   *
   *  This function always throws. It should be followed by an `unreachable`
   *  statement.
   */
  private def genThrowNullPointerException()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.throwNullPointerException)

    maybeWrapInUBE(fb, semantics.nullPointers) {
      genNewScalaClass(fb, NullPointerExceptionClass, NoArgConstructorName) {}
    }
    fb += ExternConvertAny
    fb += Throw(genTagID.exception)

    fb.buildAndAddToModule()
  }

  /** Generates the `arrayGet.x` functions. */
  private def genArrayGets()(implicit ctx: WasmContext): Unit = {
    for (baseRef <- arrayBaseRefs)
      genArrayGet(baseRef)
  }

  /** `arrayGet.x: (ref null xArray), i32 -> x`. */
  private def genArrayGet(baseRef: NonArrayTypeRef)(implicit ctx: WasmContext): Unit = {
    val origName = OriginalName("arrayGet." + charCodeForOriginalName(baseRef))

    val arrayTypeRef = ArrayTypeRef(baseRef, 1)
    val arrayStructTypeID = genTypeID.forArrayClass(arrayTypeRef)
    val underlyingTypeID = genTypeID.underlyingOf(arrayTypeRef)

    val elemWasmType = baseRef match {
      case PrimRef(tpe) => transformSingleType(tpe)
      case ClassRef(_)  => anyref
    }

    val fb = newFunctionBuilder(genFunctionID.arrayGet(baseRef), origName)
    val arrayParam = fb.addParam("array", RefType.nullable(arrayStructTypeID))
    val indexParam = fb.addParam("index", Int32)
    fb.setResultType(elemWasmType)

    val underlyingLocal = fb.addLocal("underlying", RefType(underlyingTypeID))

    // Get the underlying array
    fb += LocalGet(arrayParam)
    fb += StructGet(arrayStructTypeID, genFieldID.objStruct.arrayUnderlying)
    fb += LocalTee(underlyingLocal)

    // if underlying.length unsigned_<= index
    fb += ArrayLen
    fb += LocalGet(indexParam)
    fb += I32LeU
    fb.ifThen() {
      // then throw ArrayIndexOutOfBoundsException
      fb += LocalGet(indexParam)
      fb += Call(genFunctionID.throwArrayIndexOutOfBoundsException)
      fb += Unreachable
    }

    // Load the underlying and index
    fb += LocalGet(underlyingLocal)
    fb += LocalGet(indexParam)

    // Use the appropriate variant of array.get for sign extension
    baseRef match {
      case BooleanRef | CharRef =>
        fb += ArrayGetU(underlyingTypeID)
      case ByteRef | ShortRef =>
        fb += ArrayGetS(underlyingTypeID)
      case _ =>
        fb += ArrayGet(underlyingTypeID)
    }

    fb.buildAndAddToModule()
  }

  /** Generates the `arraySet.x` functions. */
  private def genArraySets()(implicit ctx: WasmContext): Unit = {
    for (baseRef <- arrayBaseRefs)
      genArraySet(baseRef)
  }

  /** `arraySet.x: (ref null xArray), i32, x -> []`. */
  private def genArraySet(baseRef: NonArrayTypeRef)(implicit ctx: WasmContext): Unit = {
    val origName = OriginalName("arraySet." + charCodeForOriginalName(baseRef))

    val arrayTypeRef = ArrayTypeRef(baseRef, 1)
    val arrayStructTypeID = genTypeID.forArrayClass(arrayTypeRef)
    val underlyingTypeID = genTypeID.underlyingOf(arrayTypeRef)

    val elemWasmType = baseRef match {
      case PrimRef(tpe) => transformSingleType(tpe)
      case ClassRef(_)  => anyref
    }

    val fb = newFunctionBuilder(genFunctionID.arraySet(baseRef), origName)
    val arrayParam = fb.addParam("array", RefType.nullable(arrayStructTypeID))
    val indexParam = fb.addParam("index", Int32)
    val valueParam = fb.addParam("value", elemWasmType)

    val underlyingLocal = fb.addLocal("underlying", RefType(underlyingTypeID))

    // Get the underlying array
    fb += LocalGet(arrayParam)
    fb += StructGet(arrayStructTypeID, genFieldID.objStruct.arrayUnderlying)

    // Bounds check
    if (semantics.arrayIndexOutOfBounds != CheckedBehavior.Unchecked) {
      fb += LocalTee(underlyingLocal)

      // if underlying.length unsigned_<= index
      fb += ArrayLen
      fb += LocalGet(indexParam)
      fb += I32LeU
      fb.ifThen() {
        // then throw ArrayIndexOutOfBoundsException
        fb += LocalGet(indexParam)
        fb += Call(genFunctionID.throwArrayIndexOutOfBoundsException)
        fb += Unreachable
      }
    } else {
      fb += LocalSet(underlyingLocal)
    }

    // Store check
    if (semantics.arrayStores != CheckedBehavior.Unchecked &&
        baseRef.isInstanceOf[ClassRef]) {
      val componentTypeDataLocal = fb.addLocal("componentTypeData", RefType(genTypeID.typeData))

      fb.block() { successLabel =>
        // Get the component type data
        fb += LocalGet(arrayParam)
        fb += ctx.getVTableInstr(arrayStructTypeID)
        fb += StructGet(genTypeID.ObjectVTable, genFieldID.typeData.componentType)
        fb += RefAsNonNull
        fb += LocalTee(componentTypeDataLocal)

        // Fast path: if componentTypeData eq typeDataOf[jl.Object], succeed
        fb += GlobalGet(genGlobalID.forVTable(ClassRef(ObjectClass)))
        fb += RefEq
        fb += BrIf(successLabel)

        // If componentTypeData.kind >= KindJSType, succeed
        fb += LocalGet(componentTypeDataLocal)
        fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
        fb += I32Const(KindJSType)
        fb += I32GeU
        fb += BrIf(successLabel)

        // If value is null, succeed
        fb += LocalGet(valueParam)
        fb += RefIsNull
        fb += BrIf(successLabel)

        // If isInstance(componentTypeData, value), succeed
        fb += LocalGet(componentTypeDataLocal)
        fb += LocalGet(valueParam)
        fb += Call(genFunctionID.isInstance)
        fb += BrIf(successLabel)

        // Otherwise, it is a store exception
        fb += LocalGet(valueParam)
        fb += Call(genFunctionID.throwArrayStoreException)
        fb += Unreachable // for clarity; technically redundant since the stacks align
      }
    }

    // Store the value
    fb += LocalGet(underlyingLocal)
    fb += LocalGet(indexParam)
    fb += LocalGet(valueParam)
    fb += ArraySet(underlyingTypeID)

    fb.buildAndAddToModule()
  }

  /** `specificArrayTypeData: (ref typeData), i32 -> (ref vtable.java.lang.Object)`.
   *
   *  Returns the typeData/vtable of an array with `dims` dimensions over the
   *  given base `typeData`.
   *
   *  - `typeData` must not represent a primitive type nor `jl.Object`
   *  - `dims` must be strictly positive.
   */
  private def genSpecificArrayTypeData()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)
    val specificArrayVTableType = RefType(specificArrayVTableHeapType)

    /* Array classes extend Cloneable, Serializable and Object.
     * Filter out the ones that do not have run-time type info at all, as
     * we do for other classes.
     */
    val strictAncestors = {
      List(ObjectClass, CloneableClass, SerializableClass)
        .filter(name => ctx.getClassInfoOption(name).exists(_.hasRuntimeTypeInfo))
    }

    val fb = newFunctionBuilder(genFunctionID.specificArrayTypeData)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val dimsParam = fb.addParam("dims", Int32)
    fb.setResultType(specificArrayVTableType)

    val arrayTypeDataLocal = fb.addLocal("arrayTypeData", specificArrayVTableType)

    fb.loop() { loopLabel =>
      fb.block(specificArrayVTableType) { arrayOfIsNonNullLabel =>
        // br_on_non_null $arrayOfIsNonNull typeData.arrayOf
        fb += LocalGet(typeDataParam)
        fb += StructGet(
          genTypeID.typeData,
          genFieldID.typeData.arrayOf
        )
        fb += BrOnNonNull(arrayOfIsNonNullLabel)

        // <top-of-stack> := typeData ; for the <old typeData>.arrayOf := ... later on
        fb += LocalGet(typeDataParam)

        // typeData := new typeData(...)

        if (useCustomDescriptors && hasJSInterop)
          fb += GlobalGet(genGlobalID.forJSPrototype(ObjectClass)) // jsPrototype

        if (!hasJSInterop) {
          fb += I32Const(0) // nameOffset
          fb += I32Const(0) // nameSize
          fb += I32Const(0) // nameStringIndex
        }
        fb += RefNull(noStringHeapType) // name (initialized lazily by typeDataName)

        fb += I32Const(KindArray) // kind = KindArray
        fb += I32Const(0) // specialInstanceTypes = 0

        // strictAncestors
        for (strictAncestor <- strictAncestors)
          fb += GlobalGet(genGlobalID.forVTable(strictAncestor))
        fb += ArrayNewFixed(
          genTypeID.typeDataArray,
          strictAncestors.size
        )

        fb += LocalGet(typeDataParam) // componentType
        fb += RefNull(HeapType.None) // classOf
        fb += RefNull(HeapType.None) // arrayOf

        // clone
        fb += ctx.refFuncWithDeclaration(genFunctionID.cloneArray(ObjectRef))

        // isJSClassInstance
        fb += RefNull(HeapType.NoFunc)

        // reflectiveProxies, empty since all methods of array classes exist in jl.Object
        fb += ArrayNewFixed(genTypeID.reflectiveProxies, 0)

        // itable slots
        val objectClassInfo = ctx.getClassInfo(ObjectClass)
        fb ++= ClassEmitter.genItableSlots(objectClassInfo, List(SerializableClass, CloneableClass))

        // vtable items
        fb ++= objectClassInfo.tableEntries.map { methodName =>
          ctx.refFuncWithDeclaration(objectClassInfo.resolvedMethodInfos(methodName).tableEntryID)
        }
        fb += StructNew(specificArrayVTableTypeID)
        fb += LocalTee(arrayTypeDataLocal)

        // <old typeData>.arrayOf := typeData
        fb += StructSet(genTypeID.typeData, genFieldID.typeData.arrayOf)

        // put arrayTypeData back on the stack
        fb += LocalGet(arrayTypeDataLocal)
      } // end block $arrayOfIsNonNullLabel

      // dims := dims - 1 -- leave dims on the stack
      fb += LocalGet(dimsParam)
      fb += I32Const(1)
      fb += I32Sub
      fb += LocalTee(dimsParam)

      // if dims == 0 then
      //   return typeData.arrayOf (which is on the stack)
      fb += I32Eqz
      fb.ifThen(FunctionType(List(specificArrayVTableType), List(specificArrayVTableType))) {
        fb += Return
      }

      // typeData := typeData.arrayOf (which is on the stack), then loop back to the beginning
      fb += LocalSet(typeDataParam)
      fb += Br(loopLabel)
    } // end loop $loop
    fb += Unreachable

    fb.buildAndAddToModule()
  }

  /** `checkedString{CharAt,CodePointAt}`: (ref extern), i32 -> i32`.
   *
   *  Accesses a char/code point of a string by index. Used when
   *  stringIndexOutOfBounds are checked.
   */
  private def genCheckedStringCharAtOrCodePointAt(
      checkedHelperID: FunctionID, builtinID: FunctionID)(
      implicit ctx: WasmContext): Unit = {

    val fb = newFunctionBuilder(checkedHelperID)
    val strParam = fb.addParam("str", stringType)
    val indexParam = fb.addParam("index", Int32)
    fb.setResultType(Int32)

    // if index unsigned_>= str.length
    fb += LocalGet(indexParam)
    fb += LocalGet(strParam)
    if (hasJSInterop)
      fb += Call(genFunctionID.stringBuiltins.length)
    else
      fb += StructGet(genTypeID.wasmString, genFieldID.wasmString.length)
    fb += I32GeU // unsigned comparison makes negative values of index larger than the length
    fb.ifThen() {
      // then, throw a StringIndexOutOfBoundsException
      maybeWrapInUBE(fb, semantics.stringIndexOutOfBounds) {
        genNewScalaClass(fb, StringIndexOutOfBoundsExceptionClass, IntArgConstructorName) {
          fb += LocalGet(indexParam)
        }
      }
      fb += ExternConvertAny
      fb += Throw(genTagID.exception)
    }

    // otherwise, read the char
    fb += LocalGet(strParam)
    fb += LocalGet(indexParam)
    fb += Call(builtinID)

    fb.buildAndAddToModule()
  }

  /** `checkedSubstringStart: (ref extern), i32 -> (ref extern)`.
   *
   *  Implementation of jl.String.substring(start). Used when
   *  stringIndexOutOfBounds are checked.
   */
  private def genCheckedSubstringStart()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.checkedSubstringStart)
    val strParam = fb.addParam("str", RefType.extern)
    val startParam = fb.addParam("start", Int32)
    fb.setResultType(RefType.extern)

    /* if start unsigned_> str.length
     * The unsigned comparison makes negative values larger than the length.
     */
    fb += LocalGet(startParam)
    fb += LocalGet(strParam)
    fb += Call(genFunctionID.stringBuiltins.length)
    fb += I32GtU
    fb.ifThen() {
      // then, throw a StringIndexOutOfBoundsException
      maybeWrapInUBE(fb, semantics.stringIndexOutOfBounds) {
        genNewScalaClass(fb, StringIndexOutOfBoundsExceptionClass, IntArgConstructorName) {
          fb += LocalGet(startParam)
        }
      }
      fb += ExternConvertAny
      fb += Throw(genTagID.exception)
    }

    // otherwise, call the substring builtin
    fb += LocalGet(strParam)
    fb += LocalGet(startParam)
    fb += I32Const(-1) // unsigned max value
    fb += Call(genFunctionID.stringBuiltins.substring)

    fb.buildAndAddToModule()
  }

  /** `checkedSubstringStartEnd: (ref extern), i32, i32 -> (ref extern)`.
   *
   *  Implementation of jl.String.substring(start, end). Used when
   *  stringIndexOutOfBounds are checked.
   */
  private def genCheckedSubstringStartEnd()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.checkedSubstringStartEnd)
    val strParam = fb.addParam("str", RefType.extern)
    val startParam = fb.addParam("start", Int32)
    val endParam = fb.addParam("end", Int32)
    fb.setResultType(RefType.extern)

    /* if (start unsigned_> end) | (end unsigned_> str.length)
     * The unsigned comparisons make negative values larger than the length
     * since the happy path evaluates both conditions anyway, we don't bother
     * with a short-circuiting || and implement an | instead.
     */
    fb += LocalGet(startParam)
    fb += LocalGet(endParam)
    fb += I32GtU
    fb += LocalGet(endParam)
    fb += LocalGet(strParam)
    fb += Call(genFunctionID.stringBuiltins.length)
    fb += I32GtU
    fb += I32Or
    fb.ifThen() {
      // then, throw a StringIndexOutOfBoundsException
      maybeWrapInUBE(fb, semantics.stringIndexOutOfBounds) {
        genNewScalaClass(fb, StringIndexOutOfBoundsExceptionClass, IntArgConstructorName) {
          // Redo part of the test to determine the argument
          fb += LocalGet(startParam) // value if true for Select
          fb += LocalGet(endParam) // value if false for Select

          // start unsigned_> string.length
          fb += LocalGet(startParam)
          fb += LocalGet(strParam)
          fb += Call(genFunctionID.stringBuiltins.length)
          fb += I32GtU

          fb += Select(Nil) // infer i32
        }
      }
      fb += ExternConvertAny
      fb += Throw(genTagID.exception)
    }

    // otherwise, call the substring builtin
    fb += LocalGet(strParam)
    fb += LocalGet(startParam)
    fb += LocalGet(endParam)
    fb += Call(genFunctionID.stringBuiltins.substring)

    fb.buildAndAddToModule()
  }

  /** `throwModuleInitError: [] -> []` (always throws).
   *
   *  Throws an `UndefinedBehaviorError` for a module initialization error.
   */
  private def genThrowModuleInitError()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.throwModuleInitError)
    val typeDataParam = fb.addParam("typeData", RefType(genTypeID.typeData))

    genNewScalaClass(fb, UndefinedBehaviorErrorClass, StringArgConstructorName) {
      fb ++= ctx.stringPool.getConstantStringInstr("Initializer of ")
      fb += LocalGet(typeDataParam)
      fb += Call(genFunctionID.typeDataName)
      fb += Call(genFunctionID.stringBuiltins.concat)
      fb ++= ctx.stringPool.getConstantStringInstr(
          " called before completion of its super constructor")
      fb += Call(genFunctionID.stringBuiltins.concat)
    }
    fb += ExternConvertAny
    fb += Throw(genTagID.exception)

    fb.buildAndAddToModule()
  }

  /** `isInstance: (ref typeData), anyref -> i32` (a boolean).
   *
   *  Tests whether the given value is a non-null instance of the given type.
   */
  private def genIsInstance()(implicit ctx: WasmContext): Unit = {
    import genFieldID.typeData._

    val typeDataType = RefType(genTypeID.typeData)
    val objectRefType = RefType(genTypeID.ObjectStruct)

    val fb = newFunctionBuilder(genFunctionID.isInstance)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val valueParam = fb.addParam("value", RefType.anyref)
    fb.setResultType(Int32)

    val valueNonNullLocal = fb.addLocal("valueNonNull", RefType.any)
    val specialInstanceTypesLocal = fb.addLocal("specialInstanceTypes", Int32)

    // switch (typeData.kind)
    fb.switch(Int32) { () =>
      fb += LocalGet(typeDataParam)
      fb += StructGet(genTypeID.typeData, kind)
    }(
      // case anyPrimitiveKind => false
      (KindVoid to KindLastPrimitive).toList -> { () =>
        fb += I32Const(0)
      },
      // case KindObject => value ne null
      List(KindObject) -> { () =>
        fb += LocalGet(valueParam)
        fb += RefIsNull
        fb += I32Eqz
      },
      // for each boxed class, the corresponding primitive type test
      List(KindBoxedUnit) -> { () =>
        fb += LocalGet(valueParam)
        fb += Call(genFunctionID.isUndef)
      },
      List(KindBoxedBoolean) -> { () =>
        fb += LocalGet(valueParam)
        fb += Call(genFunctionID.typeTest(BooleanRef))
      },
      List(KindBoxedCharacter) -> { () =>
        fb += LocalGet(valueParam)
        val structTypeID = genTypeID.forClass(BoxedCharacterClass)
        fb += RefTest(RefType(structTypeID))
      },
      List(KindBoxedByte) -> { () =>
        fb += LocalGet(valueParam)
        fb += Call(genFunctionID.typeTest(ByteRef))
      },
      List(KindBoxedShort) -> { () =>
        fb += LocalGet(valueParam)
        fb += Call(genFunctionID.typeTest(ShortRef))
      },
      List(KindBoxedInteger) -> { () =>
        fb += LocalGet(valueParam)
        fb += Call(genFunctionID.typeTest(IntRef))
      },
      List(KindBoxedLong) -> { () =>
        fb += LocalGet(valueParam)
        val structTypeID = genTypeID.forClass(BoxedLongClass)
        fb += RefTest(RefType(structTypeID))
      },
      List(KindBoxedFloat) -> { () =>
        fb += LocalGet(valueParam)
        fb += Call(genFunctionID.typeTest(FloatRef))
      },
      List(KindBoxedDouble) -> { () =>
        fb += LocalGet(valueParam)
        fb += Call(genFunctionID.typeTest(DoubleRef))
      },
      List(KindBoxedString) -> { () =>
        fb += LocalGet(valueParam)
        SWasmGen.genStringTest(fb)
      },
      // case KindJSType | KindJSTypeWithSuperClass => call typeData.isJSClassInstance(value) or throw if it is null
      List(KindJSType, KindJSTypeWithSuperClass) -> { () =>
        if (!hasJSInterop) {
          fb += Unreachable // shouldn't reach here
        } else {
          fb.block(RefType.anyref) { isJSClassInstanceIsNull =>
            // Load value as the argument to the function
            fb += LocalGet(valueParam)

            // Load the function reference; break if null
            fb += LocalGet(typeDataParam)
            fb += StructGet(genTypeID.typeData, isJSClassInstance)
            fb += BrOnNull(isJSClassInstanceIsNull)

            // Call the function
            fb += CallRef(genTypeID.isJSClassInstanceFuncType)
            fb += Return
          }
          fb += Drop // drop `value` which was left on the stack

          // throw new TypeError("...")
          fb ++= ctx.stringPool.getConstantStringInstr(
            "Cannot call isInstance() on a Class representing a JS trait/object"
          )
          fb += Call(genFunctionID.makeTypeError)
          fb += Throw(genTagID.exception)
        }
      }
    ) { () =>
      // case _ =>

      // valueNonNull := as_non_null value; return false if null
      fb.block(RefType.any) { nonNullLabel =>
        fb += LocalGet(valueParam)
        fb += BrOnNonNull(nonNullLabel)
        fb += I32Const(0)
        fb += Return
      }
      fb += LocalSet(valueNonNullLocal)

      /* If `typeData` represents an ancestor of a hijacked classes, we have to
       * answer `true` if `valueNonNull` is a primitive instance of any of the
       * hijacked classes that ancestor class/interface. For example, for
       * `Comparable`, we have to answer `true` if `valueNonNull` is a primitive
       * boolean, number or string.
       *
       * To do that, we use `jsValueType` and `typeData.specialInstanceTypes`.
       *
       * We test whether `jsValueType(valueNonNull)` is in the set represented by
       * `specialInstanceTypes`. Since the latter is a bitset where the bit
       * indices correspond to the values returned by `jsValueType`, we have to
       * test whether
       *
       * ((1 << jsValueType(valueNonNull)) & specialInstanceTypes) != 0
       *
       * Since computing `jsValueType` is somewhat expensive, we first test
       * whether `specialInstanceTypes != 0` before calling `jsValueType`.
       *
       * There is a more elaborated concrete example of this algorithm in
       * `genInstanceTest`.
       */
      fb += LocalGet(typeDataParam)
      fb += StructGet(genTypeID.typeData, specialInstanceTypes)
      fb += LocalTee(specialInstanceTypesLocal)
      fb.ifThen() {
        if (hasJSInterop) {
          // With JS interop, perform a single JS call to jsValueType

          // Load (1 << jsValueType(valueNonNull))
          fb += I32Const(1)
          fb += LocalGet(valueNonNullLocal)
          fb += Call(genFunctionID.jsValueType)
          fb += I32Shl

          // if ((... & specialInstanceTypes) != 0)
          fb += LocalGet(specialInstanceTypesLocal)
          fb += I32And
          fb += I32Const(0)
          fb += I32Ne
          fb.ifThen() {
            // then return true
            fb += I32Const(1)
            fb += Return
          }
        } else {
          // Without JS interop, directly handle i31ref and strings

          val jsValueTypesAndTheirHeapTypes = List(
            (JSValueTypeNumber, HeapType.I31),
            (JSValueTypeString, HeapType(genTypeID.wasmString))
          )

          for ((jsValueType, heapType) <- jsValueTypesAndTheirHeapTypes) {
            // if (jsValueType is in the bitset) ...
            fb += LocalGet(specialInstanceTypesLocal)
            fb += I32Const(1 << jsValueType)
            fb += I32And
            fb.ifThen() {
              // ... && valueNonNull is of the corresponding heapType
              fb += LocalGet(valueNonNullLocal)
              fb += RefTest(RefType(heapType))
              fb.ifThen() {
                // then return true
                fb += I32Const(1)
                fb += Return
              }
            }
          }
        }
      }

      // Get the vtable and delegate to isAssignableFrom

      // Load typeData
      fb += LocalGet(typeDataParam)

      // Load the vtable; return false if it is not one of our object
      fb.block(objectRefType) { ourObjectLabel =>
        // Try cast to jl.Object
        fb += LocalGet(valueNonNullLocal)
        fb += BrOnCast(ourObjectLabel, RefType.any, objectRefType)

        // on cast fail, return false
        fb += I32Const(0)
        fb += Return
      }
      fb += ctx.getVTableInstr(genTypeID.ObjectStruct)

      // Call isAssignableFrom
      fb += Call(genFunctionID.isAssignableFrom)
    }

    fb.buildAndAddToModule()
  }

  /** `isAssignableFrom: (ref typeData), (ref typeData) -> i32` (a boolean).
   *
   *  Specified by `java.lang.Class.isAssignableFrom(Class)`.
   */
  private def genIsAssignableFrom()(implicit ctx: WasmContext): Unit = {
    import genFieldID.typeData._

    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.isAssignableFrom)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val fromTypeDataParam = fb.addParam("fromTypeData", typeDataType)
    fb.setResultType(Int32)

    val fromAncestorsLocal = fb.addLocal("fromAncestors", RefType(genTypeID.typeDataArray))
    val lenLocal = fb.addLocal("len", Int32)
    val iLocal = fb.addLocal("i", Int32)

    // if (fromTypeData eq typeData)
    fb += LocalGet(fromTypeDataParam)
    fb += LocalGet(typeDataParam)
    fb += RefEq
    fb.ifThen() {
      // then return true
      fb += I32Const(1)
      fb += Return
    }

    // "Tail call" loop for diving into array component types
    fb.loop(Int32) { loopForArrayLabel =>
      // switch (typeData.kind)
      fb.switch(Int32) { () =>
        // typeData.kind
        fb += LocalGet(typeDataParam)
        fb += StructGet(genTypeID.typeData, kind)
      }(
        // case anyPrimitiveKind => return false
        (KindVoid to KindLastPrimitive).toList -> { () =>
          fb += I32Const(0)
        },
        // case KindArray => check that from is an array, recurse into component types
        List(KindArray) -> { () =>
          fb.block() { fromComponentTypeIsNullLabel =>
            // fromTypeData := fromTypeData.componentType; jump out if null
            fb += LocalGet(fromTypeDataParam)
            fb += StructGet(genTypeID.typeData, componentType)
            fb += BrOnNull(fromComponentTypeIsNullLabel)
            fb += LocalSet(fromTypeDataParam)

            // typeData := ref.as_non_null typeData.componentType (OK because KindArray)
            fb += LocalGet(typeDataParam)
            fb += StructGet(genTypeID.typeData, componentType)
            fb += RefAsNonNull
            fb += LocalSet(typeDataParam)

            // loop back ("tail call")
            fb += Br(loopForArrayLabel)
          }

          // return false
          fb += I32Const(0)
        },
        // case KindObject => return (fromTypeData.kind > KindLastPrimitive)
        List(KindObject) -> { () =>
          fb += LocalGet(fromTypeDataParam)
          fb += StructGet(genTypeID.typeData, kind)
          fb += I32Const(KindLastPrimitive)
          fb += I32GtU
        }
      ) { () =>
        // All other cases: test whether `fromTypeData.strictAncestors` contains `typeData`

        fb.block() { fromAncestorsIsNullLabel =>
          // fromAncestors := fromTypeData.strictAncestors; go to fromAncestorsIsNull if null
          fb += LocalGet(fromTypeDataParam)
          fb += StructGet(genTypeID.typeData, strictAncestors)
          fb += BrOnNull(fromAncestorsIsNullLabel)
          fb += LocalTee(fromAncestorsLocal)

          // if fromAncestors contains typeData, return true

          // len := fromAncestors.length
          fb += ArrayLen
          fb += LocalSet(lenLocal)

          // i := 0
          fb += I32Const(0)
          fb += LocalSet(iLocal)

          // while (i != len)
          fb.whileLoop() {
            fb += LocalGet(iLocal)
            fb += LocalGet(lenLocal)
            fb += I32Ne
          } {
            // if (fromAncestors[i] eq typeData)
            fb += LocalGet(fromAncestorsLocal)
            fb += LocalGet(iLocal)
            fb += ArrayGet(genTypeID.typeDataArray)
            fb += LocalGet(typeDataParam)
            fb += RefEq
            fb.ifThen() {
              // then return true
              fb += I32Const(1)
              fb += Return
            }

            // i := i + 1
            fb += LocalGet(iLocal)
            fb += I32Const(1)
            fb += I32Add
            fb += LocalSet(iLocal)
          }
        }

        // from.strictAncestors is null or does not contain typeData
        // return false
        fb += I32Const(0)
      }
    }

    fb.buildAndAddToModule()
  }

  /** `cast: (ref jlClass), anyref -> anyref`.
   *
   *  Casts the given value to the given type; subject to undefined behaviors.
   *
   *  This is the underlying func for the `Class_cast` operation.
   */
  private def genCast()(implicit ctx: WasmContext): Unit = {
    assert(semantics.asInstanceOfs != CheckedBehavior.Unchecked)

    val fb = newFunctionBuilder(genFunctionID.cast)
    val jlClassParam = fb.addParam("jlClass", RefType(genTypeID.ClassStruct))
    val valueParam = fb.addParam("value", RefType.anyref)
    fb.setResultType(RefType.anyref)

    val typeDataLocal = fb.addLocal("typeData", RefType(genTypeID.typeData))

    fb.block() { successLabel =>
      // typeData := jlClass.data, leave it on the stack for the kind test
      fb += LocalGet(jlClassParam)
      fb += StructGet(genTypeID.ClassStruct, genFieldID.classData)
      fb += LocalTee(typeDataLocal)

      // If typeData.kind >= KindJSType, succeed
      fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
      fb += I32Const(KindJSType)
      fb += I32GeU
      fb += BrIf(successLabel)

      // If value is null, succeed
      fb += LocalGet(valueParam)
      fb += RefIsNull // consumes `value`, unlike `BrOnNull` which would leave it on the stack
      fb += BrIf(successLabel)

      // If isInstance(typeData, value), succeed
      fb += LocalGet(typeDataLocal)
      fb += LocalGet(valueParam)
      fb += Call(genFunctionID.isInstance)
      fb += BrIf(successLabel)

      // Otherwise, it is a CCE
      fb += LocalGet(valueParam)
      fb += LocalGet(typeDataLocal)
      fb += Call(genFunctionID.classCastException)
      fb += Unreachable // for clarity; technically redundant since the stacks align
    }

    fb += LocalGet(valueParam)

    fb.buildAndAddToModule()
  }

  /** `getComponentType: (ref jlClass) -> (ref null jlClass)`.
   *
   *  This is the underlying func for the `Class_componentType` operation.
   */
  private def genGetComponentType()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.getComponentType)
    val jlClassParam = fb.addParam("jlClass", RefType(genTypeID.ClassStruct))
    fb.setResultType(RefType.nullable(HeapType(genTypeID.ClassStruct, exact = useCustomDescriptors)))

    fb.block() { nullResultLabel =>
      // Try and extract non-null component type data
      fb += LocalGet(jlClassParam)
      fb += StructGet(genTypeID.ClassStruct, genFieldID.classData)
      fb += StructGet(genTypeID.typeData, genFieldID.typeData.componentType)
      fb += BrOnNull(nullResultLabel)
      // Get the corresponding classOf
      fb += Call(genFunctionID.getClassOf)
      fb += Return
    } // end block nullResultLabel
    fb += RefNull(HeapType.None)

    fb.buildAndAddToModule()
  }

  /** `getSuperClass: (ref jlClass) -> (ref null jlClass)`.
   *
   *  This is the underlying func for the `Class_superClass` operation.
   */
  private def genGetSuperClass()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.getSuperClass)
    val jlClassParam = fb.addParam("jlClass", RefType(genTypeID.ClassStruct))
    fb.setResultType(RefType.nullable(HeapType(genTypeID.ClassStruct, exact = useCustomDescriptors)))

    val typeDataLocal = fb.addLocal("typeData", RefType(genTypeID.typeData))
    val kindLocal = fb.addLocal("kind", Int32)

    // typeData := jlClass.data
    fb += LocalGet(jlClassParam)
    fb += StructGet(genTypeID.ClassStruct, genFieldID.classData)
    fb += LocalTee(typeDataLocal)

    // kind := typeData.kind
    fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
    fb += LocalTee(kindLocal)

    /* There are 3 cases that yield non-null results:
     *
     * - Scala classes that are not jl.Object (KindObject < kind <= KindClass)
     * - JS types with a superClass (kind == KindJSTypeWithSuperClass)
     * - Array classes (kind == KindArray)
     *
     * Note that KindArray < KindObject, and KindJSTypeWithSuperClass > KindObject,
     * so we dispatch these two kinds in the two branches of an
     * `if kind > KindObject` first.
     */

    // if kind > KindObject
    fb += I32Const(KindObject)
    fb += I32GtU
    fb.ifThenElse(RefType(genTypeID.typeData)) {
      // then, we may have to load the superClass from the strictAncestors array
      fb.block() { loadSuperClassFromStrictAncestorsLabel =>
        // if kind <= KindClass, then yes
        fb += LocalGet(kindLocal)
        fb += I32Const(KindClass)
        fb += I32LeU
        fb += BrIf(loadSuperClassFromStrictAncestorsLabel)

        // if kind == KindJSTypeWithSuperClass, then yes
        fb += LocalGet(kindLocal)
        fb += I32Const(KindJSTypeWithSuperClass)
        fb += I32Eq
        fb += BrIf(loadSuperClassFromStrictAncestorsLabel)

        // otherwise, there is no superClass
        fb += RefNull(HeapType.None)
        fb += Return
      }

      // load the superClass from the strictAncestors array
      fb += LocalGet(typeDataLocal)
      fb += StructGet(genTypeID.typeData, genFieldID.typeData.strictAncestors)
      fb += I32Const(0)
      fb += ArrayGet(genTypeID.typeDataArray)
    } {
      // else, it might be an Array class

      // if kind != KindArray
      fb += LocalGet(kindLocal)
      fb += I32Const(KindArray)
      fb += I32Ne
      fb.ifThen() {
        // then return null
        fb += RefNull(HeapType.None)
        fb += Return
      }

      // otherwise, load the typeData of jl.Object
      fb += GlobalGet(genGlobalID.forVTable(ClassRef(ObjectClass)))
    }

    // Load the jl.Class from the typeData
    fb += Call(genFunctionID.getClassOf)

    fb.buildAndAddToModule()
  }

  /** `newArray: (ref jlClass), i32 -> (ref jlObject)`.
   *
   *  This is the underlying func for the `Class_newArray` operation.
   */
  private def genNewArray()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)
    val arrayTypeDataType = RefType(genTypeID.ObjectVTable)

    val fb = newFunctionBuilder(genFunctionID.newArray)
    val jlClassParam = fb.addParam("jlClass", RefType(genTypeID.ClassStruct))
    val lengthParam = fb.addParam("length", Int32)
    fb.setResultType(RefType(genTypeID.ObjectStruct))

    val componentTypeDataLocal = fb.addLocal("componentTypeData", RefType(genTypeID.typeData))

    // Check negative array size
    if (semantics.negativeArraySizes != CheckedBehavior.Unchecked) {
      fb += LocalGet(lengthParam)
      fb += I32Const(0)
      fb += I32LtS
      fb.ifThen() {
        fb += LocalGet(lengthParam)
        fb += Call(genFunctionID.throwNegativeArraySizeException)
        fb += Unreachable
      }
    }

    // componentTypeData := jlClass.data
    fb += LocalGet(jlClassParam)
    fb += StructGet(genTypeID.ClassStruct, genFieldID.classData)
    fb += LocalSet(componentTypeDataLocal)

    // switch (componentTypeData.kind)
    fb.switch(RefType(genTypeID.ObjectStruct)) { () =>
      // scrutinee
      fb += LocalGet(componentTypeDataLocal)
      fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
    }(
      // case KindPrim (or KindObject) => array.new_default underlyingPrimArray; struct.new PrimArray/ObjectArray
      (primRefsWithKinds :+ (ObjectRef, KindObject)).map { case (baseRef, kind) =>
        List(kind) -> { () =>
          if (baseRef == VoidRef) {
            // throw IllegalArgumentException for VoidRef
            genNewScalaClass(fb, IllegalArgumentExceptionClass, NoArgConstructorName) {
              // no argument
            }
            fb += ExternConvertAny
            fb += Throw(genTagID.exception)
          } else {
            val arrayTypeRef = ArrayTypeRef(baseRef, 1)
            genStructNewWithVTable(fb, genTypeID.forArrayClass(arrayTypeRef)) {
              fb += GlobalGet(genGlobalID.forArrayVTable(baseRef))
            } {
              if (!hasJSInterop)
                fb += I32Const(0) // idHashCode
              fb += LocalGet(lengthParam)
              fb += ArrayNewDefault(genTypeID.underlyingOf(arrayTypeRef))
            }
          }
          () // required for correct type inference
        }
      }: _*
    ) { () =>
      // case _ => array.new_default anyrefArray; struct.new ObjectArray
      val arrayTypeRef = ArrayTypeRef(ClassRef(ObjectClass), 1)

      genStructNewWithVTable(fb, genTypeID.forArrayClass(arrayTypeRef)) {
        fb += LocalGet(componentTypeDataLocal)
        fb += I32Const(1)
        fb += Call(genFunctionID.specificArrayTypeData)
      } {
        if (!hasJSInterop)
          fb += I32Const(0) // idHashCode
        fb += LocalGet(lengthParam)
        fb += ArrayNewDefault(genTypeID.underlyingOf(arrayTypeRef))
      }
    }

    fb.buildAndAddToModule()
  }

  /** `anyGetClass: (ref any) -> (ref null jlClass)`.
   *
   *  This is the implementation of `value.getClass()` when `value` can be an instance of a hijacked
   *  class, i.e., a primitive.
   *
   *  For `number`s, the result is based on the actual value, as specified by
   *  [[https://www.scala-js.org/doc/semantics.html#getclass]].
   */
  private def genAnyGetClass()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.anyGetClass)
    val valueParam = fb.addParam("value", RefType.any)
    fb.setResultType(RefType.nullable(HeapType(genTypeID.ClassStruct, exact = useCustomDescriptors)))

    fb.block() { typeDataIsNullLabel =>
      fb += LocalGet(valueParam)
      fb += Call(genFunctionID.anyGetTypeData)
      fb += BrOnNull(typeDataIsNullLabel)
      fb += ReturnCall(genFunctionID.getClassOf)
    }
    fb += RefNull(HeapType.None)

    fb.buildAndAddToModule()
  }

  /** `anyGetClassName: (ref any) -> (ref extern)` (a string).
   *
   *  This is the implementation of `value.getClass().getName()`, which comes
   *  to the backend as the `ObjectClassName` intrinsic.
   */
  private def genAnyGetClassName()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.anyGetClassName)
    val valueParam = fb.addParam("value", RefType.any)
    fb.setResultType(stringType)

    if (semantics.nullPointers == CheckedBehavior.Unchecked) {
      fb += LocalGet(valueParam)
      fb += Call(genFunctionID.anyGetTypeData)
      fb += RefAsNonNull // NPE for null.getName()
      fb += ReturnCall(genFunctionID.typeDataName)
    } else {
      fb.block() { npeLabel =>
        fb += LocalGet(valueParam)
        fb += Call(genFunctionID.anyGetTypeData)
        fb += BrOnNull(npeLabel) // NPE for null.getName()
        fb += ReturnCall(genFunctionID.typeDataName)
      }
      fb += Call(genFunctionID.throwNullPointerException)
      fb += Unreachable
    }

    fb.buildAndAddToModule()
  }

  /** `intGetTypeData: i32 -> (ref typeData)`.
   *
   *  Common code for `anyGetTypeData` when the value is an i32.
   *
   *  The result is based on the actual value, as specified by
   *  [[https://www.scala-js.org/doc/semantics.html#getclass]].
   */
  private def genIntGetTypeData()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.intGetTypeData)
    val valueParam = fb.addParam("value", Int32)
    fb.setResultType(typeDataType)

    def getHijackedClassTypeDataInstr(className: ClassName): Instr =
      GlobalGet(genGlobalID.forVTable(className))

    // if value.toByte.toInt == value
    fb += LocalGet(valueParam)
    fb += I32Extend8S
    fb += LocalGet(valueParam)
    fb += I32Eq
    fb.ifThenElse(typeDataType) {
      // then it is a Byte
      fb += getHijackedClassTypeDataInstr(BoxedByteClass)
    } {
      // else, if value.toShort.toInt == value
      fb += LocalGet(valueParam)
      fb += I32Extend16S
      fb += LocalGet(valueParam)
      fb += I32Eq
      fb.ifThenElse(typeDataType) {
        // then it is a Short
        fb += getHijackedClassTypeDataInstr(BoxedShortClass)
      } {
        // else, it is an Integer
        fb += getHijackedClassTypeDataInstr(BoxedIntegerClass)
      }
    }

    fb.buildAndAddToModule()
  }

  /** `doubleGetTypeData: f64 -> (ref typeData)`.
   *
   *  Common code for `anyGetTypeData` when the value is an f64.
   *
   *  The result is based on the actual value, as specified by
   *  [[https://www.scala-js.org/doc/semantics.html#getclass]].
   */
  private def genDoubleGetTypeData()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.doubleGetTypeData)
    val valueParam = fb.addParam("value", Float64)
    fb.setResultType(typeDataType)

    val intValueLocal = fb.addLocal("intValue", Int32)

    def getHijackedClassTypeDataInstr(className: ClassName): Instr =
      GlobalGet(genGlobalID.forVTable(className))

    // intValue := value.toInt
    fb += LocalGet(valueParam)
    fb += I32TruncSatF64S
    fb += LocalTee(intValueLocal)

    // if same(intValue.toDouble, value) -- same bit pattern to avoid +0.0 == -0.0
    fb += F64ConvertI32S
    fb += I64ReinterpretF64
    fb += LocalGet(valueParam)
    fb += I64ReinterpretF64
    fb += I64Eq
    fb.ifThenElse(typeDataType) {
      // then it is a Byte, a Short, or an Integer
      fb += LocalGet(intValueLocal)
      fb += Call(genFunctionID.intGetTypeData)
    } {
      // else, it is a Float or a Double

      // if value.toFloat.toDouble == value
      fb += LocalGet(valueParam)
      fb += F32DemoteF64
      fb += F64PromoteF32
      fb += LocalGet(valueParam)
      fb += F64Eq
      fb.ifThenElse(typeDataType) {
        // then it is a Float
        fb += getHijackedClassTypeDataInstr(BoxedFloatClass)
      } {
        // else, if it is NaN
        fb += LocalGet(valueParam)
        fb += LocalGet(valueParam)
        fb += F64Ne
        fb.ifThenElse(typeDataType) {
          // then it is a Float
          fb += getHijackedClassTypeDataInstr(BoxedFloatClass)
        } {
          // else, it is a Double
          fb += getHijackedClassTypeDataInstr(BoxedDoubleClass)
        }
      }
    }

    fb.buildAndAddToModule()
  }

  /** `anyGetTypeData: (ref any) -> (ref null typeData)`.
   *
   *  Common code between `anyGetClass` and `anyGetClassName`.
   */
  private def genAnyGetTypeData()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)
    val ourObjectType = RefType(genTypeID.ObjectStruct)

    val fb = newFunctionBuilder(genFunctionID.anyGetTypeData)
    val valueParam = fb.addParam("value", RefType.any)
    fb.setResultType(RefType.nullable(genTypeID.typeData))

    def getHijackedClassTypeDataInstr(className: ClassName): Instr =
      GlobalGet(genGlobalID.forVTable(className))

    fb.block(ourObjectType) { ourObjectLabel =>
      fb.block(RefType.i31) { i31Label =>
        // if value is our object or an i31, jump to the corresponding label
        fb += LocalGet(valueParam)
        fb += BrOnCast(ourObjectLabel, RefType.any, ourObjectType)
        fb += BrOnCast(i31Label, RefType.any, RefType.i31)

        if (hasJSInterop) {
          // Perform a single call to jsValueType, then dispatch

          // switch(jsValueType(value)) { ... }
          fb.switch() { () =>
            // scrutinee
            fb += LocalGet(valueParam)
            fb += Call(genFunctionID.jsValueType)
          }(
            // case JSValueTypeFalse, JSValueTypeTrue => typeDataOf[jl.Boolean]
            List(JSValueTypeFalse, JSValueTypeTrue) -> { () =>
              fb += getHijackedClassTypeDataInstr(BoxedBooleanClass)
              fb += Return
            },
            // case JSValueTypeString => typeDataOf[jl.String]
            List(JSValueTypeString) -> { () =>
              fb += getHijackedClassTypeDataInstr(BoxedStringClass)
              fb += Return
            },
            // case JSValueTypeNumber => doubleGetTypeData(unboxDouble(value))
            List(JSValueTypeNumber) -> { () =>
              fb += LocalGet(valueParam)
              fb += Call(genFunctionID.unbox(DoubleRef))
              fb += ReturnCall(genFunctionID.doubleGetTypeData)
            },
            // case JSValueTypeUndefined => typeDataOf[jl.Void]
            List(JSValueTypeUndefined) -> { () =>
              fb += getHijackedClassTypeDataInstr(BoxedUnitClass)
              fb += Return
            }
          ) { () =>
            // case _ (JSValueTypeOther) => return null
            fb += RefNull(HeapType.None)
            fb += Return
          }

          fb += Unreachable
        } else { // !hasJSInterop
          // Without JS interop, we only have wasmString and undefined left

          fb += RefTest(RefType(genTypeID.wasmString))
          fb.ifThenElse(typeDataType) {
            fb += getHijackedClassTypeDataInstr(BoxedStringClass)
          } {
            fb += getHijackedClassTypeDataInstr(BoxedUnitClass)
          }
          fb += Return
        }
      } // end of block i31label

      fb += I31GetS
      fb += ReturnCall(genFunctionID.intGetTypeData)
    } // end of block ourObjectLabel

    /* Now we have one of our objects. Normally we only have to get the vtable.
     * There is one exception when we don't have JS interop. If the value is a
     * jl.Double, we must extract its value and decide what number type data to
     * return based on the numeric value.
     */
    if (hasJSInterop) {
      fb += ctx.getVTableInstr(genTypeID.ObjectStruct)
    } else {
      fb.block(FunctionType(List(ourObjectType), List(ourObjectType))) { notADoubleBoxLabel =>
        val doubleBoxTypeID = genTypeID.forClass(BoxedDoubleClass)
        fb += BrOnCastFail(notADoubleBoxLabel, ourObjectType, RefType(doubleBoxTypeID))
        fb += StructGet(doubleBoxTypeID, genFieldID.boxValue)
        fb += ReturnCall(genFunctionID.doubleGetTypeData)
      } // end of block notADoubleBoxLabel
      fb += ctx.getVTableInstr(genTypeID.ObjectStruct)
    }

    fb.buildAndAddToModule()
  }

  /** `identityHashCode`: `anyref -> i32`.
   *
   *  This is the implementation of `IdentityHashCode`. It is also used to compute the `hashCode()`
   *  of primitive values when dispatch is required (i.e., when the receiver type is not known to be
   *  a specific primitive or hijacked class), so it must be consistent with the implementations of
   *  `hashCode()` in hijacked classes.
   *
   *  For `String` and `Double`, we actually call the hijacked class methods, as they are a bit
   *  involved. For `Boolean` and `Void`, we hard-code a copy here.
   */
  private def genIdentityHashCode()(implicit ctx: WasmContext): Unit = {
    // A global exclusively used by this function
    ctx.addGlobal(
      Global(
        genGlobalID.lastIDHashCode,
        OriginalName(genGlobalID.lastIDHashCode.toString()),
        isMutable = true,
        Int32,
        Expr(List(I32Const(0)))
      )
    )

    if (hasJSInterop)
      genIdentityHashCodeWithJS()
    else
      genIdentityHashCodeWithoutJS()
  }

  private def genIdentityHashCodeWithJS()(implicit ctx: WasmContext): Unit = {
    import MemberNamespace.Public

    val fb = newFunctionBuilder(genFunctionID.identityHashCode)
    val objParam = fb.addParam("obj", RefType.anyref)
    fb.setResultType(Int32)

    val objNonNullLocal = fb.addLocal("objNonNull", RefType.any)
    val resultLocal = fb.addLocal("result", Int32)

    // If `obj` is `null`, return 0 (by spec)
    fb.block(RefType.any) { nonNullLabel =>
      fb += LocalGet(objParam)
      fb += BrOnNonNull(nonNullLabel)
      fb += I32Const(0)
      fb += Return
    }
    fb += LocalTee(objNonNullLocal)

    // If `obj` is one of our objects, skip all the jsValueType tests
    fb += RefTest(RefType(genTypeID.ObjectStruct))
    fb += I32Eqz
    fb.ifThen() {
      fb.switch() { () =>
        fb += LocalGet(objNonNullLocal)
        fb += Call(genFunctionID.jsValueType)
      }(
        List(JSValueTypeFalse) -> { () =>
          fb += I32Const(1237) // specified by jl.Boolean.hashCode()
          fb += Return
        },
        List(JSValueTypeTrue) -> { () =>
          fb += I32Const(1231) // specified by jl.Boolean.hashCode()
          fb += Return
        },
        List(JSValueTypeString) -> { () =>
          fb += LocalGet(objNonNullLocal)
          fb += ExternConvertAny
          fb += Call(
            genFunctionID.forMethod(Public, BoxedStringClass, hashCodeMethodName)
          )
          fb += Return
        },
        List(JSValueTypeNumber) -> { () =>
          fb += LocalGet(objNonNullLocal)
          fb += Call(genFunctionID.unbox(DoubleRef))
          fb += Call(
            genFunctionID.forMethod(Public, BoxedDoubleClass, hashCodeMethodName)
          )
          fb += Return
        },
        List(JSValueTypeUndefined) -> { () =>
          fb += I32Const(0) // specified by jl.Void.hashCode(), Scala.js only
          fb += Return
        },
        List(JSValueTypeBigInt) -> { () =>
          fb += LocalGet(objNonNullLocal)
          fb += Call(genFunctionID.bigintHashCode)
          fb += Return
        },
        List(JSValueTypeSymbol) -> { () =>
          fb.block() { descriptionIsNullLabel =>
            fb += LocalGet(objNonNullLocal)
            fb += Call(genFunctionID.symbolDescription)
            fb += BrOnNull(descriptionIsNullLabel)
            fb += Call(
              genFunctionID.forMethod(Public, BoxedStringClass, hashCodeMethodName)
            )
            fb += Return
          }
          fb += I32Const(0)
          fb += Return
        }
      ) { () =>
        // JSValueTypeOther -- fall through to using idHashCodeMap
        ()
      }
    }

    // If we get here, use the idHashCodeMap

    // Read the existing idHashCode, if one exists
    fb += GlobalGet(genGlobalID.idHashCodeMap)
    fb += LocalGet(objNonNullLocal)
    fb += Call(genFunctionID.idHashCodeGet)
    fb += LocalTee(resultLocal)

    // If it is 0, there was no recorded idHashCode yet; allocate a new one
    fb += I32Eqz
    fb.ifThen() {
      // Allocate a new idHashCode
      fb += GlobalGet(genGlobalID.lastIDHashCode)
      fb += I32Const(1)
      fb += I32Add
      fb += LocalTee(resultLocal)
      fb += GlobalSet(genGlobalID.lastIDHashCode)

      // Store it for next time
      fb += GlobalGet(genGlobalID.idHashCodeMap)
      fb += LocalGet(objNonNullLocal)
      fb += LocalGet(resultLocal)
      fb += Call(genFunctionID.idHashCodeSet)
    }

    fb += LocalGet(resultLocal)

    fb.buildAndAddToModule()
  }

  private def genIdentityHashCodeWithoutJS()(implicit ctx: WasmContext): Unit = {
    import MemberNamespace.Public

    val fb = newFunctionBuilder(genFunctionID.identityHashCode)
    val objParam = fb.addParam("obj", RefType.anyref)
    fb.setResultType(Int32)

    val doubleBoxTypeID = genTypeID.forClass(BoxedDoubleClass)

    val jlObjectType = RefType(genTypeID.ObjectStruct)
    val wasmStringType = RefType(genTypeID.wasmString)

    val resultLocal = fb.addLocal("result", Int32)

    fb.block(jlObjectType) { ourObjLabel =>
      fb.block(wasmStringType) { stringLabel =>
        fb.block(RefType.i31) { i31Label =>
          fb += LocalGet(objParam)
          fb += BrOnCast(ourObjLabel, RefType.anyref, jlObjectType)
          fb += BrOnCast(i31Label, RefType.anyref, RefType.i31)
          fb += BrOnCast(stringLabel, RefType.anyref, wasmStringType)

          // The only things left here are `null` and `undefined`; return 0 (by spec)
          fb += I32Const(0)
          fb += Return
        }

        // i31
        fb += I31GetS
        fb += Return
      }

      // string
      fb += ReturnCall(genFunctionID.forMethod(Public, BoxedStringClass, hashCodeMethodName))
    }

    // our object - use the idHashCode field

    val jlObjectLocal = fb.addLocal("jlObj", jlObjectType)
    fb += LocalTee(jlObjectLocal)
    fb += StructGet(genTypeID.ObjectStruct, genFieldID.objStruct.idHashCode)
    fb += LocalTee(resultLocal)
    fb += I32Eqz
    fb.ifThen() {
      fb.block() { computedNewHashCode =>
        // If it's a DoubleBox, we must use jl.Double.hashCode
        fb.block(jlObjectType) { notADoubleBox =>
          fb += LocalGet(jlObjectLocal)
          fb += BrOnCastFail(notADoubleBox, jlObjectType, RefType(doubleBoxTypeID))

          // Get the underlying value to call jl.Double.hashCode
          fb += StructGet(doubleBoxTypeID, genFieldID.boxValue)
          fb += Call(genFunctionID.forMethod(Public, BoxedDoubleClass, hashCodeMethodName))
          fb += LocalSet(resultLocal)
          fb += Br(computedNewHashCode)
        }
        fb += Drop

        // Generate a new ID hash code
        fb += GlobalGet(genGlobalID.lastIDHashCode)
        fb += I32Const(1)
        fb += I32Add
        fb += LocalTee(resultLocal)
        fb += GlobalSet(genGlobalID.lastIDHashCode)
      }

      // store it for next time
      fb += LocalGet(jlObjectLocal)
      fb += LocalGet(resultLocal)
      fb += StructSet(genTypeID.ObjectStruct, genFieldID.objStruct.idHashCode)
    }

    fb += LocalGet(resultLocal)

    fb.buildAndAddToModule()
  }

  /** Search for a reflective proxy function with the given `methodId` in the `reflectiveProxies`
   *  field in `typeData` and returns the corresponding function reference.
   *
   *  `searchReflectiveProxy`: [typeData, i32] -> [(ref func)]
   */
  private def genSearchReflectiveProxy()(implicit ctx: WasmContext): Unit = {
    import genFieldID.typeData._

    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.searchReflectiveProxy)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val methodIDParam = fb.addParam("methodID", Int32)
    fb.setResultType(RefType(HeapType.Func))

    val reflectiveProxies =
      fb.addLocal("reflectiveProxies", Types.RefType(genTypeID.reflectiveProxies))
    val startLocal = fb.addLocal("start", Types.Int32)
    val endLocal = fb.addLocal("end", Types.Int32)
    val midLocal = fb.addLocal("mid", Types.Int32)
    val entryLocal = fb.addLocal("entry", Types.RefType(genTypeID.reflectiveProxy))

    /* This function implements a binary search. Unlike the typical binary search,
     * it does not stop early if it happens to exactly hit the target ID.
     * Instead, it systematically reduces the search range until it contains at
     * most one element. At that point, it checks whether it is the ID we are
     * looking for.
     *
     * We do this in the name of predictability, in order to avoid performance
     * cliffs. It avoids the scenario where a codebase happens to be fast
     * because a particular reflective call resolves in Θ(1), but where adding
     * or removing something completely unrelated somewhere else in the
     * codebase pushes it to a different slot where it resolves in Θ(log n).
     *
     * This function is therefore intentionally Θ(log n), not merely O(log n).
     */

    fb += LocalGet(typeDataParam)
    fb += StructGet(genTypeID.typeData, genFieldID.typeData.reflectiveProxies)
    fb += LocalTee(reflectiveProxies)

    // end := reflectiveProxies.length
    fb += ArrayLen
    fb += LocalSet(endLocal)

    // start := 0
    fb += I32Const(0)
    fb += LocalSet(startLocal)

    // while (start + 1 < end)
    fb.whileLoop() {
      fb += LocalGet(startLocal)
      fb += I32Const(1)
      fb += I32Add
      fb += LocalGet(endLocal)
      fb += I32LtU
    } {
      // mid := (start + end) >>> 1
      fb += LocalGet(startLocal)
      fb += LocalGet(endLocal)
      fb += I32Add
      fb += I32Const(1)
      fb += I32ShrU
      fb += LocalSet(midLocal)

      // if (methodID < reflectiveProxies[mid].methodID)
      fb += LocalGet(methodIDParam)
      fb += LocalGet(reflectiveProxies)
      fb += LocalGet(midLocal)
      fb += ArrayGet(genTypeID.reflectiveProxies)
      fb += StructGet(genTypeID.reflectiveProxy, genFieldID.reflectiveProxy.methodID)
      fb += I32LtU
      fb.ifThenElse() {
        // then end := mid
        fb += LocalGet(midLocal)
        fb += LocalSet(endLocal)
      } {
        // else start := mid
        fb += LocalGet(midLocal)
        fb += LocalSet(startLocal)
      }
    }

    // if (start < end)
    fb += LocalGet(startLocal)
    fb += LocalGet(endLocal)
    fb += I32LtU
    fb.ifThen() {
      // entry := reflectiveProxies[start]
      fb += LocalGet(reflectiveProxies)
      fb += LocalGet(startLocal)
      fb += ArrayGet(genTypeID.reflectiveProxies)
      fb += LocalTee(entryLocal)

      // if (entry.methodID == methodID)
      fb += StructGet(genTypeID.reflectiveProxy, genFieldID.reflectiveProxy.methodID)
      fb += LocalGet(methodIDParam)
      fb += I32Eq
      fb.ifThen() {
        // return entry.funcRef
        fb += LocalGet(entryLocal)
        fb += StructGet(genTypeID.reflectiveProxy, genFieldID.reflectiveProxy.funcRef)
        fb += Return
      }
    }

    if (!hasJSInterop) {
      genNewScalaClass(fb, NoSuchMethodExceptionClass, StringArgConstructorName) {
        fb ++= ctx.stringPool.getConstantStringInstr("Method not found")
      }
      fb += ExternConvertAny
      fb += Throw(genTagID.exception)
    } else {
      // throw new TypeError("...")
      // Originally, exception is thrown from JS saying e.g. "obj2.z1__ is not a function"
      // TODO Improve the error message to include some information about the missing method
      fb ++= ctx.stringPool.getConstantStringInstr("Method not found")
      fb += Call(genFunctionID.makeTypeError)
      fb += Throw(genTagID.exception)
    }

    fb.buildAndAddToModule()
  }

  private def genArrayCloneFunctions()(implicit ctx: WasmContext): Unit = {
    for (baseRef <- arrayBaseRefs)
      genArrayCloneFunction(baseRef)
  }

  /** Generates the clone function for the array class with the given base. */
  private def genArrayCloneFunction(baseRef: NonArrayTypeRef)(implicit ctx: WasmContext): Unit = {
    val originalName = OriginalName("cloneArray." + charCodeForOriginalName(baseRef))

    val fb = newFunctionBuilder(genFunctionID.cloneArray(baseRef), originalName)
    val fromParam = fb.addParam("from", RefType(genTypeID.ObjectStruct))
    fb.setResultType(RefType(genTypeID.ObjectStruct))
    fb.setFunctionType(genTypeID.cloneFunctionType)

    val arrayTypeRef = ArrayTypeRef(baseRef, 1)

    val arrayStructTypeID = genTypeID.forArrayClass(arrayTypeRef)
    val arrayClassType = RefType(HeapType(arrayStructTypeID, exact = useCustomDescriptors))

    val underlyingArrayTypeID = genTypeID.underlyingOf(arrayTypeRef)
    val underlyingArrayType = RefType(underlyingArrayTypeID)

    val fromLocal = fb.addLocal("fromTyped", arrayClassType)
    val fromUnderlyingLocal = fb.addLocal("fromUnderlying", underlyingArrayType)
    val lengthLocal = fb.addLocal("length", Int32)
    val resultUnderlyingLocal = fb.addLocal("resultUnderlying", underlyingArrayType)

    // Cast down the from argument
    fb += LocalGet(fromParam)
    fb += RefCast(arrayClassType)
    fb += LocalTee(fromLocal)

    // Load the underlying array
    fb += StructGet(arrayStructTypeID, genFieldID.objStruct.arrayUnderlying)
    fb += LocalTee(fromUnderlyingLocal)

    // Make a copy of the underlying array
    fb += ArrayLen
    fb += LocalTee(lengthLocal)
    fb += ArrayNewDefault(underlyingArrayTypeID)
    fb += LocalTee(resultUnderlyingLocal) // also dest for array.copy
    fb += I32Const(0) // destOffset
    fb += LocalGet(fromUnderlyingLocal) // src
    fb += I32Const(0) // srcOffset
    fb += LocalGet(lengthLocal) // length
    fb += ArrayCopy(underlyingArrayTypeID, underlyingArrayTypeID)

    // Build the result arrayStruct
    genStructNewWithVTable(fb, arrayStructTypeID) {
      fb += LocalGet(fromLocal)
      fb += ctx.getVTableInstr(arrayStructTypeID)
    } {
      if (!hasJSInterop)
        fb += I32Const(0) // idHashCode
      fb += LocalGet(resultUnderlyingLocal)
    }

    fb.buildAndAddToModule()
  }

  private def genArrayCopyFunctions()(implicit ctx: WasmContext): Unit = {
    if (semantics.arrayIndexOutOfBounds != CheckedBehavior.Unchecked)
      genArrayCopyCheckBounds()

    if (semantics.arrayStores != CheckedBehavior.Unchecked)
      genSlowRefArrayCopy()

    for (baseRef <- arrayBaseRefs)
      genSpecializedArrayCopy(baseRef)

    genGenericArrayCopy()
  }

  /** `arrayCopyCheckBounds: [i32, i32, i32, i32, i32] -> []`.
   *
   *  Checks all the bounds for an `arrayCopy`. Arguments correspond to the
   *  arguments of the `arrayCopy`, where arrays are replaced by their lengths.
   */
  private def genArrayCopyCheckBounds()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.arrayCopyCheckBounds)
    val srcLengthParam = fb.addParam("srcLength", Int32)
    val srcPosParam = fb.addParam("srcPos", Int32)
    val destLengthParam = fb.addParam("destLength", Int32)
    val destPosParam = fb.addParam("destPos", Int32)
    val lengthParam = fb.addParam("length", Int32)

    fb.block() { failureLabel =>
      /* if (srcPos < 0) || (destPos < 0) || (length < 0), fail
       * we test all of those with a single branch as follows:
       * ((srcPos | destPos | length) & 0x80000000) != 0
       */
      fb += LocalGet(srcPosParam)
      fb += LocalGet(destPosParam)
      fb += I32Or
      fb += LocalGet(lengthParam)
      fb += I32Or
      fb += I32Const(0x80000000)
      fb += I32And
      fb += BrIf(failureLabel)

      // if srcPos > (srcLength - length), fail
      fb += LocalGet(srcPosParam)
      fb += LocalGet(srcLengthParam)
      fb += LocalGet(lengthParam)
      fb += I32Sub
      fb += I32GtS
      fb += BrIf(failureLabel)

      // if destPos > (destLength - length), fail
      fb += LocalGet(destPosParam)
      fb += LocalGet(destLengthParam)
      fb += LocalGet(lengthParam)
      fb += I32Sub
      fb += I32GtS
      fb += BrIf(failureLabel)

      // otherwise, succeed
      fb += Return
    }

    maybeWrapInUBE(fb, semantics.arrayIndexOutOfBounds) {
      genNewScalaClass(fb, ArrayIndexOutOfBoundsExceptionClass, StringArgConstructorName) {
        fb += RefNull(noStringHeapType)
      }
    }
    fb += ExternConvertAny
    fb += Throw(genTagID.exception)

    fb.buildAndAddToModule()
  }

  /** `slowRefArrayCopy: [ArrayObject, i32, ArrayObject, i32, i32] -> []`
   *
   *  Used when the type of the dest is not assignable from the type of the source.
   *  Performs an `arraySet` call for every element in order to detect
   *  `ArrayStoreException`s.
   *
   *  Bounds are already known to be valid.
   */
  private def genSlowRefArrayCopy()(implicit ctx: WasmContext): Unit = {
    val baseRef = ClassRef(ObjectClass)
    val arrayTypeRef = ArrayTypeRef(baseRef, 1)
    val arrayStructTypeID = genTypeID.forArrayClass(arrayTypeRef)
    val arrayClassType = RefType.nullable(arrayStructTypeID)
    val underlyingArrayTypeID = genTypeID.underlyingOf(arrayTypeRef)

    val fb = newFunctionBuilder(genFunctionID.slowRefArrayCopy)
    val srcParam = fb.addParam("src", arrayClassType)
    val srcPosParam = fb.addParam("srcPos", Int32)
    val destParam = fb.addParam("dest", arrayClassType)
    val destPosParam = fb.addParam("destPos", Int32)
    val lengthParam = fb.addParam("length", Int32)

    val srcUnderlyingLocal = fb.addLocal("srcUnderlying", RefType(underlyingArrayTypeID))
    val iLocal = fb.addLocal("i", Int32)

    // srcUnderlying := src.underlying
    fb += LocalGet(srcParam)
    fb += StructGet(arrayStructTypeID, genFieldID.objStruct.arrayUnderlying)
    fb += LocalSet(srcUnderlyingLocal)

    // i := 0
    fb += I32Const(0)
    fb += LocalSet(iLocal)

    // while i != length
    fb.whileLoop() {
      fb += LocalGet(iLocal)
      fb += LocalGet(lengthParam)
      fb += I32Ne
    } {
      // arraySet.O(dest, destPos + i, srcUnderlying(srcPos + i))

      fb += LocalGet(destParam)

      fb += LocalGet(destPosParam)
      fb += LocalGet(iLocal)
      fb += I32Add

      fb += LocalGet(srcUnderlyingLocal)
      fb += LocalGet(srcPosParam)
      fb += LocalGet(iLocal)
      fb += I32Add
      fb += ArrayGet(underlyingArrayTypeID)

      fb += Call(genFunctionID.arraySet(baseRef))

      // i := i + 1
      fb += LocalGet(iLocal)
      fb += I32Const(1)
      fb += I32Add
      fb += LocalSet(iLocal)
    }

    fb.buildAndAddToModule()
  }

  /** Generates a specialized arrayCopy for the array class with the given base. */
  private def genSpecializedArrayCopy(baseRef: NonArrayTypeRef)(implicit ctx: WasmContext): Unit = {
    val originalName = OriginalName("arrayCopy." + charCodeForOriginalName(baseRef))

    val arrayTypeRef = ArrayTypeRef(baseRef, 1)
    val arrayStructTypeID = genTypeID.forArrayClass(arrayTypeRef)
    val arrayClassType = RefType.nullable(arrayStructTypeID)
    val underlyingArrayTypeID = genTypeID.underlyingOf(arrayTypeRef)

    val fb = newFunctionBuilder(genFunctionID.specializedArrayCopy(arrayTypeRef), originalName)
    val srcParam = fb.addParam("src", arrayClassType)
    val srcPosParam = fb.addParam("srcPos", Int32)
    val destParam = fb.addParam("dest", arrayClassType)
    val destPosParam = fb.addParam("destPos", Int32)
    val lengthParam = fb.addParam("length", Int32)

    if (semantics.arrayIndexOutOfBounds != CheckedBehavior.Unchecked) {
      fb += LocalGet(srcParam)
      fb += StructGet(arrayStructTypeID, genFieldID.objStruct.arrayUnderlying)
      fb += ArrayLen
      fb += LocalGet(srcPosParam)
      fb += LocalGet(destParam)
      fb += StructGet(arrayStructTypeID, genFieldID.objStruct.arrayUnderlying)
      fb += ArrayLen
      fb += LocalGet(destPosParam)
      fb += LocalGet(lengthParam)
      fb += Call(genFunctionID.arrayCopyCheckBounds)
    }

    if (baseRef.isInstanceOf[ClassRef] && semantics.arrayStores != CheckedBehavior.Unchecked) {
      // if !isAssignableFrom(dest.vtable, src.vtable)
      fb += LocalGet(destParam)
      fb += ctx.getVTableInstr(arrayStructTypeID)
      fb += LocalGet(srcParam)
      fb += ctx.getVTableInstr(arrayStructTypeID)
      fb += Call(genFunctionID.isAssignableFrom) // contains a fast-path for `eq` vtables
      fb += I32Eqz
      fb.ifThen() {
        // then, delegate to the slow copy method
        fb += LocalGet(srcParam)
        fb += LocalGet(srcPosParam)
        fb += LocalGet(destParam)
        fb += LocalGet(destPosParam)
        fb += LocalGet(lengthParam)
        fb += ReturnCall(genFunctionID.slowRefArrayCopy)
      }
    }

    fb += LocalGet(destParam)
    fb += StructGet(arrayStructTypeID, genFieldID.objStruct.arrayUnderlying)
    fb += LocalGet(destPosParam)
    fb += LocalGet(srcParam)
    fb += StructGet(arrayStructTypeID, genFieldID.objStruct.arrayUnderlying)
    fb += LocalGet(srcPosParam)
    fb += LocalGet(lengthParam)
    fb += ArrayCopy(underlyingArrayTypeID, underlyingArrayTypeID)

    fb.buildAndAddToModule()
  }

  /** Generates the generic arrayCopy for an unknown array class. */
  private def genGenericArrayCopy()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.genericArrayCopy)
    val srcParam = fb.addParam("src", RefType.anyref)
    val srcPosParam = fb.addParam("srcPos", Int32)
    val destParam = fb.addParam("dest", RefType.anyref)
    val destPosParam = fb.addParam("destPos", Int32)
    val lengthParam = fb.addParam("length", Int32)

    val anyrefToAnyrefBlockType =
      fb.sigToBlockType(FunctionType(List(RefType.anyref), List(RefType.anyref)))

    // note: this block is never used for Unchecked arrayStores, but it does not hurt much
    fb.block(anyref) { mismatchLabel =>
      // Dispatch done based on the type of src
      fb += LocalGet(srcParam)

      for (baseRef <- arrayBaseRefs) {
        val arrayTypeRef = ArrayTypeRef(baseRef, 1)
        val arrayStructTypeID = genTypeID.forArrayClass(arrayTypeRef)
        val nonNullArrayClassType = RefType(arrayStructTypeID)

        fb.block(anyrefToAnyrefBlockType) { notThisArrayTypeLabel =>
          fb += BrOnCastFail(notThisArrayTypeLabel, RefType.anyref, nonNullArrayClassType)

          fb += LocalGet(srcPosParam)
          fb += LocalGet(destParam)
          if (semantics.arrayStores == CheckedBehavior.Unchecked)
            fb += RefCast(nonNullArrayClassType)
          else
            fb += BrOnCastFail(mismatchLabel, anyref, nonNullArrayClassType)
          fb += LocalGet(destPosParam)
          fb += LocalGet(lengthParam)

          fb += ReturnCall(genFunctionID.specializedArrayCopy(arrayTypeRef))
        }
      }
    }

    // Mismatch of array types, or either array was not an array
    if (semantics.arrayStores == CheckedBehavior.Unchecked) {
      fb += Unreachable // trap
    } else {
      maybeWrapInUBE(fb, semantics.arrayStores) {
        genNewScalaClass(fb, ArrayStoreExceptionClass, StringArgConstructorName) {
          fb += RefNull(noStringHeapType)
        }
      }
      fb += ExternConvertAny
      fb += Throw(genTagID.exception)
    }

    fb.buildAndAddToModule()
  }

  // --- Wasm-without-JS helper definitions ---

  private def genWasmWithoutJSHelpers()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val exceptionSig = FunctionType(List(RefType.externref), Nil)
    val typeID = ctx.moduleBuilder.functionTypeToTypeID(exceptionSig)
    ctx.moduleBuilder.addTag(
      Tag(
        genTagID.exception,
        OriginalName(genTagID.exception.toString()),
        typeID
      )
    )

    genUndefinedAndIsUndef()
    genNoJSStringHelpers()

    /* `Boolean`: `box` is generated by `genBoxBoolean`. Here we add `unbox`
     * and `typeTest`, which are imported from JS (`uZ` and `tZ`) for JS-Wasm.
     */
    genUnbox(genFunctionID.unbox(BooleanRef), BooleanType)
    genTestBoolean()

    /* `Byte`/`Short`: no `box` helper; boxing emits `ref.i31`.
     * `unbox` and `typeTest` are shared with JS-Wasm.
     */

    /* `Int`: `box`/`unbox` are generated by `genBoxInt`/`genUnboxInt`,
     * shared with JS-Wasm. Here we define `bIFallback` and `uIFallback`
     * for values that do not use the i31ref fast path, as well as `typeTest`.
     */
    genBox(genFunctionID.bIFallback, IntType)
    genUnboxIntFallback()
    genTestInteger()

    /* `Char`/`Long`: represented by box structs.
     * Boxing, unboxing and type tests are inlined at the call sites.
     */

    /* `Float`/`Double`: both use `DoubleBoxClass`, and emulate Scala.js number
     * semantics where boxed numeric values are tested by value.
     */
    for (primType <- List(FloatType, DoubleType))
      genBox(genFunctionID.box(primType.primRef), primType)
    genUnboxFloat()
    genUnboxDouble()
    genTestFloat()
    genTestDouble()

    genIs()

    val wasmArrayBaseRefs: List[PrimRef] = List(
      ByteRef,
      ShortRef,
      IntRef,
      LongRef,
      FloatRef,
      DoubleRef
    )
    for (baseRef <- wasmArrayBaseRefs) {
      genArrayToWasmArray(baseRef)
      genWasmArrayToArray(baseRef)
    }
  }

  private def genUndefinedAndIsUndef()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    ctx.addGlobal(
      Global(
        genGlobalID.undef,
        OriginalName(genGlobalID.undef.toString()),
        isMutable = false,
        RefType(genTypeID.undefined),
        Expr(List(StructNew(genTypeID.undefined)))
      )
    )

    val fb = newFunctionBuilder(genFunctionID.isUndef)
    val xParam = fb.addParam("x", RefType.anyref)
    fb.setResultType(Int32)
    fb += LocalGet(xParam)
    fb += RefTest(RefType(genTypeID.undefined))

    fb.buildAndAddToModule()
  }

  private def genNoJSStringHelpers()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    genStringLiteral()
    genStringConcat()
    genStringEquals()
    genCharCodeAt()
    genGetWholeChars()
    genCollapseString()
    genLtoa()
    genItoa()
    genHijackedValueToString()

    ctx.addGlobal(
      Global(
        genGlobalID.emptyStringArray,
        NoOriginalName,
        isMutable = false,
        RefType(genTypeID.wasmString),
        Expr(List(
          ArrayNewFixed(genTypeID.i16Array, 0),
          I32Const(0),
          RefNull(HeapType.None),
          StructNew(genTypeID.wasmString)
        ))
      )
    )
  }

  private def genStringLiteral()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.stringLiteral)
    val offsetParam = fb.addParam("offset", Int32)
    val sizeParam = fb.addParam("size", Int32)
    val stringIndexParam = fb.addParam("stringIndex", Int32)

    fb.setResultType(RefType(genTypeID.wasmString))

    val str = fb.addLocal("newString", RefType(genTypeID.wasmString))

    fb.block(RefType(genTypeID.wasmString)) { cacheHit =>
      fb += GlobalGet(genGlobalID.stringLiteralCache)
      fb += LocalGet(stringIndexParam)
      fb += ArrayGet(genTypeID.wasmStringArray)

      fb += BrOnNonNull(cacheHit)

      // cache miss, create a new string and cache it
      fb += LocalGet(offsetParam)
      fb += LocalGet(sizeParam)
      fb += ArrayNewData(genTypeID.i16Array, genDataID.string)
      fb += LocalGet(sizeParam)
      fb += RefNull(HeapType.None)
      fb += StructNew(genTypeID.wasmString)
      fb += LocalSet(str)

      fb += GlobalGet(genGlobalID.stringLiteralCache)
      fb += LocalGet(stringIndexParam)
      fb += LocalGet(str)
      fb += ArraySet(genTypeID.wasmStringArray)

      fb += LocalGet(str)
    }

    fb.buildAndAddToModule()
  }

  private def genStringConcat()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.wasmString.stringConcat)
    val str1 = fb.addParam("str1", stringType)
    val str2 = fb.addParam("str2", stringType)
    fb.setResultType(stringType)

    /* Lazily concatenate nodes instead of copying strings.
     * The node stores the right-hand string's chars and points to the left-hand
     * string in `left`. `getWholeChars`/`collapseString` flatten the chains
     * when character access is required.
     */

    fb += LocalGet(str2) // right
    fb += Call(genFunctionID.wasmString.getWholeChars)

    // The `length` is the sum of left and right.
    fb += LocalGet(str1)
    fb += StructGet(genTypeID.wasmString, genFieldID.wasmString.length)
    fb += LocalGet(str2)
    fb += StructGet(genTypeID.wasmString, genFieldID.wasmString.length)
    fb += I32Add

    // Link the new node to the `left`
    fb += LocalGet(str1)

    fb += StructNew(genTypeID.wasmString)

    fb.buildAndAddToModule()
  }

  private def genStringEquals()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.wasmString.stringEquals)
    val str1 = fb.addParam("str1", nullableStringType)
    val str2 = fb.addParam("str2", nullableStringType)
    fb.setResultType(Int32)

    val len1 = fb.addLocal("len1", Int32)
    val len2 = fb.addLocal("len2", Int32)
    val iLocal = fb.addLocal("i", Int32)
    val chars1 = fb.addLocal("chars1", RefType(genTypeID.i16Array))
    val chars2 = fb.addLocal("chars2", RefType(genTypeID.i16Array))

    // null == null => true
    fb += LocalGet(str1)
    fb += RefIsNull
    fb += LocalGet(str2)
    fb += RefIsNull
    fb += I32And
    fb.ifThen() {
      fb += I32Const(1)
      fb += Return
    }

    // null vs non-null
    fb += LocalGet(str1)
    fb += RefIsNull
    fb += LocalGet(str2)
    fb += RefIsNull
    fb += I32Or
    fb.ifThen() {
      fb += I32Const(0)
      fb += Return
    }

    // Fast path: different lengths cannot be equal
    fb += LocalGet(str1)
    fb += StructGet(genTypeID.wasmString, genFieldID.wasmString.length)
    fb += LocalTee(len1)
    fb += LocalGet(str2)
    fb += StructGet(genTypeID.wasmString, genFieldID.wasmString.length)
    fb += LocalTee(len2)
    fb += I32Ne
    fb.ifThen() {
      fb += I32Const(0)
      fb += Return
    }

    /* Compare the flattened character arrays.
     * `getWholeChars` collapses lazy concatenated chains of string.
     */
    fb += LocalGet(str1)
    fb += RefAsNonNull
    fb += Call(genFunctionID.wasmString.getWholeChars)
    fb += LocalSet(chars1)

    fb += LocalGet(str2)
    fb += RefAsNonNull
    fb += Call(genFunctionID.wasmString.getWholeChars)
    fb += LocalSet(chars2)

    // Iterate over UTF-16 code points.
    fb += I32Const(0)
    fb += LocalSet(iLocal)
    fb.whileLoop() {
      fb += LocalGet(iLocal)
      fb += LocalGet(len1)
      fb += I32Ne
    } {
      fb += LocalGet(chars1)
      fb += LocalGet(iLocal)
      fb += ArrayGetU(genTypeID.i16Array)

      fb += LocalGet(chars2)
      fb += LocalGet(iLocal)
      fb += ArrayGetU(genTypeID.i16Array)

      fb += I32Ne
      fb.ifThen() {
        fb += I32Const(0)
        fb += Return
      }
      fb += LocalGet(iLocal)
      fb += I32Const(1)
      fb += I32Add
      fb += LocalSet(iLocal)
    }
    fb += I32Const(1)

    fb.buildAndAddToModule()
  }

  private def genCharCodeAt()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.wasmString.charCodeAt)
    val strParam = fb.addParam("str", RefType(genTypeID.wasmString))
    val idxParam = fb.addParam("idx", Int32)
    fb.setResultType(Int32)

    fb += LocalGet(strParam)
    fb += Call(genFunctionID.wasmString.getWholeChars)
    fb += LocalGet(idxParam)
    fb += ArrayGetU(genTypeID.i16Array)

    fb.buildAndAddToModule()
  }

  /** Generates the helper that returns a "flat" array for a Wasm-without-JS string.
   *
   *  Lazy concatenation nodes are collapsed before returning, so callers can
   *  index the returned array directly.
   */
  private def genGetWholeChars()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.wasmString.getWholeChars)
    val strParam = fb.addParam("str", RefType(genTypeID.wasmString))
    fb.setResultType(RefType(genTypeID.i16Array))

    /* If `left` is non-null, this string is a lazy concat node. Collapse it
     * before exposing the chars array so callers always see the full contents.
     */
    fb += LocalGet(strParam)
    fb += StructGet(genTypeID.wasmString, genFieldID.wasmString.left)
    fb += RefIsNull
    fb += I32Eqz
    fb.ifThen() {
      fb += LocalGet(strParam)
      fb += Call(genFunctionID.wasmString.collapseString)
    }

    // After the optional collapse, `chars` contains the entire string.
    fb += LocalGet(strParam)
    fb += StructGet(genTypeID.wasmString, genFieldID.wasmString.chars)

    fb.buildAndAddToModule()
  }

  /** Flattens a lazy Wasm-without-JS string in place.
   *
   *  It copies all segments from the concat chain on `left` into a single i16 array,
   *  stores that array to the original string, and clears its `left` reference.
   */
  private def genCollapseString()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.wasmString.collapseString)
    val strParam = fb.addParam("str", RefType(genTypeID.wasmString))

    val newArray = fb.addLocal("newArray", RefType(genTypeID.i16Array))
    val currentString = fb.addLocal("currentString", RefType(genTypeID.wasmString))
    val currentChars = fb.addLocal("currentChars", RefType(genTypeID.i16Array))
    val currentCharsLen = fb.addLocal("currentCharsLen", Int32)
    val currentIdx = fb.addLocal("currentIdx", Int32)

    // Allocate an array for the concatenated string
    fb += LocalGet(strParam)
    fb += LocalTee(currentString)
    fb += StructGet(genTypeID.wasmString, genFieldID.wasmString.length)
    fb += LocalTee(currentIdx) // currentIdx = length
    fb += ArrayNewDefault(genTypeID.i16Array)
    fb += LocalSet(newArray)

    // traverse `left` string segments, and copies to the `newArray`.
    fb.block() { loopDoneLabel =>
      fb.loop() { loopLabel =>
        // currentIdx = currentIdx - array.len(currentString.chars)
        fb += LocalGet(currentIdx)
        fb += LocalGet(currentString)
        fb += StructGet(genTypeID.wasmString, genFieldID.wasmString.chars)
        fb += LocalTee(currentChars)
        fb += ArrayLen
        fb += LocalTee(currentCharsLen)
        fb += I32Sub
        fb += LocalSet(currentIdx)

        // copy current i16Array to the `newArray`
        fb += LocalGet(newArray)
        fb += LocalGet(currentIdx)
        fb += LocalGet(currentChars)
        fb += I32Const(0)
        fb += LocalGet(currentCharsLen)
        fb += ArrayCopy(genTypeID.i16Array, genTypeID.i16Array)

        // currentString = currentString.left; break on null
        fb += LocalGet(currentString)
        fb += StructGet(genTypeID.wasmString, genFieldID.wasmString.left)
        fb += BrOnNull(loopDoneLabel)
        fb += LocalSet(currentString)
        fb += Br(loopLabel)
      }
    }

    // update the original string's `chars` and `left` fields.
    fb += LocalGet(strParam)
    fb += LocalGet(newArray)
    fb += StructSet(genTypeID.wasmString, genFieldID.wasmString.chars)
    fb += LocalGet(strParam)
    fb += RefNull(HeapType.None)
    fb += StructSet(genTypeID.wasmString, genFieldID.wasmString.left)

    fb.buildAndAddToModule()
  }

  private def genLtoa()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.longToString)
    val value = fb.addParam("value", Int64)
    fb.setResultType(stringType)

    val isNegative = fb.addLocal("isNegative", Int32)
    val arrayLen = fb.addLocal("arrayLen", Int32)
    val tmp = fb.addLocal("tmp", Int64)
    val iLocal = fb.addLocal("i", Int32)
    val result = fb.addLocal("result", RefType(genTypeID.i16Array))

    // Special-case for 0
    fb += LocalGet(value)
    fb += I64Eqz
    fb.ifThen() {
      fb += I32Const('0'.toInt)
      SWasmGen.genWasmStringFromCharCode(fb)
      fb += Return
    }

    // isNegative = value < 0
    fb += LocalGet(value)
    fb += I64Const(0L)
    fb += I64LtS
    fb += LocalSet(isNegative)

    // value = abs(value) -- tmp = value >> 63; value = (tmp ^ value) - tmp
    fb += LocalGet(value)
    fb += I64Const(63)
    fb += I64ShrS
    fb += LocalTee(tmp)
    fb += LocalGet(value)
    fb += I64Xor
    fb += LocalGet(tmp)
    fb += I64Sub
    fb += LocalSet(value)

    // arrayLen = isNegative ? 1 : 0
    fb += LocalGet(isNegative)
    fb += LocalSet(arrayLen)

    // tmp = value
    fb += LocalGet(value)
    fb += LocalSet(tmp)

    // while (tmp != 0)
    fb.whileLoop() {
      fb += LocalGet(tmp)
      fb += I64Const(0)
      fb += I64Ne
    } {
      // tmp := tmp / 10
      fb += LocalGet(tmp)
      fb += I64Const(10L)
      fb += I64DivU
      fb += LocalSet(tmp)

      // arrayLen += 1
      fb += LocalGet(arrayLen)
      fb += I32Const(1)
      fb += I32Add
      fb += LocalSet(arrayLen)
    }

    // result = new Array[Char](arrayLen)
    fb += LocalGet(arrayLen)
    fb += ArrayNewDefault(genTypeID.i16Array)
    fb += LocalSet(result)

    // tmp = value
    fb += LocalGet(value)
    fb += LocalSet(tmp)

    // i = arrayLen - 1
    fb += LocalGet(arrayLen)
    fb += I32Const(1)
    fb += I32Sub
    fb += LocalSet(iLocal)

    // while (tmp != 0)
    fb.whileLoop() {
      fb += LocalGet(tmp)
      fb += I64Const(0)
      fb += I64Ne
    } {
      // result(i) = '0' + (tmp % 10)
      fb += LocalGet(result)
      fb += LocalGet(iLocal)
      fb += LocalGet(tmp)
      fb += I64Const(10L)
      fb += I64RemU
      fb += I32WrapI64
      fb += I32Const('0'.toInt)
      fb += I32Add
      fb += ArraySet(genTypeID.i16Array)

      // i -= 1
      fb += LocalGet(iLocal)
      fb += I32Const(1)
      fb += I32Sub
      fb += LocalSet(iLocal)

      // tmp = tmp / 10
      fb += LocalGet(tmp)
      fb += I64Const(10L)
      fb += I64DivU
      fb += LocalSet(tmp)
    }

    // if (isNegative)
    fb += LocalGet(isNegative)
    fb.ifThen() {
      // result(0) = '-'
      fb += LocalGet(result)
      fb += I32Const(0)
      fb += I32Const('-'.toInt)
      fb += ArraySet(genTypeID.i16Array)
    }

    // build the result wasmString
    fb += LocalGet(result)
    fb += LocalGet(arrayLen)
    fb += RefNull(HeapType.None)
    fb += StructNew(genTypeID.wasmString)

    fb.buildAndAddToModule()
  }

  private def genItoa()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.intToString)
    val value = fb.addParam("value", Int32)
    fb.setResultType(stringType)

    fb += LocalGet(value)
    fb += I64ExtendI32S
    fb += ReturnCall(genFunctionID.longToString)

    fb.buildAndAddToModule()
  }

  /** Stringify no-JS Wasm values that are not represented as our objects.
   *
   *  Boxed `Float`/`Double` (and large `Int`) values are `DoubleBoxClass` or
   *  `IntegerBoxClass` instances, and they are handled by normal vtable dispatch.
   */
  private def genHijackedValueToString()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.hijackedValueToString)
    val value = fb.addParam("value", anyref)
    fb.setResultType(stringType)

    fb.block(RefType(genTypeID.wasmString)) { labelString =>
      fb.block(RefType.i31) { labelI31 =>
        fb += LocalGet(value)
        fb += BrOnCast(labelI31, anyref, RefType.i31)
        fb += BrOnCast(labelString, anyref, RefType(genTypeID.wasmString))
        fb ++= ctx.stringPool.getConstantStringInstr("null")
        fb += Return
      }

      fb += I31GetS
      fb += Call(genFunctionID.intToString)
    }

    fb.buildAndAddToModule()
  }

  private def genBox(functionID: FunctionID, targetTpe: PrimType)(
      implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(functionID)
    val xParam = fb.addParam("x", transformPrimType(targetTpe))
    fb.setResultType(RefType.any)

    val boxClass =
      if (targetTpe == FloatType) BoxedDoubleClass
      else PrimTypeToBoxedClass(targetTpe)

    genStructNewWithVTable(fb, genTypeID.forClass(boxClass)) {
      fb += GlobalGet(genGlobalID.forVTable(boxClass))
    } {
      if (targetTpe == IntType)
        fb += LocalGet(xParam) // the value is also the hash code; store it immediately
      else
        fb += I32Const(0)
      fb += LocalGet(xParam)
      if (targetTpe == FloatType)
        fb += F64PromoteF32
    }

    fb.buildAndAddToModule()
  }

  private def genUnbox(functionID: FunctionID, targetTpe: PrimType)(
      implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(functionID)
    val xParam = fb.addParam("x", RefType.anyref)
    val resultType = transformPrimType(targetTpe)
    fb.setResultType(resultType)

    val boxClass = PrimTypeToBoxedClass(targetTpe)

    fb += LocalGet(xParam)
    fb.block(FunctionType(List(RefType.anyref), List(resultType))) { doneLabel =>
      fb.block(FunctionType(List(RefType.anyref), Nil)) { isNullLabel =>
        fb += BrOnNull(isNullLabel)
        val structTypeID = genTypeID.forClass(boxClass)
        fb += RefCast(RefType(structTypeID))
        fb += StructGet(structTypeID, genFieldID.boxValue)
        fb += Br(doneLabel)
      }
      fb += genZeroOf(targetTpe)
    }

    fb.buildAndAddToModule()
  }

  /** Generates the Wasm-without-JS fallback for `unbox(IntRef)`.
   *
   *  `typeTest(IntRef)` is value-based and accepts both `IntegerBoxClass`
   *  and `DoubleBoxClass` values that are valid Ints. Keep that normalization
   *  inside unboxing so checked casts can call `unbox(IntRef)` uniformly.
   */
  private def genUnboxIntFallback()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.uIFallback)
    val xParam = fb.addParam("x", RefType.anyref)
    fb.setResultType(Int32)

    fb += LocalGet(xParam)
    fb += Call(genFunctionID.unbox(DoubleRef))
    fb += I32TruncF64S

    fb.buildAndAddToModule()
  }

  private def genUnboxFloat()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.unbox(FloatRef))
    val xParam = fb.addParam("x", RefType.anyref)
    fb.setResultType(Float32)

    fb += LocalGet(xParam)
    fb += Call(genFunctionID.unbox(DoubleRef))
    fb += F32DemoteF64

    fb.buildAndAddToModule()
  }

  private def genUnboxDouble()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.unbox(DoubleRef))
    val xParam = fb.addParam("x", RefType.anyref)
    fb.setResultType(Float64)

    val intBoxType = genTypeID.forClass(BoxedIntegerClass)
    val doubleBoxType = genTypeID.forClass(BoxedDoubleClass)
    val intBoxRef = RefType(intBoxType)
    val doubleBoxRef = RefType(doubleBoxType)

    val anyrefToF64 = FunctionType(List(RefType.anyref), List(Float64))
    val anyrefToNothing = FunctionType(List(RefType.anyref), Nil)
    val anyrefToI31 = FunctionType(List(RefType.anyref), List(RefType.i31))
    val anyrefToIntBox = FunctionType(List(RefType.anyref), List(intBoxRef))
    val anyrefToDoubleBox = FunctionType(List(RefType.anyref), List(doubleBoxRef))

    fb += LocalGet(xParam)
    fb.block(anyrefToF64) { doneLabel =>
      fb.block(anyrefToNothing) { isNullLabel =>
        fb.block(anyrefToI31) { isI31Label =>
          fb.block(anyrefToIntBox) { isIntLabel =>
            fb.block(anyrefToDoubleBox) { isDoubleLabel =>
              fb += BrOnNull(isNullLabel)
              fb += BrOnCast(isI31Label, RefType.anyref, RefType.i31)
              fb += BrOnCast(isIntLabel, RefType.anyref, intBoxRef)
              fb += BrOnCast(isDoubleLabel, RefType.anyref, doubleBoxRef)
              fb += Unreachable
            }
            // `Float` and `Double` both use `DoubleBoxClass`
            fb += StructGet(doubleBoxType, genFieldID.boxValue)
            fb += Br(doneLabel)
          }
          // Large `Int` values that do not fit in i31ref use `IntegerBoxClass`.
          fb += StructGet(intBoxType, genFieldID.boxValue)
          fb += F64ConvertI32S
          fb += Br(doneLabel)
        }
        // Small `Int` values use i31ref, `Short` and `Byte`.
        fb += I31GetS
        fb += F64ConvertI32S
        fb += Br(doneLabel)
      }
      // null
      fb += F64Const(0.0)
    }

    fb.buildAndAddToModule()
  }

  private def genTestBoolean()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.typeTest(BooleanRef))
    val xParam = fb.addParam("x", RefType.anyref)
    fb.setResultType(Int32)

    fb += LocalGet(xParam)
    fb += RefTest(RefType(genTypeID.forClass(BoxedBooleanClass)))

    fb.buildAndAddToModule()
  }

  private def genTestFloat()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.typeTest(FloatRef))
    val xParam = fb.addParam("x", RefType.anyref)
    fb.setResultType(Int32)

    val doubleValue = fb.addLocal("doubleValue", Float64)

    fb += LocalGet(xParam)
    fb += Call(genFunctionID.typeTest(DoubleRef))
    fb.ifThenElse(Int32) {
      fb += LocalGet(xParam)
      fb += Call(genFunctionID.unbox(DoubleRef))
      fb += LocalTee(doubleValue)

      // Math.fround(value) === value
      fb += F32DemoteF64
      fb += F64PromoteF32
      fb += LocalGet(doubleValue)
      fb += F64Eq
      fb.ifThenElse(Int32) {
        fb += I32Const(1)
      } {
        // NaN
        fb += LocalGet(doubleValue)
        fb += LocalGet(doubleValue)
        fb += F64Ne
      }
    } {
      fb += I32Const(0)
    }

    fb.buildAndAddToModule()
  }

  private def genTestDouble()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.typeTest(DoubleRef))
    val xParam = fb.addParam("x", RefType.anyref)
    fb.setResultType(Int32)

    val intBoxRef = RefType(genTypeID.forClass(BoxedIntegerClass))
    val doubleBoxRef = RefType(genTypeID.forClass(BoxedDoubleClass))

    // Emulates `typeof(value) === "number"` check.
    // For no-JS Wasm, numbers can be stored i31ref, `IntegerBoxClass`,
    // or floating-point values in `DoubleBoxClass`.
    fb += LocalGet(xParam)
    fb += RefTest(RefType.i31)
    fb.ifThenElse(Int32) {
      fb += I32Const(1)
    } {
      // Both integer fallback boxes and f64 boxes are in the number domain.
      fb += LocalGet(xParam)
      fb += RefTest(intBoxRef)
      fb.ifThenElse(Int32) {
        fb += I32Const(1)
      } {
        fb += LocalGet(xParam)
        fb += RefTest(doubleBoxRef)
      }
    }

    fb.buildAndAddToModule()
  }

  private def genTestInteger()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.typeTest(IntRef))
    val xParam = fb.addParam("x", RefType.anyref)
    fb.setResultType(Int32)

    val intBoxRef = RefType(genTypeID.forClass(BoxedIntegerClass))
    val doubleValue = fb.addLocal("doubleValue", Float64)
    val truncatedDoubleValue = fb.addLocal("truncDoubleValue", Float64)

    // check small numbers that fits i31ref
    fb += LocalGet(xParam)
    fb += RefTest(RefType.i31)
    fb.ifThenElse(Int32) {
      fb += I32Const(1)
    } {
      // Larger `Int`s `IntegerBoxClass`.
      fb += LocalGet(xParam)
      fb += RefTest(intBoxRef)
      fb.ifThenElse(Int32) {
        fb += I32Const(1)
      } {
        // Other Scala.js numbers are `Int` iff truncating to i32 and back
        // preserves the original value.
        fb += LocalGet(xParam)
        fb += Call(genFunctionID.typeTest(DoubleRef))
        fb.ifThenElse(Int32) {
          fb += LocalGet(xParam)
          fb += Call(genFunctionID.unbox(DoubleRef))
          fb += LocalTee(doubleValue)

          // value == value.toInt.toDouble.
          fb += LocalGet(doubleValue)
          fb += I32TruncSatF64S
          fb += F64ConvertI32S
          fb += LocalTee(truncatedDoubleValue)
          fb += F64Eq

          // bit equality check to reject -0.0
          fb += LocalGet(doubleValue)
          fb += I64ReinterpretF64
          fb += LocalGet(truncatedDoubleValue)
          fb += I64ReinterpretF64
          fb += I64Eq
          fb += I32And
        } {
          fb += I32Const(0)
        }
      }
    }

    fb.buildAndAddToModule()
  }

  /** Emulate the `Object.is` implementation. */
  private def genIs()(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val fb = newFunctionBuilder(genFunctionID.is)
    val aParam = fb.addParam("a", anyref)
    val bParam = fb.addParam("b", anyref)
    fb.setResultType(Int32)

    val doubleA = fb.addLocal("doubleA", Float64)
    val doubleB = fb.addLocal("doubleB", Float64)

    // In Wasm-without-JS, all our values are actually eqref, so first test ref.eq
    fb += LocalGet(aParam)
    fb += RefCast(RefType.eqref)
    fb += LocalGet(bParam)
    fb += RefCast(RefType.eqref)
    fb += RefEq
    fb.ifThen() {
      fb += I32Const(1)
      fb += Return
    }

    /* Otherwise, we have two cases to handle:
     * - strings, which must be compared by content
     * - numbers where at least one side is not an i31ref, which must be compared by value
     */
    fb.block(RefType.anyref) { resultIsFalse =>
      fb.block(RefType.anyref) { notString =>
        fb += LocalGet(aParam)
        fb += BrOnCastFail(notString, RefType.anyref, RefType(genTypeID.wasmString))
        fb += LocalGet(bParam)
        fb += BrOnCastFail(resultIsFalse, RefType.anyref, RefType(genTypeID.wasmString))
        fb += ReturnCall(genFunctionID.wasmString.stringEquals)
      }
      fb += Drop

      val integerBoxTypeID = genTypeID.forClass(BoxedIntegerClass)
      val integerBoxType = RefType(integerBoxTypeID)
      val doubleBoxTypeID = genTypeID.forClass(BoxedDoubleClass)
      val doubleBoxType = RefType(doubleBoxTypeID)

      fb.block(Float64) { aWasNumber =>
        fb.block(doubleBoxType) { aIsDoubleBox =>
          fb.block(integerBoxType) { aIsIntegerBox =>
            fb += LocalGet(aParam)
            fb += BrOnCast(aIsIntegerBox, RefType.anyref, integerBoxType)
            fb += BrOnCast(aIsDoubleBox, RefType.anyref, doubleBoxType)
            fb += BrOnCastFail(resultIsFalse, RefType.anyref, RefType.i31)

            // When a is an i31, quickly exit if b is also an i31, as a fast-path
            fb += LocalGet(bParam)
            fb += RefTest(RefType.i31)
            fb.ifThen() {
              fb += I32Const(0)
              fb += Return
            }

            // Extract the value of a
            fb += I31GetS
            fb += F64ConvertI32S
            fb += Br(aWasNumber)
          }

          // IntegerBox
          fb += StructGet(integerBoxTypeID, genFieldID.boxValue)
          fb += F64ConvertI32S
          fb += Br(aWasNumber)
        }

        // DoubleBox
        fb += StructGet(doubleBoxTypeID, genFieldID.boxValue)
      }
      fb += LocalTee(doubleA)

      fb.block(Float64) { bWasNumber =>
        fb.block(doubleBoxType) { bIsDoubleBox =>
          fb.block(integerBoxType) { bIsIntegerBox =>
            fb += LocalGet(bParam)
            fb += BrOnCast(bIsIntegerBox, RefType.anyref, integerBoxType)
            fb += BrOnCast(bIsDoubleBox, RefType.anyref, doubleBoxType)
            fb += BrOnCastFail(resultIsFalse, RefType.anyref, RefType.i31)

            // Extract the value of b
            fb += I31GetS
            fb += F64ConvertI32S
            fb += Br(bWasNumber)
          }

          // IntegerBox
          fb += StructGet(integerBoxTypeID, genFieldID.boxValue)
          fb += F64ConvertI32S
          fb += Br(bWasNumber)
        }

        // DoubleBox
        fb += StructGet(doubleBoxTypeID, genFieldID.boxValue)
      }
      fb += LocalTee(doubleB)

      // Emulate JS `Object.is` for numbers, comparing doubleA and doubleB.
      // -0.0 !== 0.0 => true, and NaN equals NaN.
      fb += F64Eq
      fb.ifThenElse(Int32) {
        // bit pattern equality to refect -0.0 vs 0.0
        fb += LocalGet(doubleA)
        fb += I64ReinterpretF64
        fb += LocalGet(doubleB)
        fb += I64ReinterpretF64
        fb += I64Eq
      } {
        // NaN vs Nan => true
        fb += LocalGet(doubleA)
        fb += LocalGet(doubleA)
        fb += F64Ne
        fb += LocalGet(doubleB)
        fb += LocalGet(doubleB)
        fb += F64Ne
        fb += I32And
      }
      fb += Return
    }

    // return false
    fb += Drop
    fb += I32Const(0)

    fb.buildAndAddToModule()
  }

  /** Generates the conversion from an array to the corresponding Wasm array, for Wasm interop. */
  private def genArrayToWasmArray(baseRef: PrimRef)(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val originalName = OriginalName("arrayToWasmArray." + charCodeForOriginalName(baseRef))

    val arrayTypeRef = ArrayTypeRef(baseRef, 1)
    val arrayStructTypeID = genTypeID.forArrayClass(arrayTypeRef)
    val underlyingArrayTypeID = genTypeID.underlyingOf(arrayTypeRef)
    val underlyingArrayType = RefType(underlyingArrayTypeID)

    val fb = newFunctionBuilder(genFunctionID.arrayToWasmArray(baseRef), originalName)
    val srcParam = fb.addParam("src", RefType.nullable(arrayStructTypeID))
    fb.setResultType(underlyingArrayType)

    val srcUnderlying = fb.addLocal("srcu", underlyingArrayType)
    val length = fb.addLocal("length", Int32)
    val dest = fb.addLocal("dest", underlyingArrayType)

    // Load the underlying source array; throw NPE if required
    if (ctx.coreSpec.semantics.nullPointers == CheckedBehavior.Unchecked) {
      fb += LocalGet(srcParam)
    } else {
      fb.block(RefType(arrayStructTypeID)) { nonNullLabel =>
        fb += LocalGet(srcParam)
        fb += BrOnNonNull(nonNullLabel)
        fb += Call(genFunctionID.throwNullPointerException)
        fb += Unreachable
      }
    }
    fb += StructGet(arrayStructTypeID, genFieldID.objStruct.arrayUnderlying)
    fb += LocalTee(srcUnderlying)

    // Do the copy
    fb += ArrayLen
    fb += LocalTee(length)
    fb += ArrayNewDefault(underlyingArrayTypeID)
    fb += LocalTee(dest)
    fb += I32Const(0)
    fb += LocalGet(srcUnderlying)
    fb += I32Const(0)
    fb += LocalGet(length)
    fb += ArrayCopy(underlyingArrayTypeID, underlyingArrayTypeID)

    // Return the result
    fb += LocalGet(dest)

    fb.buildAndAddToModule()
  }

  /** Generates the conversion from a Wasm array to the corresponding array, for Wasm interop. */
  private def genWasmArrayToArray(baseRef: PrimRef)(implicit ctx: WasmContext): Unit = {
    assert(!hasJSInterop)

    val originalName = OriginalName("wasmArrayToArray." + charCodeForOriginalName(baseRef))

    val arrayTypeRef = ArrayTypeRef(baseRef, 1)
    val arrayStructTypeID = genTypeID.forArrayClass(arrayTypeRef)
    val underlyingArrayTypeID = genTypeID.underlyingOf(arrayTypeRef)
    val underlyingArrayType = RefType(underlyingArrayTypeID)

    val fb = newFunctionBuilder(genFunctionID.wasmArrayToArray(baseRef), originalName)
    val srcParam = fb.addParam("src", underlyingArrayType)
    fb.setResultType(RefType(arrayStructTypeID))

    val length = fb.addLocal("length", Int32)
    val destUnderlying = fb.addLocal("destu", underlyingArrayType)

    // Do the copy to a new underlying array
    fb += LocalGet(srcParam)
    fb += ArrayLen
    fb += LocalTee(length)
    fb += ArrayNewDefault(underlyingArrayTypeID)
    fb += LocalTee(destUnderlying)
    fb += I32Const(0)
    fb += LocalGet(srcParam)
    fb += I32Const(0)
    fb += LocalGet(length)
    fb += ArrayCopy(underlyingArrayTypeID, underlyingArrayTypeID)

    // Wrap in a new array and return
    genArrayValueFromUnderlying(fb, arrayTypeRef) {
      fb += LocalGet(destUnderlying)
    }

    fb.buildAndAddToModule()
  }

  // --- Utils ---

  private def maybeWrapInUBE(fb: FunctionBuilder, behavior: CheckedBehavior)(
      genExceptionInstance: => Unit): Unit = {
    if (behavior == CheckedBehavior.Fatal) {
      genNewScalaClass(fb, UndefinedBehaviorErrorClass, ThrowableArgConsructorName) {
        genExceptionInstance
      }
    } else {
      genExceptionInstance
    }
  }

  private def genNewScalaClass(fb: FunctionBuilder, cls: ClassName, ctor: MethodName)(
      genCtorArgs: => Unit): Unit = {
    val instanceLocal = fb.addLocal(NoOriginalName, RefType(genTypeID.forClass(cls)))

    fb += Call(genFunctionID.newDefault(cls))
    fb += LocalTee(instanceLocal)
    genCtorArgs
    fb += Call(genFunctionID.forMethod(MemberNamespace.Constructor, cls, ctor))
    fb += LocalGet(instanceLocal)
  }

}
