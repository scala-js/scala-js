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

import org.scalajs.linker.interface.CheckedBehavior
import org.scalajs.linker.standard.CoreSpec

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.Instructions._
import org.scalajs.linker.backend.webassembly.Identitities._
import org.scalajs.linker.backend.webassembly.Modules._
import org.scalajs.linker.backend.webassembly.Types._

import EmbeddedConstants._
import VarGen._
import TypeTransformer._

final class CoreWasmLib(coreSpec: CoreSpec) {
  import RefType.anyref
  import coreSpec.semantics

  private implicit val noPos: Position = Position.NoPosition

  private val arrayBaseRefs: List[NonArrayTypeRef] = List(
    BooleanRef,
    CharRef,
    ByteRef,
    ShortRef,
    IntRef,
    LongRef,
    FloatRef,
    DoubleRef,
    ClassRef(ObjectClass)
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

    List(
      make(nameOffset, Int32, isMutable = false),
      make(nameSize, Int32, isMutable = false),
      make(nameStringIndex, Int32, isMutable = false),
      make(kind, Int32, isMutable = false),
      make(specialInstanceTypes, Int32, isMutable = false),
      make(strictAncestors, nullable(genTypeID.typeDataArray), isMutable = false),
      make(componentType, nullable(genTypeID.typeData), isMutable = false),
      make(name, RefType.anyref, isMutable = true),
      make(classOfValue, nullable(genTypeID.ClassStruct), isMutable = true),
      make(arrayOf, nullable(genTypeID.ObjectVTable), isMutable = true),
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

    genImports()

    genPrimitiveTypeDataGlobals()

    genHelperDefinitions()
  }

  /** Generates definitions that must come *after* the code generated for regular classes.
   *
   *  This notably includes the array class definitions, since they are subtypes of the `jl.Object`
   *  struct type.
   */
  def genPostClasses()(implicit ctx: WasmContext): Unit = {
    genArrayClassTypes()

    genBoxedZeroGlobals()
    genArrayClassGlobals()
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
      genTypeID.itables,
      ArrayType(FieldType(RefType.nullable(HeapType.Struct), isMutable = true))
    )
    genCoreType(
      genTypeID.reflectiveProxies,
      ArrayType(FieldType(RefType(genTypeID.reflectiveProxy), isMutable = false))
    )

    ctx.mainRecType.addSubType(
      SubType(
        genTypeID.typeData,
        OriginalName(genTypeID.typeData.toString()),
        isFinal = false,
        None,
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
  }

  private def genArrayClassTypes()(implicit ctx: WasmContext): Unit = {
    // The vtable type is always the same as j.l.Object
    val vtableTypeID = genTypeID.ObjectVTable
    val vtableField = StructField(
      genFieldID.objStruct.vtable,
      OriginalName(genFieldID.objStruct.vtable.toString()),
      RefType(vtableTypeID),
      isMutable = false
    )
    val itablesField = StructField(
      genFieldID.objStruct.itables,
      OriginalName(genFieldID.objStruct.itables.toString()),
      RefType.nullable(genTypeID.itables),
      isMutable = false
    )

    val typeRefsWithArrays: List[(TypeID, TypeID)] =
      List(
        (genTypeID.BooleanArray, genTypeID.i8Array),
        (genTypeID.CharArray, genTypeID.i16Array),
        (genTypeID.ByteArray, genTypeID.i8Array),
        (genTypeID.ShortArray, genTypeID.i16Array),
        (genTypeID.IntArray, genTypeID.i32Array),
        (genTypeID.LongArray, genTypeID.i64Array),
        (genTypeID.FloatArray, genTypeID.f32Array),
        (genTypeID.DoubleArray, genTypeID.f64Array),
        (genTypeID.ObjectArray, genTypeID.anyArray)
      )

    for ((structTypeID, underlyingArrayTypeID) <- typeRefsWithArrays) {
      val origName = OriginalName(structTypeID.toString())

      val underlyingArrayField = StructField(
        genFieldID.objStruct.arrayUnderlying,
        OriginalName(genFieldID.objStruct.arrayUnderlying.toString()),
        RefType(underlyingArrayTypeID),
        isMutable = false
      )

      val superType = genTypeID.ObjectStruct
      val structType = StructType(
        List(vtableField, itablesField, underlyingArrayField)
      )
      val subType = SubType(structTypeID, origName, isFinal = true, Some(superType), structType)
      ctx.mainRecType.addSubType(subType)
    }
  }

  // --- Imports ---

  private def genImports()(implicit ctx: WasmContext): Unit = {
    genTagImports()
    genGlobalImports()
    genHelperImports()
  }

  private def genTagImports()(implicit ctx: WasmContext): Unit = {
    val exceptionSig = FunctionType(List(RefType.externref), Nil)
    val typeID = ctx.moduleBuilder.functionTypeToTypeID(exceptionSig)
    ctx.moduleBuilder.addImport(
      Import(
        "__scalaJSHelpers",
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
          "__scalaJSHelpers",
          id.toString(), // import name, guaranteed by JSHelperGlobalID
          ImportDesc.Global(id, OriginalName(id.toString()), isMutable = false, tpe)
        )
      )
    }

    addGlobalHelperImport(genGlobalID.jsLinkingInfo, RefType.any)
    addGlobalHelperImport(genGlobalID.undef, RefType.any)
    addGlobalHelperImport(genGlobalID.bFalse, RefType.any)
    addGlobalHelperImport(genGlobalID.bZero, RefType.any)
    addGlobalHelperImport(genGlobalID.emptyString, RefType.any)
    addGlobalHelperImport(genGlobalID.idHashCodeMap, RefType.extern)
  }

  private def genHelperImports()(implicit ctx: WasmContext): Unit = {
    def addHelperImport(id: genFunctionID.JSHelperFunctionID,
        params: List[Type], results: List[Type]): Unit = {
      val sig = FunctionType(params, results)
      val typeID = ctx.moduleBuilder.functionTypeToTypeID(sig)
      ctx.moduleBuilder.addImport(
        Import(
          "__scalaJSHelpers",
          id.toString(), // import name, guaranteed by JSHelperFunctionID
          ImportDesc.Func(id, OriginalName(id.toString()), typeID)
        )
      )
    }

    addHelperImport(genFunctionID.is, List(anyref, anyref), List(Int32))

    addHelperImport(genFunctionID.isUndef, List(anyref), List(Int32))

    for (primRef <- List(BooleanRef, ByteRef, ShortRef, FloatRef, DoubleRef)) {
      val wasmType = primRef match {
        case FloatRef  => Float32
        case DoubleRef => Float64
        case _         => Int32
      }
      addHelperImport(genFunctionID.box(primRef), List(wasmType), List(RefType.any))
      addHelperImport(genFunctionID.unbox(primRef), List(anyref), List(wasmType))
      addHelperImport(genFunctionID.typeTest(primRef), List(anyref), List(Int32))
    }

    addHelperImport(genFunctionID.bIFallback, List(Int32), List(RefType.any))
    addHelperImport(genFunctionID.uIFallback, List(anyref), List(Int32))
    addHelperImport(genFunctionID.typeTest(IntRef), List(anyref), List(Int32))

    addHelperImport(genFunctionID.fmod, List(Float64, Float64), List(Float64))

    addHelperImport(
      genFunctionID.closure,
      List(RefType.func, anyref),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionID.closureThis,
      List(RefType.func, anyref),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionID.closureRest,
      List(RefType.func, anyref, Int32),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionID.closureThisRest,
      List(RefType.func, anyref, Int32),
      List(RefType.any)
    )

    addHelperImport(genFunctionID.makeExportedDef, List(RefType.func), List(RefType.any))
    addHelperImport(
      genFunctionID.makeExportedDefRest,
      List(RefType.func, Int32),
      List(RefType.any)
    )

    addHelperImport(genFunctionID.stringLength, List(RefType.any), List(Int32))
    addHelperImport(genFunctionID.stringCharAt, List(RefType.any, Int32), List(Int32))
    addHelperImport(genFunctionID.jsValueToString, List(RefType.any), List(RefType.any))
    addHelperImport(genFunctionID.jsValueToStringForConcat, List(anyref), List(RefType.any))
    addHelperImport(genFunctionID.booleanToString, List(Int32), List(RefType.any))
    addHelperImport(genFunctionID.charToString, List(Int32), List(RefType.any))
    addHelperImport(genFunctionID.intToString, List(Int32), List(RefType.any))
    addHelperImport(genFunctionID.longToString, List(Int64), List(RefType.any))
    addHelperImport(genFunctionID.doubleToString, List(Float64), List(RefType.any))
    addHelperImport(
      genFunctionID.stringConcat,
      List(RefType.any, RefType.any),
      List(RefType.any)
    )
    addHelperImport(genFunctionID.isString, List(anyref), List(Int32))

    addHelperImport(genFunctionID.jsValueType, List(RefType.any), List(Int32))
    addHelperImport(genFunctionID.jsValueDescription, List(anyref), List(RefType.any))
    addHelperImport(genFunctionID.bigintHashCode, List(RefType.any), List(Int32))
    addHelperImport(
      genFunctionID.symbolDescription,
      List(RefType.any),
      List(RefType.anyref)
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

    addHelperImport(genFunctionID.jsGlobalRefGet, List(RefType.any), List(anyref))
    addHelperImport(genFunctionID.jsGlobalRefSet, List(RefType.any, anyref), Nil)
    addHelperImport(genFunctionID.jsGlobalRefTypeof, List(RefType.any), List(RefType.any))
    addHelperImport(genFunctionID.jsNewArray, Nil, List(RefType.any))
    addHelperImport(genFunctionID.jsArrayPush, List(RefType.any, anyref), List(RefType.any))
    addHelperImport(
      genFunctionID.jsArraySpreadPush,
      List(RefType.any, anyref),
      List(RefType.any)
    )
    addHelperImport(genFunctionID.jsNewObject, Nil, List(RefType.any))
    addHelperImport(
      genFunctionID.jsObjectPush,
      List(RefType.any, anyref, anyref),
      List(RefType.any)
    )
    addHelperImport(genFunctionID.jsSelect, List(anyref, anyref), List(anyref))
    addHelperImport(genFunctionID.jsSelectSet, List(anyref, anyref, anyref), Nil)
    addHelperImport(genFunctionID.jsNew, List(anyref, anyref), List(anyref))
    addHelperImport(genFunctionID.jsFunctionApply, List(anyref, anyref), List(anyref))
    addHelperImport(
      genFunctionID.jsMethodApply,
      List(anyref, anyref, anyref),
      List(anyref)
    )
    addHelperImport(genFunctionID.jsImportCall, List(anyref), List(anyref))
    addHelperImport(genFunctionID.jsImportMeta, Nil, List(anyref))
    addHelperImport(genFunctionID.jsDelete, List(anyref, anyref), Nil)
    addHelperImport(genFunctionID.jsForInSimple, List(anyref, anyref), Nil)
    addHelperImport(genFunctionID.jsIsTruthy, List(anyref), List(Int32))

    for ((op, funcID) <- genFunctionID.jsUnaryOps)
      addHelperImport(funcID, List(anyref), List(anyref))

    for ((op, funcID) <- genFunctionID.jsBinaryOps) {
      val resultType =
        if (op == JSBinaryOp.=== || op == JSBinaryOp.!==) Int32
        else anyref
      addHelperImport(funcID, List(anyref, anyref), List(resultType))
    }

    addHelperImport(genFunctionID.newSymbol, Nil, List(anyref))
    addHelperImport(
      genFunctionID.createJSClass,
      List(anyref, anyref, RefType.func, RefType.func, RefType.func, anyref),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionID.createJSClassRest,
      List(anyref, anyref, RefType.func, RefType.func, RefType.func, anyref, Int32),
      List(RefType.any)
    )
    addHelperImport(
      genFunctionID.installJSField,
      List(anyref, anyref, anyref),
      Nil
    )
    addHelperImport(
      genFunctionID.installJSMethod,
      List(anyref, anyref, anyref, RefType.func, Int32),
      Nil
    )
    addHelperImport(
      genFunctionID.installJSStaticMethod,
      List(anyref, anyref, anyref, RefType.func, Int32),
      Nil
    )
    addHelperImport(
      genFunctionID.installJSProperty,
      List(anyref, anyref, anyref, RefType.funcref, RefType.funcref),
      Nil
    )
    addHelperImport(
      genFunctionID.installJSStaticProperty,
      List(anyref, anyref, anyref, RefType.funcref, RefType.funcref),
      Nil
    )
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
    addHelperImport(
      genFunctionID.jsSuperCall,
      List(anyref, anyref, anyref, anyref),
      List(anyref)
    )
  }

  // --- Global definitions ---

  private def genPrimitiveTypeDataGlobals()(implicit ctx: WasmContext): Unit = {
    import genFieldID.typeData._

    val primRefsWithTypeData = List(
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

    val typeDataTypeID = genTypeID.typeData

    // Other than `name` and `kind`, all the fields have the same value for all primitives
    val commonFieldValues = List(
      // specialInstanceTypes
      I32Const(0),
      // strictAncestors
      RefNull(HeapType.None),
      // componentType
      RefNull(HeapType.None),
      // name - initially `null`; filled in by the `typeDataName` helper
      RefNull(HeapType.None),
      // the classOf instance - initially `null`; filled in by the `createClassOf` helper
      RefNull(HeapType.None),
      // arrayOf, the typeData of an array of this type - initially `null`; filled in by the `arrayTypeData` helper
      RefNull(HeapType.None),
      // cloneFunction
      RefNull(HeapType.NoFunc),
      // isJSClassInstance
      RefNull(HeapType.NoFunc),
      // reflectiveProxies
      ArrayNewFixed(genTypeID.reflectiveProxies, 0)
    )

    for ((primRef, kind) <- primRefsWithTypeData) {
      val nameDataValue: List[Instr] =
        ctx.stringPool.getConstantStringDataInstr(primRef.displayName)

      val instrs: List[Instr] = {
        nameDataValue ::: I32Const(kind) :: commonFieldValues :::
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

  private def genBoxedZeroGlobals()(implicit ctx: WasmContext): Unit = {
    val primTypesWithBoxClasses: List[(GlobalID, ClassName, Instr)] = List(
      (genGlobalID.bZeroChar, SpecialNames.CharBoxClass, I32Const(0)),
      (genGlobalID.bZeroLong, SpecialNames.LongBoxClass, I64Const(0))
    )

    for ((globalID, boxClassName, zeroValueInstr) <- primTypesWithBoxClasses) {
      val boxStruct = genTypeID.forClass(boxClassName)
      val instrs: List[Instr] = List(
        GlobalGet(genGlobalID.forVTable(boxClassName)),
        GlobalGet(genGlobalID.forITable(boxClassName)),
        zeroValueInstr,
        StructNew(boxStruct)
      )

      ctx.addGlobal(
        Global(
          globalID,
          OriginalName(globalID.toString()),
          isMutable = false,
          RefType(boxStruct),
          Expr(instrs)
        )
      )
    }
  }

  private def genArrayClassGlobals()(implicit ctx: WasmContext): Unit = {
    // Common itable global for all array classes
    val itablesInit = List(
      I32Const(ctx.itablesLength),
      ArrayNewDefault(genTypeID.itables)
    )
    ctx.addGlobal(
      Global(
        genGlobalID.arrayClassITable,
        OriginalName(genGlobalID.arrayClassITable.toString()),
        isMutable = false,
        RefType(genTypeID.itables),
        init = Expr(itablesInit)
      )
    )
  }

  // --- Function definitions ---

  /** Generates all the helper function definitions of the core Wasm lib. */
  private def genHelperDefinitions()(implicit ctx: WasmContext): Unit = {
    genBoxInt()
    genUnboxInt()
    genStringLiteral()
    genCreateStringFromData()
    genTypeDataName()
    genCreateClassOf()
    genGetClassOf()
    genArrayTypeData()

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
      genCheckedStringCharAt()
    }

    if (semantics.nullPointers != CheckedBehavior.Unchecked) {
      genThrowNullPointerException()
    }

    if (semantics.moduleInit == CheckedBehavior.Fatal) {
      genThrowModuleInitError()
    }

    genIsInstanceExternal()
    genIsInstance()
    genIsAssignableFromExternal()
    genIsAssignableFrom()
    genCheckCast()
    genGetComponentType()
    genNewArrayOfThisClass()
    genAnyGetClass()
    genAnyGetClassName()
    genAnyGetTypeData()
    genNewArrayObject()
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

  private def genStringLiteral()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.stringLiteral)
    val offsetParam = fb.addParam("offset", Int32)
    val sizeParam = fb.addParam("size", Int32)
    val stringIndexParam = fb.addParam("stringIndex", Int32)
    fb.setResultType(RefType.any)

    val str = fb.addLocal("str", RefType.any)

    fb.block(RefType.any) { cacheHit =>
      fb += GlobalGet(genGlobalID.stringLiteralCache)
      fb += LocalGet(stringIndexParam)
      fb += ArrayGet(genTypeID.anyArray)

      fb += BrOnNonNull(cacheHit)

      // cache miss, create a new string and cache it
      fb += GlobalGet(genGlobalID.stringLiteralCache)
      fb += LocalGet(stringIndexParam)

      fb += LocalGet(offsetParam)
      fb += LocalGet(sizeParam)
      fb += ArrayNewData(genTypeID.i16Array, genDataID.string)
      fb += Call(genFunctionID.createStringFromData)
      fb += LocalTee(str)
      fb += ArraySet(genTypeID.anyArray)

      fb += LocalGet(str)
    }

    fb.buildAndAddToModule()
  }

  /** `createStringFromData: (ref array u16) -> (ref any)` (representing a `string`). */
  private def genCreateStringFromData()(implicit ctx: WasmContext): Unit = {
    val dataType = RefType(genTypeID.i16Array)

    val fb = newFunctionBuilder(genFunctionID.createStringFromData)
    val dataParam = fb.addParam("data", dataType)
    fb.setResultType(RefType.any)

    val lenLocal = fb.addLocal("len", Int32)
    val iLocal = fb.addLocal("i", Int32)
    val resultLocal = fb.addLocal("result", RefType.any)

    // len := data.length
    fb += LocalGet(dataParam)
    fb += ArrayLen
    fb += LocalSet(lenLocal)

    // i := 0
    fb += I32Const(0)
    fb += LocalSet(iLocal)

    // result := ""
    fb += GlobalGet(genGlobalID.emptyString)
    fb += LocalSet(resultLocal)

    fb.loop() { labelLoop =>
      // if i == len
      fb += LocalGet(iLocal)
      fb += LocalGet(lenLocal)
      fb += I32Eq
      fb.ifThen() {
        // then return result
        fb += LocalGet(resultLocal)
        fb += Return
      }

      // result := concat(result, charToString(data(i)))
      fb += LocalGet(resultLocal)
      fb += LocalGet(dataParam)
      fb += LocalGet(iLocal)
      fb += ArrayGetU(genTypeID.i16Array)
      fb += Call(genFunctionID.charToString)
      fb += Call(genFunctionID.stringConcat)
      fb += LocalSet(resultLocal)

      // i := i + 1
      fb += LocalGet(iLocal)
      fb += I32Const(1)
      fb += I32Add
      fb += LocalSet(iLocal)

      // loop back to the beginning
      fb += Br(labelLoop)
    } // end loop $loop
    fb += Unreachable

    fb.buildAndAddToModule()
  }

  /** `typeDataName: (ref typeData) -> (ref any)` (representing a `string`).
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
    val nameDataType = RefType(genTypeID.i16Array)

    val fb = newFunctionBuilder(genFunctionID.typeDataName)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(RefType.any)

    val componentTypeDataLocal = fb.addLocal("componentTypeData", typeDataType)
    val componentNameDataLocal = fb.addLocal("componentNameData", nameDataType)
    val firstCharLocal = fb.addLocal("firstChar", Int32)
    val nameLocal = fb.addLocal("name", RefType.any)

    fb.block(RefType.any) { alreadyInitializedLabel =>
      // br_on_non_null $alreadyInitialized typeData.name
      fb += LocalGet(typeDataParam)
      fb += StructGet(genTypeID.typeData, genFieldID.typeData.name)
      fb += BrOnNonNull(alreadyInitializedLabel)

      // for the STRUCT_SET typeData.name near the end
      fb += LocalGet(typeDataParam)

      // if typeData.kind == KindArray
      fb += LocalGet(typeDataParam)
      fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
      fb += I32Const(KindArray)
      fb += I32Eq
      fb.ifThenElse(RefType.any) {
        // it is an array; compute its name from the component type name

        // <top of stack> := "[", for the CALL to stringConcat near the end
        fb += I32Const('['.toInt)
        fb += Call(genFunctionID.charToString)

        // componentTypeData := ref_as_non_null(typeData.componentType)
        fb += LocalGet(typeDataParam)
        fb += StructGet(
          genTypeID.typeData,
          genFieldID.typeData.componentType
        )
        fb += RefAsNonNull
        fb += LocalSet(componentTypeDataLocal)

        // switch (componentTypeData.kind)
        // the result of this switch is the string that must come after "["
        fb.switch(RefType.any) { () =>
          // scrutinee
          fb += LocalGet(componentTypeDataLocal)
          fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
        }(
          List(KindBoolean) -> { () =>
            fb += I32Const('Z'.toInt)
            fb += Call(genFunctionID.charToString)
          },
          List(KindChar) -> { () =>
            fb += I32Const('C'.toInt)
            fb += Call(genFunctionID.charToString)
          },
          List(KindByte) -> { () =>
            fb += I32Const('B'.toInt)
            fb += Call(genFunctionID.charToString)
          },
          List(KindShort) -> { () =>
            fb += I32Const('S'.toInt)
            fb += Call(genFunctionID.charToString)
          },
          List(KindInt) -> { () =>
            fb += I32Const('I'.toInt)
            fb += Call(genFunctionID.charToString)
          },
          List(KindLong) -> { () =>
            fb += I32Const('J'.toInt)
            fb += Call(genFunctionID.charToString)
          },
          List(KindFloat) -> { () =>
            fb += I32Const('F'.toInt)
            fb += Call(genFunctionID.charToString)
          },
          List(KindDouble) -> { () =>
            fb += I32Const('D'.toInt)
            fb += Call(genFunctionID.charToString)
          },
          List(KindArray) -> { () =>
            // the component type is an array; get its own name
            fb += LocalGet(componentTypeDataLocal)
            fb += Call(genFunctionID.typeDataName)
          }
        ) { () =>
          // default: the component type is neither a primitive nor an array;
          // concatenate "L" + <its own name> + ";"
          fb += I32Const('L'.toInt)
          fb += Call(genFunctionID.charToString)
          fb += LocalGet(componentTypeDataLocal)
          fb += Call(genFunctionID.typeDataName)
          fb += Call(genFunctionID.stringConcat)
          fb += I32Const(';'.toInt)
          fb += Call(genFunctionID.charToString)
          fb += Call(genFunctionID.stringConcat)
        }

        // At this point, the stack contains "[" and the string that must be concatenated with it
        fb += Call(genFunctionID.stringConcat)
      } {
        // it is not an array; its name is stored in nameData
        for (
          idx <- List(
            genFieldID.typeData.nameOffset,
            genFieldID.typeData.nameSize,
            genFieldID.typeData.nameStringIndex
          )
        ) {
          fb += LocalGet(typeDataParam)
          fb += StructGet(genTypeID.typeData, idx)
        }
        fb += Call(genFunctionID.stringLiteral)
      }

      // typeData.name := <top of stack> ; leave it on the stack
      fb += LocalTee(nameLocal)
      fb += StructSet(genTypeID.typeData, genFieldID.typeData.name)
      fb += LocalGet(nameLocal)
    }

    fb.buildAndAddToModule()
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

    val fb = newFunctionBuilder(genFunctionID.createClassOf)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(RefType(genTypeID.ClassStruct))

    val classInstanceLocal = fb.addLocal("classInstance", RefType(genTypeID.ClassStruct))

    // classInstance := newDefault$java.lang.Class()
    // leave it on the stack for the constructor call
    fb += Call(genFunctionID.newDefault(ClassClass))
    fb += LocalTee(classInstanceLocal)

    /* The JS object containing metadata to pass as argument to the `jl.Class` constructor.
     * Specified by https://lampwww.epfl.ch/~doeraene/sjsir-semantics/#sec-sjsir-createclassdataof
     * Leave it on the stack.
     */
    fb += Call(genFunctionID.jsNewObject)
    // "__typeData": typeData (TODO hide this better? although nobody will notice anyway)
    // (this is used by `isAssignableFromExternal`)
    fb ++= ctx.stringPool.getConstantStringInstr("__typeData")
    fb += LocalGet(typeDataParam)
    fb += Call(genFunctionID.jsObjectPush)
    // "name": typeDataName(typeData)
    fb ++= ctx.stringPool.getConstantStringInstr("name")
    fb += LocalGet(typeDataParam)
    fb += Call(genFunctionID.typeDataName)
    fb += Call(genFunctionID.jsObjectPush)
    // "isPrimitive": (typeData.kind <= KindLastPrimitive)
    fb ++= ctx.stringPool.getConstantStringInstr("isPrimitive")
    fb += LocalGet(typeDataParam)
    fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
    fb += I32Const(KindLastPrimitive)
    fb += I32LeU
    fb += Call(genFunctionID.box(BooleanRef))
    fb += Call(genFunctionID.jsObjectPush)
    // "isArrayClass": (typeData.kind == KindArray)
    fb ++= ctx.stringPool.getConstantStringInstr("isArrayClass")
    fb += LocalGet(typeDataParam)
    fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
    fb += I32Const(KindArray)
    fb += I32Eq
    fb += Call(genFunctionID.box(BooleanRef))
    fb += Call(genFunctionID.jsObjectPush)
    // "isInterface": (typeData.kind == KindInterface)
    fb ++= ctx.stringPool.getConstantStringInstr("isInterface")
    fb += LocalGet(typeDataParam)
    fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
    fb += I32Const(KindInterface)
    fb += I32Eq
    fb += Call(genFunctionID.box(BooleanRef))
    fb += Call(genFunctionID.jsObjectPush)
    // "isInstance": closure(isInstance, typeData)
    fb ++= ctx.stringPool.getConstantStringInstr("isInstance")
    fb += ctx.refFuncWithDeclaration(genFunctionID.isInstanceExternal)
    fb += LocalGet(typeDataParam)
    fb += Call(genFunctionID.closure)
    fb += Call(genFunctionID.jsObjectPush)
    // "isAssignableFrom": closure(isAssignableFrom, typeData)
    fb ++= ctx.stringPool.getConstantStringInstr("isAssignableFrom")
    fb += ctx.refFuncWithDeclaration(genFunctionID.isAssignableFromExternal)
    fb += LocalGet(typeDataParam)
    fb += Call(genFunctionID.closure)
    fb += Call(genFunctionID.jsObjectPush)
    // "checkCast": closure(checkCast, typeData)
    fb ++= ctx.stringPool.getConstantStringInstr("checkCast")
    fb += ctx.refFuncWithDeclaration(genFunctionID.checkCast)
    fb += LocalGet(typeDataParam)
    fb += Call(genFunctionID.closure)
    fb += Call(genFunctionID.jsObjectPush)
    // "getComponentType": closure(getComponentType, typeData)
    fb ++= ctx.stringPool.getConstantStringInstr("getComponentType")
    fb += ctx.refFuncWithDeclaration(genFunctionID.getComponentType)
    fb += LocalGet(typeDataParam)
    fb += Call(genFunctionID.closure)
    fb += Call(genFunctionID.jsObjectPush)
    // "newArrayOfThisClass": closure(newArrayOfThisClass, typeData)
    fb ++= ctx.stringPool.getConstantStringInstr("newArrayOfThisClass")
    fb += ctx.refFuncWithDeclaration(genFunctionID.newArrayOfThisClass)
    fb += LocalGet(typeDataParam)
    fb += Call(genFunctionID.closure)
    fb += Call(genFunctionID.jsObjectPush)

    // Call java.lang.Class::<init>(dataObject)
    fb += Call(
      genFunctionID.forMethod(
        MemberNamespace.Constructor,
        ClassClass,
        SpecialNames.AnyArgConstructorName
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

    val fb = newFunctionBuilder(genFunctionID.getClassOf)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(RefType(genTypeID.ClassStruct))

    fb.block(RefType(genTypeID.ClassStruct)) { alreadyInitializedLabel =>
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

  /** `valueDescription: anyref -> (ref any)` (a string).
   *
   *  Returns a safe string description of a value. This helper is never called
   *  for `value === null`. As implemented, it would return `"object"` if it were.
   */
  private def genValueDescription()(implicit ctx: WasmContext): Unit = {
    val objectType = RefType(genTypeID.ObjectStruct)

    val fb = newFunctionBuilder(genFunctionID.valueDescription)
    val valueParam = fb.addParam("value", anyref)
    fb.setResultType(RefType.any)

    fb.block(anyref) { notOurObjectLabel =>
      fb.block(objectType) { isCharLabel =>
        fb.block(objectType) { isLongLabel =>
          // If it not our object, jump out of notOurObject
          fb += LocalGet(valueParam)
          fb += BrOnCastFail(notOurObjectLabel, anyref, objectType)

          // If is a long or char box, jump out to the appropriate label
          fb += BrOnCast(isLongLabel, objectType, RefType(genTypeID.forClass(SpecialNames.LongBoxClass)))
          fb += BrOnCast(isCharLabel, objectType, RefType(genTypeID.forClass(SpecialNames.CharBoxClass)))

          // Get and return the class name
          fb += StructGet(genTypeID.ObjectStruct, genFieldID.objStruct.vtable)
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
      genNewScalaClass(fb, ClassCastExceptionClass, SpecialNames.StringArgConstructorName) {
        fb += LocalGet(objParam)
        fb += Call(genFunctionID.valueDescription)

        fb ++= ctx.stringPool.getConstantStringInstr(" cannot be cast to ")
        fb += Call(genFunctionID.stringConcat)

        fb += LocalGet(typeDataParam)
        fb += Call(genFunctionID.typeDataName)
        fb += Call(genFunctionID.stringConcat)
      }
    }

    fb += ExternConvertAny
    fb += Throw(genTagID.exception)

    fb.buildAndAddToModule()
  }

  /** Generates the `asInstance` functions for primitive types.
   */
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
      val boxedClassType = ClassType(PrimTypeToBoxedClass(primType), nullable = true)
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

    fb.block() { objIsNullLabel =>
      primType match {
        // For char and long, use br_on_cast_fail to test+cast to the box class
        case CharType | LongType =>
          val boxClass =
            if (primType == CharType) SpecialNames.CharBoxClass
            else SpecialNames.LongBoxClass
          val structTypeID = genTypeID.forClass(boxClass)

          fb.block(RefType.anyref) { castFailLabel =>
            fb += LocalGet(objParam)
            fb += BrOnNull(objIsNullLabel)
            fb += BrOnCastFail(castFailLabel, RefType.anyref, RefType(structTypeID))

            // Extract the `value` field if unboxing
            if (isUnbox) {
              val fieldName = FieldName(boxClass, SpecialNames.valueFieldSimpleName)
              fb += StructGet(structTypeID, genFieldID.forClassInstanceField(fieldName))
            }

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
              fb += Call(genFunctionID.isString)
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
                case primType: PrimTypeWithRef =>
                  fb += LocalGet(objParam)
                  fb += Call(genFunctionID.unbox(primType.primRef))
              }
            } else {
              fb += LocalGet(objParam)
            }

            fb += Return
          }

          // Fall through for CCE
          fb += LocalGet(objParam)
      }

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
      genFunctionID.asInstance(irtpe.ArrayType(arrayTypeRef, nullable = true)),
      OriginalName("asArray." + baseRef.displayName)
    )
    val objParam = fb.addParam("obj", anyref)
    fb.setResultType(resultType)

    fb.block(resultType) { successLabel =>
      fb += LocalGet(objParam)
      fb += BrOnCast(successLabel, anyref, resultType)

      // If we get here, it's a CCE -- `obj` is still on the stack
      fb += GlobalGet(genGlobalID.forVTable(baseRef))
      fb += I32Const(1)
      fb += Call(genFunctionID.arrayTypeData)
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
          fb += StructGet(genTypeID.ObjectStruct, genFieldID.objStruct.vtable)
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
      genNewScalaClass(fb, ArrayStoreExceptionClass,
          SpecialNames.StringArgConstructorName) {
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
      genNewScalaClass(fb, ArrayIndexOutOfBoundsExceptionClass,
          SpecialNames.StringArgConstructorName) {
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
      genNewScalaClass(fb, NegativeArraySizeExceptionClass,
          SpecialNames.StringArgConstructorName) {
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
      genNewScalaClass(fb, NullPointerExceptionClass, NoArgConstructorName) {
      }
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
        fb += StructGet(arrayStructTypeID, genFieldID.objStruct.vtable)
        fb += StructGet(genTypeID.ObjectVTable, genFieldID.typeData.componentType)
        fb += RefAsNonNull
        fb += LocalTee(componentTypeDataLocal)

        // Fast path: if componentTypeData eq typeDataOf[jl.Object], succeed
        fb += GlobalGet(genGlobalID.forVTable(ClassRef(ObjectClass)))
        fb += RefEq
        fb += BrIf(successLabel)

        // If componentTypeData.kind == KindJSType, succeed
        fb += LocalGet(componentTypeDataLocal)
        fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
        fb += I32Const(KindJSType)
        fb += I32Eq
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

  /** `arrayTypeData: (ref typeData), i32 -> (ref vtable.java.lang.Object)`.
   *
   *  Returns the typeData/vtable of an array with `dims` dimensions over the given typeData. `dims`
   *  must be be strictly positive.
   */
  private def genArrayTypeData()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)
    val objectVTableType = RefType(genTypeID.ObjectVTable)

    /* Array classes extend Cloneable, Serializable and Object.
     * Filter out the ones that do not have run-time type info at all, as
     * we do for other classes.
     */
    val strictAncestors =
      List(CloneableClass, SerializableClass, ObjectClass)
        .filter(name => ctx.getClassInfoOption(name).exists(_.hasRuntimeTypeInfo))

    val fb = newFunctionBuilder(genFunctionID.arrayTypeData)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val dimsParam = fb.addParam("dims", Int32)
    fb.setResultType(objectVTableType)

    val arrayTypeDataLocal = fb.addLocal("arrayTypeData", objectVTableType)

    fb.loop() { loopLabel =>
      fb.block(objectVTableType) { arrayOfIsNonNullLabel =>
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
        fb += I32Const(0) // nameOffset
        fb += I32Const(0) // nameSize
        fb += I32Const(0) // nameStringIndex
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
        fb += RefNull(HeapType.None) // name
        fb += RefNull(HeapType.None) // classOf
        fb += RefNull(HeapType.None) // arrayOf

        // clone
        fb.switch(RefType(genTypeID.cloneFunctionType)) { () =>
          fb += LocalGet(typeDataParam)
          fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
        }(
          List(KindBoolean) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.cloneArray(BooleanRef))
          },
          List(KindChar) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.cloneArray(CharRef))
          },
          List(KindByte) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.cloneArray(ByteRef))
          },
          List(KindShort) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.cloneArray(ShortRef))
          },
          List(KindInt) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.cloneArray(IntRef))
          },
          List(KindLong) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.cloneArray(LongRef))
          },
          List(KindFloat) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.cloneArray(FloatRef))
          },
          List(KindDouble) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.cloneArray(DoubleRef))
          }
        ) { () =>
          fb += ctx.refFuncWithDeclaration(
            genFunctionID.cloneArray(ClassRef(ObjectClass))
          )
        }

        // isJSClassInstance
        fb += RefNull(HeapType.NoFunc)

        // reflectiveProxies, empty since all methods of array classes exist in jl.Object
        fb += ArrayNewFixed(genTypeID.reflectiveProxies, 0)

        val objectClassInfo = ctx.getClassInfo(ObjectClass)
        fb ++= objectClassInfo.tableEntries.map { methodName =>
          ctx.refFuncWithDeclaration(objectClassInfo.resolvedMethodInfos(methodName).tableEntryID)
        }
        fb += StructNew(genTypeID.ObjectVTable)
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
      fb.ifThen(FunctionType(List(objectVTableType), List(objectVTableType))) {
        fb += Return
      }

      // typeData := typeData.arrayOf (which is on the stack), then loop back to the beginning
      fb += LocalSet(typeDataParam)
      fb += Br(loopLabel)
    } // end loop $loop
    fb += Unreachable

    fb.buildAndAddToModule()
  }

  /** `checkedStringCharAt: (ref any), i32 -> i32`.
   *
   *  Accesses a char of a string by index. Used when stringIndexOutOfBounds
   *  are checked.
   */
  private def genCheckedStringCharAt()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.checkedStringCharAt)
    val strParam = fb.addParam("str", RefType.any)
    val indexParam = fb.addParam("index", Int32)
    fb.setResultType(Int32)

    // if index unsigned_>= str.length
    fb += LocalGet(indexParam)
    fb += LocalGet(strParam)
    fb += Call(genFunctionID.stringLength)
    fb += I32GeU // unsigned comparison makes negative values of index larger than the length
    fb.ifThen() {
      // then, throw a StringIndexOutOfBoundsException
      maybeWrapInUBE(fb, semantics.stringIndexOutOfBounds) {
        genNewScalaClass(fb, StringIndexOutOfBoundsExceptionClass,
            SpecialNames.IntArgConstructorName) {
          fb += LocalGet(indexParam)
        }
      }
      fb += ExternConvertAny
      fb += Throw(genTagID.exception)
    }

    // otherwise, read the char
    fb += LocalGet(strParam)
    fb += LocalGet(indexParam)
    fb += Call(genFunctionID.stringCharAt)

    fb.buildAndAddToModule()
  }

  /** `throwModuleInitError: [] -> []` (always throws).
   *
   *  Throws an `UndefinedBehaviorError` for a module initialization error.
   */
  private def genThrowModuleInitError()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.throwModuleInitError)
    val typeDataParam = fb.addParam("typeData", RefType(genTypeID.typeData))

    genNewScalaClass(fb, SpecialNames.UndefinedBehaviorErrorClass,
        SpecialNames.StringArgConstructorName) {
      fb ++= ctx.stringPool.getConstantStringInstr("Initializer of ")
      fb += LocalGet(typeDataParam)
      fb += Call(genFunctionID.typeDataName)
      fb += Call(genFunctionID.stringConcat)
      fb ++= ctx.stringPool.getConstantStringInstr(
          " called before completion of its super constructor")
      fb += Call(genFunctionID.stringConcat)
    }
    fb += ExternConvertAny
    fb += Throw(genTagID.exception)

    fb.buildAndAddToModule()
  }

  /** `isInstanceExternal: (ref typeData), anyref -> anyref` (a boxed boolean).
   *
   *  Tests whether the given value is a non-null instance of the given type.
   *
   *  Specified by `"isInstance"` at
   *  [[https://lampwww.epfl.ch/~doeraene/sjsir-semantics/#sec-sjsir-createclassdataof]].
   */
  private def genIsInstanceExternal()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.isInstanceExternal)
    val typeDataParam = fb.addParam("typeData", RefType(genTypeID.typeData))
    val valueParam = fb.addParam("value", RefType.anyref)
    fb.setResultType(anyref)

    fb += LocalGet(typeDataParam)
    fb += LocalGet(valueParam)
    fb += Call(genFunctionID.isInstance)
    fb += Call(genFunctionID.box(BooleanRef))

    fb.buildAndAddToModule()
  }

  /** `isInstance: (ref typeData), anyref -> i32` (a boolean).
   *
   *  Tests whether the given value is a non-null instance of the given type.
   *
   *  Internal implementation of `isInstanceExternal`, returning a primitive
   *  `i32` instead of a boxed boolean.
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
        val structTypeID = genTypeID.forClass(SpecialNames.CharBoxClass)
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
        val structTypeID = genTypeID.forClass(SpecialNames.LongBoxClass)
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
        fb += Call(genFunctionID.isString)
      },
      // case KindJSType => call typeData.isJSClassInstance(value) or throw if it is null
      List(KindJSType) -> { () =>
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
        fb ++= ctx.stringPool.getConstantStringInstr("TypeError")
        fb += Call(genFunctionID.jsGlobalRefGet)
        fb += Call(genFunctionID.jsNewArray)
        fb ++= ctx.stringPool.getConstantStringInstr(
          "Cannot call isInstance() on a Class representing a JS trait/object"
        )
        fb += Call(genFunctionID.jsArrayPush)
        fb += Call(genFunctionID.jsNew)
        fb += ExternConvertAny
        fb += Throw(genTagID.exception)
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
      fb += I32Const(0)
      fb += I32Ne
      fb.ifThen() {
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
      fb += StructGet(genTypeID.ObjectStruct, genFieldID.objStruct.vtable)

      // Call isAssignableFrom
      fb += Call(genFunctionID.isAssignableFrom)
    }

    fb.buildAndAddToModule()
  }

  /** `isAssignableFromExternal: (ref typeData), anyref -> i32` (a boolean).
   *
   *  This is the underlying func for the `isAssignableFrom()` closure inside class data objects.
   */
  private def genIsAssignableFromExternal()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.isAssignableFromExternal)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val fromParam = fb.addParam("from", RefType.anyref)
    fb.setResultType(anyref)

    // load typeData
    fb += LocalGet(typeDataParam)

    // load ref.cast<typeData> from["__typeData"] (as a JS selection)
    fb += LocalGet(fromParam)
    fb ++= ctx.stringPool.getConstantStringInstr("__typeData")
    fb += Call(genFunctionID.jsSelect)
    fb += RefCast(RefType(typeDataType.heapType))

    // delegate to isAssignableFrom
    fb += Call(genFunctionID.isAssignableFrom)
    fb += Call(genFunctionID.box(BooleanRef))

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

  /** `checkCast: (ref typeData), anyref -> []`.
   *
   *  Casts the given value to the given type; subject to undefined behaviors.
   */
  private def genCheckCast()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.checkCast)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val valueParam = fb.addParam("value", RefType.anyref)

    if (semantics.asInstanceOfs != CheckedBehavior.Unchecked) {
      fb.block() { successLabel =>
        // If typeData.kind == KindJSType, succeed
        fb += LocalGet(typeDataParam)
        fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
        fb += I32Const(KindJSType)
        fb += I32Eq
        fb += BrIf(successLabel)

        // If value is null, succeed
        fb += LocalGet(valueParam)
        fb += RefIsNull // consumes `value`, unlike `BrOnNull` which would leave it on the stack
        fb += BrIf(successLabel)

        // If isInstance(typeData, value), succeed
        fb += LocalGet(typeDataParam)
        fb += LocalGet(valueParam)
        fb += Call(genFunctionID.isInstance)
        fb += BrIf(successLabel)

        // Otherwise, it is a CCE
        fb += LocalGet(valueParam)
        fb += LocalGet(typeDataParam)
        fb += Call(genFunctionID.classCastException)
        fb += Unreachable // for clarity; technically redundant since the stacks align
      }
    }

    fb.buildAndAddToModule()
  }

  /** `getComponentType: (ref typeData) -> (ref null jlClass)`.
   *
   *  This is the underlying func for the `getComponentType()` closure inside class data objects.
   */
  private def genGetComponentType()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.getComponentType)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    fb.setResultType(RefType.nullable(genTypeID.ClassStruct))

    val componentTypeDataLocal = fb.addLocal("componentTypeData", typeDataType)

    fb.block() { nullResultLabel =>
      // Try and extract non-null component type data
      fb += LocalGet(typeDataParam)
      fb += StructGet(genTypeID.typeData, genFieldID.typeData.componentType)
      fb += BrOnNull(nullResultLabel)
      // Get the corresponding classOf
      fb += Call(genFunctionID.getClassOf)
      fb += Return
    } // end block nullResultLabel
    fb += RefNull(HeapType(genTypeID.ClassStruct))

    fb.buildAndAddToModule()
  }

  /** `newArrayOfThisClass: (ref typeData), anyref -> (ref jlObject)`.
   *
   *  This is the underlying func for the `newArrayOfThisClass()` closure inside class data objects.
   */
  private def genNewArrayOfThisClass()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)
    val i32ArrayType = RefType(genTypeID.i32Array)

    val fb = newFunctionBuilder(genFunctionID.newArrayOfThisClass)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val lengthsParam = fb.addParam("lengths", RefType.anyref)
    fb.setResultType(RefType(genTypeID.ObjectStruct))

    val lengthsLenLocal = fb.addLocal("lengthsLenLocal", Int32)
    val lengthsValuesLocal = fb.addLocal("lengthsValues", i32ArrayType)
    val iLocal = fb.addLocal("i", Int32)

    // lengthsLen := lengths.length // as a JS field access
    fb += LocalGet(lengthsParam)
    fb ++= ctx.stringPool.getConstantStringInstr("length")
    fb += Call(genFunctionID.jsSelect)
    fb += Call(genFunctionID.unbox(IntRef))
    fb += LocalTee(lengthsLenLocal)

    // lengthsValues := array.new<i32Array> lengthsLen
    fb += ArrayNewDefault(genTypeID.i32Array)
    fb += LocalSet(lengthsValuesLocal)

    // i := 0
    fb += I32Const(0)
    fb += LocalSet(iLocal)

    // while (i != lengthsLen)
    fb.whileLoop() {
      fb += LocalGet(iLocal)
      fb += LocalGet(lengthsLenLocal)
      fb += I32Ne
    } {
      // lengthsValue[i] := lengths[i] (where the rhs is a JS field access)

      fb += LocalGet(lengthsValuesLocal)
      fb += LocalGet(iLocal)

      fb += LocalGet(lengthsParam)
      fb += LocalGet(iLocal)
      fb += RefI31
      fb += Call(genFunctionID.jsSelect)
      fb += Call(genFunctionID.unbox(IntRef))

      fb += ArraySet(genTypeID.i32Array)

      // i += 1
      fb += LocalGet(iLocal)
      fb += I32Const(1)
      fb += I32Add
      fb += LocalSet(iLocal)
    }

    // return newArrayObject(arrayTypeData(typeData, lengthsLen), lengthsValues, 0)
    fb += LocalGet(typeDataParam)
    fb += LocalGet(lengthsLenLocal)
    fb += Call(genFunctionID.arrayTypeData)
    fb += LocalGet(lengthsValuesLocal)
    fb += I32Const(0)
    fb += Call(genFunctionID.newArrayObject)

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
    fb.setResultType(RefType.nullable(genTypeID.ClassStruct))

    fb.block() { typeDataIsNullLabel =>
      fb += LocalGet(valueParam)
      fb += Call(genFunctionID.anyGetTypeData)
      fb += BrOnNull(typeDataIsNullLabel)
      fb += ReturnCall(genFunctionID.getClassOf)
    }
    fb += RefNull(HeapType.None)

    fb.buildAndAddToModule()
  }

  /** `anyGetClassName: (ref any) -> (ref any)` (a string).
   *
   *  This is the implementation of `value.getClass().getName()`, which comes
   *  to the backend as the `ObjectClassName` intrinsic.
   */
  private def genAnyGetClassName()(implicit ctx: WasmContext): Unit = {
    val fb = newFunctionBuilder(genFunctionID.anyGetClassName)
    val valueParam = fb.addParam("value", RefType.any)
    fb.setResultType(RefType.any)

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

  /** `anyGetTypeData: (ref any) -> (ref null typeData)`.
   *
   *  Common code between `anyGetClass` and `anyGetClassName`.
   */
  private def genAnyGetTypeData()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.anyGetTypeData)
    val valueParam = fb.addParam("value", RefType.any)
    fb.setResultType(RefType.nullable(genTypeID.typeData))

    val doubleValueLocal = fb.addLocal("doubleValue", Float64)
    val intValueLocal = fb.addLocal("intValue", Int32)
    val ourObjectLocal = fb.addLocal("ourObject", RefType(genTypeID.ObjectStruct))

    def getHijackedClassTypeDataInstr(className: ClassName): Instr =
      GlobalGet(genGlobalID.forVTable(className))

    fb.block(RefType(genTypeID.ObjectStruct)) { ourObjectLabel =>
      // if value is our object, jump to $ourObject
      fb += LocalGet(valueParam)
      fb += BrOnCast(
        ourObjectLabel,
        RefType.any,
        RefType(genTypeID.ObjectStruct)
      )

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
        // case JSValueTypeNumber => ...
        List(JSValueTypeNumber) -> { () =>
          /* For `number`s, the result is based on the actual value, as specified by
           * [[https://www.scala-js.org/doc/semantics.html#getclass]].
           */

          // doubleValue := unboxDouble(value)
          fb += LocalGet(valueParam)
          fb += Call(genFunctionID.unbox(DoubleRef))
          fb += LocalTee(doubleValueLocal)

          // intValue := doubleValue.toInt
          fb += I32TruncSatF64S
          fb += LocalTee(intValueLocal)

          // if same(intValue.toDouble, doubleValue) -- same bit pattern to avoid +0.0 == -0.0
          fb += F64ConvertI32S
          fb += I64ReinterpretF64
          fb += LocalGet(doubleValueLocal)
          fb += I64ReinterpretF64
          fb += I64Eq
          fb.ifThenElse(typeDataType) {
            // then it is a Byte, a Short, or an Integer

            // if intValue.toByte.toInt == intValue
            fb += LocalGet(intValueLocal)
            fb += I32Extend8S
            fb += LocalGet(intValueLocal)
            fb += I32Eq
            fb.ifThenElse(typeDataType) {
              // then it is a Byte
              fb += getHijackedClassTypeDataInstr(BoxedByteClass)
            } {
              // else, if intValue.toShort.toInt == intValue
              fb += LocalGet(intValueLocal)
              fb += I32Extend16S
              fb += LocalGet(intValueLocal)
              fb += I32Eq
              fb.ifThenElse(typeDataType) {
                // then it is a Short
                fb += getHijackedClassTypeDataInstr(BoxedShortClass)
              } {
                // else, it is an Integer
                fb += getHijackedClassTypeDataInstr(BoxedIntegerClass)
              }
            }
          } {
            // else, it is a Float or a Double

            // if doubleValue.toFloat.toDouble == doubleValue
            fb += LocalGet(doubleValueLocal)
            fb += F32DemoteF64
            fb += F64PromoteF32
            fb += LocalGet(doubleValueLocal)
            fb += F64Eq
            fb.ifThenElse(typeDataType) {
              // then it is a Float
              fb += getHijackedClassTypeDataInstr(BoxedFloatClass)
            } {
              // else, if it is NaN
              fb += LocalGet(doubleValueLocal)
              fb += LocalGet(doubleValueLocal)
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
          fb += Return
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
    }

    /* Now we have one of our objects. Normally we only have to get the
     * vtable, but there are two exceptions. If the value is an instance of
     * `jl.CharacterBox` or `jl.LongBox`, we must use the typeData of
     * `jl.Character` or `jl.Long`, respectively.
     */
    fb += LocalTee(ourObjectLocal)
    fb += RefTest(RefType(genTypeID.forClass(SpecialNames.CharBoxClass)))
    fb.ifThenElse(typeDataType) {
      fb += getHijackedClassTypeDataInstr(BoxedCharacterClass)
    } {
      fb += LocalGet(ourObjectLocal)
      fb += RefTest(RefType(genTypeID.forClass(SpecialNames.LongBoxClass)))
      fb.ifThenElse(typeDataType) {
        fb += getHijackedClassTypeDataInstr(BoxedLongClass)
      } {
        fb += LocalGet(ourObjectLocal)
        fb += StructGet(genTypeID.ObjectStruct, genFieldID.objStruct.vtable)
      }
    }

    fb.buildAndAddToModule()
  }

  /** `newArrayObject`: `(ref typeData), (ref array i32), i32 -> (ref jl.Object)`.
   *
   *  The arguments are `arrayTypeData`, `lengths` and `lengthIndex`.
   *
   *  This recursive function creates a multi-dimensional array. The resulting array has type data
   *  `arrayTypeData` and length `lengths(lengthIndex)`. If `lengthIndex < `lengths.length - 1`, its
   *  elements are recursively initialized with `newArrayObject(arrayTypeData.componentType,
   *  lengths, lengthIndex - 1)`.
   */
  private def genNewArrayObject()(implicit ctx: WasmContext): Unit = {
    import genFieldID.typeData._

    val typeDataType = RefType(genTypeID.typeData)
    val i32ArrayType = RefType(genTypeID.i32Array)
    val objectVTableType = RefType(genTypeID.ObjectVTable)
    val arrayTypeDataType = objectVTableType
    val itablesType = RefType.nullable(genTypeID.itables)
    val nonNullObjectType = RefType(genTypeID.ObjectStruct)
    val anyArrayType = RefType(genTypeID.anyArray)

    val fb = newFunctionBuilder(genFunctionID.newArrayObject)
    val arrayTypeDataParam = fb.addParam("arrayTypeData", arrayTypeDataType)
    val lengthsParam = fb.addParam("lengths", i32ArrayType)
    val lengthIndexParam = fb.addParam("lengthIndex", Int32)
    fb.setResultType(nonNullObjectType)

    val lenLocal = fb.addLocal("len", Int32)
    val underlyingLocal = fb.addLocal("underlying", anyArrayType)
    val subLengthIndexLocal = fb.addLocal("subLengthIndex", Int32)
    val arrayComponentTypeDataLocal = fb.addLocal("arrayComponentTypeData", arrayTypeDataType)
    val iLocal = fb.addLocal("i", Int32)

    /* High-level pseudo code of what this function does:
     *
     * def newArrayObject(arrayTypeData, lengths, lengthIndex) {
     *   // create an array of the right primitive type
     *   val len = lengths(lengthIndex)
     *   // possibly: check negative array size
     *   switch (arrayTypeData.componentType.kind) {
     *     // for primitives, return without recursion
     *     case KindBoolean => new Array[Boolean](len)
     *     ...
     *     case KindDouble => new Array[Double](len)
     *
     *     // for reference array types, maybe recursively initialize
     *     case _ =>
     *       val result = new Array[Object](len) // with arrayTypeData as vtable
     *       val subLengthIndex = lengthIndex + 1
     *       if (subLengthIndex != lengths.length) {
     *         val arrayComponentTypeData = arrayTypeData.componentType
     *         for (i <- 0 until len)
     *           result(i) = newArrayObject(arrayComponentTypeData, lengths, subLengthIndex)
     *       }
     *       result
     *   }
     * }
     */

    val primRefsWithArrayTypes = List(
      BooleanRef -> KindBoolean,
      CharRef -> KindChar,
      ByteRef -> KindByte,
      ShortRef -> KindShort,
      IntRef -> KindInt,
      LongRef -> KindLong,
      FloatRef -> KindFloat,
      DoubleRef -> KindDouble
    )

    // Load the vtable and itable of the resulting array on the stack
    fb += LocalGet(arrayTypeDataParam) // vtable
    fb += GlobalGet(genGlobalID.arrayClassITable) // itable

    // Load the first length
    fb += LocalGet(lengthsParam)
    fb += LocalGet(lengthIndexParam)
    fb += ArrayGet(genTypeID.i32Array)

    // Check negative array size
    if (semantics.negativeArraySizes != CheckedBehavior.Unchecked) {
      fb += LocalTee(lenLocal)
      fb += I32Const(0)
      fb += I32LtS
      fb.ifThen() {
        fb += LocalGet(lenLocal)
        fb += Call(genFunctionID.throwNegativeArraySizeException)
        fb += Unreachable
      }
      fb += LocalGet(lenLocal)
    }

    // componentTypeData := ref_as_non_null(arrayTypeData.componentType)
    // switch (componentTypeData.kind)
    val switchClauseSig = FunctionType(
      List(arrayTypeDataType, itablesType, Int32),
      List(nonNullObjectType)
    )
    fb.switch(switchClauseSig) { () =>
      // scrutinee
      fb += LocalGet(arrayTypeDataParam)
      fb += StructGet(genTypeID.typeData, genFieldID.typeData.componentType)
      fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
    }(
      // For all the primitive types, by construction, this is the bottom dimension
      // case KindPrim => array.new_default underlyingPrimArray; struct.new PrimArray
      primRefsWithArrayTypes.map { case (primRef, kind) =>
        List(kind) -> { () =>
          val arrayTypeRef = ArrayTypeRef(primRef, 1)
          fb += ArrayNewDefault(genTypeID.underlyingOf(arrayTypeRef))
          fb += StructNew(genTypeID.forArrayClass(arrayTypeRef))
          () // required for correct type inference
        }
      }: _*
    ) { () =>
      // default -- all non-primitive array types

      // len := <top-of-stack> (which is the first length)
      fb += LocalTee(lenLocal)

      // underlying := array.new_default anyArray
      val arrayTypeRef = ArrayTypeRef(ClassRef(ObjectClass), 1)
      fb += ArrayNewDefault(genTypeID.underlyingOf(arrayTypeRef))
      fb += LocalSet(underlyingLocal)

      // subLengthIndex := lengthIndex + 1
      fb += LocalGet(lengthIndexParam)
      fb += I32Const(1)
      fb += I32Add
      fb += LocalTee(subLengthIndexLocal)

      // if subLengthIndex != lengths.length
      fb += LocalGet(lengthsParam)
      fb += ArrayLen
      fb += I32Ne
      fb.ifThen() {
        // then, recursively initialize all the elements

        // arrayComponentTypeData := ref_cast<arrayTypeDataType> arrayTypeData.componentTypeData
        fb += LocalGet(arrayTypeDataParam)
        fb += StructGet(genTypeID.typeData, genFieldID.typeData.componentType)
        fb += RefCast(RefType(arrayTypeDataType.heapType))
        fb += LocalSet(arrayComponentTypeDataLocal)

        // i := 0
        fb += I32Const(0)
        fb += LocalSet(iLocal)

        // while (i != len)
        fb.whileLoop() {
          fb += LocalGet(iLocal)
          fb += LocalGet(lenLocal)
          fb += I32Ne
        } {
          // underlying[i] := newArrayObject(arrayComponentType, lengths, subLengthIndex)

          fb += LocalGet(underlyingLocal)
          fb += LocalGet(iLocal)

          fb += LocalGet(arrayComponentTypeDataLocal)
          fb += LocalGet(lengthsParam)
          fb += LocalGet(subLengthIndexLocal)
          fb += Call(genFunctionID.newArrayObject)

          fb += ArraySet(genTypeID.anyArray)

          // i += 1
          fb += LocalGet(iLocal)
          fb += I32Const(1)
          fb += I32Add
          fb += LocalSet(iLocal)
        }
      }

      // load underlying; struct.new ObjectArray
      fb += LocalGet(underlyingLocal)
      fb += StructNew(genTypeID.forArrayClass(arrayTypeRef))
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
    import MemberNamespace.Public
    import SpecialNames.hashCodeMethodName
    import genFieldID.typeData._

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

    // throw new TypeError("...")
    fb ++= ctx.stringPool.getConstantStringInstr("TypeError")
    fb += Call(genFunctionID.jsGlobalRefGet)
    fb += Call(genFunctionID.jsNewArray)
    // Originally, exception is thrown from JS saying e.g. "obj2.z1__ is not a function"
    // TODO Improve the error message to include some information about the missing method
    fb ++= ctx.stringPool.getConstantStringInstr("Method not found")
    fb += Call(genFunctionID.jsArrayPush)
    fb += Call(genFunctionID.jsNew)
    fb += ExternConvertAny
    fb += Throw(genTagID.exception)

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
    val arrayClassType = RefType(arrayStructTypeID)

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
    fb += LocalGet(fromLocal)
    fb += StructGet(arrayStructTypeID, genFieldID.objStruct.vtable) // vtable
    fb += GlobalGet(genGlobalID.arrayClassITable) // itable
    fb += LocalGet(resultUnderlyingLocal)
    fb += StructNew(arrayStructTypeID)

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
      genNewScalaClass(fb, ArrayIndexOutOfBoundsExceptionClass,
          SpecialNames.StringArgConstructorName) {
        fb += RefNull(HeapType.None)
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
      fb += StructGet(arrayStructTypeID, genFieldID.objStruct.vtable)
      fb += LocalGet(srcParam)
      fb += StructGet(arrayStructTypeID, genFieldID.objStruct.vtable)
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
        genNewScalaClass(fb, ArrayStoreExceptionClass,
            SpecialNames.StringArgConstructorName) {
          fb += RefNull(HeapType.None)
        }
      }
      fb += ExternConvertAny
      fb += Throw(genTagID.exception)
    }

    fb.buildAndAddToModule()
  }

  private def maybeWrapInUBE(fb: FunctionBuilder, behavior: CheckedBehavior)(
      genExceptionInstance: => Unit): Unit = {
    if (behavior == CheckedBehavior.Fatal) {
      genNewScalaClass(fb, SpecialNames.UndefinedBehaviorErrorClass,
          SpecialNames.ThrowableArgConsructorName) {
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
