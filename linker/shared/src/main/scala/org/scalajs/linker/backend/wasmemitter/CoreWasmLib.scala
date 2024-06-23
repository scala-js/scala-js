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
import org.scalajs.ir.Trees.{JSUnaryOp, JSBinaryOp, MemberNamespace}
import org.scalajs.ir.Types.{Type => _, ArrayType => _, _}
import org.scalajs.ir.{OriginalName, Position}

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.Instructions._
import org.scalajs.linker.backend.webassembly.Identitities._
import org.scalajs.linker.backend.webassembly.Modules._
import org.scalajs.linker.backend.webassembly.Types._

import EmbeddedConstants._
import VarGen._
import TypeTransformer._

object CoreWasmLib {
  import RefType.anyref

  private implicit val noPos: Position = Position.NoPosition

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

    genTags()

    genGlobalImports()
    genPrimitiveTypeDataGlobals()

    genHelperImports()
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
            genFieldID.reflectiveProxy.func_name,
            OriginalName(genFieldID.reflectiveProxy.func_name.toString()),
            Int32,
            isMutable = false
          ),
          StructField(
            genFieldID.reflectiveProxy.func_ref,
            OriginalName(genFieldID.reflectiveProxy.func_ref.toString()),
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

  private def genTags()(implicit ctx: WasmContext): Unit = {
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
    def addGlobalHelperImport(
        id: genGlobalID.JSHelperGlobalID,
        isMutable: Boolean,
        tpe: Type
    ): Unit = {
      ctx.moduleBuilder.addImport(
        Import(
          "__scalaJSHelpers",
          id.toString(), // import name, guaranteed by JSHelperGlobalID
          ImportDesc.Global(id, OriginalName(id.toString()), isMutable, tpe)
        )
      )
    }

    addGlobalHelperImport(genGlobalID.undef, isMutable = false, RefType.any)
    addGlobalHelperImport(genGlobalID.bFalse, isMutable = false, RefType.any)
    addGlobalHelperImport(genGlobalID.bZero, isMutable = false, RefType.any)
    addGlobalHelperImport(genGlobalID.emptyString, isMutable = false, RefType.any)
    addGlobalHelperImport(genGlobalID.idHashCodeMap, isMutable = false, RefType.extern)
  }

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
        ctx.getConstantStringDataInstr(primRef.displayName)

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

  private def genHelperImports()(implicit ctx: WasmContext): Unit = {
    import RefType.anyref

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

    for (primRef <- List(BooleanRef, ByteRef, ShortRef, IntRef, FloatRef, DoubleRef)) {
      val wasmType = primRef match {
        case FloatRef  => Float32
        case DoubleRef => Float64
        case _         => Int32
      }
      addHelperImport(genFunctionID.box(primRef), List(wasmType), List(anyref))
      addHelperImport(genFunctionID.unbox(primRef), List(anyref), List(wasmType))
      addHelperImport(genFunctionID.typeTest(primRef), List(anyref), List(Int32))
    }

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
    addHelperImport(genFunctionID.jsNewArray, Nil, List(anyref))
    addHelperImport(genFunctionID.jsArrayPush, List(anyref, anyref), List(anyref))
    addHelperImport(
      genFunctionID.jsArraySpreadPush,
      List(anyref, anyref),
      List(anyref)
    )
    addHelperImport(genFunctionID.jsNewObject, Nil, List(anyref))
    addHelperImport(
      genFunctionID.jsObjectPush,
      List(anyref, anyref, anyref),
      List(anyref)
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
    addHelperImport(genFunctionID.jsLinkingInfo, Nil, List(anyref))

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

  /** Generates all the non-type definitions of the core Wasm lib. */
  private def genHelperDefinitions()(implicit ctx: WasmContext): Unit = {
    genStringLiteral()
    genCreateStringFromData()
    genTypeDataName()
    genCreateClassOf()
    genGetClassOf()
    genArrayTypeData()
    genIsInstance()
    genIsAssignableFromExternal()
    genIsAssignableFrom()
    genCheckCast()
    genGetComponentType()
    genNewArrayOfThisClass()
    genAnyGetClass()
    genNewArrayObject()
    genIdentityHashCode()
    genSearchReflectiveProxy()
    genArrayCloneFunctions()
  }

  private def newFunctionBuilder(functionID: FunctionID, originalName: OriginalName)(
      implicit ctx: WasmContext): FunctionBuilder = {
    new FunctionBuilder(ctx.moduleBuilder, functionID, originalName, noPos)
  }

  private def newFunctionBuilder(functionID: FunctionID)(
      implicit ctx: WasmContext): FunctionBuilder = {
    newFunctionBuilder(functionID, OriginalName(functionID.toString()))
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

      // i := i - 1
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
    fb ++= ctx.getConstantStringInstr("__typeData")
    fb += LocalGet(typeDataParam)
    fb += Call(genFunctionID.jsObjectPush)
    // "name": typeDataName(typeData)
    fb ++= ctx.getConstantStringInstr("name")
    fb += LocalGet(typeDataParam)
    fb += Call(genFunctionID.typeDataName)
    fb += Call(genFunctionID.jsObjectPush)
    // "isPrimitive": (typeData.kind <= KindLastPrimitive)
    fb ++= ctx.getConstantStringInstr("isPrimitive")
    fb += LocalGet(typeDataParam)
    fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
    fb += I32Const(KindLastPrimitive)
    fb += I32LeU
    fb += Call(genFunctionID.box(BooleanRef))
    fb += Call(genFunctionID.jsObjectPush)
    // "isArrayClass": (typeData.kind == KindArray)
    fb ++= ctx.getConstantStringInstr("isArrayClass")
    fb += LocalGet(typeDataParam)
    fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
    fb += I32Const(KindArray)
    fb += I32Eq
    fb += Call(genFunctionID.box(BooleanRef))
    fb += Call(genFunctionID.jsObjectPush)
    // "isInterface": (typeData.kind == KindInterface)
    fb ++= ctx.getConstantStringInstr("isInterface")
    fb += LocalGet(typeDataParam)
    fb += StructGet(genTypeID.typeData, genFieldID.typeData.kind)
    fb += I32Const(KindInterface)
    fb += I32Eq
    fb += Call(genFunctionID.box(BooleanRef))
    fb += Call(genFunctionID.jsObjectPush)
    // "isInstance": closure(isInstance, typeData)
    fb ++= ctx.getConstantStringInstr("isInstance")
    fb += ctx.refFuncWithDeclaration(genFunctionID.isInstance)
    fb += LocalGet(typeDataParam)
    fb += Call(genFunctionID.closure)
    fb += Call(genFunctionID.jsObjectPush)
    // "isAssignableFrom": closure(isAssignableFrom, typeData)
    fb ++= ctx.getConstantStringInstr("isAssignableFrom")
    fb += ctx.refFuncWithDeclaration(genFunctionID.isAssignableFromExternal)
    fb += LocalGet(typeDataParam)
    fb += Call(genFunctionID.closure)
    fb += Call(genFunctionID.jsObjectPush)
    // "checkCast": closure(checkCast, typeData)
    fb ++= ctx.getConstantStringInstr("checkCast")
    fb += ctx.refFuncWithDeclaration(genFunctionID.checkCast)
    fb += LocalGet(typeDataParam)
    fb += Call(genFunctionID.closure)
    fb += Call(genFunctionID.jsObjectPush)
    // "getComponentType": closure(getComponentType, typeData)
    fb ++= ctx.getConstantStringInstr("getComponentType")
    fb += ctx.refFuncWithDeclaration(genFunctionID.getComponentType)
    fb += LocalGet(typeDataParam)
    fb += Call(genFunctionID.closure)
    fb += Call(genFunctionID.jsObjectPush)
    // "newArrayOfThisClass": closure(newArrayOfThisClass, typeData)
    fb ++= ctx.getConstantStringInstr("newArrayOfThisClass")
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
            fb += ctx.refFuncWithDeclaration(genFunctionID.clone(BooleanRef))
          },
          List(KindChar) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.clone(CharRef))
          },
          List(KindByte) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.clone(ByteRef))
          },
          List(KindShort) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.clone(ShortRef))
          },
          List(KindInt) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.clone(IntRef))
          },
          List(KindLong) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.clone(LongRef))
          },
          List(KindFloat) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.clone(FloatRef))
          },
          List(KindDouble) -> { () =>
            fb += ctx.refFuncWithDeclaration(genFunctionID.clone(DoubleRef))
          }
        ) { () =>
          fb += ctx.refFuncWithDeclaration(
            genFunctionID.clone(ClassRef(ObjectClass))
          )
        }

        // isJSClassInstance
        fb += RefNull(HeapType.NoFunc)

        // reflectiveProxies
        fb += ArrayNewFixed(genTypeID.reflectiveProxies, 0) // TODO

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

  /** `isInstance: (ref typeData), anyref -> i32` (a boolean).
   *
   *  Tests whether the given value is a non-null instance of the given type.
   *
   *  Specified by `"isInstance"` at
   *  [[https://lampwww.epfl.ch/~doeraene/sjsir-semantics/#sec-sjsir-createclassdataof]].
   */
  private def genIsInstance()(implicit ctx: WasmContext): Unit = {
    import genFieldID.typeData._

    val typeDataType = RefType(genTypeID.typeData)
    val objectRefType = RefType(genTypeID.forClass(ObjectClass))

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
        fb ++= ctx.getConstantStringInstr("TypeError")
        fb += Call(genFunctionID.jsGlobalRefGet)
        fb += Call(genFunctionID.jsNewArray)
        fb ++= ctx.getConstantStringInstr(
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
        fb += BrOnCast(
          ourObjectLabel,
          RefType.any,
          RefType(objectRefType.heapType)
        )

        // on cast fail, return false
        fb += I32Const(0)
        fb += Return
      }
      fb += StructGet(
        genTypeID.forClass(ObjectClass),
        genFieldID.objStruct.vtable
      )

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
    fb.setResultType(Int32)

    // load typeData
    fb += LocalGet(typeDataParam)

    // load ref.cast<typeData> from["__typeData"] (as a JS selection)
    fb += LocalGet(fromParam)
    fb ++= ctx.getConstantStringInstr("__typeData")
    fb += Call(genFunctionID.jsSelect)
    fb += RefCast(RefType(typeDataType.heapType))

    // delegate to isAssignableFrom
    fb += Call(genFunctionID.isAssignableFrom)

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

  /** `checkCast: (ref typeData), anyref -> anyref`.
   *
   *  Casts the given value to the given type; subject to undefined behaviors.
   */
  private def genCheckCast()(implicit ctx: WasmContext): Unit = {
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.checkCast)
    val typeDataParam = fb.addParam("typeData", typeDataType)
    val valueParam = fb.addParam("value", RefType.anyref)
    fb.setResultType(RefType.anyref)

    /* Given that we only implement `CheckedBehavior.Unchecked` semantics for
     * now, this is always the identity.
     */

    fb += LocalGet(valueParam)

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
    fb ++= ctx.getConstantStringInstr("length")
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
    val typeDataType = RefType(genTypeID.typeData)

    val fb = newFunctionBuilder(genFunctionID.anyGetClass)
    val valueParam = fb.addParam("value", RefType.any)
    fb.setResultType(RefType.nullable(genTypeID.ClassStruct))

    val typeDataLocal = fb.addLocal("typeData", typeDataType)
    val doubleValueLocal = fb.addLocal("doubleValue", Float64)
    val intValueLocal = fb.addLocal("intValue", Int32)
    val ourObjectLocal = fb.addLocal("ourObject", RefType(genTypeID.ObjectStruct))

    def getHijackedClassTypeDataInstr(className: ClassName): Instr =
      GlobalGet(genGlobalID.forVTable(className))

    fb.block(RefType.nullable(genTypeID.ClassStruct)) { nonNullClassOfLabel =>
      fb.block(typeDataType) { gotTypeDataLabel =>
        fb.block(RefType(genTypeID.ObjectStruct)) { ourObjectLabel =>
          // if value is our object, jump to $ourObject
          fb += LocalGet(valueParam)
          fb += BrOnCast(
            ourObjectLabel,
            RefType.any,
            RefType(genTypeID.ObjectStruct)
          )

          // switch(jsValueType(value)) { ... }
          fb.switch(typeDataType) { () =>
            // scrutinee
            fb += LocalGet(valueParam)
            fb += Call(genFunctionID.jsValueType)
          }(
            // case JSValueTypeFalse, JSValueTypeTrue => typeDataOf[jl.Boolean]
            List(JSValueTypeFalse, JSValueTypeTrue) -> { () =>
              fb += getHijackedClassTypeDataInstr(BoxedBooleanClass)
            },
            // case JSValueTypeString => typeDataOf[jl.String]
            List(JSValueTypeString) -> { () =>
              fb += getHijackedClassTypeDataInstr(BoxedStringClass)
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
            },
            // case JSValueTypeUndefined => typeDataOf[jl.Void]
            List(JSValueTypeUndefined) -> { () =>
              fb += getHijackedClassTypeDataInstr(BoxedUnitClass)
            }
          ) { () =>
            // case _ (JSValueTypeOther) => return null
            fb += RefNull(HeapType(genTypeID.ClassStruct))
            fb += Return
          }

          fb += Br(gotTypeDataLabel)
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
            fb += StructGet(
              genTypeID.forClass(ObjectClass),
              genFieldID.objStruct.vtable
            )
          }
        }
      }

      fb += Call(genFunctionID.getClassOf)
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

    // Load the vtable and itable or the resulting array on the stack
    fb += LocalGet(arrayTypeDataParam) // vtable
    fb += GlobalGet(genGlobalID.arrayClassITable) // itable

    // Load the first length
    fb += LocalGet(lengthsParam)
    fb += LocalGet(lengthIndexParam)
    fb += ArrayGet(genTypeID.i32Array)

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

      // if (methodID < reflectiveProxies[mid].func_name)
      fb += LocalGet(methodIDParam)
      fb += LocalGet(reflectiveProxies)
      fb += LocalGet(midLocal)
      fb += ArrayGet(genTypeID.reflectiveProxies)
      fb += StructGet(genTypeID.reflectiveProxy, genFieldID.reflectiveProxy.func_name)
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

      // if (entry.func_name == methodID)
      fb += StructGet(genTypeID.reflectiveProxy, genFieldID.reflectiveProxy.func_name)
      fb += LocalGet(methodIDParam)
      fb += I32Eq
      fb.ifThen() {
        // return entry.func_ref
        fb += LocalGet(entryLocal)
        fb += StructGet(genTypeID.reflectiveProxy, genFieldID.reflectiveProxy.func_ref)
        fb += Return
      }
    }

    // throw new TypeError("...")
    fb ++= ctx.getConstantStringInstr("TypeError")
    fb += Call(genFunctionID.jsGlobalRefGet)
    fb += Call(genFunctionID.jsNewArray)
    // Originally, exception is thrown from JS saying e.g. "obj2.z1__ is not a function"
    fb ++= ctx.getConstantStringInstr("Method not found")
    fb += Call(genFunctionID.jsArrayPush)
    fb += Call(genFunctionID.jsNew)
    fb += ExternConvertAny
    fb += Throw(genTagID.exception)

    fb.buildAndAddToModule()
  }

  private def genArrayCloneFunctions()(implicit ctx: WasmContext): Unit = {
    val baseRefs = List(
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

    for (baseRef <- baseRefs)
      genArrayCloneFunction(baseRef)
  }

  /** Generates the clone function for the array class with the given base. */
  private def genArrayCloneFunction(baseRef: NonArrayTypeRef)(implicit ctx: WasmContext): Unit = {
    val charCodeForOriginalName = baseRef match {
      case baseRef: PrimRef => baseRef.charCode
      case _: ClassRef      => 'O'
    }
    val originalName = OriginalName("cloneArray." + charCodeForOriginalName)

    val fb = newFunctionBuilder(genFunctionID.clone(baseRef), originalName)
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

}
