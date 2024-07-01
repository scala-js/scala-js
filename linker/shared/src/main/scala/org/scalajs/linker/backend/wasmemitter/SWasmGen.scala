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
import org.scalajs.ir.Trees.JSNativeLoadSpec
import org.scalajs.ir.Types._

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.Instructions._

import VarGen._

/** Scala.js-specific Wasm generators that are used across the board. */
object SWasmGen {

  def genZeroOf(tpe: Type)(implicit ctx: WasmContext): Instr = {
    tpe match {
      case BooleanType | CharType | ByteType | ShortType | IntType =>
        I32Const(0)

      case LongType   => I64Const(0L)
      case FloatType  => F32Const(0.0f)
      case DoubleType => F64Const(0.0)
      case StringType => GlobalGet(genGlobalID.emptyString)
      case UndefType  => GlobalGet(genGlobalID.undef)

      case AnyType | ClassType(_) | ArrayType(_) | NullType | ClosureType(_, _) =>
        RefNull(Types.HeapType.None)

      case NoType | NothingType | _: RecordType =>
        throw new AssertionError(s"Unexpected type for field: ${tpe.show()}")
    }
  }

  def genBoxedZeroOf(tpe: Type)(implicit ctx: WasmContext): Instr = {
    tpe match {
      case BooleanType =>
        GlobalGet(genGlobalID.bFalse)
      case CharType =>
        GlobalGet(genGlobalID.bZeroChar)
      case ByteType | ShortType | IntType | FloatType | DoubleType =>
        GlobalGet(genGlobalID.bZero)
      case LongType =>
        GlobalGet(genGlobalID.bZeroLong)
      case AnyType | ClassType(_) | ArrayType(_) | StringType | UndefType | NullType | ClosureType(_, _) =>
        RefNull(Types.HeapType.None)

      case NoType | NothingType | _: RecordType =>
        throw new AssertionError(s"Unexpected type for field: ${tpe.show()}")
    }
  }

  def genLoadTypeData(fb: FunctionBuilder, typeRef: TypeRef): Unit = typeRef match {
    case typeRef: NonArrayTypeRef => genLoadNonArrayTypeData(fb, typeRef)
    case typeRef: ArrayTypeRef    => genLoadArrayTypeData(fb, typeRef)
    case typeRef: ClosureTypeRef  => throw new IllegalArgumentException(typeRef.toString())
  }

  def genLoadNonArrayTypeData(fb: FunctionBuilder, typeRef: NonArrayTypeRef): Unit = {
    fb += GlobalGet(genGlobalID.forVTable(typeRef))
  }

  def genLoadArrayTypeData(fb: FunctionBuilder, arrayTypeRef: ArrayTypeRef): Unit = {
    genLoadNonArrayTypeData(fb, arrayTypeRef.base)
    fb += I32Const(arrayTypeRef.dimensions)
    fb += Call(genFunctionID.arrayTypeData)
  }

  /** Gen code to load the vtable and the itable of the given array type. */
  def genLoadVTableAndITableForArray(fb: FunctionBuilder, arrayTypeRef: ArrayTypeRef): Unit = {
    // Load the typeData of the resulting array type. It is the vtable of the resulting object.
    genLoadArrayTypeData(fb, arrayTypeRef)

    // Load the itables for the array type
    fb += GlobalGet(genGlobalID.arrayClassITable)
  }

  def genArrayValue(fb: FunctionBuilder, arrayTypeRef: ArrayTypeRef, length: Int)(
      genElems: => Unit): Unit = {
    genLoadVTableAndITableForArray(fb, arrayTypeRef)

    // Create the underlying array
    genElems
    val underlyingArrayType = genTypeID.underlyingOf(arrayTypeRef)
    fb += ArrayNewFixed(underlyingArrayType, length)

    // Create the array object
    fb += StructNew(genTypeID.forArrayClass(arrayTypeRef))
  }

  def genLoadJSConstructor(fb: FunctionBuilder, className: ClassName)(
      implicit ctx: WasmContext): Unit = {
    val info = ctx.getClassInfo(className)

    info.jsNativeLoadSpec match {
      case None =>
        // This is a non-native JS class
        fb += Call(genFunctionID.loadJSClass(className))

      case Some(loadSpec) =>
        genLoadJSFromSpec(fb, loadSpec)
    }
  }

  def genLoadJSFromSpec(fb: FunctionBuilder, loadSpec: JSNativeLoadSpec)(
      implicit ctx: WasmContext): Unit = {
    def genFollowPath(path: List[String]): Unit = {
      for (prop <- path) {
        fb ++= ctx.stringPool.getConstantStringInstr(prop)
        fb += Call(genFunctionID.jsSelect)
      }
    }

    loadSpec match {
      case JSNativeLoadSpec.Global(globalRef, path) =>
        fb ++= ctx.stringPool.getConstantStringInstr(globalRef)
        fb += Call(genFunctionID.jsGlobalRefGet)
        genFollowPath(path)
      case JSNativeLoadSpec.Import(module, path) =>
        fb += GlobalGet(genGlobalID.forImportedModule(module))
        genFollowPath(path)
      case JSNativeLoadSpec.ImportWithGlobalFallback(importSpec, _) =>
        genLoadJSFromSpec(fb, importSpec)
    }
  }

}
