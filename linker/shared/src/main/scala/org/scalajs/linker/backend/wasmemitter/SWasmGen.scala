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

import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.Identitities._
import org.scalajs.linker.backend.webassembly.Instructions._

import VarGen._
import org.scalajs.linker.backend.webassembly.Types.HeapType
import org.scalajs.linker.backend.webassembly.Identitities.LocalID

/** Scala.js-specific Wasm generators that are used across the board. */
object SWasmGen {

  def hasNonDefaultZero(tpe: Type): Boolean = tpe match {
    case StringType | UndefType => true
    case _                      => false
  }

  def genZeroOf(tpe: Type)(implicit ctx: WasmContext): List[Instr] =
    List(genZeroOf0(tpe))

  private def genZeroOf0(tpe: Type)(implicit ctx: WasmContext): Instr = {
    tpe match {
      case BooleanType | CharType | ByteType | ShortType | IntType =>
        I32Const(0)

      case LongType   => I64Const(0L)
      case FloatType  => F32Const(0.0f)
      case DoubleType => F64Const(0.0)
      case StringType => ctx.stringPool.getEmptyStringInstr()
      case UndefType  => GlobalGet(genGlobalID.undef)

      case ClassType(BoxedStringClass, true, _) =>
        if (!ctx.hasJSInterop)
          RefNull(HeapType(genTypeID.wasmString))
        else
          RefNull(Types.HeapType.NoExtern)

      case AnyType | ClassType(_, true, _) | ArrayType(_, true, _) |
          ClosureType(_, _, true) | NullType =>
        RefNull(Types.HeapType.None)

      case NothingType | VoidType | ClassType(_, false, _) | ArrayType(_, false, _) |
          ClosureType(_, _, false) | AnyNotNullType | _:RecordType =>
        throw new AssertionError(s"Unexpected type for field: ${tpe.show()}")
    }
  }

  def genLoadTypeData(fb: FunctionBuilder, typeRef: TypeRef): Unit = typeRef match {
    case typeRef: NonArrayTypeRef  => genLoadNonArrayTypeData(fb, typeRef)
    case typeRef: ArrayTypeRef     => genLoadArrayTypeData(fb, typeRef)
    case typeRef: TransientTypeRef => throw new IllegalArgumentException(typeRef.toString())
  }

  def genLoadNonArrayTypeData(fb: FunctionBuilder, typeRef: NonArrayTypeRef): Unit =
    fb += GlobalGet(genGlobalID.forVTable(typeRef))

  def genLoadArrayTypeData(fb: FunctionBuilder, arrayTypeRef: ArrayTypeRef): Unit = {
    val ArrayTypeRef(base, dimensions) = arrayTypeRef

    base match {
      case ClassRef(ObjectClass) | _:PrimRef =>
        /* We can and must directly load level 1 of the array.
         * Then we load one fewer level of dimensions (possibly none at all).
         */
        fb += GlobalGet(genGlobalID.forArrayVTable(base))
        if (dimensions > 1) {
          fb += I32Const(dimensions - 1)
          fb += Call(genFunctionID.specificArrayTypeData)
        }
      case _ =>
        genLoadNonArrayTypeData(fb, base)
        fb += I32Const(dimensions)
        fb += Call(genFunctionID.specificArrayTypeData)
    }
  }

  def genArrayValue(fb: FunctionBuilder, arrayTypeRef: ArrayTypeRef, length: Int)(
      genElems: => Unit)(
      implicit ctx: WasmContext): Unit = {
    genArrayValueFromUnderlying(fb, arrayTypeRef) {
      // Create the underlying array
      genElems
      fb += ArrayNewFixed(genTypeID.underlyingOf(arrayTypeRef), length)
    }
  }

  def genArrayValueFromUnderlying(fb: FunctionBuilder, arrayTypeRef: ArrayTypeRef)(
      genUnderlying: => Unit)(
      implicit ctx: WasmContext): Unit = {
    genStructNewWithVTable(fb, genTypeID.forArrayClass(arrayTypeRef)) {
      genLoadArrayTypeData(fb, arrayTypeRef) // vtable
    } {
      if (!ctx.hasJSInterop)
        fb += I32Const(0) // idHashCode
      genUnderlying
    }
  }

  def genStructNewWithVTable(fb: FunctionBuilder, structTypeID: TypeID)(
      genVTable: => Unit)(genLoadFields: => Unit)(
      implicit ctx: WasmContext): Unit = {
    if (!ctx.useCustomDescriptors) {
      genVTable
      genLoadFields
      fb += StructNew(structTypeID)
    } else {
      genLoadFields
      genVTable
      fb += StructNewDesc(structTypeID)
    }
  }

  def genWasmStringFromCharCode(fb: FunctionBuilder): Unit = {
    fb += ArrayNewFixed(genTypeID.i16Array, 1)
    fb += I32Const(1)
    fb += RefNull(HeapType(genTypeID.wasmString))
    fb += StructNew(genTypeID.wasmString)
  }

  def genStringTest(fb: FunctionBuilder)(implicit ctx: WasmContext): Unit = {
    if (ctx.hasJSInterop) {
      fb += ExternConvertAny
      fb += Call(genFunctionID.stringBuiltins.test)
    } else {
      fb += RefTest(Types.RefType(genTypeID.wasmString))
    }
  }

  def genStringCast(fb: FunctionBuilder)(implicit ctx: WasmContext): Unit = {
    if (ctx.hasJSInterop) {
      fb += ExternConvertAny
    } else {
      fb += RefCast(Types.RefType.nullable(genTypeID.wasmString))
    }
  }

}
