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
      case StringType => ctx.stringPool.getConstantStringInstr("")
      case UndefType  => GlobalGet(genGlobalID.undef)

      case ClassType(BoxedStringClass, true) =>
        RefNull(Types.HeapType.NoExtern)

      case AnyType | ClassType(_, true) | ArrayType(_, true) | ClosureType(_, _, true) | NullType =>
        RefNull(Types.HeapType.None)

      case NothingType | VoidType | ClassType(_, false) | ArrayType(_, false) |
          ClosureType(_, _, false) | AnyNotNullType | _:RecordType =>
        throw new AssertionError(s"Unexpected type for field: ${tpe.show()}")
    }
  }

  def genLoadTypeData(fb: FunctionBuilder, typeRef: TypeRef): Unit = typeRef match {
    case typeRef: NonArrayTypeRef  => genLoadNonArrayTypeData(fb, typeRef)
    case typeRef: ArrayTypeRef     => genLoadArrayTypeData(fb, typeRef)
    case typeRef: TransientTypeRef => throw new IllegalArgumentException(typeRef.toString())
  }

  def genLoadNonArrayTypeData(fb: FunctionBuilder, typeRef: NonArrayTypeRef): Unit = {
    fb += GlobalGet(genGlobalID.forVTable(typeRef))
  }

  def genLoadArrayTypeData(fb: FunctionBuilder, arrayTypeRef: ArrayTypeRef): Unit = {
    genLoadNonArrayTypeData(fb, arrayTypeRef.base)
    fb += I32Const(arrayTypeRef.dimensions)
    fb += Call(genFunctionID.arrayTypeData)
  }

  def genArrayValue(fb: FunctionBuilder, arrayTypeRef: ArrayTypeRef, length: Int)(
      genElems: => Unit): Unit = {
    genArrayValueFromUnderlying(fb, arrayTypeRef) {
      // Create the underlying array
      genElems
      fb += ArrayNewFixed(genTypeID.underlyingOf(arrayTypeRef), length)
    }
  }

  def genArrayValueFromUnderlying(fb: FunctionBuilder, arrayTypeRef: ArrayTypeRef)(
      genUnderlying: => Unit): Unit = {
    genLoadArrayTypeData(fb, arrayTypeRef) // vtable
    genUnderlying
    fb += StructNew(genTypeID.forArrayClass(arrayTypeRef))
  }

}
