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
import org.scalajs.ir.OriginalName.NoOriginalName

import org.scalajs.linker.interface.CheckedBehavior

import org.scalajs.linker.backend.webassembly.FunctionBuilder
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

import SWasmGen._
import VarGen._
import TypeTransformer._

/** Generates conversions implementing the `@WasmImport`/`@WasmExport` ABI
 *
 *  | Scala.js type          | Wasm type               |
 *  |------------------------|-------------------------|
 *  | `Boolean`              | `i32`                   |
 *  | `Byte`, `Short`, `Int` | `i32`                   |
 *  | `Long`                 | `i64`                   |
 *  | `Float`                | `f32`                   |
 *  | `Double`               | `f64`                   |
 *  | `Array[Byte]`          | `(ref $i8Array)`        |
 *  | `Array[Short]`         | `(ref $i16Array)`       |
 *  | `Array[Int]`           | `(ref $i32Array)`       |
 *  | `Array[Long]`          | `(ref $i64Array)`       |
 *  | `Array[Float]`         | `(ref $f32Array)`       |
 *  | `Array[Double]`        | `(ref $f64Array)`       |
 *  | `Unit` result          | no result               |
 *
 *  Passing a `null` Scala.js array to an imported function,
 *  or returning one from an exported function, is an undefined
 *  behavior NPE. The other direction is ruled out by Wasm type validation since
 *  the boundary array type is non-nullable.
 */
private[wasmemitter] object WasmInteropGen {

  def genScalaToWasm(fb: FunctionBuilder, tpe: Type)(implicit ctx: WasmContext): Unit = {
    tpe match {
      case ArrayType(arrayTypeRef, _, _) =>
        val arrayStructTypeID = genTypeID.forArrayClass(arrayTypeRef)

        if (ctx.coreSpec.semantics.nullPointers == CheckedBehavior.Unchecked) {
          fb += wa.RefAsNonNull
        } else {
          val nullableType = watpe.RefType.nullable(arrayStructTypeID)
          val nonNullType = watpe.RefType(arrayStructTypeID)
          fb.block(watpe.FunctionType(List(nullableType), List(nonNullType))) { nonNullLabel =>
            fb += wa.BrOnNonNull(nonNullLabel)
            fb += wa.Call(genFunctionID.throwNullPointerException)
            fb += wa.Unreachable
          }
        }
        fb += wa.StructGet(arrayStructTypeID, genFieldID.objStruct.arrayUnderlying)

      case VoidType =>

      case tpe: PrimType if isSupportedWasmInteropPrimType(tpe) =>

      case _ =>
        throw new AssertionError(s"Unexpected $tpe")
    }
  }

  def genWasmToScala(fb: FunctionBuilder, tpe: Type)(implicit ctx: WasmContext): Unit = {
    tpe match {
      case ArrayType(arrayTypeRef, _, _) =>
        val arrayStructTypeID = genTypeID.forArrayClass(arrayTypeRef)
        val underlyingArrayTypeID = genTypeID.underlyingOf(arrayTypeRef)
        val rawValueLocal =
          fb.addLocal(NoOriginalName, watpe.RefType(underlyingArrayTypeID))

        fb += wa.LocalSet(rawValueLocal)
        SWasmGen.genLoadArrayTypeData(fb, arrayTypeRef)
        if (!ctx.hasJSInterop)
          fb += wa.I32Const(0) // idHashCode
        fb += wa.LocalGet(rawValueLocal)
        fb += wa.StructNew(arrayStructTypeID)

      case VoidType =>

      case tpe: PrimType if isSupportedWasmInteropPrimType(tpe) =>

      case _ =>
        throw new AssertionError(s"Unexpected $tpe")
    }
  }
}
