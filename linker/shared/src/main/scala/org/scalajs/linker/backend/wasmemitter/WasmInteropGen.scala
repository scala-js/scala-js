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
import org.scalajs.linker.backend.webassembly.{Identitities => wanme}
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

import SWasmGen._
import VarGen._
import TypeTransformer._

/** Generates conversions implementing the `@WasmImport`/`@WasmExport` ABI.
 *
 *  | Scala.js type   | Wasm type         |
 *  |-----------------|-------------------|
 *  | `Int`           | `i32`             |
 *  | `Long`          | `i64`             |
 *  | `Float`         | `f32`             |
 *  | `Double`        | `f64`             |
 *  | `Array[Byte]`   | `(ref $i8Array)`  |
 *  | `Array[Short]`  | `(ref $i16Array)` |
 *  | `Array[Int]`    | `(ref $i32Array)` |
 *  | `Array[Long]`   | `(ref $i64Array)` |
 *  | `Array[Float]`  | `(ref $f32Array)` |
 *  | `Array[Double]` | `(ref $f64Array)` |
 *  | `Unit` result   | no result         |
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

        val copyLocal = genUnderlyingArrayCopy(fb, genTypeID.underlyingOf(arrayTypeRef))
        fb += wa.LocalGet(copyLocal)

      case VoidType =>

      case tpe: PrimType if isSupportedWasmInteropPrimType(tpe) =>

      case _ =>
        throw new AssertionError(s"Unexpected $tpe")
    }
  }

  def genWasmToScala(fb: FunctionBuilder, tpe: Type)(implicit ctx: WasmContext): Unit = {
    tpe match {
      case ArrayType(arrayTypeRef, _, _) =>
        val underlyingLocal = genUnderlyingArrayCopy(fb, genTypeID.underlyingOf(arrayTypeRef))
        genArrayValueFromUnderlying(fb, arrayTypeRef) {
          fb += wa.LocalGet(underlyingLocal)
        }

      case VoidType =>

      case tpe: PrimType if isSupportedWasmInteropPrimType(tpe) =>

      case _ =>
        throw new AssertionError(s"Unexpected $tpe")
    }
  }

  /** Copies the array on the stack to a new array in the local.
   *
   *  @return the ID of the local containing the result
   */
  private def genUnderlyingArrayCopy(fb: FunctionBuilder,
      arrayTypeID: wanme.TypeID): wanme.LocalID = {

    val source = fb.addLocal(NoOriginalName, watpe.RefType(arrayTypeID))
    val dest = fb.addLocal(NoOriginalName, watpe.RefType(arrayTypeID))
    val length = fb.addLocal(NoOriginalName, watpe.Int32)

    // Allocate the new array
    fb += wa.LocalTee(source)
    fb += wa.ArrayLen
    fb += wa.LocalTee(length)
    fb += wa.ArrayNewDefault(arrayTypeID)
    fb += wa.LocalTee(dest)

    // Do the copy - `dest` is still on the stack
    fb += wa.I32Const(0)
    fb += wa.LocalGet(source)
    fb += wa.I32Const(0)
    fb += wa.LocalGet(length)
    fb += wa.ArrayCopy(arrayTypeID, arrayTypeID)

    dest
  }
}
