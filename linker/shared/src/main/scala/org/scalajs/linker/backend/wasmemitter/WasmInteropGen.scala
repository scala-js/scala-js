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

import org.scalajs.linker.backend.webassembly.FunctionBuilder
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

import SWasmGen._
import VarGen._
import TypeTransformer._

private[wasmemitter] object WasmInteropGen {
  def genScalaToWasm(fb: FunctionBuilder, tpe: Type)(implicit ctx: WasmContext): Unit = {
    tpe match {
      case ArrayType(arrayTypeRef, _, _) =>
        val arrayStructTypeID = genTypeID.forArrayClass(arrayTypeRef)
        val underlyingArrayTypeID = genTypeID.underlyingOf(arrayTypeRef)
        val valueLocal =
          fb.addLocal(NoOriginalName, watpe.RefType.nullable(arrayStructTypeID))

        fb += wa.LocalTee(valueLocal)
        fb += wa.RefIsNull
        fb.ifThenElse(watpe.RefType.nullable(underlyingArrayTypeID)) {
          fb += wa.RefNull(watpe.HeapType.None)
        } {
          fb += wa.LocalGet(valueLocal)
          fb += wa.RefAsNonNull
          fb += wa.StructGet(arrayStructTypeID, genFieldID.objStruct.arrayUnderlying)
        }

      case VoidType =>

      case tpe: PrimType if isSupportedWasmInteropPrimType(tpe) =>

      case _ =>
        throw new AssertionError(s"Unexpected $tpe")
    }
  }

  def genWasmToScala(fb: FunctionBuilder, tpe: Type)(implicit ctx: WasmContext): Unit = tpe match {
    case ArrayType(arrayTypeRef, _, _) =>
      val arrayStructTypeID = genTypeID.forArrayClass(arrayTypeRef)
      val underlyingArrayTypeID = genTypeID.underlyingOf(arrayTypeRef)
      val rawValueLocal =
        fb.addLocal(NoOriginalName, watpe.RefType.nullable(underlyingArrayTypeID))

      fb += wa.LocalTee(rawValueLocal)
      fb += wa.RefIsNull
      fb.ifThenElse(watpe.RefType.nullable(arrayStructTypeID)) {
        fb += wa.RefNull(watpe.HeapType.None)
      } {
        SWasmGen.genLoadArrayTypeData(fb, arrayTypeRef)
        fb += wa.LocalGet(rawValueLocal)
        fb += wa.RefAsNonNull
        fb += wa.StructNew(arrayStructTypeID)
      }

    case VoidType =>

    case tpe: PrimType if isSupportedWasmInteropPrimType(tpe) =>

    case _ =>
      throw new AssertionError(s"Unexpected $tpe")
  }
}
