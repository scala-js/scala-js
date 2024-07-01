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

  def genBooleanNot(fb: FunctionBuilder): Unit = {
    fb += I32Const(1)
    fb += I32Xor
  }

  def genZeroOf(tpe: Type)(implicit ctx: WasmContext): Instr = {
    tpe match {
      case BooleanType | CharType | ByteType | ShortType | IntType =>
        I32Const(0)

      case LongType   => I64Const(0L)
      case FloatType  => F32Const(0.0f)
      case DoubleType => F64Const(0.0)
      case StringType => GlobalGet(genGlobalID.emptyString)
      case UndefType  => GlobalGet(genGlobalID.undef)

      case AnyType | ClassType(_) | ArrayType(_) | NullType =>
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
      case AnyType | ClassType(_) | ArrayType(_) | StringType | UndefType | NullType =>
        RefNull(Types.HeapType.None)

      case NoType | NothingType | _: RecordType =>
        throw new AssertionError(s"Unexpected type for field: ${tpe.show()}")
    }
  }

  def genLoadJSConstructor(fb: FunctionBuilder, className: ClassName)(implicit
      ctx: WasmContext
  ): Unit = {
    val info = ctx.getClassInfo(className)

    info.jsNativeLoadSpec match {
      case None =>
        // This is a non-native JS class
        fb += Call(genFunctionID.loadJSClass(className))

      case Some(loadSpec) =>
        genLoadJSFromSpec(fb, loadSpec)
    }
  }

  def genLoadJSFromSpec(fb: FunctionBuilder, loadSpec: JSNativeLoadSpec)(implicit
      ctx: WasmContext
  ): Unit = {
    def genFollowPath(path: List[String]): Unit = {
      for (prop <- path) {
        fb ++= ctx.getConstantStringInstr(prop)
        fb += Call(genFunctionID.jsSelect)
      }
    }

    loadSpec match {
      case JSNativeLoadSpec.Global(globalRef, path) =>
        fb ++= ctx.getConstantStringInstr(globalRef)
        fb += Call(genFunctionID.jsGlobalRefGet)
        genFollowPath(path)
      case JSNativeLoadSpec.Import(module, path) =>
        fb += GlobalGet(genGlobalID.forImportedModule(module))
        genFollowPath(path)
      case JSNativeLoadSpec.ImportWithGlobalFallback(importSpec, globalSpec) =>
        genLoadJSFromSpec(fb, importSpec)
    }
  }

}
