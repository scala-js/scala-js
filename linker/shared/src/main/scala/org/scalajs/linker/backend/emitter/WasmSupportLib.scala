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
package org.scalajs.linker.backend.emitter

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

object WasmSupportLib {
  val moduleBytes: Array[Byte] = new Builder().build()

  private object Builder {
    private final case class SimpleGlobalID(name: String) extends GlobalID
    private final case class SimpleFunctionID(name: String) extends FunctionID
  }

  private final class Builder {
    import Builder._

    private implicit val noPos: Position = Position.NoPosition

    private val moduleBuilder = new ModuleBuilder()

    def build(): Array[Byte] = {
      genImports()
      genGlobals()
      genFunctions()

      val buffer = BinaryWriter.write(moduleBuilder.build(), emitDebugInfo = false)
      val bytes = new Array[Byte](buffer.remaining())
      buffer.get(bytes)
      bytes
    }

    private object globals {
      val longResultHi = SimpleGlobalID("longResultHi")
    }

    private object funcs {
      val throwDivByZero = SimpleFunctionID("throwDivByZero")

      val floatToBits = SimpleFunctionID("floatToBits")
      val floatFromBits = SimpleFunctionID("floatFromBits")
      val doubleToBitsLo = SimpleFunctionID("doubleToBitsLo")
      val doubleToBitsHi = SimpleFunctionID("doubleToBitsHi")
      val doubleFromBits = SimpleFunctionID("doubleFromBits")
      val longMulHi = SimpleFunctionID("longMulHi")
      val longDivS = SimpleFunctionID("longDivS")
      val longRemS = SimpleFunctionID("longRemS")
      val longDivU = SimpleFunctionID("longDivU")
      val longRemU = SimpleFunctionID("longRemU")
    }

    private def addImportedFunc(functionID: SimpleFunctionID, typeID: TypeID): Unit = {
      moduleBuilder.addImport(Import(
        module = "",
        name = functionID.name,
        ImportDesc.Func(functionID, NoOriginalName, typeID)
      ))
    }

    private def addImportedFunc(functionID: SimpleFunctionID, sig: FunctionType): Unit =
      addImportedFunc(functionID, moduleBuilder.functionTypeToTypeID(sig))

    private def genImports(): Unit = {
      addImportedFunc(funcs.throwDivByZero, FunctionType(Nil, Nil))
    }

    private def addExportedGlobal(globalID: SimpleGlobalID, isMutable: Boolean,
        tpe: Type, init: Expr): Unit = {

      moduleBuilder.addExport(Export(globalID.name, ExportDesc.Global(globalID)))
      moduleBuilder.addGlobal(Global(globalID, NoOriginalName, isMutable, tpe, init))
    }

    private def genGlobals(): Unit = {
      addExportedGlobal(
        globals.longResultHi,
        isMutable = true,
        Int32,
        Expr(List(I32Const(0)))
      )
    }

    private def newFunctionBuilder(functionID: FunctionID): FunctionBuilder =
      new FunctionBuilder(moduleBuilder, functionID, NoOriginalName, Position.NoPosition)

    private def newExportedFunctionBuilder(functionID: SimpleFunctionID): FunctionBuilder = {
      moduleBuilder.addExport(Export(functionID.name, ExportDesc.Func(functionID)))
      newFunctionBuilder(functionID)
    }

    private def genFunctions(): Unit = {
      genFloatToBits()
      genFloatFromBits()
      genDoubleToBitsLo()
      genDoubleToBitsHi()
      genDoubleFromBits()
      genLongMulHi()
      genLongDivFuncs()
    }

    private def genFloatToBits(): Unit = {
      val fb = newExportedFunctionBuilder(funcs.floatToBits)
      val x = fb.addParam("x", Float32)
      fb.setResultType(Int32)

      fb += LocalGet(x)
      fb += I32ReinterpretF32

      fb.buildAndAddToModule()
    }

    private def genFloatFromBits(): Unit = {
      val fb = newExportedFunctionBuilder(funcs.floatFromBits)
      val x = fb.addParam("x", Int32)
      fb.setResultType(Float32)

      fb += LocalGet(x)
      fb += F32ReinterpretI32

      fb.buildAndAddToModule()
    }

    private def genDoubleToBitsLo(): Unit = {
      val fb = newExportedFunctionBuilder(funcs.doubleToBitsLo)
      val x = fb.addParam("x", Float64)
      fb.setResultType(Int32)

      fb += LocalGet(x)
      fb += I64ReinterpretF64
      fb += I32WrapI64

      fb.buildAndAddToModule()
    }

    private def genDoubleToBitsHi(): Unit = {
      val fb = newExportedFunctionBuilder(funcs.doubleToBitsHi)
      val x = fb.addParam("x", Float64)
      fb.setResultType(Int32)

      fb += LocalGet(x)
      fb += I64ReinterpretF64
      genExtractLongHi(fb)

      fb.buildAndAddToModule()
    }

    private def genDoubleFromBits(): Unit = {
      val fb = newExportedFunctionBuilder(funcs.doubleFromBits)
      val lo = fb.addParam("lo", Int32)
      val hi = fb.addParam("hi", Int32)
      fb.setResultType(Float64)

      genLocalGetLongArg(fb, lo, hi)
      fb += F64ReinterpretI64

      fb.buildAndAddToModule()
    }

    private def genLongMulHi(): Unit = {
      val fb = newExportedFunctionBuilder(funcs.longMulHi)
      val alo = fb.addParam("alo", Int32)
      val ahi = fb.addParam("ahi", Int32)
      val blo = fb.addParam("blo", Int32)
      val bhi = fb.addParam("bhi", Int32)
      fb.setResultType(Int32)

      genLocalGetLongArg(fb, alo, ahi)
      genLocalGetLongArg(fb, blo, bhi)
      fb += I64Mul
      genExtractLongHi(fb)

      fb.buildAndAddToModule()
    }

    private def genLongDivFuncs(): Unit = {
      val functionIDsAndInstrs: List[(SimpleFunctionID, SimpleInstr)] = List(
        funcs.longDivS -> I64DivS,
        funcs.longRemS -> I64RemS,
        funcs.longDivU -> I64DivU,
        funcs.longRemU -> I64RemU
      )

      for ((functionID, divInstr) <- functionIDsAndInstrs) {
        val fb = newExportedFunctionBuilder(functionID)
        val alo = fb.addParam("alo", Int32)
        val ahi = fb.addParam("ahi", Int32)
        val blo = fb.addParam("blo", Int32)
        val bhi = fb.addParam("bhi", Int32)
        fb.setResultType(Int32)

        /* Used both for b and then for the result.
         * The blob we generate is opaque to any downstream optimizer,
         * so exceptionally we are doing these low-level tricks ourselves.
         */
        val temp = fb.addLocal("temp", Int64)

        genLocalGetLongArg(fb, blo, bhi)
        fb += LocalTee(temp)

        // Division by 0
        fb += I64Eqz
        fb.ifThen() {
          fb += Call(funcs.throwDivByZero)
          fb += Unreachable
        }

        // Compute the result and leave it on the stack
        if (divInstr == I64DivS) {
          // Special-case when dividing by -1; need to handle the overflow case
          fb += LocalGet(temp)
          fb += I64Const(-1L)
          fb += I64Eq
          fb.ifThenElse(Int64) {
            fb += I64Const(0L)
            genLocalGetLongArg(fb, alo, ahi)
            fb += I64Sub
          } {
            genLocalGetLongArg(fb, alo, ahi)
            fb += LocalGet(temp)
            fb += divInstr
          }
        } else {
          // Regular path
          genLocalGetLongArg(fb, alo, ahi)
          fb += LocalGet(temp)
          fb += divInstr
        }

        // Separate (lo, hi)
        fb += LocalTee(temp)
        genExtractLongHi(fb)
        fb += GlobalSet(globals.longResultHi)
        fb += LocalGet(temp)
        fb += I32WrapI64

        fb.buildAndAddToModule()
      }
    }

    private def genLocalGetLongArg(fb: FunctionBuilder,
        lo: LocalID, hi: LocalID): Unit = {

      fb += LocalGet(lo)
      fb += I64ExtendI32U
      fb += LocalGet(hi)
      fb += I64ExtendI32U
      fb += I64Const(32L)
      fb += I64Shl
      fb += I64Or
    }

    private def genExtractLongHi(fb: FunctionBuilder): Unit = {
      fb += I64Const(32)
      fb += I64ShrU
      fb += I32WrapI64
    }
  }
}
