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

import scala.collection.mutable

import org.scalajs.ir.OriginalName

import org.scalajs.linker.backend.webassembly.Instructions._
import org.scalajs.linker.backend.webassembly.Modules._
import org.scalajs.linker.backend.webassembly.Types._

import VarGen._

private[wasmemitter] final class StringPool {
  import StringPool._

  private val registeredStrings = new mutable.AnyRefMap[String, StringData]
  private val rawData = new mutable.ArrayBuffer[Byte]()
  private var nextIndex: Int = 0

  /** Registers the given constant string ahead of requiring instructions for it. */
  def register(str: String): Unit =
    registerInternal(str)

  /** Registers the given constant string and returns its allocated data. */
  private def registerInternal(str: String): StringData = {
    registeredStrings.get(str) match {
      case Some(data) =>
        data

      case None =>
        // Compute new entry
        val bytes = str.toCharArray.flatMap { char =>
          Array((char & 0xFF).toByte, (char >> 8).toByte)
        }
        val offset = rawData.size
        val data = StringData(nextIndex, offset)

        // Enter the new entry in our state
        registeredStrings(str) = data
        rawData ++= bytes
        nextIndex += 1

        data
    }
  }

  /** Returns the list of instructions that load the given constant string.
   *
   *  The resulting list is *not* a Wasm constant expression, since it includes
   *  a `call` to the helper function `stringLiteral`.
   */
  def getConstantStringInstr(str: String): List[Instr] =
    getConstantStringDataInstr(str) :+ Call(genFunctionID.stringLiteral)

  /** Returns the list of 3 constant integers that must be passed to `stringLiteral`.
   *
   *  The resulting list is a Wasm constant expression, and hence can be used
   *  in the initializer of globals.
   */
  def getConstantStringDataInstr(str: String): List[I32Const] = {
    val data = registerInternal(str)
    List(
      I32Const(data.offset),
      I32Const(str.length()),
      I32Const(data.constantStringIndex)
    )
  }

  def genPool()(implicit ctx: WasmContext): Unit = {
    ctx.moduleBuilder.addData(
      Data(
        genDataID.string,
        OriginalName("stringPool"),
        rawData.toArray,
        Data.Mode.Passive
      )
    )

    ctx.addGlobal(
      Global(
        genGlobalID.stringLiteralCache,
        OriginalName("stringLiteralCache"),
        isMutable = false,
        RefType(genTypeID.anyArray),
        Expr(
          List(
            I32Const(nextIndex), // number of entries in the pool
            ArrayNewDefault(genTypeID.anyArray)
          )
        )
      )
    )
  }
}

private[wasmemitter] object StringPool {
  private final case class StringData(constantStringIndex: Int, offset: Int)
}
