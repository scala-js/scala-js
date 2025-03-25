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

package org.scalajs.linker.backend.webassembly

import Instructions._
import Types._

/** Instructions grouped by type, to generically write algorithms on various datatypes.
 *
 *  The instructions here are not exhaustive. We include exactly what we need.
 */
object GenericInstructions {
  /** Instructions on `i32`, `i64`, `f32` or `f64`. */
  abstract class NumericInstrs {
    type Value

    val tpe: Type
    val bits: Int

    def Const(value: Value): Instr
    def ConstFromInt(value: Int): Instr

    val Zero: Instr
    val One: Instr
  }

  /** Instructions on `i32` or `i64`. */
  abstract class IntInstrs extends NumericInstrs {
    def ConstFromLong(value: Long): Instr

    val Eqz: Instr
    val Eq: Instr
    val LeU: Instr

    val Clz: Instr

    val Sub: Instr
    val DivS: Instr
    val RemS: Instr

    val And: Instr
    val Or: Instr
    val Shl: Instr
    val ShrU: Instr

    val ReinterpretF: Instr
  }

  /** Instructions on `i32`. */
  object I32 extends IntInstrs {
    type Value = Int

    val tpe = Int32
    val bits = 32

    def Const(value: Int): Instr = I32Const(value)
    def ConstFromInt(value: Int): Instr = I32Const(value)
    def ConstFromLong(value: Long): Instr = I32Const(value.toInt)

    val Zero = I32Const(0)
    val One = I32Const(1)

    val Eqz = I32Eqz
    val Eq = I32Eq
    val LeU = I32LeU

    val Clz = I32Clz

    val Sub = I32Sub
    val DivS = I32DivS
    val RemS = I32RemS

    val And = I32And
    val Or = I32Or
    val Shl = I32Shl
    val ShrU = I32ShrU

    val ReinterpretF = I32ReinterpretF32
  }

  /** Instructions on `i64`. */
  object I64 extends IntInstrs {
    type Value = Long

    val tpe = Int64
    val bits = 64

    def Const(value: Long): Instr = I64Const(value)
    def ConstFromInt(value: Int): Instr = I64Const(value.toLong)
    def ConstFromLong(value: Long): Instr = I64Const(value)

    val Zero = I64Const(0L)
    val One = I64Const(1L)

    val Eqz = I64Eqz
    val Eq = I64Eq
    val LeU = I64LeU

    val Clz = I64Clz

    val Sub = I64Sub
    val DivS = I64DivS
    val RemS = I64RemS

    val And = I64And
    val Or = I64Or
    val Shl = I64Shl
    val ShrU = I64ShrU

    val ReinterpretF = I64ReinterpretF64
  }

  /** Instructions on `f32` or `f64`. */
  abstract class FloatInstrs extends NumericInstrs {
    def intInstrs: IntInstrs

    val mbits: Int
    val mmask: Long
    val ebits: Int
    val emask: Int

    val Ne: Instr

    val Mul: Instr
    val Div: Instr

    val ReinterpretI: Instr
  }

  /** Instructions on `f32`. */
  object F32 extends FloatInstrs {
    type Value = Float

    val tpe = Float32
    val bits = 32

    def Const(value: Float): Instr = F32Const(value)
    def ConstFromInt(value: Int): Instr = F32Const(value.toFloat)

    val Zero = F32Const(0.0f)
    val One = F32Const(1.0f)

    def intInstrs: IntInstrs = I32

    val mbits = 23
    val mmask = (1 << mbits) - 1
    val ebits = 8
    val emask = (1 << ebits) - 1

    val Ne = F32Ne

    val Mul = F32Mul
    val Div = F32Div

    val ReinterpretI = F32ReinterpretI32
  }

  /** Instructions on `f64`. */
  object F64 extends FloatInstrs {
    type Value = Double

    val tpe = Float64
    val bits = 64

    def Const(value: Double): Instr = F64Const(value)
    def ConstFromInt(value: Int): Instr = F64Const(value.toDouble)

    val Zero = F64Const(0.0)
    val One = F64Const(1.0)

    def intInstrs: IntInstrs = I64

    val mbits = 52
    val mmask = (1L << mbits) - 1
    val ebits = 11
    val emask = (1 << ebits) - 1

    val Ne = F64Ne

    val Mul = F64Mul
    val Div = F64Div

    val ReinterpretI = F64ReinterpretI64
  }
}
