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

/** Instructions grouped by type, to generically write algorithms on various datatypes.
 *
 *  The instructions here are not exhaustive. We include exactly what we need.
 */
object GenericInstructions {
  /** Instructions on `i32`, `i64`, `f32` or `f64`. */
  abstract class NumericInstrs {
    type Value

    def Const(value: Value): Instr
  }

  /** Instructions on `i32` or `i64`. */
  abstract class IntInstrs extends NumericInstrs {
    val Zero: Instr

    val Eqz: Instr
    val Eq: Instr

    val Sub: Instr
    val DivS: Instr
    val RemS: Instr
  }

  /** Instructions on `i32`. */
  object I32 extends IntInstrs {
    type Value = Int

    def Const(value: Int): Instr = I32Const(value)

    val Zero = I32Const(0)

    val Eqz = I32Eqz
    val Eq = I32Eq

    val Sub = I32Sub
    val DivS = I32DivS
    val RemS = I32RemS
  }

  /** Instructions on `i64`. */
  object I64 extends IntInstrs {
    type Value = Long

    def Const(value: Long): Instr = I64Const(value)

    val Zero = I64Const(0L)

    val Eqz = I64Eqz
    val Eq = I64Eq

    val Sub = I64Sub
    val DivS = I64DivS
    val RemS = I64RemS
  }
}
