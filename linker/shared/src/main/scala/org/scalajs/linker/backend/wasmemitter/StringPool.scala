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

import org.scalajs.linker.backend.webassembly.Instructions.Instr

private[wasmemitter] trait StringPool {

  /** Returns instructions that load the given constant string data for typeData.name. */
  def getConstantStringDataInstr(str: String): List[Instr]

  /** Returns instructions that load the given constant string. */
  def getConstantStringInstr(str: String): List[Instr]

  /** Returns an instruction that loads the empty string. */
  def getEmptyStringInstr(): Instr

  /** Generates the string pool. */
  def genPool()(implicit ctx: WasmContext): List[(String, String)]
}
