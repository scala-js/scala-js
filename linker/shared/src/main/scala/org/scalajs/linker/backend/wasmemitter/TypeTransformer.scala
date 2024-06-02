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
import org.scalajs.ir.Types._

import org.scalajs.linker.backend.webassembly.{Types => watpe}

import VarGen._

object TypeTransformer {

  /** Transforms an IR type for a local definition (including parameters).
   *
   *  `void` is not a valid input for this method. It is rejected by the
   *  `ClassDefChecker`.
   *
   *  `nothing` translates to `i32` in this specific case, because it is a valid
   *  type for a `ParamDef` or `VarDef`. Obviously, assigning a value to a local
   *  of type `nothing` (either locally or by calling the method for a param)
   *  can never complete, and therefore reading the value of such a local is
   *  always unreachable. It is up to the reading codegen to handle this case.
   */
  def transformLocalType(tpe: Type)(implicit ctx: WasmContext): watpe.Type = {
    tpe match {
      case NothingType => watpe.Int32
      case _           => transformType(tpe)
    }
  }

  /** Transforms an IR type to the Wasm result types of a function or block.
   *
   *  `void` translates to an empty result type list, as expected.
   *
   *  `nothing` translates to an empty result type list as well, because Wasm does
   *  not have a bottom type (at least not one that can expressed at the user level).
   *  A block or function call that returns `nothing` should typically be followed
   *  by an extra `unreachable` statement to recover a stack-polymorphic context.
   *
   *  @see
   *    https://webassembly.github.io/spec/core/syntax/types.html#result-types
   */
  def transformResultType(tpe: Type)(implicit ctx: WasmContext): List[watpe.Type] = {
    tpe match {
      case NoType      => Nil
      case NothingType => Nil
      case _           => List(transformType(tpe))
    }
  }

  /** Transforms a value type to a unique Wasm type.
   *
   *  This method cannot be used for `void` and `nothing`, since they have no corresponding Wasm
   *  value type.
   */
  def transformType(tpe: Type)(implicit ctx: WasmContext): watpe.Type = {
    tpe match {
      case AnyType                => watpe.RefType.anyref
      case ClassType(className)   => transformClassType(className)
      case StringType | UndefType => watpe.RefType.any
      case tpe: PrimTypeWithRef   => transformPrimType(tpe)

      case tpe: ArrayType =>
        watpe.RefType.nullable(genTypeID.forArrayClass(tpe.arrayTypeRef))

      case RecordType(fields) =>
        throw new AssertionError(s"Unexpected record type $tpe")
    }
  }

  def transformClassType(className: ClassName)(implicit ctx: WasmContext): watpe.RefType = {
    val info = ctx.getClassInfo(className)
    if (info.isAncestorOfHijackedClass)
      watpe.RefType.anyref
    else if (info.isInterface)
      watpe.RefType.nullable(genTypeID.ObjectStruct)
    else
      watpe.RefType.nullable(genTypeID.forClass(className))
  }

  private def transformPrimType(tpe: PrimTypeWithRef): watpe.Type = {
    tpe match {
      case BooleanType => watpe.Int32
      case ByteType    => watpe.Int32
      case ShortType   => watpe.Int32
      case IntType     => watpe.Int32
      case CharType    => watpe.Int32
      case LongType    => watpe.Int64
      case FloatType   => watpe.Float32
      case DoubleType  => watpe.Float64
      case NullType    => watpe.RefType.nullref

      case NoType | NothingType =>
        throw new IllegalArgumentException(
            s"${tpe.show()} does not have a corresponding Wasm type")
    }
  }
}
