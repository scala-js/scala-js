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

  /** Transforms an IR type for a field definition.
   *
   *  This method cannot be used for `void` and `nothing`, since they are not
   *  valid types for fields.
   */
  def transformFieldType(tpe: Type)(implicit ctx: WasmContext): watpe.Type = {
    transformSingleType(tpe)
  }

  /** Transforms an IR type for a parameter definition.
   *
   *  `void` is not a valid input for this method. It is rejected by the
   *  `ClassDefChecker`.
   *
   *  Likewise, `RecordType`s are not valid, since they cannot be used for
   *  parameters.
   *
   *  `nothing` translates to `i32` in this specific case, because it is a
   *  valid type for a `ParamDef`. Obviously, calling a method that has a
   *  param of type `nothing` can never complete, and therefore reading the
   *  value of such a parameter is always unreachable. It is up to the reading
   *  codegen to handle this case.
   */
  def transformParamType(tpe: Type)(implicit ctx: WasmContext): watpe.Type = {
    tpe match {
      case NothingType   => watpe.Int32
      case _: RecordType => throw new AssertionError(s"Unexpected $tpe for parameter")
      case _             => transformSingleType(tpe)
    }
  }

  /** Transforms an IR type to the Wasm result types of a function or block.
   *
   *  `void` translates to an empty result type list, as expected.
   *
   *  `RecordType`s are flattened.
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
      case NoType             => Nil
      case NothingType        => Nil
      case RecordType(fields) => fields.flatMap(f => transformResultType(f.tpe))
      case _                  => List(transformSingleType(tpe))
    }
  }

  /** Transforms a value type to a unique Wasm type.
   *
   *  This method cannot be used for `void` and `nothing`, since they have no corresponding Wasm
   *  value type.
   *
   *  Likewise, it cannot be used for `RecordType`s, since they must be
   *  flattened into several Wasm types.
   */
  def transformSingleType(tpe: Type)(implicit ctx: WasmContext): watpe.Type = {
    tpe match {
      case AnyType              => watpe.RefType.anyref
      case ClassType(className) => transformClassType(className)
      case tpe: PrimType        => transformPrimType(tpe)

      case tpe: ArrayType =>
        watpe.RefType.nullable(genTypeID.forArrayClass(tpe.arrayTypeRef))

      case RecordType(fields) =>
        throw new AssertionError(s"Unexpected record type $tpe")
    }
  }

  def transformClassType(className: ClassName)(implicit ctx: WasmContext): watpe.RefType = {
    ctx.getClassInfoOption(className) match {
      case Some(info) =>
        if (info.isAncestorOfHijackedClass)
          watpe.RefType.anyref
        else if (!info.hasInstances)
          watpe.RefType.nullref
        else if (info.isInterface)
          watpe.RefType.nullable(genTypeID.ObjectStruct)
        else
          watpe.RefType.nullable(genTypeID.forClass(className))

      case None =>
        watpe.RefType.nullref
    }
  }

  def transformPrimType(tpe: PrimType): watpe.Type = {
    tpe match {
      case UndefType   => watpe.RefType.any
      case BooleanType => watpe.Int32
      case ByteType    => watpe.Int32
      case ShortType   => watpe.Int32
      case IntType     => watpe.Int32
      case CharType    => watpe.Int32
      case LongType    => watpe.Int64
      case FloatType   => watpe.Float32
      case DoubleType  => watpe.Float64
      case StringType  => watpe.RefType.any
      case NullType    => watpe.RefType.nullref

      case NoType | NothingType =>
        throw new IllegalArgumentException(
            s"${tpe.show()} does not have a corresponding Wasm type")
    }
  }
}
