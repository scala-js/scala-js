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
import org.scalajs.ir.OriginalName
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.linker.backend.webassembly.{Types => watpe}

import VarGen._
import SpecialNames.normalizedSimpleMethodName

object TypeTransformer {

  /** Transforms an IR type for a field definition.
   *
   *  This method cannot be used for `void` and `nothing`, since they are not
   *  valid types for fields.
   */
  def transformFieldType(tpe: Type)(implicit ctx: WasmContext): watpe.Type =
    transformSingleType(tpe)

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
      case VoidType           => Nil
      case NothingType        => Nil
      case RecordType(fields) => fields.flatMap(f => transformResultType(f.tpe))
      case _                  => List(transformSingleType(tpe))
    }
  }

  /** Transforms an IR type at the boundary of `@WasmImport`/`@WasmExport`. */
  def transformWasmInteropParamType(tpe: Type)(implicit ctx: WasmContext): watpe.Type = {
    tpe match {
      case tpe: PrimType =>
        transformPrimType(tpe)
      case ArrayType(arrayTypeRef, _, _) =>
        watpe.RefType(genTypeID.underlyingOf(arrayTypeRef))
      case _ =>
        throw new AssertionError(s"Unexpected $tpe at Wasm import/export boundary")
    }
  }

  /** Transforms an IR result type at the boundary of `@WasmImport`/`@WasmExport`. */
  def transformWasmInteropResultType(tpe: Type)(implicit ctx: WasmContext): List[watpe.Type] = {
    tpe match {
      case VoidType => Nil
      case _        => transformWasmInteropParamType(tpe) :: Nil
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
      case AnyType                               => watpe.RefType.anyref
      case AnyNotNullType                        => watpe.RefType.any
      case ClassType(className, nullable, exact) => transformClassType(className, nullable, exact)
      case tpe: PrimType                         => transformPrimType(tpe)

      case ArrayType(arrayTypeRef, nullable, exact) =>
        val typeID = genTypeID.forArrayClass(arrayTypeRef)
        val heapType = watpe.HeapType(typeID, exact = exact && ctx.useCustomDescriptors)
        watpe.RefType(nullable, heapType)

      case tpe @ ClosureType(_, _, nullable) =>
        val (_, typedClosureTypeID) = ctx.genTypedClosureStructType(tpe)
        watpe.RefType(nullable, typedClosureTypeID)

      case RecordType(fields) =>
        throw new AssertionError(s"Unexpected record type $tpe")
    }
  }

  def transformClassType(className: ClassName, nullable: Boolean, exact: Boolean)(
      implicit ctx: WasmContext): watpe.RefType = {
    val heapType: watpe.HeapType = ctx.getClassInfoOption(className) match {
      case Some(info) =>
        if (className == BoxedStringClass)
          ctx.stringType.heapType
        else if (info.isAncestorOfHijackedClass && !exact)
          watpe.HeapType.Any
        else if (!info.hasInstances)
          watpe.HeapType.None
        else if (info.isInterface)
          watpe.HeapType(genTypeID.ObjectStruct)
        else
          watpe.HeapType(genTypeID.forClass(className), exact = exact && ctx.useCustomDescriptors)

      case None =>
        watpe.HeapType.None
    }

    watpe.RefType(nullable, heapType)
  }

  def transformPrimType(tpe: PrimType)(implicit ctx: WasmContext): watpe.Type = {
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
      case StringType  => ctx.stringType
      case NullType    => watpe.RefType.nullref

      case VoidType | NothingType =>
        throw new IllegalArgumentException(
            s"${tpe.show()} does not have a corresponding Wasm type")
    }
  }

  def transformTableFunctionType(normalizedName: MethodName)(
      implicit ctx: WasmContext): watpe.SubType = {
    assert(normalizedName.simpleName == normalizedSimpleMethodName)

    val typeID = genTypeID.forTableFunctionType(normalizedName)
    val regularParamTyps = normalizedName.paramTypeRefs.map { typeRef =>
      transformParamType(inferTypeFromTypeRef(typeRef))
    }
    val resultType = transformResultType(inferTypeFromTypeRef(normalizedName.resultTypeRef))

    watpe.SubType(
      typeID,
      ClassEmitter.makeTableEntryTypeOriginalName(normalizedName),
      watpe.FunctionType(watpe.RefType.any :: regularParamTyps, resultType)
    )
  }

  def normalizeTableMethodName(methodName: MethodName): MethodName = {
    MethodName(
      normalizedSimpleMethodName,
      methodName.paramTypeRefs,
      methodName.resultTypeRef,
      methodName.isReflectiveProxy
    )
  }

  def transformClosureDataStructType(captureParamTypes: List[Type])(
      implicit ctx: WasmContext): watpe.StructType = {
    val fields: List[watpe.StructField] = {
      for ((tpe, i) <- captureParamTypes.zipWithIndex) yield {
        watpe.StructField(
          genFieldID.captureParam(i),
          NoOriginalName,
          transformParamType(tpe),
          isMutable = false
        )
      }
    }
    watpe.StructType(fields)
  }

  def transformTypedClosureType(tpe: ClosureType)(
      implicit ctx: WasmContext): (watpe.SubType, watpe.SubType) = {
    assert(!tpe.nullable)

    val tpeNameString = tpe.show()

    val funType = watpe.FunctionType(
      watpe.RefType.struct :: tpe.paramTypes.map(TypeTransformer.transformParamType(_)),
      TypeTransformer.transformResultType(tpe.resultType)
    )
    val funTypeID = genTypeID.forClosureFunType(tpe)
    val funSubType = watpe.SubType(funTypeID, OriginalName("fun" + tpeNameString), funType)

    val fields: List[watpe.StructField] = List(
      watpe.StructField(
        genFieldID.typedClosure.data,
        OriginalName("data"),
        watpe.RefType.struct,
        isMutable = false
      ),
      watpe.StructField(
        genFieldID.typedClosure.fun,
        OriginalName("fun"),
        watpe.RefType(funTypeID),
        isMutable = false
      )
    )

    val structTypeID = genTypeID.forClosureType(tpe)
    val structType = watpe.StructType(fields)
    val structSubType = watpe.SubType(structTypeID, OriginalName(tpeNameString), structType)

    (funSubType, structSubType)
  }

  def inferTypeFromTypeRef(typeRef: TypeRef)(implicit ctx: WasmContext): Type = typeRef match {
    case PrimRef(tpe) =>
      tpe
    case ClassRef(className) =>
      if (className == ObjectClass || ctx.getClassInfo(className).kind.isJSType)
        AnyType
      else
        ClassType(className, nullable = true, exact = false)
    case typeRef: ArrayTypeRef =>
      ArrayType(typeRef, nullable = true, exact = false)
    case typeRef: TransientTypeRef =>
      typeRef.tpe
  }
}
