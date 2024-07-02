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

import org.scalajs.ir.OriginalName

import Identitities._

/** WebAssembly types.
 *
 *  @see
 *    [[https://webassembly.github.io/gc/core/syntax/types.html]]
 */
object Types {

  /** A WebAssembly `storagetype`. */
  sealed trait StorageType

  /** A WebAssembly `valtype`.
   *
   *  We call it `Type` because it matches the concept of `Type` in the
   *  Scala.js IR. It is the type of a term, from an "execution semantic
   *  typing" point of view. It is also the kind of type we manipulate the most
   *  across the backend, so it also makes sense for it to be the "default".
   */
  sealed abstract class Type extends StorageType

  /** Convenience superclass for `Type`s that are encoded with a simple opcode. */
  sealed abstract class SimpleType(val textName: String, val binaryCode: Byte) extends Type

  case object Int32 extends SimpleType("i32", 0x7F)
  case object Int64 extends SimpleType("i64", 0x7E)
  case object Float32 extends SimpleType("f32", 0x7D)
  case object Float64 extends SimpleType("f64", 0x7C)

  /** A WebAssembly `packedtype`. */
  sealed abstract class PackedType(val textName: String, val binaryCode: Byte) extends StorageType

  case object Int8 extends PackedType("i8", 0x78)
  case object Int16 extends PackedType("i16", 0x77)

  /** A WebAssembly `reftype`. */
  final case class RefType(nullable: Boolean, heapType: HeapType) extends Type {
    def toNullable: RefType = RefType(true, heapType)
    def toNonNullable: RefType = RefType(false, heapType)
  }

  object RefType {

    /** Builds a non-nullable `(ref heapType)` for the given `heapType`. */
    def apply(heapType: HeapType): RefType = RefType(false, heapType)

    /** Builds a non-nullable `(ref typeID)` for the given `typeID`. */
    def apply(typeID: TypeID): RefType = apply(HeapType(typeID))

    /** Builds a nullable `(ref null heapType)` for the given `heapType`. */
    def nullable(heapType: HeapType): RefType = RefType(true, heapType)

    /** Builds a nullable `(ref null typeID)` for the given `typeID`. */
    def nullable(typeID: TypeID): RefType = nullable(HeapType(typeID))

    /** `(ref any)`. */
    val any: RefType = apply(HeapType.Any)

    /** `(ref null any)`, i.e., `anyref`. */
    val anyref: RefType = nullable(HeapType.Any)

    /** `(ref func)`. */
    val func: RefType = apply(HeapType.Func)

    /** `(ref null func)`, i.e., `funcref`. */
    val funcref: RefType = nullable(HeapType.Func)

    /** `(ref struct)`. */
    val struct: RefType = apply(HeapType.Struct)

    /** `(ref extern)`. */
    val extern: RefType = apply(HeapType.Extern)

    /** `(ref null extern)`, i.e., `externref`. */
    val externref: RefType = nullable(HeapType.Extern)

    /** `(ref null exn)`, i.e., `exnref`. */
    val exnref: RefType = nullable(HeapType.Exn)

    /** `(ref null none)`, i.e., `nullref`. */
    val nullref: RefType = nullable(HeapType.None)
  }

  /** A WebAssembly `heaptype`. */
  sealed abstract class HeapType

  object HeapType {

    /** Reference to a named composite type. */
    final case class Type(typeID: TypeID) extends HeapType

    /** A WebAssembly `absheaptype`. */
    sealed abstract class AbsHeapType(val textName: String,
        val nullableRefTextName: String, val binaryCode: Byte)
        extends HeapType

    // Ordered by decreasing value of opcodes -- matches the order in the binary format

    case object NoExn extends AbsHeapType("noexn", "nullexnref", 0x74)
    case object NoFunc extends AbsHeapType("nofunc", "nullfuncref", 0x73)
    case object NoExtern extends AbsHeapType("noextern", "nullexternref", 0x72)
    case object None extends AbsHeapType("none", "nullref", 0x71)
    case object Func extends AbsHeapType("func", "funcref", 0x70)
    case object Extern extends AbsHeapType("extern", "externref", 0x6F)
    case object Any extends AbsHeapType("any", "anyref", 0x6E)
    case object Eq extends AbsHeapType("eq", "eqref", 0x6D)
    case object Struct extends AbsHeapType("struct", "structref", 0x6B)
    case object Array extends AbsHeapType("array", "arrayref", 0x6A)
    case object Exn extends AbsHeapType("exn", "exnref", 0x69)

    def apply(typeID: TypeID): HeapType.Type =
      HeapType.Type(typeID)
  }

  /** A WebAssembly `rectype`. */
  final case class RecType(subTypes: List[SubType])

  object RecType {

    /** Builds a `rectype` with a single `subtype`. */
    def apply(singleSubType: SubType): RecType =
      RecType(singleSubType :: Nil)
  }

  /** A WebAssembly `subtype` with an associated name.
   *
   *  It has the form `sub isFinal? superType* compositeType` in the spec.
   *  There is an additional constraint that `superType` can contain at most
   *  one element, which we why we store it as an `Option`.
   */
  final case class SubType(
      id: TypeID,
      originalName: OriginalName,
      isFinal: Boolean,
      superType: Option[TypeID],
      compositeType: CompositeType
  )

  object SubType {

    /** Builds a `subtype` that is `final` and without any super type. */
    def apply(id: TypeID, originalName: OriginalName, compositeType: CompositeType): SubType =
      SubType(id, originalName, isFinal = true, superType = None, compositeType)
  }

  /** A WebAssembly `comptype`. */
  sealed abstract class CompositeType

  /** A WebAssembly `functype`. */
  final case class FunctionType(params: List[Type], results: List[Type]) extends CompositeType

  object FunctionType {
    val NilToNil: FunctionType = FunctionType(Nil, Nil)
  }

  /** A WebAssembly `structtype` with associated field names. */
  final case class StructType(fields: List[StructField]) extends CompositeType

  /** A member of a `StructType`, with a field name and a WebAssembly `fieldtype`. */
  final case class StructField(id: FieldID, originalName: OriginalName, fieldType: FieldType)

  /** A WebAssembly `arraytype`. */
  final case class ArrayType(fieldType: FieldType) extends CompositeType

  /** A WebAssembly `fieldtype`. */
  final case class FieldType(tpe: StorageType, isMutable: Boolean)

  object StructField {
    def apply(id: FieldID, originalName: OriginalName, tpe: StorageType,
        isMutable: Boolean): StructField = {
      StructField(id, originalName, FieldType(tpe, isMutable))
    }
  }
}
