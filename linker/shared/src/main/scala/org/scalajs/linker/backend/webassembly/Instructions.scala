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

import org.scalajs.ir.Position

import Identitities._
import Types._

/** WebAssembly instructions.
 *
 *  @see
 *    [[https://webassembly.github.io/spec/core/syntax/instructions.html]]
 */
object Instructions {

  /** A WebAssembly `expr`. */
  final case class Expr(instr: List[Instr])

  /** A WebAssembly `instr`. */
  sealed abstract class Instr(val mnemonic: String, val opcode: Int)

  // Semantic categories of instructions

  /** A stack-polymorphic instruction. */
  sealed trait StackPolymorphicInstr extends Instr

  /** An instruction that opens a structured control block. */
  sealed trait StructuredLabeledInstr extends Instr {
    val label: Option[LabelID]
  }

  // Convenience subclasses of instructions for writing text/binary

  /** An instruction without any immediate argument. */
  sealed abstract class SimpleInstr(mnemonic: String, opcode: Int)
      extends Instr(mnemonic, opcode)

  /** A structured labeled instruction with a single `BlockType` argument. */
  sealed abstract class BlockTypeLabeledInstr(mnemonic: String, opcode: Int,
      val blockTypeArgument: BlockType)
      extends Instr(mnemonic, opcode) with StructuredLabeledInstr

  /** An instruction with a single `LabelIdx` argument. */
  sealed abstract class LabelInstr(mnemonic: String, opcode: Int,
      val labelArgument: LabelID)
      extends Instr(mnemonic, opcode)

  /** An instruction with a single `FunctionID` argument. */
  sealed abstract class FuncInstr(mnemonic: String, opcode: Int,
      val funcArgument: FunctionID)
      extends Instr(mnemonic, opcode)

  /** An instruction with a single `TypeID` argument. */
  sealed abstract class TypeInstr(mnemonic: String, opcode: Int,
      val typeArgument: TypeID)
      extends Instr(mnemonic, opcode)

  /** An instruction with a single `TagID` argument. */
  sealed abstract class TagInstr(mnemonic: String, opcode: Int,
      val tagArgument: TagID)
      extends Instr(mnemonic, opcode)

  /** An instruction with a single `LocalID` argument. */
  sealed abstract class LocalInstr(mnemonic: String, opcode: Int,
      val localArgument: LocalID)
      extends Instr(mnemonic, opcode)

  /** An instruction with a single `GlobalID` argument. */
  sealed abstract class GlobalInstr(mnemonic: String, opcode: Int,
      val globalArgument: GlobalID)
      extends Instr(mnemonic, opcode)

  /** An instruction with a single `HeapType` argument. */
  sealed abstract class HeapTypeInstr(mnemonic: String, opcode: Int,
      val heapTypeArgument: HeapType)
      extends Instr(mnemonic, opcode)

  /** An instruction with a single `RefType` argument
   *
   *  In the binary format, it has split opcodes for the nullable and non-nullable variants.
   */
  sealed abstract class RefTypeInstr(mnemonic: String, nonNullOpcode: Int,
      nullOpcode: Int, val refTypeArgument: RefType)
      extends Instr(mnemonic, if (refTypeArgument.nullable) nullOpcode else nonNullOpcode)

  /** An instruction with a pair of `TypeID`, `FieldID` arguments. */
  sealed abstract class StructFieldInstr(mnemonic: String, opcode: Int,
      val structTypeID: TypeID, val fieldID: FieldID)
      extends Instr(mnemonic, opcode)

  // --- The actual instruction list -- sorted by opcode ----------------------

  /** Fake instruction to mark position changes. */
  final case class PositionMark(pos: Position) extends Instr("pos", -1)

  // Control instructions

  case object Unreachable extends SimpleInstr("unreachable", 0x00) with StackPolymorphicInstr
  case object Nop extends SimpleInstr("nop", 0x01)

  final case class Block(i: BlockType, label: Option[LabelID])
      extends BlockTypeLabeledInstr("block", 0x02, i)

  final case class Loop(i: BlockType, label: Option[LabelID])
      extends BlockTypeLabeledInstr("loop", 0x03, i)

  final case class If(i: BlockType, label: Option[LabelID] = None)
      extends BlockTypeLabeledInstr("if", 0x04, i)

  case object Else extends SimpleInstr("else", 0x05)

  /** `try` in the legacy exception system.
   *
   *  @see
   *    [[https://webassembly.github.io/exception-handling/legacy/exceptions/core/syntax.html]]
   */
  final case class Try(i: BlockType, label: Option[LabelID] = None)
      extends BlockTypeLabeledInstr("try", 0x06, i)

  /** `catch` in the legacy exception system. */
  final case class Catch(i: TagID) extends TagInstr("catch", 0x07, i)

  final case class Throw(i: TagID) extends TagInstr("throw", 0x08, i) with StackPolymorphicInstr
  case object ThrowRef extends SimpleInstr("throw_ref", 0x0A) with StackPolymorphicInstr

  case object End extends SimpleInstr("end", 0x0B)

  final case class Br(i: LabelID) extends LabelInstr("br", 0x0C, i) with StackPolymorphicInstr
  final case class BrIf(i: LabelID) extends LabelInstr("br_if", 0x0D, i)

  final case class BrTable(table: List[LabelID], default: LabelID)
      extends Instr("br_table", 0x0E) with StackPolymorphicInstr

  case object Return extends SimpleInstr("return", 0x0F) with StackPolymorphicInstr

  final case class Call(i: FunctionID) extends FuncInstr("call", 0x10, i)

  final case class ReturnCall(i: FunctionID)
      extends FuncInstr("return_call", 0x12, i) with StackPolymorphicInstr

  final case class CallRef(i: TypeID) extends TypeInstr("call_ref", 0x14, i)

  final case class ReturnCallRef(i: TypeID)
      extends TypeInstr("return_call_ref", 0x15, i) with StackPolymorphicInstr

  case object Drop extends SimpleInstr("drop", 0x1A)

  final case class TryTable(i: BlockType, cs: List[CatchClause], label: Option[LabelID] = None)
      extends Instr("try_table", 0x1F) with StructuredLabeledInstr

  // Instructions on local and global variables

  final case class LocalGet(i: LocalID) extends LocalInstr("local.get", 0x20, i)
  final case class LocalSet(i: LocalID) extends LocalInstr("local.set", 0x21, i)
  final case class LocalTee(i: LocalID) extends LocalInstr("local.tee", 0x22, i)
  final case class GlobalGet(i: GlobalID) extends GlobalInstr("global.get", 0x23, i)
  final case class GlobalSet(i: GlobalID) extends GlobalInstr("global.set", 0x24, i)

  // Literals of primitive numeric types

  final case class I32Const(v: Int) extends Instr("i32.const", 0x41)
  final case class I64Const(v: Long) extends Instr("i64.const", 0x42)
  final case class F32Const(v: Float) extends Instr("f32.const", 0x43)
  final case class F64Const(v: Double) extends Instr("f64.const", 0x44)

  // Simple operations on primitive numeric types

  case object I32Eqz extends SimpleInstr("i32.eqz", 0x45)
  case object I32Eq extends SimpleInstr("i32.eq", 0x46)
  case object I32Ne extends SimpleInstr("i32.ne", 0x47)
  case object I32LtS extends SimpleInstr("i32.lt_s", 0x48)
  case object I32LtU extends SimpleInstr("i32.lt_u", 0x49)
  case object I32GtS extends SimpleInstr("i32.gt_s", 0x4A)
  case object I32GtU extends SimpleInstr("i32.gt_u", 0x4B)
  case object I32LeS extends SimpleInstr("i32.le_s", 0x4C)
  case object I32LeU extends SimpleInstr("i32.le_u", 0x4D)
  case object I32GeS extends SimpleInstr("i32.ge_s", 0x4E)
  case object I32GeU extends SimpleInstr("i32.ge_u", 0x4F)
  case object I64Eqz extends SimpleInstr("i64.eqz", 0x50)
  case object I64Eq extends SimpleInstr("i64.eq", 0x51)
  case object I64Ne extends SimpleInstr("i64.ne", 0x52)
  case object I64LtS extends SimpleInstr("i64.lt_s", 0x53)
  case object I64LtU extends SimpleInstr("i64.lt_u", 0x54)
  case object I64GtS extends SimpleInstr("i64.gt_s", 0x55)
  case object I64GtU extends SimpleInstr("i64.gt_u", 0x56)
  case object I64LeS extends SimpleInstr("i64.le_s", 0x57)
  case object I64LeU extends SimpleInstr("i64.le_u", 0x58)
  case object I64GeS extends SimpleInstr("i64.ge_s", 0x59)
  case object I64GeU extends SimpleInstr("i64.ge_u", 0x5A)
  case object F32Eq extends SimpleInstr("f32.eq", 0x5B)
  case object F32Ne extends SimpleInstr("f32.ne", 0x5C)
  case object F32Lt extends SimpleInstr("f32.lt", 0x5D)
  case object F32Gt extends SimpleInstr("f32.gt", 0x5E)
  case object F32Le extends SimpleInstr("f32.le", 0x5F)
  case object F32Ge extends SimpleInstr("f32.ge", 0x60)
  case object F64Eq extends SimpleInstr("f64.eq", 0x61)
  case object F64Ne extends SimpleInstr("f64.ne", 0x62)
  case object F64Lt extends SimpleInstr("f64.lt", 0x63)
  case object F64Gt extends SimpleInstr("f64.gt", 0x64)
  case object F64Le extends SimpleInstr("f64.le", 0x65)
  case object F64Ge extends SimpleInstr("f64.ge", 0x66)
  case object I32Clz extends SimpleInstr("i32.clz", 0x67)
  case object I32Ctz extends SimpleInstr("i32.ctz", 0x68)
  case object I32Popcnt extends SimpleInstr("i32.popcnt", 0x69)
  case object I32Add extends SimpleInstr("i32.add", 0x6A)
  case object I32Sub extends SimpleInstr("i32.sub", 0x6B)
  case object I32Mul extends SimpleInstr("i32.mul", 0x6C)
  case object I32DivS extends SimpleInstr("i32.div_s", 0x6D)
  case object I32DivU extends SimpleInstr("i32.div_u", 0x6E)
  case object I32RemS extends SimpleInstr("i32.rem_s", 0x6F)
  case object I32RemU extends SimpleInstr("i32.rem_u", 0x70)
  case object I32And extends SimpleInstr("i32.and", 0x71)
  case object I32Or extends SimpleInstr("i32.or", 0x72)
  case object I32Xor extends SimpleInstr("i32.xor", 0x73)
  case object I32Shl extends SimpleInstr("i32.shl", 0x74)
  case object I32ShrS extends SimpleInstr("i32.shr_s", 0x75)
  case object I32ShrU extends SimpleInstr("i32.shr_u", 0x76)
  case object I32Rotl extends SimpleInstr("i32.rotl", 0x77)
  case object I32Rotr extends SimpleInstr("i32.rotr", 0x78)
  case object I64Clz extends SimpleInstr("i64.clz", 0x79)
  case object I64Ctz extends SimpleInstr("i64.ctz", 0x7A)
  case object I64Popcnt extends SimpleInstr("i64.popcnt", 0x7B)
  case object I64Add extends SimpleInstr("i64.add", 0x7C)
  case object I64Sub extends SimpleInstr("i64.sub", 0x7D)
  case object I64Mul extends SimpleInstr("i64.mul", 0x7E)
  case object I64DivS extends SimpleInstr("i64.div_s", 0x7F)
  case object I64DivU extends SimpleInstr("i64.div_u", 0x80)
  case object I64RemS extends SimpleInstr("i64.rem_s", 0x81)
  case object I64RemU extends SimpleInstr("i64.rem_u", 0x82)
  case object I64And extends SimpleInstr("i64.and", 0x83)
  case object I64Or extends SimpleInstr("i64.or", 0x84)
  case object I64Xor extends SimpleInstr("i64.xor", 0x85)
  case object I64Shl extends SimpleInstr("i64.shl", 0x86)
  case object I64ShrS extends SimpleInstr("i64.shr_s", 0x87)
  case object I64ShrU extends SimpleInstr("i64.shr_u", 0x88)
  case object I64Rotl extends SimpleInstr("i64.rotl", 0x89)
  case object I64Rotr extends SimpleInstr("i64.rotr", 0x8A)
  case object F32Abs extends SimpleInstr("f32.abs", 0x8B)
  case object F32Neg extends SimpleInstr("f32.neg", 0x8C)
  case object F32Ceil extends SimpleInstr("f32.ceil", 0x8D)
  case object F32Floor extends SimpleInstr("f32.floor", 0x8E)
  case object F32Trunc extends SimpleInstr("f32.trunc", 0x8F)
  case object F32Nearest extends SimpleInstr("f32.nearest", 0x90)
  case object F32Sqrt extends SimpleInstr("f32.sqrt", 0x91)
  case object F32Add extends SimpleInstr("f32.add", 0x92)
  case object F32Sub extends SimpleInstr("f32.sub", 0x93)
  case object F32Mul extends SimpleInstr("f32.mul", 0x94)
  case object F32Div extends SimpleInstr("f32.div", 0x95)
  case object F32Min extends SimpleInstr("f32.min", 0x96)
  case object F32Max extends SimpleInstr("f32.max", 0x97)
  case object F32Copysign extends SimpleInstr("f32.copysign", 0x98)
  case object F64Abs extends SimpleInstr("f64.abs", 0x99)
  case object F64Neg extends SimpleInstr("f64.neg", 0x9A)
  case object F64Ceil extends SimpleInstr("f64.ceil", 0x9B)
  case object F64Floor extends SimpleInstr("f64.floor", 0x9C)
  case object F64Trunc extends SimpleInstr("f64.trunc", 0x9D)
  case object F64Nearest extends SimpleInstr("f64.nearest", 0x9E)
  case object F64Sqrt extends SimpleInstr("f64.sqrt", 0x9F)
  case object F64Add extends SimpleInstr("f64.add", 0xA0)
  case object F64Sub extends SimpleInstr("f64.sub", 0xA1)
  case object F64Mul extends SimpleInstr("f64.mul", 0xA2)
  case object F64Div extends SimpleInstr("f64.div", 0xA3)
  case object F64Min extends SimpleInstr("f64.min", 0xA4)
  case object F64Max extends SimpleInstr("f64.max", 0xA5)
  case object F64Copysign extends SimpleInstr("f64.copysign", 0xA6)
  case object I32WrapI64 extends SimpleInstr("i32.wrap_i64", 0xA7)
  case object I32TruncF32S extends SimpleInstr("i32.trunc_f32_s", 0xA8)
  case object I32TruncF32U extends SimpleInstr("i32.trunc_f32_u", 0xA9)
  case object I32TruncF64S extends SimpleInstr("i32.trunc_f64_s", 0xAA)
  case object I32TruncF64U extends SimpleInstr("i32.trunc_f64_u", 0xAB)
  case object I64ExtendI32S extends SimpleInstr("i64.extend_i32_s", 0xAC)
  case object I64ExtendI32U extends SimpleInstr("i64.extend_i32_u", 0xAD)
  case object I64TruncF32S extends SimpleInstr("i64.trunc_f32_s", 0xAE)
  case object I64TruncF32U extends SimpleInstr("i64.trunc_f32_u", 0xAF)
  case object I64TruncF64S extends SimpleInstr("i64.trunc_f64_s", 0xB0)
  case object I64TruncF64U extends SimpleInstr("i64.trunc_f64_u", 0xB1)
  case object F32ConvertI32S extends SimpleInstr("f32.convert_i32_s", 0xB2)
  case object F32ConvertI32U extends SimpleInstr("f32.convert_i32_u", 0xB3)
  case object F32ConvertI64S extends SimpleInstr("f32.convert_i64_s", 0xB4)
  case object F32ConvertI64U extends SimpleInstr("f32.convert_i64_u", 0xB5)
  case object F32DemoteF64 extends SimpleInstr("f32.demote_f64", 0xB6)
  case object F64ConvertI32S extends SimpleInstr("f64.convert_i32_s", 0xB7)
  case object F64ConvertI32U extends SimpleInstr("f64.convert_i32_u", 0xB8)
  case object F64ConvertI64S extends SimpleInstr("f64.convert_i64_s", 0xB9)
  case object F64ConvertI64U extends SimpleInstr("f64.convert_i64_u", 0xBA)
  case object F64PromoteF32 extends SimpleInstr("f64.promote_f32", 0xBB)
  case object I32ReinterpretF32 extends SimpleInstr("i32.reinterpret_f32", 0xBC)
  case object I64ReinterpretF64 extends SimpleInstr("i64.reinterpret_f64", 0xBD)
  case object F32ReinterpretI32 extends SimpleInstr("f32.reinterpret_i32", 0xBE)
  case object F64ReinterpretI64 extends SimpleInstr("f64.reinterpret_i64", 0xBF)
  case object I32Extend8S extends SimpleInstr("i32.extend8_s", 0xC0)
  case object I32Extend16S extends SimpleInstr("i32.extend16_s", 0xC1)
  case object I64Extend8S extends SimpleInstr("i64.extend8_s", 0xC2)
  case object I64Extend16S extends SimpleInstr("i64.extend16_s", 0xC3)
  case object I64Extend32S extends SimpleInstr("i64.extend32_s", 0xC4)

  // Generic reference instructions

  final case class RefNull(i: HeapType) extends HeapTypeInstr("ref.null", 0xD0, i)
  case object RefIsNull extends SimpleInstr("ref.is_null", 0xD1)
  final case class RefFunc(i: FunctionID) extends FuncInstr("ref.func", 0xD2, i)
  case object RefEq extends SimpleInstr("ref.eq", 0xD3)
  case object RefAsNonNull extends SimpleInstr("ref.as_non_null", 0xD4)
  final case class BrOnNull(i: LabelID) extends LabelInstr("br_on_null", 0xD5, i)
  final case class BrOnNonNull(i: LabelID) extends LabelInstr("br_on_non_null", 0xD6, i)

  // Struct instructions

  final case class StructNew(i: TypeID) extends TypeInstr("struct.new", 0xFB00, i)
  final case class StructNewDefault(i: TypeID) extends TypeInstr("struct.new_default", 0xFB01, i)
  final case class StructGet(tyidx: TypeID, fidx: FieldID)
      extends StructFieldInstr("struct.get", 0xFB02, tyidx, fidx)
  final case class StructSet(tyidx: TypeID, fidx: FieldID)
      extends StructFieldInstr("struct.set", 0xFB05, tyidx, fidx)

  // Array instructions

  final case class ArrayNew(i: TypeID) extends TypeInstr("array.new", 0xFB06, i)
  final case class ArrayNewDefault(i: TypeID) extends TypeInstr("array.new_default", 0xFB07, i)
  final case class ArrayNewFixed(i: TypeID, size: Int) extends Instr("array.new_fixed", 0xFB08)
  final case class ArrayNewData(i: TypeID, d: DataID) extends Instr("array.new_data", 0xFB09)
  final case class ArrayGet(i: TypeID) extends TypeInstr("array.get", 0xFB0B, i)
  final case class ArrayGetS(i: TypeID) extends TypeInstr("array.get_s", 0xFB0C, i)
  final case class ArrayGetU(i: TypeID) extends TypeInstr("array.get_u", 0xFB0D, i)
  final case class ArraySet(i: TypeID) extends TypeInstr("array.set", 0xFB0E, i)
  case object ArrayLen extends SimpleInstr("array.len", 0xFB0F)
  final case class ArrayCopy(destType: TypeID, srcType: TypeID) extends Instr("array.copy", 0xFB11)

  // Extended generic reference instructions

  final case class RefTest(i: RefType) extends RefTypeInstr("ref.test", 0xFB14, 0xFB15, i)
  final case class RefCast(i: RefType) extends RefTypeInstr("ref.cast", 0xFB16, 0xFB17, i)

  final case class BrOnCast(label: LabelID, from: RefType, to: RefType)
      extends Instr("br_on_cast", 0xFB18)
  final case class BrOnCastFail(label: LabelID, from: RefType, to: RefType)
      extends Instr("br_on_cast_fail", 0xFB19)

  // Instructions for interop between externref's and anyref's

  case object AnyConvertExtern extends SimpleInstr("any.convert_extern", 0xFB1A)
  case object ExternConvertAny extends SimpleInstr("extern.convert_any", 0xFB1B)

  // i31 reference instructions

  case object RefI31 extends SimpleInstr("ref.i31", 0xFB1C)
  case object I31GetS extends SimpleInstr("i31.get_s", 0xFB1D)
  case object I31GetU extends SimpleInstr("i31.get_u", 0xFB1E)

  // Extended operations on primitive types

  case object I32TruncSatF64S extends SimpleInstr("i32.trunc_sat_f64_s", 0xFC02)
  case object I64TruncSatF64S extends SimpleInstr("i64.trunc_sat_f64_s", 0xFC06)

  // --- End of the instruction list ------------------------------------------

  // Catch clauses for TryTable

  /** Catch clause of a [[TryTable]] instruction. */
  sealed abstract class CatchClause(
      val mnemonic: String,
      val opcode: Int,
      val tag: Option[TagID],
      val label: LabelID
  )

  object CatchClause {
    final case class Catch(x: TagID, l: LabelID) extends CatchClause("catch", 0x00, Some(x), l)
    final case class CatchRef(x: TagID, l: LabelID) extends CatchClause("catch_ref", 0x01, Some(x), l)
    final case class CatchAll(l: LabelID) extends CatchClause("catch_all", 0x02, None, l)
    final case class CatchAllRef(l: LabelID) extends CatchClause("catch_all_ref", 0x03, None, l)
  }

  // Block types

  /** A structured instruction can consume input and produce output on the operand stack according
   *  to its annotated block type.
   *
   *  It is given either as a type index that refers to a suitable function type,
   *  or as an optional value type inline, which is a shorthand for the function type
   *  `[] -> [valtype]`.
   *
   *  @see
   *    https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions
   */
  sealed abstract class BlockType

  object BlockType {
    final case class FunctionType(ty: TypeID) extends BlockType
    final case class ValueType(ty: Option[Type]) extends BlockType

    object ValueType {
      def apply(ty: Type): ValueType = ValueType(Some(ty))
      def apply(): ValueType = ValueType(None)
    }
  }
}
