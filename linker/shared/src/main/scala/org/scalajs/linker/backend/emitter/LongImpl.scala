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

private[linker] object LongImpl {
  final val RuntimeLongClass = "sjsr_RuntimeLong"
  final val RuntimeLongModuleClass = "sjsr_RuntimeLong$"

  final val lo = "lo__I"
  final val hi = "hi__I"

  private final val SigUnary   = "__sjsr_RuntimeLong"
  private final val SigBinary  = "__sjsr_RuntimeLong__sjsr_RuntimeLong"
  private final val SigShift   = "__I__sjsr_RuntimeLong"
  private final val SigCompare = "__sjsr_RuntimeLong__Z"

  final val UNARY_- = "unary$und$minus" + SigUnary
  final val UNARY_~ = "unary$und$tilde" + SigUnary

  final val + = "$$plus"    + SigBinary
  final val - = "$$minus"   + SigBinary
  final val * = "$$times"   + SigBinary
  final val / = "$$div"     + SigBinary
  final val % = "$$percent" + SigBinary

  final val | = "$$bar" + SigBinary
  final val & = "$$amp" + SigBinary
  final val ^ = "$$up"  + SigBinary

  final val <<  = "$$less$less"               + SigShift
  final val >>> = "$$greater$greater$greater" + SigShift
  final val >>  = "$$greater$greater"         + SigShift

  final val === = "equals"       + SigCompare
  final val !== = "notEquals"    + SigCompare
  final val <   = "$$less"       + SigCompare
  final val <=  = "$$less$eq"    + SigCompare
  final val >   = "$$greater"    + SigCompare
  final val >=  = "$$greater$eq" + SigCompare

  final val toInt    = "toInt"    + "__I"
  final val toDouble = "toDouble" + "__D"

  final val byteValue   = "byteValue__B"
  final val shortValue  = "shortValue__S"
  final val intValue    = "intValue__I"
  final val longValue   = "longValue__J"
  final val floatValue  = "floatValue__F"
  final val doubleValue = "doubleValue__D"

  final val equals_    = "equals__O__Z"
  final val hashCode_  = "hashCode__I"
  final val compareTo  = "compareTo__jl_Long__I"
  final val compareToO = "compareTo__O__I"

  private val OperatorMethods = Set(
      UNARY_-, UNARY_~, this.+, this.-, *, /, %, |, &, ^, <<, >>>, >>,
      ===, !==, <, <=, >, >=, toInt, toDouble)

  private val BoxedLongMethods = Set(
      byteValue, shortValue, intValue, longValue, floatValue, doubleValue,
      equals_, hashCode_, compareTo, compareToO)

  val AllMethods = OperatorMethods ++ BoxedLongMethods

  // Methods used for intrinsics

  final val divideUnsigned    = "divideUnsigned__sjsr_RuntimeLong__sjsr_RuntimeLong"
  final val remainderUnsigned = "remainderUnsigned__sjsr_RuntimeLong__sjsr_RuntimeLong"

  val AllIntrinsicMethods = Set(
      divideUnsigned, remainderUnsigned)

  // Constructors

  final val initFromParts = "init___I__I"

  val AllConstructors = Set(
      initFromParts)

  // Methods on the companion

  final val fromInt    = "fromInt__I__sjsr_RuntimeLong"
  final val fromDouble = "fromDouble__D__sjsr_RuntimeLong"

  val AllModuleMethods = Set(
      fromInt, fromDouble)

  // Extract the parts to give to the initFromParts constructor

  def extractParts(value: Long): (Int, Int) =
    (value.toInt, (value >>> 32).toInt)
}
