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

import org.scalajs.ir.Definitions._

private[linker] object LongImpl {
  final val RuntimeLongClass = ClassName("sjsr_RuntimeLong")
  final val RuntimeLongModuleClass = ClassName("sjsr_RuntimeLong$")

  final val lo = MethodName("lo__I")
  final val hi = MethodName("hi__I")

  private final val SigUnary   = "__sjsr_RuntimeLong"
  private final val SigBinary  = "__sjsr_RuntimeLong__sjsr_RuntimeLong"
  private final val SigShift   = "__I__sjsr_RuntimeLong"
  private final val SigCompare = "__sjsr_RuntimeLong__Z"

  final val UNARY_- = MethodName("unary$und$minus" + SigUnary)
  final val UNARY_~ = MethodName("unary$und$tilde" + SigUnary)

  final val + = MethodName("$$plus"    + SigBinary)
  final val - = MethodName("$$minus"   + SigBinary)
  final val * = MethodName("$$times"   + SigBinary)
  final val / = MethodName("$$div"     + SigBinary)
  final val % = MethodName("$$percent" + SigBinary)

  final val | = MethodName("$$bar" + SigBinary)
  final val & = MethodName("$$amp" + SigBinary)
  final val ^ = MethodName("$$up"  + SigBinary)

  final val <<  = MethodName("$$less$less"               + SigShift)
  final val >>> = MethodName("$$greater$greater$greater" + SigShift)
  final val >>  = MethodName("$$greater$greater"         + SigShift)

  final val === = MethodName("equals"      + SigCompare)
  final val !== = MethodName("notEquals"   + SigCompare)
  final val <   = MethodName("$$less"       + SigCompare)
  final val <=  = MethodName("$$less$eq"    + SigCompare)
  final val >   = MethodName("$$greater"    + SigCompare)
  final val >=  = MethodName("$$greater$eq" + SigCompare)

  final val toInt    = MethodName("toInt"    + "__I")
  final val toDouble = MethodName("toDouble" + "__D")

  final val byteValue   = MethodName("byteValue__B")
  final val shortValue  = MethodName("shortValue__S")
  final val intValue    = MethodName("intValue__I")
  final val longValue   = MethodName("longValue__J")
  final val floatValue  = MethodName("floatValue__F")
  final val doubleValue = MethodName("doubleValue__D")

  final val toString_  = MethodName("toString__T")
  final val equals_    = MethodName("equals__O__Z")
  final val hashCode_  = MethodName("hashCode__I")
  final val compareTo  = MethodName("compareTo__jl_Long__I")
  final val compareToO = MethodName("compareTo__O__I")

  private val OperatorMethods = Set(
      UNARY_-, UNARY_~, this.+, this.-, *, /, %, |, &, ^, <<, >>>, >>,
      ===, !==, <, <=, >, >=, toInt, toDouble)

  private val BoxedLongMethods = Set(
      byteValue, shortValue, intValue, longValue, floatValue, doubleValue,
      equals_, hashCode_, compareTo, compareToO)

  val AllMethods = OperatorMethods ++ BoxedLongMethods

  // Methods used for intrinsics

  final val compareToRTLong   = MethodName("compareTo__sjsr_RuntimeLong__I")
  final val divideUnsigned    = MethodName("divideUnsigned__sjsr_RuntimeLong__sjsr_RuntimeLong")
  final val remainderUnsigned = MethodName("remainderUnsigned__sjsr_RuntimeLong__sjsr_RuntimeLong")

  val AllIntrinsicMethods = Set(
      compareToRTLong, divideUnsigned, remainderUnsigned)

  // Constructors

  final val initFromParts = MethodName("init___I__I")

  val AllConstructors = Set(
      initFromParts)

  // Methods on the companion

  final val fromInt    = MethodName("fromInt__I__sjsr_RuntimeLong")
  final val fromDouble = MethodName("fromDouble__D__sjsr_RuntimeLong")

  val AllModuleMethods = Set(
      fromInt, fromDouble)

  // Extract the parts to give to the initFromParts constructor

  def extractParts(value: Long): (Int, Int) =
    (value.toInt, (value >>> 32).toInt)
}
