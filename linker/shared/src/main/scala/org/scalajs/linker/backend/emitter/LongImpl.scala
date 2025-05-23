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

import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

private[linker] object LongImpl {
  final val RuntimeLongClass = ClassName("org.scalajs.linker.runtime.RuntimeLong")
  final val RuntimeLongModuleClass = ClassName("org.scalajs.linker.runtime.RuntimeLong$")

  final val lo = MethodName("lo", Nil, IntRef)
  final val hi = MethodName("hi", Nil, IntRef)

  private final val RTLongRef = ClassRef(RuntimeLongClass)
  private final val OneRTLongRef = RTLongRef :: Nil

  def unaryOp(name: String): MethodName =
    MethodName(name, Nil, RTLongRef)

  def binaryOp(name: String): MethodName =
    MethodName(name, OneRTLongRef, RTLongRef)

  def shiftOp(name: String): MethodName =
    MethodName(name, List(IntRef), RTLongRef)

  def compareOp(name: String): MethodName =
    MethodName(name, OneRTLongRef, BooleanRef)

  final val UNARY_- = unaryOp("unary_$minus")
  final val UNARY_~ = unaryOp("unary_$tilde")

  final val + = binaryOp("$plus")
  final val - = binaryOp("$minus")
  final val * = binaryOp("$times")
  final val / = binaryOp("$div")
  final val % = binaryOp("$percent")

  final val divideUnsigned = binaryOp("divideUnsigned")
  final val remainderUnsigned = binaryOp("remainderUnsigned")

  final val | = binaryOp("$bar")
  final val & = binaryOp("$amp")
  final val ^ = binaryOp("$up")

  final val <<  = shiftOp("$less$less")
  final val >>> = shiftOp("$greater$greater$greater")
  final val >>  = shiftOp("$greater$greater")

  final val === = compareOp("equals")
  final val !== = compareOp("notEquals")
  final val <   = compareOp("$less")
  final val <=  = compareOp("$less$eq")
  final val >   = compareOp("$greater")
  final val >=  = compareOp("$greater$eq")

  final val toInt = MethodName("toInt", Nil, IntRef)
  final val toFloat = MethodName("toFloat", Nil, FloatRef)
  final val toDouble = MethodName("toDouble", Nil, DoubleRef)
  final val bitsToDouble = MethodName("bitsToDouble", List(ObjectRef), DoubleRef)

  final val byteValue   = MethodName("byteValue", Nil, ByteRef)
  final val shortValue  = MethodName("shortValue", Nil, ShortRef)
  final val intValue    = MethodName("intValue", Nil, IntRef)
  final val longValue   = MethodName("longValue", Nil, LongRef)
  final val floatValue  = MethodName("floatValue", Nil, FloatRef)
  final val doubleValue = MethodName("doubleValue", Nil, DoubleRef)

  final val toString_  = MethodName("toString", Nil, ClassRef(BoxedStringClass))
  final val equals_    = MethodName("equals", List(ClassRef(ObjectClass)), BooleanRef)
  final val hashCode_  = MethodName("hashCode", Nil, IntRef)
  final val compareTo  = MethodName("compareTo", List(ClassRef(BoxedLongClass)), IntRef)
  final val compareToO = MethodName("compareTo", List(ClassRef(ObjectClass)), IntRef)

  private val OperatorMethods = Set(
      UNARY_-, UNARY_~, this.+, this.-, *, /, %, divideUnsigned, remainderUnsigned,
      |, &, ^, <<, >>>, >>, ===, !==, <, <=, >, >=, toInt, toFloat, toDouble, bitsToDouble)

  private val BoxedLongMethods = Set(
      byteValue, shortValue, intValue, longValue, floatValue, doubleValue,
      equals_, hashCode_, compareTo, compareToO)

  val AllMethods = OperatorMethods ++ BoxedLongMethods

  // Methods used for intrinsics

  final val compareToRTLong = MethodName("compareTo", List(RTLongRef), IntRef)

  val AllIntrinsicMethods = Set(
      compareToRTLong)

  // Constructors

  final val initFromParts = MethodName.constructor(List(IntRef, IntRef))

  val AllConstructors = Set(
      initFromParts)

  // Methods on the companion

  final val fromInt = MethodName("fromInt", List(IntRef), RTLongRef)
  final val fromDouble = MethodName("fromDouble", List(DoubleRef), RTLongRef)
  final val fromDoubleBits = MethodName("fromDoubleBits", List(DoubleRef, ObjectRef), RTLongRef)

  val AllModuleMethods = Set(
      fromInt, fromDouble, fromDoubleBits)

  // Methods on the companion used for intrinsics

  final val multiplyFull = MethodName("multiplyFull", List(IntRef, IntRef), RTLongRef)

  val AllIntrinsicModuleMethods = Set(
      multiplyFull)

  // Extract the parts to give to the initFromParts constructor

  def extractParts(value: Long): (Int, Int) =
    (value.toInt, (value >>> 32).toInt)
}
