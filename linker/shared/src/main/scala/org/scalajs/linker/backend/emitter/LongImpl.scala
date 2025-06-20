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

  final val lo = MethodName("lo", Nil, IntRef)
  final val hi = MethodName("hi", Nil, IntRef)

  private final val RTLongRef = ClassRef(RuntimeLongClass)
  private final val OneRTLongRef = RTLongRef :: Nil
  private final val TwoRTLongRefs = RTLongRef :: OneRTLongRef

  def unaryOp(name: String): MethodName =
    MethodName(name, OneRTLongRef, RTLongRef)

  def binaryOp(name: String): MethodName =
    MethodName(name, TwoRTLongRefs, RTLongRef)

  def shiftOp(name: String): MethodName =
    MethodName(name, List(RTLongRef, IntRef), RTLongRef)

  def compareOp(name: String): MethodName =
    MethodName(name, TwoRTLongRefs, BooleanRef)

  // Instance methods that we need to reach as part of the jl.Long boxing

  private final val byteValue = MethodName("byteValue", Nil, ByteRef)
  private final val shortValue = MethodName("shortValue", Nil, ShortRef)
  private final val intValue = MethodName("intValue", Nil, IntRef)
  private final val longValue = MethodName("longValue", Nil, LongRef)
  private final val floatValue = MethodName("floatValue", Nil, FloatRef)
  private final val doubleValue = MethodName("doubleValue", Nil, DoubleRef)

  private final val equalsO = MethodName("equals", List(ClassRef(ObjectClass)), BooleanRef)
  private final val hashCode_ = MethodName("hashCode", Nil, IntRef)
  private final val compareTo = MethodName("compareTo", List(ClassRef(BoxedLongClass)), IntRef)
  private final val compareToO = MethodName("compareTo", List(ClassRef(ObjectClass)), IntRef)

  val BoxedLongMethods = Set(
      byteValue, shortValue, intValue, longValue, floatValue, doubleValue,
      equalsO, hashCode_, compareTo, compareToO)

  // Operator methods

  final val add = binaryOp("add")
  final val sub = binaryOp("sub")
  final val mul = binaryOp("mul")
  final val divide = binaryOp("divide")
  final val remainder = binaryOp("remainder")

  final val divideUnsigned = binaryOp("divideUnsigned")
  final val remainderUnsigned = binaryOp("remainderUnsigned")

  final val or = binaryOp("or")
  final val and = binaryOp("and")
  final val xor = binaryOp("xor")

  final val shl = shiftOp("shl")
  final val shr = shiftOp("shr")
  final val sar = shiftOp("sar")

  final val equals_ = compareOp("equals")
  final val notEquals = compareOp("notEquals")
  final val lt = compareOp("lt")
  final val le = compareOp("le")
  final val gt = compareOp("gt")
  final val ge = compareOp("ge")
  final val ltu = compareOp("ltu")
  final val leu = compareOp("leu")
  final val gtu = compareOp("gtu")
  final val geu = compareOp("geu")

  final val toInt = MethodName("toInt", OneRTLongRef, IntRef)
  final val toFloat = MethodName("toFloat", OneRTLongRef, FloatRef)
  final val toDouble = MethodName("toDouble", OneRTLongRef, DoubleRef)
  final val bitsToDouble = MethodName("bitsToDouble", List(RTLongRef, ObjectRef), DoubleRef)
  final val clz = MethodName("clz", OneRTLongRef, IntRef)

  final val fromInt = MethodName("fromInt", List(IntRef), RTLongRef)
  final val fromUnsignedInt = MethodName("fromUnsignedInt", List(IntRef), RTLongRef)
  final val fromDouble = MethodName("fromDouble", List(DoubleRef), RTLongRef)
  final val fromDoubleBits = MethodName("fromDoubleBits", List(DoubleRef, ObjectRef), RTLongRef)

  val OperatorMethods = Set(
    add, sub, mul,
    divide, remainder, divideUnsigned, remainderUnsigned,
    or, and, xor, shl, shr, sar,
    equals_, notEquals, lt, le, gt, ge, ltu, leu, gtu, geu,
    toInt, toFloat, toDouble, bitsToDouble, clz,
    fromInt, fromUnsignedInt, fromDouble, fromDoubleBits
  )

  // Methods used for intrinsics

  final val toString_ = MethodName("toString", OneRTLongRef, ClassRef(BoxedStringClass))

  final val compare = MethodName("compare", TwoRTLongRefs, IntRef)

  final val abs = MethodName("abs", OneRTLongRef, RTLongRef)
  final val multiplyFull = MethodName("multiplyFull", List(IntRef, IntRef), RTLongRef)

  val AllIntrinsicMethods = Set(
    toString_,
    compare,
    abs,
    multiplyFull
  )

  // Constructors

  final val initFromParts = MethodName.constructor(List(IntRef, IntRef))

  val AllConstructors = Set(
      initFromParts)

  // Extract the parts to give to the initFromParts constructor

  def extractParts(value: Long): (Int, Int) =
    (value.toInt, (value >>> 32).toInt)
}
