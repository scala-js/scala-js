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
  final val RuntimeLongModClass = ClassName("org.scalajs.linker.runtime.RuntimeLong$")

  final val lo = MethodName("lo", Nil, IntRef)
  final val hi = MethodName("hi", Nil, IntRef)

  private final val RTLongRef = ClassRef(RuntimeLongClass)
  private final val OneRTLongRef = RTLongRef :: Nil
  private final val TwoRTLongRefs = RTLongRef :: OneRTLongRef

  private final val TwoIntRefs = IntRef :: IntRef :: Nil
  private final val ThreeIntRefs = IntRef :: TwoIntRefs
  private final val FourIntRefs = IntRef :: ThreeIntRefs

  final val pack = MethodName("pack", TwoIntRefs, LongRef)

  def unaryOp(name: String): MethodName =
    MethodName(name, TwoIntRefs, LongRef)

  def binaryOp(name: String): MethodName =
    MethodName(name, FourIntRefs, LongRef)

  def shiftOp(name: String): MethodName =
    MethodName(name, ThreeIntRefs, LongRef)

  def compareOp(name: String): MethodName =
    MethodName(name, FourIntRefs, BooleanRef)

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

  final val toInt = MethodName("toInt", TwoIntRefs, IntRef)
  final val toFloat = MethodName("toFloat", TwoIntRefs, FloatRef)
  final val toDouble = MethodName("toDouble", TwoIntRefs, DoubleRef)
  final val bitsToDouble = MethodName("bitsToDouble", List(IntRef, IntRef, ObjectRef), DoubleRef)
  final val clz = MethodName("clz", TwoIntRefs, IntRef)

  final val fromInt = MethodName("fromInt", List(IntRef), LongRef)
  final val fromUnsignedInt = MethodName("fromUnsignedInt", List(IntRef), LongRef)
  final val fromDouble = MethodName("fromDouble", List(DoubleRef), LongRef)
  final val fromDoubleBits = MethodName("fromDoubleBits", List(DoubleRef, ObjectRef), LongRef)

  final val toString_ = MethodName("toString", TwoIntRefs, ClassRef(BoxedStringClass))

  val OperatorMethods = Set(
    add, sub, mul,
    divide, remainder, divideUnsigned, remainderUnsigned,
    or, and, xor, shl, shr, sar,
    equals_, notEquals, lt, le, gt, ge, ltu, leu, gtu, geu,
    toInt, toFloat, toDouble, bitsToDouble, clz,
    fromInt, fromUnsignedInt, fromDouble, fromDoubleBits,
    toString_
  )

  // Methods used for intrinsics

  final val compare = MethodName("compare", FourIntRefs, IntRef)

  final val abs = MethodName("abs", TwoIntRefs, LongRef)
  final val multiplyFull = MethodName("multiplyFull", TwoIntRefs, LongRef)

  val AllIntrinsicMethods = Set(
    compare,
    abs,
    multiplyFull
  )

  // Extract the parts to give to the initFromParts constructor

  def extractParts(value: Long): (Int, Int) =
    (value.toInt, (value >>> 32).toInt)
}
