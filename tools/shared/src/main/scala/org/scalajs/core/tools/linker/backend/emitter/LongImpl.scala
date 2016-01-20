/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.backend.emitter

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

  val AllIntrinsicMethods = Set.empty[String]

  // TODO Move these to AllIntrinsicMethods when we can break binary compatibility
  val OptionalIntrinsicMethods = Set(
      divideUnsigned, remainderUnsigned)

  // Constructors

  final val initFromParts    = "init___I__I"
  final val initFromPartsOld = "init___I__I__I"
  final val initFromInt      = "init___I"

  /* Note that initFromPartsOld depends on initFromParts in the new
   * implementation of RuntimeLong, so this ensures that the new one is
   * reachable.
   * We do not directly refer to the new one, other we would crash on earlier
   * versions of RuntimeLong that do not yet have the new constructor.
   * TODO Get rid of this when we break backward binary compatibility
   */
  val AllConstructors = Set(
      initFromPartsOld, initFromInt)

  // Methods on the companion

  final val fromDouble = "fromDouble__D__sjsr_RuntimeLong"

  final val Zero = "Zero__sjsr_RuntimeLong"

  val AllModuleMethods = Set(
      fromDouble, Zero)

  // Extract the parts to give to the initFromParts constructor

  def extractParts(value: Long): (Int, Int) =
    (value.toInt, (value >>> 32).toInt)

  /* Boldly copied from the old version of library/scala.scalajs.runtime.RuntimeLong
   * TODO Get rid of this when we break backward binary compatibility
   */

  /** Number of relevant bits in l and m each. */
  private final val BITS = 22
  /** Number of relevant bits in l and m together. */
  private final val BITS01 = 2 * BITS
  /** Number of relevant bits in h. */
  private final val BITS2 = 64 - BITS01
  /** Bitmask for l and m. */
  private final val MASK = (1 << BITS) - 1
  /** Bitmask for h. */
  private final val MASK_2 = (1 << BITS2) - 1

  def extractPartsOld(value: Long): (Int, Int, Int) =
    (value.toInt & MASK, (value >> BITS).toInt & MASK, (value >> BITS01).toInt & MASK_2)
}
