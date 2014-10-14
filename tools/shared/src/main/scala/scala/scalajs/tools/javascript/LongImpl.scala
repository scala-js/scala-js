/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.javascript

object LongImpl {
  final val RuntimeLongClass = "sjsr_RuntimeLong"
  final val RuntimeLongModuleClass = "sjsr_RuntimeLong$"

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

  def AllMethods = Set(
      UNARY_-, UNARY_~, this.+, this.-, *, /, %, |, &, ^, <<, >>>, >>,
      ===, !==, <, <=, >, >=, toInt, toDouble)

  // Constructors

  final val initFromParts = "init___I__I__I"
  final val initFromInt   = "init___I"

  def AllConstructors = Set(
      initFromParts, initFromInt)

  // Methods on the companion

  final val fromDouble = "fromDouble__D__sjsr_RuntimeLong"

  final val Zero = "Zero__sjsr_RuntimeLong"

  def AllModuleMethods = Set(
      fromDouble, Zero)

  // Boldly copied from library/scala.scalajs.runtime.RuntimeLong

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

  def extractParts(value: Long): (Int, Int, Int) =
    (value.toInt & MASK, (value >> BITS).toInt & MASK, (value >> BITS01).toInt & MASK_2)
}
