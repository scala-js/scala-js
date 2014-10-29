/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.sem

final class Semantics private (
    val checkedBehaviors: CheckedBehaviors,
    val strictFloats: Boolean) {

  import Semantics._

  def withCheckedBehaviors(behaviors: CheckedBehaviors): Semantics =
    copy(checkedBehaviors = behaviors)

  def transformCheckedBehaviors(f: CheckedBehaviors => CheckedBehaviors): Semantics =
    withCheckedBehaviors(f(checkedBehaviors))

  def withStrictFloats(strictFloats: Boolean): Semantics =
    copy(strictFloats = strictFloats)

  def optimized: Semantics =
    transformCheckedBehaviors(_.optimized)

  override def equals(that: Any): Boolean = that match {
    case that: Semantics =>
      this.checkedBehaviors == that.checkedBehaviors &&
      this.strictFloats == that.strictFloats
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, checkedBehaviors.hashCode)
    acc = mixLast(acc, strictFloats.##)
    finalizeHash(acc, 1)
  }

  private def copy(
      checkedBehaviors: CheckedBehaviors = this.checkedBehaviors,
      strictFloats: Boolean = this.strictFloats): Semantics = {
    new Semantics(
        checkedBehaviors = checkedBehaviors,
        strictFloats = strictFloats)
  }
}

object Semantics {
  private val HashSeed =
    scala.util.hashing.MurmurHash3.stringHash(classOf[Semantics].getName)

  val Defaults = new Semantics(
      checkedBehaviors = CheckedBehaviors.Defaults,
      strictFloats = false)
}
