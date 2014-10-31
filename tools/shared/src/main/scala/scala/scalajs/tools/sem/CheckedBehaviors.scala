/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.sem

import CheckedBehaviors._

final class CheckedBehaviors private (
    val asInstanceOfs: Behavior) {

  def withAsInstanceOfs(v: Behavior): CheckedBehaviors =
    copy(asInstanceOfs = v)

  def optimized: CheckedBehaviors = {
    new CheckedBehaviors(
        asInstanceOfs = asInstanceOfs.optimized)
  }

  override def equals(that: Any): Boolean = that match {
    case that: CheckedBehaviors =>
      this.asInstanceOfs == that.asInstanceOfs
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mixLast(acc, asInstanceOfs.hashCode)
    finalizeHash(acc, 1)
  }

  override def toString(): String = {
    s"""CheckedBehaviors(
       |  asInstanceOfs = $asInstanceOfs
       |)""".stripMargin
  }

  private def copy(
      asInstanceOfs: Behavior = this.asInstanceOfs): CheckedBehaviors = {
    new CheckedBehaviors(
        asInstanceOfs = asInstanceOfs)
  }
}

object CheckedBehaviors {
  private val HashSeed =
    scala.util.hashing.MurmurHash3.stringHash(classOf[CheckedBehaviors].getName)

  sealed abstract class Behavior {
    def optimized: Behavior = this match {
      case Fatal => Unchecked
      case _     => this
    }
  }

  case object Compliant extends Behavior
  case object Fatal extends Behavior
  case object Unchecked extends Behavior

  val Defaults = new CheckedBehaviors(
      asInstanceOfs = Fatal)
}
