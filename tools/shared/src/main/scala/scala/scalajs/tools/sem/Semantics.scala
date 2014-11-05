/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.sem

final class Semantics private (
    val asInstanceOfs: CheckedBehavior,
    val strictFloats: Boolean) {

  import Semantics._

  def withAsInstanceOfs(behavior: CheckedBehavior): Semantics =
    copy(asInstanceOfs = behavior)

  def withStrictFloats(strictFloats: Boolean): Semantics =
    copy(strictFloats = strictFloats)

  def optimized: Semantics =
    copy(asInstanceOfs = this.asInstanceOfs.optimized)

  override def equals(that: Any): Boolean = that match {
    case that: Semantics =>
      this.asInstanceOfs == that.asInstanceOfs &&
      this.strictFloats  == that.strictFloats
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, asInstanceOfs.hashCode)
    acc = mixLast(acc, strictFloats.##)
    finalizeHash(acc, 1)
  }

  override def toString(): String = {
    s"""Semantics(
       |  asInstanceOfs = $asInstanceOfs,
       |  strictFloats  = $strictFloats
       |)""".stripMargin
  }

  private def copy(
      asInstanceOfs: CheckedBehavior = this.asInstanceOfs,
      strictFloats: Boolean = this.strictFloats): Semantics = {
    new Semantics(
        asInstanceOfs = asInstanceOfs,
        strictFloats  = strictFloats)
  }
}

object Semantics {
  private val HashSeed =
    scala.util.hashing.MurmurHash3.stringHash(classOf[Semantics].getName)

  val Defaults = new Semantics(
      asInstanceOfs = CheckedBehavior.Fatal,
      strictFloats  = false)
}
