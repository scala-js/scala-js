/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.sem

import scala.collection.immutable.Traversable

import org.scalajs.core.tools.linker.LinkedClass

final class Semantics private (
    val asInstanceOfs: CheckedBehavior,
    val moduleInit: CheckedBehavior,
    val strictFloats: Boolean,
    val productionMode: Boolean,
    val runtimeClassName: Semantics.RuntimeClassNameFunction) {

  import Semantics._

  def withAsInstanceOfs(behavior: CheckedBehavior): Semantics =
    copy(asInstanceOfs = behavior)

  def withModuleInit(moduleInit: CheckedBehavior): Semantics =
    copy(moduleInit = moduleInit)

  def withStrictFloats(strictFloats: Boolean): Semantics =
    copy(strictFloats = strictFloats)

  def withProductionMode(productionMode: Boolean): Semantics =
    copy(productionMode = productionMode)

  def withRuntimeClassName(runtimeClassName: RuntimeClassNameFunction): Semantics =
    copy(runtimeClassName = runtimeClassName)

  def optimized: Semantics = {
    copy(asInstanceOfs = this.asInstanceOfs.optimized,
        moduleInit = this.moduleInit.optimized,
        productionMode = true)
  }

  override def equals(that: Any): Boolean = that match {
    case that: Semantics =>
      this.asInstanceOfs  == that.asInstanceOfs &&
      this.moduleInit     == that.moduleInit &&
      this.strictFloats   == that.strictFloats &&
      this.productionMode == that.productionMode
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, asInstanceOfs.hashCode)
    acc = mix(acc, moduleInit.hashCode)
    acc = mix(acc, strictFloats.##)
    acc = mixLast(acc, productionMode.##)
    finalizeHash(acc, 4)
  }

  override def toString(): String = {
    s"""Semantics(
       |  asInstanceOfs  = $asInstanceOfs,
       |  moduleInit     = $moduleInit,
       |  strictFloats   = $strictFloats,
       |  productionMode = $productionMode
       |)""".stripMargin
  }

  /** Checks whether the given semantics setting is Java compliant */
  def isCompliant(name: String): Boolean = name match {
    case "asInstanceOfs" => asInstanceOfs == CheckedBehavior.Compliant
    case "moduleInit"    => moduleInit == CheckedBehavior.Compliant
    case "strictFloats"  => strictFloats
    case _               => false
  }

  /** Retrieve a list of semantics which are set to compliant */
  def compliants: List[String] = {
    def cl(name: String, cond: Boolean) = if (cond) List(name) else Nil

    cl("asInstanceOfs", asInstanceOfs == CheckedBehavior.Compliant) ++
    cl("moduleInit",    moduleInit == CheckedBehavior.Compliant) ++
    cl("strictFloats",  strictFloats)
  }

  private def copy(
      asInstanceOfs: CheckedBehavior = this.asInstanceOfs,
      moduleInit: CheckedBehavior = this.moduleInit,
      strictFloats: Boolean = this.strictFloats,
      productionMode: Boolean = this.productionMode,
      runtimeClassName: RuntimeClassNameFunction = this.runtimeClassName): Semantics = {
    new Semantics(
        asInstanceOfs    = asInstanceOfs,
        moduleInit       = moduleInit,
        strictFloats     = strictFloats,
        productionMode   = productionMode,
        runtimeClassName = runtimeClassName)
  }
}

object Semantics {
  private val HashSeed =
    scala.util.hashing.MurmurHash3.stringHash(classOf[Semantics].getName)

  type RuntimeClassNameFunction = LinkedClass => String

  val Defaults: Semantics = new Semantics(
      asInstanceOfs    = CheckedBehavior.Fatal,
      moduleInit       = CheckedBehavior.Unchecked,
      strictFloats     = false,
      productionMode   = false,
      runtimeClassName = _.fullName)

  def compliantTo(semantics: Traversable[String]): Semantics = {
    import Defaults._
    import CheckedBehavior._

    val semsSet = semantics.toSet

    def sw[T](name: String, compliant: T, default: T): T =
      if (semsSet.contains(name)) compliant else default

    new Semantics(
        asInstanceOfs    = sw("asInstanceOfs", Compliant, asInstanceOfs),
        moduleInit       = sw("moduleInit",    Compliant, moduleInit),
        strictFloats     = sw("strictFloats",  true,      strictFloats),
        productionMode   = false,
        runtimeClassName = Defaults.runtimeClassName)
  }
}
