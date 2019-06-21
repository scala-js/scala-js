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

package org.scalajs.core.tools.sem

import scala.collection.immutable.Traversable

import org.scalajs.core.tools.linker.LinkedClass

import CheckedBehavior._

final class Semantics private (
    val asInstanceOfs: CheckedBehavior,
    val arrayIndexOutOfBounds: CheckedBehavior,
    val moduleInit: CheckedBehavior,
    val strictFloats: Boolean,
    val productionMode: Boolean,
    val runtimeClassNameMapper: Semantics.RuntimeClassNameMapper) {

  import Semantics._

  @deprecated("Use runtimeClassNameMapper instead.", "0.6.19")
  def runtimeClassName: Semantics.RuntimeClassNameFunction =
    runtimeClassNameMapper(_)

  def withAsInstanceOfs(behavior: CheckedBehavior): Semantics =
    copy(asInstanceOfs = behavior)

  def withArrayIndexOutOfBounds(behavior: CheckedBehavior): Semantics =
    copy(arrayIndexOutOfBounds = behavior)

  def withModuleInit(moduleInit: CheckedBehavior): Semantics =
    copy(moduleInit = moduleInit)

  def withStrictFloats(strictFloats: Boolean): Semantics =
    copy(strictFloats = strictFloats)

  def withProductionMode(productionMode: Boolean): Semantics =
    copy(productionMode = productionMode)

  def withRuntimeClassNameMapper(
      runtimeClassNameMapper: RuntimeClassNameMapper): Semantics = {
    copy(runtimeClassNameMapper = runtimeClassNameMapper)
  }

  @deprecated("Use withRuntimeClassNameMapper instead.", "0.6.19")
  def withRuntimeClassName(runtimeClassName: RuntimeClassNameFunction): Semantics =
    withRuntimeClassNameMapper(RuntimeClassNameMapper.custom(runtimeClassName))

  def optimized: Semantics = {
    copy(asInstanceOfs = this.asInstanceOfs.optimized,
        arrayIndexOutOfBounds = this.arrayIndexOutOfBounds.optimized,
        moduleInit = this.moduleInit.optimized,
        productionMode = true)
  }

  override def equals(that: Any): Boolean = that match {
    case that: Semantics =>
      this.asInstanceOfs == that.asInstanceOfs &&
      this.arrayIndexOutOfBounds == that.arrayIndexOutOfBounds &&
      this.moduleInit == that.moduleInit &&
      this.strictFloats == that.strictFloats &&
      this.productionMode == that.productionMode &&
      this.runtimeClassNameMapper == that.runtimeClassNameMapper
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, asInstanceOfs.##)
    acc = mix(acc, arrayIndexOutOfBounds.##)
    acc = mix(acc, moduleInit.##)
    acc = mix(acc, strictFloats.##)
    acc = mix(acc, productionMode.##)
    acc = mixLast(acc, runtimeClassNameMapper.##)
    finalizeHash(acc, 6)
  }

  override def toString(): String = {
    s"""Semantics(
       |  asInstanceOfs         = $asInstanceOfs,
       |  arrayIndexOutOfBounds = $arrayIndexOutOfBounds,
       |  moduleInit            = $moduleInit,
       |  strictFloats          = $strictFloats,
       |  productionMode        = $productionMode
       |)""".stripMargin
  }

  /** Checks whether the given semantics setting is Java compliant */
  def isCompliant(name: String): Boolean = name match {
    case "asInstanceOfs"         => asInstanceOfs == Compliant
    case "arrayIndexOutOfBounds" => arrayIndexOutOfBounds == Compliant
    case "moduleInit"            => moduleInit == Compliant
    case "strictFloats"          => strictFloats
    case _                       => false
  }

  /** Retrieve a list of semantics which are set to compliant */
  def compliants: List[String] = {
    def cl(name: String, cond: Boolean) = if (cond) List(name) else Nil

    cl("asInstanceOfs", asInstanceOfs == Compliant) ++
    cl("arrayIndexOutOfBounds", arrayIndexOutOfBounds == Compliant) ++
    cl("moduleInit", moduleInit == Compliant) ++
    cl("strictFloats", strictFloats)
  }

  private def copy(
      asInstanceOfs: CheckedBehavior = this.asInstanceOfs,
      arrayIndexOutOfBounds: CheckedBehavior = this.arrayIndexOutOfBounds,
      moduleInit: CheckedBehavior = this.moduleInit,
      strictFloats: Boolean = this.strictFloats,
      productionMode: Boolean = this.productionMode,
      runtimeClassNameMapper: RuntimeClassNameMapper =
        this.runtimeClassNameMapper): Semantics = {
    new Semantics(
        asInstanceOfs = asInstanceOfs,
        arrayIndexOutOfBounds = arrayIndexOutOfBounds,
        moduleInit = moduleInit,
        strictFloats = strictFloats,
        productionMode = productionMode,
        runtimeClassNameMapper = runtimeClassNameMapper)
  }
}

object Semantics {
  private val HashSeed =
    scala.util.hashing.MurmurHash3.stringHash(classOf[Semantics].getName)

  sealed abstract class RuntimeClassNameMapper {
    import RuntimeClassNameMapper._

    def andThen(that: RuntimeClassNameMapper): RuntimeClassNameMapper = {
      require(!that.isInstanceOf[Custom],
          "RuntimeClassNameMapper.custom(...) is not a valid argument to " +
          "RuntimeClassNameMapper#andThen(), because it takes a LinkedClass " +
          "as input instead of a String.")
      AndThen(this, that)
    }

    private[tools] def apply(linkedClass: LinkedClass): String = {
      def rec(mapper: RuntimeClassNameMapper, className: String): String = {
        mapper match {
          case KeepAll =>
            className
          case DiscardAll =>
            ""
          case mapper @ RegexReplace(_, _, replacement) =>
            mapper.compiledPattern.matcher(className).replaceAll(replacement)
          case AndThen(first, second) =>
            rec(second, rec(first, className))
          case Custom(mapper) =>
            /* Discards `className`, but that's fine because we cannot
             * construct an AndThen(_, Custom()).
             */
            mapper(linkedClass)
        }
      }

      rec(this, linkedClass.fullName)
    }
  }

  object RuntimeClassNameMapper {
    private case object KeepAll extends RuntimeClassNameMapper

    private case object DiscardAll extends RuntimeClassNameMapper

    /* We use `pattern` and `flags` in the case parameters, rather than the
     * `j.u.regex.Pattern`, because the latter does not have meaningful
     * equality.
     */
    private case class RegexReplace(pattern: String, flags: Int,
        replacement: String)(
        val compiledPattern: java.util.regex.Pattern)
        extends RuntimeClassNameMapper

    private case class AndThen(first: RuntimeClassNameMapper,
        second: RuntimeClassNameMapper)
        extends RuntimeClassNameMapper

    /** For compatibility with `RuntimeClassNameFunction`s only. */
    private case class Custom(mapper: LinkedClass => String)
        extends RuntimeClassNameMapper {
      /* For compatibility of `Semantics.==` of previous versions, we need to
       * consider all `Custom` instances as being equal, even though this
       * definitely breaks the case class contract.
       * Since this is deprecated, this issue will eventually go away.
       */

      override def equals(that: Any): Boolean = that.isInstanceOf[Custom]

      override def hashCode(): Int = 369581025 // generated at random
    }

    def keepAll(): RuntimeClassNameMapper = KeepAll

    def discardAll(): RuntimeClassNameMapper = DiscardAll

    /** Returns a mapper that performs regular expression-based replacements.
     *
     *  Given an input class name `className`, the mapper will return a new
     *  class name equivalent to
     *  {{{
     *  pattern.matcher(className).replaceAll(replacement)
     *  }}}
     */
    def regexReplace(pattern: java.util.regex.Pattern,
        replacement: String): RuntimeClassNameMapper = {
      RegexReplace(pattern.pattern(), pattern.flags(), replacement)(pattern)
    }

    /** Returns a mapper that performs regular expression-based replacements.
     *
     *  Given an input class name `className`, the mapper will return a new
     *  class name equivalent to
     *  {{{
     *  regex.replaceAllIn(className, replacement)
     *  }}}
     */
    def regexReplace(regex: scala.util.matching.Regex,
        replacement: String): RuntimeClassNameMapper = {
      regexReplace(regex.pattern, replacement)
    }

    @deprecated("Will be removed in Scala.js 1.x.", "0.6.19")
    def custom(mapper: LinkedClass => String): RuntimeClassNameMapper =
      Custom(mapper)
  }

  @deprecated("Use RuntimeClassNameMapper instead.", "0.6.19")
  type RuntimeClassNameFunction = LinkedClass => String

  val Defaults: Semantics = new Semantics(
      asInstanceOfs = Fatal,
      arrayIndexOutOfBounds = Fatal,
      moduleInit = Unchecked,
      strictFloats = false,
      productionMode = false,
      runtimeClassNameMapper = RuntimeClassNameMapper.keepAll())

  def compliantTo(semantics: Set[String]): Semantics = {
    import Defaults._

    def sw[T](name: String, compliant: T, default: T): T =
      if (semantics.contains(name)) compliant else default

    new Semantics(
        asInstanceOfs = sw("asInstanceOfs", Compliant, asInstanceOfs),
        arrayIndexOutOfBounds =
          sw("arrayIndexOutOfBounds", Compliant, arrayIndexOutOfBounds),
        moduleInit = sw("moduleInit", Compliant, moduleInit),
        strictFloats = sw("strictFloats", true, strictFloats),
        productionMode = false,
        runtimeClassNameMapper = RuntimeClassNameMapper.keepAll())
  }

  @deprecated("Use the overload with a Set instead.", "0.6.29")
  def compliantTo(semantics: Traversable[String]): Semantics =
    compliantTo(semantics.toSet)
}
