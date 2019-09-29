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

package org.scalajs.linker.interface

import CheckedBehavior._

final class Semantics private (
    val asInstanceOfs: CheckedBehavior,
    val arrayIndexOutOfBounds: CheckedBehavior,
    val moduleInit: CheckedBehavior,
    val strictFloats: Boolean,
    val productionMode: Boolean,
    val runtimeClassNameMapper: Semantics.RuntimeClassNameMapper) {

  import Semantics._

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

    def andThen(that: RuntimeClassNameMapper): RuntimeClassNameMapper =
      AndThen(this, that)

    private[linker] def apply(className: String): String = {
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
        }
      }

      rec(this, className)
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
  }

  val Defaults: Semantics = new Semantics(
      asInstanceOfs = Fatal,
      arrayIndexOutOfBounds = Fatal,
      moduleInit = Unchecked,
      strictFloats = false,
      productionMode = false,
      runtimeClassNameMapper = RuntimeClassNameMapper.keepAll())
}
