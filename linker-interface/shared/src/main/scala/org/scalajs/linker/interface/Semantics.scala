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
import Fingerprint.FingerprintBuilder

final class Semantics private (
    val asInstanceOfs: CheckedBehavior,
    val arrayErrors: CheckedBehavior,
    val stringIndexOutOfBounds: CheckedBehavior,
    val moduleInit: CheckedBehavior,
    val strictFloats: Boolean,
    val productionMode: Boolean,
    val runtimeClassNameMapper: Semantics.RuntimeClassNameMapper) {

  import Semantics._

  @deprecated("Use arrayErrors instead", "1.11.0")
  val arrayIndexOutOfBounds: CheckedBehavior = arrayErrors

  def withAsInstanceOfs(behavior: CheckedBehavior): Semantics =
    copy(asInstanceOfs = behavior)

  def withArrayErrors(behavior: CheckedBehavior): Semantics =
    copy(arrayErrors = behavior)

  @deprecated("Use withArrayErrors instead", "1.11.0")
  def withArrayIndexOutOfBounds(behavior: CheckedBehavior): Semantics =
    withArrayErrors(behavior)

  def withStringIndexOutOfBounds(behavior: CheckedBehavior): Semantics =
    copy(stringIndexOutOfBounds = behavior)

  def withModuleInit(moduleInit: CheckedBehavior): Semantics =
    copy(moduleInit = moduleInit)

  @deprecated(
      "Scala.js now uses strict floats by default. " +
      "Non-strict float semantics are deprecated and will eventually be " +
      "removed.",
      "1.9.0")
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
        arrayErrors = this.arrayErrors.optimized,
        stringIndexOutOfBounds = this.stringIndexOutOfBounds.optimized,
        moduleInit = this.moduleInit.optimized,
        productionMode = true)
  }

  override def equals(that: Any): Boolean = that match {
    case that: Semantics =>
      this.asInstanceOfs == that.asInstanceOfs &&
      this.arrayErrors == that.arrayErrors &&
      this.stringIndexOutOfBounds == that.stringIndexOutOfBounds &&
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
    acc = mix(acc, arrayErrors.##)
    acc = mix(acc, stringIndexOutOfBounds.##)
    acc = mix(acc, moduleInit.##)
    acc = mix(acc, strictFloats.##)
    acc = mix(acc, productionMode.##)
    acc = mixLast(acc, runtimeClassNameMapper.##)
    finalizeHash(acc, 7)
  }

  override def toString(): String = {
    s"""Semantics(
       |  asInstanceOfs          = $asInstanceOfs,
       |  arrayErrors            = $arrayErrors,
       |  stringIndexOutOfBounds = $stringIndexOutOfBounds,
       |  moduleInit             = $moduleInit,
       |  strictFloats           = $strictFloats,
       |  productionMode         = $productionMode
       |)""".stripMargin
  }

  private def copy(
      asInstanceOfs: CheckedBehavior = this.asInstanceOfs,
      arrayErrors: CheckedBehavior = this.arrayErrors,
      stringIndexOutOfBounds: CheckedBehavior = this.stringIndexOutOfBounds,
      moduleInit: CheckedBehavior = this.moduleInit,
      strictFloats: Boolean = this.strictFloats,
      productionMode: Boolean = this.productionMode,
      runtimeClassNameMapper: RuntimeClassNameMapper =
        this.runtimeClassNameMapper): Semantics = {
    new Semantics(
        asInstanceOfs = asInstanceOfs,
        arrayErrors = arrayErrors,
        stringIndexOutOfBounds = stringIndexOutOfBounds,
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

    private[interface] def apply(className: String): String = {
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
    private final case class RegexReplace(pattern: String, flags: Int,
        replacement: String)(
        val compiledPattern: java.util.regex.Pattern)
        extends RuntimeClassNameMapper

    private final case class AndThen(first: RuntimeClassNameMapper,
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

    private[interface] implicit object RuntimeClassNameMapperFingerprint
        extends Fingerprint[RuntimeClassNameMapper] {

      override def fingerprint(mapper: RuntimeClassNameMapper): String = {
        mapper match {
          case KeepAll    => "KeepAll"
          case DiscardAll => "DiscardAll"

          case RegexReplace(pattern, flags, replacement) =>
            new FingerprintBuilder("RegexReplace")
              .addField("pattern", pattern)
              .addField("flags", flags)
              .addField("replacement", replacement)
              .build()

          case AndThen(first, second) =>
            new FingerprintBuilder("AndThen")
              .addField("first", fingerprint(first))
              .addField("second", fingerprint(second))
              .build()
        }
      }
    }
  }

  private[interface] implicit object SemanticsFingerprint
      extends Fingerprint[Semantics] {

    override def fingerprint(semantics: Semantics): String = {
      new FingerprintBuilder("Semantics")
        .addField("asInstanceOfs", semantics.asInstanceOfs)
        .addField("arrayErrors", semantics.arrayErrors)
        .addField("stringIndexOutOfBounds", semantics.stringIndexOutOfBounds)
        .addField("moduleInit", semantics.moduleInit)
        .addField("strictFloats", semantics.strictFloats)
        .addField("productionMode", semantics.productionMode)
        .addField("runtimeClassNameMapper", semantics.runtimeClassNameMapper)
        .build()
    }
  }

  val Defaults: Semantics = new Semantics(
      asInstanceOfs = Fatal,
      arrayErrors = Fatal,
      stringIndexOutOfBounds = Fatal,
      moduleInit = Unchecked,
      strictFloats = true,
      productionMode = false,
      runtimeClassNameMapper = RuntimeClassNameMapper.keepAll())
}
