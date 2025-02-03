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

import scala.annotation.compileTimeOnly

import CheckedBehavior._
import Fingerprint.FingerprintBuilder

final class Semantics private (
    val asInstanceOfs: CheckedBehavior,
    val arrayIndexOutOfBounds: CheckedBehavior,
    val arrayStores: CheckedBehavior,
    val negativeArraySizes: CheckedBehavior,
    val nullPointers: CheckedBehavior,
    val stringIndexOutOfBounds: CheckedBehavior,
    val moduleInit: CheckedBehavior,
    val productionMode: Boolean,
    val runtimeClassNameMapper: Semantics.RuntimeClassNameMapper) {

  import Semantics._

  @deprecated(
      "non-strict floats are not supported anymore; strictFloats is always true",
      since = "1.19.0")
  val strictFloats: Boolean = true

  def withAsInstanceOfs(behavior: CheckedBehavior): Semantics =
    copy(asInstanceOfs = behavior)

  def withArrayIndexOutOfBounds(behavior: CheckedBehavior): Semantics =
    copy(arrayIndexOutOfBounds = behavior)

  def withArrayStores(behavior: CheckedBehavior): Semantics =
    copy(arrayStores = behavior)

  def withNegativeArraySizes(behavior: CheckedBehavior): Semantics =
    copy(negativeArraySizes = behavior)

  def withNullPointers(behavior: CheckedBehavior): Semantics =
    copy(nullPointers = behavior)

  def withStringIndexOutOfBounds(behavior: CheckedBehavior): Semantics =
    copy(stringIndexOutOfBounds = behavior)

  def withModuleInit(moduleInit: CheckedBehavior): Semantics =
    copy(moduleInit = moduleInit)

  @compileTimeOnly(
      "Non-strict floats are not supported anymore. " +
      "The default is `true` and cannot be turned to `false`.")
  def withStrictFloats(strictFloats: Boolean): Semantics =
    this

  def withProductionMode(productionMode: Boolean): Semantics =
    copy(productionMode = productionMode)

  def withRuntimeClassNameMapper(
      runtimeClassNameMapper: RuntimeClassNameMapper): Semantics = {
    copy(runtimeClassNameMapper = runtimeClassNameMapper)
  }

  def optimized: Semantics = {
    copy(asInstanceOfs = this.asInstanceOfs.optimized,
        arrayIndexOutOfBounds = this.arrayIndexOutOfBounds.optimized,
        arrayStores = this.arrayStores.optimized,
        negativeArraySizes = this.negativeArraySizes.optimized,
        nullPointers = this.nullPointers.optimized,
        stringIndexOutOfBounds = this.stringIndexOutOfBounds.optimized,
        moduleInit = this.moduleInit.optimized,
        productionMode = true)
  }

  override def equals(that: Any): Boolean = that match {
    case that: Semantics =>
      this.asInstanceOfs == that.asInstanceOfs &&
      this.arrayIndexOutOfBounds == that.arrayIndexOutOfBounds &&
      this.arrayStores == that.arrayStores &&
      this.negativeArraySizes == that.negativeArraySizes &&
      this.nullPointers == that.nullPointers &&
      this.stringIndexOutOfBounds == that.stringIndexOutOfBounds &&
      this.moduleInit == that.moduleInit &&
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
    acc = mix(acc, arrayStores.##)
    acc = mix(acc, negativeArraySizes.##)
    acc = mix(acc, nullPointers.##)
    acc = mix(acc, stringIndexOutOfBounds.##)
    acc = mix(acc, moduleInit.##)
    acc = mix(acc, productionMode.##)
    acc = mixLast(acc, runtimeClassNameMapper.##)
    finalizeHash(acc, 10)
  }

  override def toString(): String = {
    s"""Semantics(
       |  asInstanceOfs          = $asInstanceOfs,
       |  arrayIndexOutOfBounds  = $arrayIndexOutOfBounds,
       |  arrayStores            = $arrayStores,
       |  negativeArraySizes     = $negativeArraySizes,
       |  nullPointers           = $nullPointers,
       |  stringIndexOutOfBounds = $stringIndexOutOfBounds,
       |  moduleInit             = $moduleInit,
       |  productionMode         = $productionMode
       |)""".stripMargin
  }

  private def copy(
      asInstanceOfs: CheckedBehavior = this.asInstanceOfs,
      arrayIndexOutOfBounds: CheckedBehavior = this.arrayIndexOutOfBounds,
      arrayStores: CheckedBehavior = this.arrayStores,
      negativeArraySizes: CheckedBehavior = this.negativeArraySizes,
      nullPointers: CheckedBehavior = this.nullPointers,
      stringIndexOutOfBounds: CheckedBehavior = this.stringIndexOutOfBounds,
      moduleInit: CheckedBehavior = this.moduleInit,
      productionMode: Boolean = this.productionMode,
      runtimeClassNameMapper: RuntimeClassNameMapper =
        this.runtimeClassNameMapper): Semantics = {
    new Semantics(
        asInstanceOfs = asInstanceOfs,
        arrayIndexOutOfBounds = arrayIndexOutOfBounds,
        arrayStores = arrayStores,
        negativeArraySizes = negativeArraySizes,
        nullPointers = nullPointers,
        stringIndexOutOfBounds = stringIndexOutOfBounds,
        moduleInit = moduleInit,
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
        .addField("arrayIndexOutOfBounds", semantics.arrayIndexOutOfBounds)
        .addField("arrayStores", semantics.arrayStores)
        .addField("negativeArraySizes", semantics.negativeArraySizes)
        .addField("nullPointers", semantics.nullPointers)
        .addField("stringIndexOutOfBounds", semantics.stringIndexOutOfBounds)
        .addField("moduleInit", semantics.moduleInit)
        .addField("productionMode", semantics.productionMode)
        .addField("runtimeClassNameMapper", semantics.runtimeClassNameMapper)
        .build()
    }
  }

  val Defaults: Semantics = new Semantics(
      asInstanceOfs = Fatal,
      arrayIndexOutOfBounds = Fatal,
      arrayStores = Fatal,
      negativeArraySizes = Fatal,
      nullPointers = Fatal,
      stringIndexOutOfBounds = Fatal,
      moduleInit = Unchecked,
      productionMode = false,
      runtimeClassNameMapper = RuntimeClassNameMapper.keepAll())
}
