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

import Fingerprint.FingerprintBuilder

/** ECMAScript features to use when linking to JavaScript.
 *
 *  The options in `ESFeatures` specify what features of modern versions of
 *  JavaScript are used by the Scala.js linker.
 *
 *  - Options whose name is of the form `useX` *force* the linker to use the
 *    corresponding features, guaranteeing that the specific semantics that
 *    they provide will be used.
 *  - Options whose name is of the form `allowX` *allow* the linker to use the
 *    corresponding features if it supports them. Support for such options can
 *    be dropped in any subsequent version of the linker, including patch
 *    versions.
 *  - Options whose name is of the form `avoidX` *hint* at the linker to avoid
 *    the corresponding features *when it does not affect observable
 *    semantics*. They are related to optimizations (for performance or code
 *    size). The linker is free to ignore those options.
 */
final class ESFeatures private (
    /* We define `val`s separately below so that we can attach Scaladoc to them
     * (putting Scaladoc comments on constructor param `val`s has no effect).
     */
    _useECMAScript2015: Boolean,
    _allowBigIntsForLongs: Boolean,
    _avoidClasses: Boolean,
    _avoidLetsAndsConsts: Boolean
) {
  import ESFeatures._

  private def this() = {
    this(
        _useECMAScript2015 = true,
        _allowBigIntsForLongs = false,
        _avoidClasses = true,
        _avoidLetsAndsConsts = true
    )
  }

  /** Use ECMAScript 2015 features, such as classes and arrow functions.
   *
   *  Default: `true`
   *
   *  When `true`, the following behaviors are guaranteed:
   *
   *  - JavaScript classes are true `class`'es, therefore a) they can extend
   *    native JavaScript `class`'es and b) they inherit static members from
   *    their parent class.
   *  - Throwable classes are proper JavaScript error classes, recognized as
   *    such by debuggers.
   *  - In Script (`NoModule`) mode, top-level exports are defined as `let`s
   *    rather than `var`s, and behave as such.
   */
  val useECMAScript2015 = _useECMAScript2015

  /** EXPERIMENTAL: Primitive `Long`s *may* be compiled as primitive JavaScript
   *  `bigint`s.
   *
   *  Default: `false`
   *
   *  Future versions of Scala.js may decide to ignore this setting.
   */
  val allowBigIntsForLongs = _allowBigIntsForLongs

  /** Avoid `class`'es when using `function`s and `prototype`s has the same
   *  observable semantics.
   *
   *  Default: `true`
   *
   *  SpiderMonkey is known to exhibit terrible performance with JavaScript
   *  `class`'es, with up to an order of magnitude of performance degradation.
   *
   *  Setting this option to `true` provides a hint to the Scala.js linker to
   *  avoid using `class`'es when using other JavaScript features (typically
   *  `function`s and `prototype`s) has the same observable semantics, in order
   *  to improve expected performance. Setting it to `false` provides a hint
   *  not to avoid `class`'es. Either way, the linker is free to ignore this
   *  option.
   *
   *  Avoiding `class`'es has a negative impact on code size. If the code is
   *  only targeted at engines that are known to have good performance with
   *  `class`'es, it is desirable to set this option to `false`. If the code
   *  is targeted at browsers (among others), it is recommended to set it to
   *  `true`.
   *
   *  This option never affects the code emitted for JavaScript classes
   *  (classes extending `js.Any`), since that would have an impact on
   *  observable semantics.
   *
   *  This option is always ignored when `useECMAScript2015` is `false`.
   */
  val avoidClasses = _avoidClasses

  /** Avoid `let`s and `const`s when using `var`s has the same observable
   *  semantics.
   *
   *  Default: `true`
   *
   *  Due to their semantics in JavaScript (their Temporal Dead Zone, TDZ),
   *  `let`s and `const`s are more difficult for engines to optimize than
   *  `var`s. There have been known cases of dramatic performance issues with
   *  them, such as the Webkit issue
   *  [[https://bugs.webkit.org/show_bug.cgi?id=199866]].
   *
   *  Setting this option to `true` provides a hint to the Scala.js linker to
   *  avoid using them when using a `var` has the same observable semantics, in
   *  order to improve expected performance. Setting it to `false` provides a
   *  hint not to avoid `let`s and `const`s. Either way, the linker is free to
   *  ignore this option.
   *
   *  Using `let`s and `const`s has benefits for humans writing code as they
   *  help readability and debugging, but there is little to no benefit in
   *  using them when the code is compiler-generated.
   *
   *  This option is always ignored when `useECMAScript2015` is `false`.
   */
  val avoidLetsAndConsts = _avoidLetsAndsConsts

  def withUseECMAScript2015(useECMAScript2015: Boolean): ESFeatures =
    copy(useECMAScript2015 = useECMAScript2015)

  def withAllowBigIntsForLongs(allowBigIntsForLongs: Boolean): ESFeatures =
    copy(allowBigIntsForLongs = allowBigIntsForLongs)

  def withAvoidClasses(avoidClasses: Boolean): ESFeatures =
    copy(avoidClasses = avoidClasses)

  def withAvoidLetsAndConsts(avoidLetsAndConsts: Boolean): ESFeatures =
    copy(avoidLetsAndConsts = avoidLetsAndConsts)

  override def equals(that: Any): Boolean = that match {
    case that: ESFeatures =>
      this.useECMAScript2015 == that.useECMAScript2015 &&
      this.allowBigIntsForLongs == that.allowBigIntsForLongs &&
      this.avoidClasses == that.avoidClasses &&
      this.avoidLetsAndConsts == that.avoidLetsAndConsts
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, useECMAScript2015.##)
    acc = mix(acc, allowBigIntsForLongs.##)
    acc = mix(acc, avoidClasses.##)
    acc = mixLast(acc, avoidLetsAndConsts.##)
    finalizeHash(acc, 4)
  }

  override def toString(): String = {
    s"""ESFeatures(
       |  useECMAScript2015 = $useECMAScript2015,
       |  allowBigIntsForLongs = $allowBigIntsForLongs,
       |  avoidClasses = $avoidClasses,
       |  avoidLetsAndConsts = $avoidLetsAndConsts
       |)""".stripMargin
  }

  private def copy(
      useECMAScript2015: Boolean = this.useECMAScript2015,
      allowBigIntsForLongs: Boolean = this.allowBigIntsForLongs,
      avoidClasses: Boolean = this.avoidClasses,
      avoidLetsAndConsts: Boolean = this.avoidLetsAndConsts
  ): ESFeatures = {
    new ESFeatures(
        _useECMAScript2015 = useECMAScript2015,
        _allowBigIntsForLongs = allowBigIntsForLongs,
        _avoidClasses = avoidClasses,
        _avoidLetsAndsConsts = avoidLetsAndConsts
    )
  }
}

object ESFeatures {
  private val HashSeed =
    scala.util.hashing.MurmurHash3.stringHash(classOf[ESFeatures].getName)

  /** Default configuration of ECMAScript features.
   *
   *  - `useECMAScript2015`: true
   *  - `allowBigIntsForLongs`: false
   *  - `avoidClasses`: true
   *  - `avoidLetsAndConsts`: true
   */
  val Defaults: ESFeatures = new ESFeatures()

  private[interface] implicit object ESFeaturesFingerprint
      extends Fingerprint[ESFeatures] {

    override def fingerprint(esFeatures: ESFeatures): String = {
      new FingerprintBuilder("ESFeatures")
        .addField("useECMAScript2015", esFeatures.useECMAScript2015)
        .addField("allowBigIntsForLongs", esFeatures.allowBigIntsForLongs)
        .addField("avoidClasses", esFeatures.avoidClasses)
        .addField("avoidLetsAndConsts", esFeatures.avoidLetsAndConsts)
        .build()
    }
  }
}
