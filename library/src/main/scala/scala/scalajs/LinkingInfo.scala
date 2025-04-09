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

package scala.scalajs

import scala.scalajs.annotation.linkTimeProperty

object LinkingInfo {

  /** Returns true if we are linking for production, false otherwise.
   *
   *  `productionMode` is always equal to `!developmentMode`.
   *
   *  This ends up being constant-folded to a constant at link-time. So
   *  constant-folding, inlining, and other local optimizations can be
   *  leveraged with this "constant" to write code that should only be
   *  executed in production mode or development mode.
   *
   *  A typical usage of this method is:
   *  {{{
   *  val warningsLogger =
   *    if (productionMode) new NullLogger
   *    else new ConsoleLogger
   *  }}}
   *
   *  At link-time, `productionMode` will either be a constant true, in which
   *  case the above snippet folds into
   *  {{{
   *  val warningsLogger = new NullLogger
   *  }}}
   *  or a constant false, in which case it folds into
   *  {{{
   *  val warningsLogger = new ConsoleLogger.
   *  }}}
   *
   *  @see [[developmentMode]]
   */
  @inline @linkTimeProperty("core/productionMode")
  def productionMode: Boolean =
    linkTimePropertyBoolean("core/productionMode")

  /** Returns true if we are linking for development, false otherwise.
   *
   *  `developmentMode` is always equal to `!productionMode`.
   *
   *  This ends up being constant-folded to a constant at link-time. So
   *  constant-folding, inlining, and other local optimizations can be
   *  leveraged with this "constant" to write code that should only be
   *  executed in production mode or development mode.
   *
   *  A typical usage of this method is:
   *  {{{
   *  if (developmentMode) {
   *    performExpensiveSanityChecks()
   *  }
   *  }}}
   *
   *  At link-time, `developmentMode` will either be a constant true, in which
   *  case the above snippet folds into
   *  {{{
   *  performExpensiveSanityChecks()
   *  }}}
   *  or a constant false, in which case it is dead-code-eliminated away,
   *  yielding maximum performance in production.
   *
   *  @see [[productionMode]]
   */
  @inline
  def developmentMode: Boolean =
    !productionMode

  /** Version (edition) of the ECMAScript Language Specification that is
   *  assumed to be supported by the runtime.
   *
   *  This is an integer that represents the *edition* of the ECMAScript
   *  Language Specification. For example, ECMAScript 2015 is represented with
   *  the value `6`.
   *
   *  As an exception, ECMAScript 5.1 is represented with the value `5`.
   *
   *  This value can be used to:
   *
   *  - avoid feature tests and dead-code-eliminate polyfills (see below), or
   *  - conditionally offer library features that depend on underlying
   *    ECMAScript support.
   *
   *  ---
   *
   *  This ends up being constant-folded to a constant at link-time. So
   *  constant-folding, inlining, and other local optimizations can be
   *  leveraged with this "constant" to write polyfills that can be
   *  dead-code-eliminated.
   *
   *  A typical usage of this method is:
   *  {{{
   *  if (esVersion >= ESVersion.ES2018 || featureTest())
   *    useES2018Feature()
   *  else
   *    usePolyfill()
   *  }}}
   *
   *  At link-time, `esVersion` will either be a constant less than
   *  `ESVersion.ES2018`, in which case the above snippet folds into
   *  {{{
   *  if (featureTest())
   *    useES2018Feature()
   *  else
   *    usePolyfill()
   *  }}}
   *  or a constant greater or equal to `ESVersion.ES2018`, in which case it
   *  folds into
   *  {{{
   *  useES2018Feature()
   *  }}}
   */
  @inline @linkTimeProperty("core/esVersion")
  def esVersion: Int =
    linkTimePropertyInt("core/esVersion")

  /** Returns true if we are assuming that the target platform supports
   *  ECMAScript 6, false otherwise.
   *
   *  This is `true` if and only if `esVersion >= ESVersion.ES2015`.
   *
   *  ---
   *
   *  This ends up being constant-folded to a constant at link-time. So
   *  constant-folding, inlining, and other local optimizations can be
   *  leveraged with this "constant" to write polyfills that can be
   *  dead-code-eliminated.
   *
   *  A typical usage of this method is:
   *  {{{
   *  if (assumingES6 || featureTest())
   *    useES6Feature()
   *  else
   *    usePolyfill()
   *  }}}
   *
   *  At link-time, `assumingES6` will either be a constant false, in which
   *  case the above snippet folds into
   *  {{{
   *  if (featureTest())
   *    useES6Feature()
   *  else
   *    usePolyfill()
   *  }}}
   *  or a constant true, in which case it folds into
   *  {{{
   *  useES6Feature()
   *  }}}
   */
  @deprecated("use esVersion >= ESVersion.ES2015 instead", "1.6.0")
  @inline
  def assumingES6: Boolean =
    esVersion >= ESVersion.ES2015

  /** Whether Scala.js language features use ECMAScript 2015 (edition 6)
   *  semantics or not.
   *
   *  When `true`, the following semantics apply:
   *
   *  - JavaScript classes are true `class`'es, therefore a) they can extend
   *    native JavaScript `class`'es and b) they inherit static members from
   *    their parent class.
   *  - Lambdas for `js.Function`s that are not also `js.ThisFunction`s are
   *    JavaScript arrow functions (`=>`). Lambdas for `js.ThisFunction`s are
   *    `function` functions.
   *  - Throwable classes are proper JavaScript error classes, recognized as
   *    such by debuggers.
   *  - In Script (`NoModule`) mode, top-level exports are defined as `let`s.
   *
   *  When `false`, the following semantics apply:
   *
   *  - All classes defined in Scala.js are `function`s instead of `class`'es.
   *    Non-native JS classes cannot extend native JS `class`'es and they do
   *    not inherit static members from their parent class.
   *  - All lambdas for `js.Function`s are `function`s.
   *  - Throwable classes have JavaScript's `Error.prototype` in their
   *    prototype chain, but they are not considered proper error classes.
   *  - In Script (`NoModule`) mode, top-level exports are defined as `var`s.
   *
   *  Prefer reading this value instead of `esVersion` to determine which
   *  semantics apply.
   *
   *  For example, it can be used in tests whose results depend on which
   *  semantics are used.
   *
   *  ---
   *
   *  This ends up being constant-folded to a constant at link-time. So
   *  constant-folding, inlining, and other local optimizations can be
   *  leveraged with this "constant" to write alternatives that can be
   *  dead-code-eliminated.
   *
   *  A typical usage of this method is:
   *  {{{
   *  if (useECMAScript2015Semantics)
   *    implementationWithES2015Semantics()
   *  else
   *    implementationWithoutES2015Semantics()
   *  }}}
   *
   *  At link-time, `useECMAScript2015Semantics` will either be a constant
   *  true, in which case the above snippet folds into
   *  {{{
   *  implementationWithES2015Semantics()
   *  }}}
   *  or a constant false, in which case it folds into
   *  {{{
   *  implementationWithoutES2015Semantics()
   *  }}}
   */
  @inline @linkTimeProperty("core/useECMAScript2015Semantics")
  def useECMAScript2015Semantics: Boolean =
    linkTimePropertyBoolean("core/useECMAScript2015Semantics")

  /** Whether we are linking to WebAssembly.
   *
   *  This property can be used to delegate to different code paths optimized
   *  for WebAssembly rather than for JavaScript.
   *
   *  ---
   *
   *  This ends up being constant-folded to a constant at link-time. So
   *  constant-folding, inlining, and other local optimizations can be
   *  leveraged with this "constant" to write alternatives that can be
   *  dead-code-eliminated.
   *
   *  A typical usage of this method is:
   *  {{{
   *  if (isWebAssembly)
   *    implementationOptimizedForWebAssembly()
   *  else
   *    implementationOptimizedForJavaScript()
   *  }}}
   *
   *  At link-time, `isWebAssembly` will either be a constant
   *  true, in which case the above snippet folds into
   *  {{{
   *  implementationOptimizedForWebAssembly()
   *  }}}
   *  or a constant false, in which case it folds into
   *  {{{
   *  implementationOptimizedForJavaScript()
   *  }}}
   */
  @inline @linkTimeProperty("core/isWebAssembly")
  def isWebAssembly: Boolean =
    linkTimePropertyBoolean("core/isWebAssembly")

  /** Version of the linker. */
  @inline @linkTimeProperty("core/linkerVersion")
  def linkerVersion: String =
    linkTimePropertyString("core/linkerVersion")

  /** Link-time conditional branching.
   *
   *  A `linkTimeIf` expression behaves like an `if`, but it is guaranteed to
   *  be resolved at link-time. This prevents the unused branch to be linked at
   *  all. It can therefore reference APIs or language features that would
   *  otherwise fail to link.
   *
   *  The condition `cond` can be constructed using:
   *
   *  - Calls to methods annotated with `@linkTimeProperty`
   *  - Integer or boolean constants
   *  - Binary operators that return a boolean value
   *
   *  A typical use case is to leverage the `**` operator on JavaScript
   *  `bigint`s if it is available, and otherwise fall back on using Scala
   *  `BigInt`s. Indeed, the `**` operator refuses to link when the target
   *  `esVersion` is too low.
   *
   *  {{{
   *  // Returns true iff 2^x < 10^y, for x and y positive integers
   *  def compareTwoPowTenPow(x: Int, y: Int): Boolean = {
   *    import scala.scalajs.LinkingInfo._
   *    linkTimeIf(esVersion >= ESVersion.ES2020) {
   *      // JS bigints are available, and a fortiori their ** operator
   *      (js.BigInt(2) ** js.BigInt(x)) < (js.BigInt(10) ** js.BigInt(y))
   *    } {
   *      // Fall back on Scala's BigInt's, which use a lot more code size
   *      BigInt(2).pow(x) < BigInt(10).pow(y)
   *    }
   *  }
   *  }}}
   */
  def linkTimeIf[T](cond: Boolean)(thenp: T)(elsep: T): T =
    throw new Error("stub")

  /** Constants for the value of `esVersion`. */
  object ESVersion {
    /** ECMAScrîpt 5.1. */
    final val ES5_1 = 5

    /** ECMAScript 2015 (6th edition). */
    final val ES2015 = 6

    /** ECMAScript 2016 (7th edition).
     *
     *  Contains the following notable features:
     *
     *  - The `**` operator for numbers
     *  - `async`/`await`
     */
    final val ES2016 = 7

    /** ECMAScript 2017 (8th edition).
     *
     *  Contains the following notable features:
     *
     *  - Async functions
     *  - Shared Memory and Atomics (via `SharedArrayBuffer`)
     *  - `Object.values`, `Object.entries`, and `Object.getOwnPropertyDescriptors`
     */
    final val ES2017 = 8

    /** ECMAScript 2018 (9th edition).
     *
     *  Contains the following notable features:
     *
     *  - Asynchronous iteration via the `AsyncIterator` protocol and async generators
     *  - Regular expression features: the dotAll flag `'s'`, named capture groups,
     *    Unicode property escapes (`\p{}` and `\P{}`) and look-behind assertions
     *  - Rest parameter and spread operator support for object properties
     */
    final val ES2018 = 9

    /** ECMAScript 2019 (10th edition).
     *
     *  Contains the following notable features:
     *
     *  - Minor additions to the built-in library functions
     */
    final val ES2019 = 10

    /** ECMAScript 2020 (11th edition).
     *
     *  Contains the following notable features:
     *
     *  - Dynamic `import()` calls
     *  - `BigInt`
     *  - `globalThis`
     *  - `export * as ns from 'module'`
     *  - `import.meta`
     */
    final val ES2020 = 11

    /** ECMAScript 2021 (12th edition).
     *
     *  Contains the following notable features:
     *
     *  - `WeakRef` and `FinalizationRegistry`
     *  - `AggregateError`
     *  - Separators for numeric literals (e.g., `1_000`)
     */
    final val ES2021 = 12
  }

  private[scalajs] def linkTimePropertyInt(name: String): Int =
    throw new java.lang.Error("stub")

  private[scalajs] def linkTimePropertyBoolean(name: String): Boolean =
    throw new java.lang.Error("stub")

  private[scalajs] def linkTimePropertyString(name: String): String =
    throw new java.lang.Error("stub")
}
