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

final class ESVersion private (val edition: Int, val name: String) extends Ordered[ESVersion] {

  import ESVersion._

  def compare(that: ESVersion): Int = this.edition.compareTo(that.edition)

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode(): Int = edition.##

  override def toString(): String =
    s"$name (edition $edition)"
}

object ESVersion {
  // !!! When we actually remove this, remove the code mentioning it in ClosureLinkerBackend.scala
  /** ECMAScript 5.1. */
  @deprecated(
      "Support for ECMAScript 5.1 is deprecated and will eventually be removed.",
      since = "1.19.0")
  val ES5_1: ESVersion = new ESVersion(5, "ECMAScript 5.1")

  /** ECMAScript 2015 (6th edition). */
  val ES2015: ESVersion = new ESVersion(6, "ECMAScript 2015")

  /** ECMAScript 2016 (7th edition).
   *
   *  Contains the following notable features:
   *
   *  - The `**` operator for numbers
   *  - `async`/`await`
   */
  val ES2016: ESVersion = new ESVersion(7, "ECMAScript 2016")

  /** ECMAScript 2017 (8th edition).
   *
   *  Contains the following notable features:
   *
   *  - Async functions
   *  - Shared Memory and Atomics (via `SharedArrayBuffer`)
   *  - `Object.values`, `Object.entries`, and `Object.getOwnPropertyDescriptors`
   */
  val ES2017: ESVersion = new ESVersion(8, "ECMAScript 2017")

  /** ECMAScript 2018 (9th edition).
   *
   *  Contains the following notable features:
   *
   *  - Asynchronous iteration via the `AsyncIterator` protocol and async generators
   *  - Regular expression features: the dotAll flag `'s'`, named capture groups,
   *    Unicode property escapes (`\p{}` and `\P{}`) and look-behind assertions
   *  - Rest parameter and spread operator support for object properties
   */
  val ES2018: ESVersion = new ESVersion(9, "ECMAScript 2018")

  /** ECMAScript 2019 (10th edition).
   *
   *  Contains the following notable features:
   *
   *  - Minor additions to the built-in library functions
   */
  val ES2019: ESVersion = new ESVersion(10, "ECMAScript 2019")

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
  val ES2020: ESVersion = new ESVersion(11, "ECMAScript 2020")

  /** ECMAScript 2021 (12th edition).
   *
   *  Contains the following notable features:
   *
   *  - `WeakRef` and `FinalizationRegistry`
   *  - `AggregateError`
   *  - Separators for numeric literals (e.g., `1_000`)
   */
  val ES2021: ESVersion = new ESVersion(12, "ECMAScript 2021")

  /** ECMAScript 2022 (13th edition).
   *
   *  Contains the following notable features:
   *
   *  - Top-level `await`
   *  - New class elements: public/private instance/static fields/methods/accessors
   *  - `RegExp` match indices with the `/d` flag
   *  - `Error.cause`
   *  - `Object.hasOwn`
   */
  val ES2022: ESVersion = new ESVersion(13, "ECMAScript 2022")

  /** ECMAScript 2023 (14th edition).
   *
   *  Contains the following notable features:
   *
   *  - New methods on Arrays and TypedArrays
   *  - `#!` comments at the top of the file
   *  - Most Symbols in weak collections
   */
  val ES2023: ESVersion = new ESVersion(14, "ECMAScript 2023")

  /** ECMAScript 2024 (15th edition).
   *
   *  Contains the following notable features:
   *
   *  - `RegExp` advanced features for sets of strings with the `/v` flag
   *  - `Atomics.waitAsync`
   */
  val ES2024: ESVersion = new ESVersion(15, "ECMAScript 2024")

  /** ECMAScript 2025 (16th edition).
   *
   *  Contains the following notable features:
   *
   *  - 16-bit floating point: `Math.f16round`, `Float16Array`, `DataView.{get,set}Float16`
   *  - `Iterator` global
   *  - `RegExp.escape`
   *  - `Promise.try`
   */
  val ES2025: ESVersion = new ESVersion(16, "ECMAScript 2025")

  /** ECMAScript 2026 (17th edition).
   *
   *  Contains the following notable features:
   *
   *  - `Uint8Array` methods for converting to/from hex and base64 strings
   *  - `Math.sumPrecise`
   */
  val ES2026: ESVersion = new ESVersion(17, "ECMAScript 2026")

  private[interface] implicit object ESVersionFingerprint extends Fingerprint[ESVersion] {

    override def fingerprint(esVersion: ESVersion): String = {
      new FingerprintBuilder("ESVersion")
        .addField("edition", esVersion.edition)
        .build()
    }
  }
}
