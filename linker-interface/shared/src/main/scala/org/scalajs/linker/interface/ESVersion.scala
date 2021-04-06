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

final class ESVersion private (val edition: Int, val name: String)
    extends Ordered[ESVersion] {

  import ESVersion._

  def compare(that: ESVersion): Int = this.edition.compareTo(that.edition)

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode(): Int = edition.##

  override def toString(): String =
    s"$name (edition $edition)"
}

object ESVersion {
  /** ECMAScrîpt 5.1. */
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

  private[interface] implicit object ESVersionFingerprint
      extends Fingerprint[ESVersion] {

    override def fingerprint(esVersion: ESVersion): String = {
      new FingerprintBuilder("ESVersion")
        .addField("edition", esVersion.edition)
        .build()
    }
  }
}
