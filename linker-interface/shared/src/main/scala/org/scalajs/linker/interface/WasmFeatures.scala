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

/** WebAssembly features to use when linking.
 *
 *  The options in `WasmFeatures` specify what features of modern versions of
 *  WebAssembly are used by the Scala.js linker.
 */
final class WasmFeatures private (
    /* We define `val`s separately below so that we can attach Scaladoc to them
     * (putting Scaladoc comments on constructor param `val`s has no effect).
     */
    _customDescriptors: Boolean
) {
  import WasmFeatures._

  private def this() = {
    this(
      _customDescriptors = false
    )
  }

  /** EXPERIMENTAL: Emit custom descriptors and JS propotypes for `@JSExport`.
   *
   *  Default: `false`
   *
   *  @see [[https://github.com/WebAssembly/custom-descriptors/blob/main/proposals/custom-descriptors/Overview.md]]
   */
  val customDescriptors = _customDescriptors

  def withCustomDescriptors(customDescriptors: Boolean): WasmFeatures =
    copy(customDescriptors = customDescriptors)

  override def equals(that: Any): Boolean = that match {
    case that: WasmFeatures =>
      this.customDescriptors == that.customDescriptors
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mixLast(acc, customDescriptors.##)
    finalizeHash(acc, 1)
  }

  override def toString(): String = {
    s"""WasmFeatures(
       |  customDescriptors = $customDescriptors
       |)""".stripMargin
  }

  private def copy(
      customDescriptors: Boolean = this.customDescriptors
  ): WasmFeatures = {
    new WasmFeatures(
      _customDescriptors = customDescriptors
    )
  }
}

object WasmFeatures {
  private val HashSeed =
    scala.util.hashing.MurmurHash3.stringHash(classOf[WasmFeatures].getName)

  /** Default configuration of Wasm features.
   *
   *  - `customDescriptors`: false
   */
  val Defaults: WasmFeatures = new WasmFeatures()

  private[interface] implicit object WasmFeaturesFingerprint extends Fingerprint[WasmFeatures] {

    override def fingerprint(wasmFeatures: WasmFeatures): String = {
      new FingerprintBuilder("WasmFeatures")
        .addField("customDescriptors", wasmFeatures.customDescriptors)
        .build()
    }
  }
}
