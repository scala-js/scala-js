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
    _useJSPI: Boolean,
    _useCustomDescriptors: Boolean
) {
  import WasmFeatures._

  private def this() = {
    this(
      _useJSPI = false,
      _useCustomDescriptors = false
    )
  }

  /** Enables JSPI, the JavaScript Promise Integration proposal.
   *
   *  Default: `false`
   *
   *  When `true`, the backend supports usage of `js.async/js.await`.
   *
   *  @see [[https://github.com/WebAssembly/js-promise-integration/blob/main/proposals/js-promise-integration/Overview.md]]
   */
  val useJSPI = _useJSPI

  def withUseJSPI(useJSPI: Boolean): WasmFeatures =
    copy(useJSPI = useJSPI)

  /** EXPERIMENTAL: Emit custom descriptors and JS propotypes for `@JSExport`.
   *
   *  Default: `false`
   *
   *  @see [[https://github.com/WebAssembly/custom-descriptors/blob/main/proposals/custom-descriptors/Overview.md]]
   */
  def experimentalUseCustomDescriptors: Boolean = _useCustomDescriptors // def while experimental

  def withExperimentalUseCustomDescriptors(useCustomDescriptors: Boolean): WasmFeatures =
    copy(useCustomDescriptors = useCustomDescriptors)

  override def equals(that: Any): Boolean = that match {
    case that: WasmFeatures =>
      this.useJSPI == that.useJSPI &&
      this.experimentalUseCustomDescriptors == that.experimentalUseCustomDescriptors
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, useJSPI.##)
    acc = mixLast(acc, experimentalUseCustomDescriptors.##)
    finalizeHash(acc, 2)
  }

  override def toString(): String = {
    s"""WasmFeatures(
       |  useJSPI = $useJSPI,
       |  experimentalUseCustomDescriptors = $experimentalUseCustomDescriptors,
       |)""".stripMargin
  }

  private def copy(
      useJSPI: Boolean = this.useJSPI,
      useCustomDescriptors: Boolean = this.experimentalUseCustomDescriptors
  ): WasmFeatures = {
    new WasmFeatures(
      _useJSPI = useJSPI,
      _useCustomDescriptors = useCustomDescriptors
    )
  }
}

object WasmFeatures {
  private val HashSeed =
    scala.util.hashing.MurmurHash3.stringHash(classOf[WasmFeatures].getName)

  /** Default configuration of Wasm features.
   *
   *  - `useJSPI`: false
   *  - `experimentalUseCustomDescriptors`: false
   */
  val Defaults: WasmFeatures = new WasmFeatures()

  private[interface] implicit object WasmFeaturesFingerprint extends Fingerprint[WasmFeatures] {

    override def fingerprint(wasmFeatures: WasmFeatures): String = {
      new FingerprintBuilder("WasmFeatures")
        .addField("useJSPI", wasmFeatures.useJSPI)
        .addField("experimentalUseCustomDescriptors", wasmFeatures.experimentalUseCustomDescriptors)
        .build()
    }
  }
}
