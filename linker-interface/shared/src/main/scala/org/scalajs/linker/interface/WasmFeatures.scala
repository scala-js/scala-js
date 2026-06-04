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
    _useJSPI: Boolean
) {
  import WasmFeatures._

  private def this() = {
    this(
      _useJSPI = false
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

  override def equals(that: Any): Boolean = that match {
    case that: WasmFeatures =>
      this.useJSPI == that.useJSPI
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mixLast(acc, useJSPI.##)
    finalizeHash(acc, 1)
  }

  override def toString(): String = {
    s"""WasmFeatures(
       |  useJSPI = $useJSPI
       |)""".stripMargin
  }

  private def copy(
      useJSPI: Boolean = this.useJSPI
  ): WasmFeatures = {
    new WasmFeatures(
      _useJSPI = useJSPI
    )
  }
}

object WasmFeatures {
  private val HashSeed =
    scala.util.hashing.MurmurHash3.stringHash(classOf[WasmFeatures].getName)

  /** Default configuration of Wasm features.
   *
   *  - `useJSPI`: false
   */
  val Defaults: WasmFeatures = new WasmFeatures()

  private[interface] implicit object WasmFeaturesFingerprint extends Fingerprint[WasmFeatures] {

    override def fingerprint(wasmFeatures: WasmFeatures): String = {
      new FingerprintBuilder("WasmFeatures")
        .addField("useJSPI", wasmFeatures.useJSPI)
        .build()
    }
  }
}
