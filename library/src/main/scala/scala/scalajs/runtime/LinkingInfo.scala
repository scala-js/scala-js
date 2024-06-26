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

package scala.scalajs.runtime

import scala.scalajs.js

/** Information about link-time configuration of Scala.js. */
sealed trait LinkingInfo extends js.Object {
  /** Version (edition) of ECMAScript that is assumed to be supported by the
   *  runtime.
   *
   *  This is an integer that represents the *edition* of the ECMAScript
   *  Language Specification. For example, ECMAScript 2015 is represented with
   *  the value `6`.
   *
   *  As an exception, ECMAScript 5.1 is represented with the value `5`.
   */
  val esVersion: Int

  // Note: this cannot be renamed because it would prevent a newer linker from linking an older library
  /** Whether Scala.js language features use ECMAScript 2015 (edition 6)
   *  semantics or not.
   *
   *  For historical reasons, this is called `assumingES6`, but a better name
   *  would be `useECMAScript2015Semantics`.
   */
  val assumingES6: Boolean

  /** Whether we are linking to WebAssembly.
   *
   *  This property can be used to delegate to different code paths optimized
   *  for WebAssembly rather than for JavaScript.
   */
  val isWebAssembly: Boolean

  /** Whether we are linking in production mode. */
  val productionMode: Boolean

  /** Version of the linker */
  val linkerVersion: String

  /** The value of the file-level JavaScript `this`. */
  val fileLevelThis: Any
}
