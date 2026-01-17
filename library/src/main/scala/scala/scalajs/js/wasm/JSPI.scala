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

package scala.scalajs.js.wasm

import scala.scalajs.js

/** Features related to the WebAssembly JavaScript Promise Integration (JSPI). */
object JSPI {

  /** Allow arbitrary calls to `js.await()`.
   *
   *  Import this object to allow arbitrary calls to `js.await()`, even when
   *  they are not directly nested within a `js.async { ... }` block.
   *  The resulting code will then only link when targeting WebAssembly.
   *
   *  @see [[scala.scalajs.js.async]]
   */
  implicit object allowOrphanJSAwait extends js.AwaitPermit
}
