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

package object wasm {

  /** Denotes a method body as imported from a Wasm host. */
  def native: Nothing = {
    throw new java.lang.Error(
        "A Wasm native method has been called on the JVM.")
  }
}
