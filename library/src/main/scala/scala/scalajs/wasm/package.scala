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

  /** Denotes a method body as imported from a Wasm host.
   *
   *  This is used as the right-hand-side of `@WasmImport`ed function definitions:
   *
   *  {{{
   *  object Imports {
   *    @WasmImport("some-module", "some-function")
   *    def someFunction(x: Int): Int = wasm.native
   *  }
   *  }}}
   */
  def native: Nothing = {
    throw new java.lang.Error(
        "A Wasm native method has been called on the JVM. " +
        "This is most likely because you tried to run Scala.js binaries on the JVM. " +
        "Make sure you are using the JVM version of the libraries.")
  }
}
