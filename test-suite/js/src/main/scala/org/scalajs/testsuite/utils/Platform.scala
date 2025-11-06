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

package org.scalajs.testsuite.utils

import scala.scalajs.js
import scala.scalajs.LinkingInfo.ESVersion

object Platform {

  def scalaVersion: String = BuildInfo.scalaVersion

  /** Returns `true` if and only if the code is executing on a JVM.
   *  Note: Returns `false` when executing on any JS VM.
   */
  final val executingInJVM = false

  def executingInJVMOnLowerThanJDK(version: Int): Boolean = false

  def executingInJVMWithJDKIn(range: Range): Boolean = false

  def executingInWebAssembly: Boolean = BuildInfo.isWebAssembly

  def executingInNodeJS: Boolean = {
    js.typeOf(js.Dynamic.global.process) != "undefined" &&
    !js.isUndefined(js.Dynamic.global.process.release) &&
    (js.Dynamic.global.process.release.name: Any) == "node"
  }

  /** The assumed ECMAScript version. */
  def assumedESVersion: Int = BuildInfo.esVersion

  /** Convenience for `assumedESVersion >= ESVersion.ES2015`. */
  def assumeES2015: Boolean = assumedESVersion >= ESVersion.ES2015

  /** Whether Scala.js language features use ECMAScript 2015 semantics. */
  def useECMAScript2015Semantics: Boolean = BuildInfo.useECMAScript2015Semantics

  def jsSymbols: Boolean =
    assumeES2015 || js.typeOf(js.Dynamic.global.Symbol) != "undefined"

  def typedArrays: Boolean =
    assumeES2015 || js.typeOf(js.Dynamic.global.Int32Array) != "undefined"

  def jsMaps: Boolean =
    assumeES2015 || js.typeOf(js.Dynamic.global.Map) != "undefined"

  def jsBigInts: Boolean =
    assumedESVersion >= ESVersion.ES2020 || js.typeOf(js.Dynamic.global.BigInt) != "undefined"

  lazy val jsRegExps2018: Boolean =
    assumedESVersion >= ESVersion.ES2018 || regexFeatureTest("(?<=a)(?<!b)\\p{L}\\P{L}", "us")

  def sourceMaps: Boolean = BuildInfo.hasSourceMaps && executingInNodeJS

  def usesClosureCompiler: Boolean = BuildInfo.usesClosureCompiler

  def hasMinifiedNames: Boolean = BuildInfo.hasMinifiedNames

  def isInProductionMode: Boolean = BuildInfo.productionMode

  def hasCompliantAsInstanceOfs: Boolean = BuildInfo.compliantAsInstanceOfs

  def hasCompliantArrayIndexOutOfBounds: Boolean =
    BuildInfo.compliantArrayIndexOutOfBounds

  def hasCompliantArrayStores: Boolean =
    BuildInfo.compliantArrayStores

  def hasCompliantNegativeArraySizes: Boolean =
    BuildInfo.compliantNegativeArraySizes

  def hasCompliantNullPointers: Boolean = BuildInfo.compliantNullPointers

  def hasCompliantStringIndexOutOfBounds: Boolean =
    BuildInfo.compliantStringIndexOutOfBounds

  def hasCompliantModuleInit: Boolean = BuildInfo.compliantModuleInit

  def hasDirectBuffers: Boolean = typedArrays

  def regexSupportsUnicodeCase: Boolean =
    assumedESVersion >= ESVersion.ES2015

  def regexSupportsUnicodeCharacterClasses: Boolean =
    assumedESVersion >= ESVersion.ES2018

  def regexSupportsLookBehinds: Boolean =
    assumedESVersion >= ESVersion.ES2018

  def isNoModule: Boolean = BuildInfo.isNoModule
  def isESModule: Boolean = BuildInfo.isESModule
  def isCommonJSModule: Boolean = BuildInfo.isCommonJSModule

  def hasWasmCustomDescriptors: Boolean = BuildInfo.hasWasmCustomDescriptors

  /** Does the target support `@JSExport` and the setup of JS prototype chains
   *  on Scala classes?
   *
   *  This is true on JS, or on Wasm with the Custom Descriptors proposal.
   */
  def hasJSExportsAndJSPrototypes: Boolean =
    !executingInWebAssembly || hasWasmCustomDescriptors

  /** Runs the specified piece of code in the global context.
   *
   *  This only works on Node.js. It needs functionality from the `vm` module.
   *
   *  This method can be used to declare global let/const/classes. Any other
   *  attempt to do so (e.g., using a `require`d source file or a `js.eval`)
   *  would not expose the bindings in the global scope.
   */
  def nodejs_runInThisContext(code: String): Unit = {
    val vm = js.Dynamic.global.require("vm")
    val script = js.Dynamic.newInstance(vm.Script)(code)
    script.runInThisContext()
  }

  private def regexFeatureTest(pattern: String, flags: String): Boolean = {
    try {
      new js.RegExp(pattern, flags)
      true
    } catch {
      case _: js.JavaScriptException => false
    }
  }
}
