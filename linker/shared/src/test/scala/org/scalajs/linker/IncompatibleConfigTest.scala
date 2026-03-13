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

package org.scalajs.linker

import org.junit.Test
import org.junit.Assert._

import org.scalajs.linker.interface._

/** Tests for the error messages that we get when trying to use invalid configurations. */
class IncompatibleConfigTest {
  private val baseC = StandardConfig()

  private def test(expectedMessage: String, config: StandardConfig): Unit = {
    val e = assertThrows(classOf[IllegalArgumentException],
        () => StandardImpl.linker(config))
    assertEquals(expectedMessage, e.getMessage().stripPrefix("requirement failed: "))
  }

  @Test def inconsistentConfigTest(): Unit = {
    // #5335 NoModule requires ModuleSplitStyle.FewestModules
    test("NoModule requires ModuleSplitStyle.FewestModules; was SmallestModules.",
        baseC.withModuleSplitStyle(ModuleSplitStyle.SmallestModules))
    test("NoModule requires ModuleSplitStyle.FewestModules; was SmallModulesFor(List(foo)).",
        baseC.withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("foo"))))
  }

  @deprecated("tests deprecated APIs", since = "forever")
  @Test def configNotSupportedByWasmBackend(): Unit = {
    val wasmC = baseC
      .withExperimentalUseWebAssembly(true)
      .withModuleKind(ModuleKind.ESModule)

    // Unsupported ModuleKind
    val supportedModuleKinds = Set[ModuleKind](ModuleKind.ESModule)
    for (moduleKind <- ModuleKind.All if !supportedModuleKinds.contains(moduleKind)) {
      test(s"The WebAssembly backend only supports ES modules; was $moduleKind.",
          wasmC.withModuleKind(moduleKind))
    }

    // ES 5.1
    test("The WebAssembly backend only supports the ECMAScript 2015 semantics.",
        wasmC.withESFeatures(_.withESVersion(ESVersion.ES5_1)))
  }
}
