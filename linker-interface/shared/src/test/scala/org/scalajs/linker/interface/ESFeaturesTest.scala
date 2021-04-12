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

import org.junit.Test
import org.junit.Assert._

class ESFeaturesTest {
  @deprecated("test deprecated features", "forever")
  @Test def esVersionAndUseECMAScript2015AreLinked(): Unit = {
    import ESFeatures.Defaults

    assertEquals(ESVersion.ES2015, Defaults.esVersion)
    assertTrue(Defaults.useECMAScript2015)

    assertFalse(Defaults.withESVersion(ESVersion.ES5_1).useECMAScript2015)
    assertEquals(ESVersion.ES5_1, Defaults.withUseECMAScript2015(false).esVersion)

    val esFeaturesWithES2018 = Defaults.withESVersion(ESVersion.ES2018)
    assertTrue(esFeaturesWithES2018.useECMAScript2015)

    assertEquals(ESVersion.ES2018, esFeaturesWithES2018.withUseECMAScript2015(true).esVersion)
    assertEquals(ESVersion.ES5_1, esFeaturesWithES2018.withUseECMAScript2015(false).esVersion)

    assertFalse(esFeaturesWithES2018.withESVersion(ESVersion.ES5_1).useECMAScript2015)
  }
}
