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

package org.scalajs.testsuite.library

import scala.scalajs.LinkingInfo
import scala.scalajs.LinkingInfo.ESVersion

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.Platform

class LinkingInfoTest {
  @Test def productionMode(): Unit = {
    assumeFalse("Not implemented yet on WebAssembly", Platform.executingInWebAssembly)

    assertEquals(Platform.isInProductionMode, LinkingInfo.productionMode)
  }

  @Test def developmentMode(): Unit = {
    assumeFalse("Not implemented yet on WebAssembly", Platform.executingInWebAssembly)

    assertEquals(!Platform.isInProductionMode, LinkingInfo.developmentMode)
  }

  @Test def esVersion(): Unit =
    assertEquals(Platform.assumedESVersion, LinkingInfo.esVersion)

  @Test def assumingES6(): Unit =
    assertEquals(Platform.assumedESVersion >= ESVersion.ES2015, LinkingInfo.assumingES6)

  @Test def useECMAScript2015Semantics(): Unit =
    assertEquals(Platform.useECMAScript2015Semantics, LinkingInfo.useECMAScript2015Semantics)

  @Test def isWebAssembly(): Unit =
    assertEquals(Platform.executingInWebAssembly, LinkingInfo.isWebAssembly)

  @Test def esVersionConstants(): Unit = {
    // The numeric values behind the constants are meaningful, so we test them.
    assertEquals(5, ESVersion.ES5_1)
    assertEquals(6, ESVersion.ES2015)
    assertEquals(7, ESVersion.ES2016)
    assertEquals(8, ESVersion.ES2017)
    assertEquals(9, ESVersion.ES2018)
    assertEquals(10, ESVersion.ES2019)
    assertEquals(11, ESVersion.ES2020)
    assertEquals(12, ESVersion.ES2021)
  }
}
