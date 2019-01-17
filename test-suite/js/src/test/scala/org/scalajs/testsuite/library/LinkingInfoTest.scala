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

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.Platform

class LinkingInfoTest {
  @Test def productionMode(): Unit =
    assertEquals(Platform.isInProductionMode, LinkingInfo.productionMode)

  @Test def developmentMode(): Unit =
    assertEquals(!Platform.isInProductionMode, LinkingInfo.developmentMode)

  @Test def assumingES6(): Unit =
    assertEquals(Platform.assumeES2015, LinkingInfo.assumingES6)

  @Test def runtime(): Unit = {
    import scala.scalajs.runtime.{linkingInfo, LinkingInfo}

    def isCompliant(f: LinkingInfo.Semantics => Int) =
      f(linkingInfo.semantics) == LinkingInfo.Semantics.Compliant

    assertEquals(Platform.hasCompliantAsInstanceOfs, isCompliant(_.asInstanceOfs))
    assertEquals(Platform.hasCompliantArrayIndexOutOfBounds, isCompliant(_.arrayIndexOutOfBounds))
    assertEquals(Platform.hasCompliantModuleInit, isCompliant(_.moduleInit))
    assertEquals(Platform.hasStrictFloats, linkingInfo.semantics.strictFloats)
  }
}
