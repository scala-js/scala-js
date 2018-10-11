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

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.Platform._

class LinkingInfoTest {

  import scala.scalajs.LinkingInfo

  @Test def productionMode_when_in_production_mode(): Unit = {
    assumeTrue("Assumed in production mode", isInProductionMode)
    assertTrue(LinkingInfo.productionMode)
  }

  @Test def productionMode_when_in_development_mode(): Unit = {
    assumeTrue("Assumed in development mode", isInDevelopmentMode)
    assertFalse(LinkingInfo.productionMode)
  }

  @Test def developmentMode_when_in_production_mode(): Unit = {
    assumeTrue("Assumed in production mode", isInProductionMode)
    assertFalse(LinkingInfo.developmentMode)
  }

  @Test def developmentMode_when_in_development_mode(): Unit = {
    assumeTrue("Assumed in development mode", isInDevelopmentMode)
    assertTrue(LinkingInfo.developmentMode)
  }

}
