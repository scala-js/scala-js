/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
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
