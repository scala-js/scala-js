/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.library

import scala.scalajs.js
import org.scalajs.jasminetest.JasmineTest

object LinkingInfoTest extends JasmineTest {

  import scala.scalajs.LinkingInfo

  describe("scala.scalajs.LinkingInfo") {

    when("production-mode").
    it("productionMode when in production mode") {
      expect(LinkingInfo.productionMode).toBeTruthy
    }

    when("development-mode").
    it("productionMode when in development mode") {
      expect(LinkingInfo.productionMode).toBeFalsy
    }

    when("production-mode").
    it("developmentMode when in production mode") {
      expect(LinkingInfo.developmentMode).toBeFalsy
    }

    when("development-mode").
    it("developmentMode when in development mode") {
      expect(LinkingInfo.developmentMode).toBeTruthy
    }

  }

}
