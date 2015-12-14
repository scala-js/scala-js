/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.utils

import org.scalajs.jasminetest.JasmineTest

object PlatformTest extends JasmineTest {
  when("typedarray").
  describe("org.scalajs.testsuite.utils.Platform") {
    it("areTypedArraysSupported should be true") {
      // Note that if we don't have the tag set we can't say anything
      // on the typed arrays support.
      expect(Platform.areTypedArraysSupported).toBeTruthy
    }
  }
}
