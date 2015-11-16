package org.scalajs.testsuite.javalib.util

import org.scalajs.jasminetest.JasmineTest
import java.util.Objects

object ObjectsTestOnJDK8 extends JasmineTest {

  describe("java.util.Objects") {
    it("should provide `isNull`") {
      expect(Objects.isNull(null)).toBeTruthy
      expect(Objects.isNull(new Object)).toBeFalsy
    }

    it("should provide `nonNull`") {
      expect(Objects.nonNull(null)).toBeFalsy
      expect(Objects.nonNull(new Object)).toBeTruthy
    }
  }
}
