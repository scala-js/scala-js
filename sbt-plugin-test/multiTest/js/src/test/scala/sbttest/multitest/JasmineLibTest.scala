package sbttest.multitest.test

import sbttest.multitest.Lib
import org.scalajs.jasminetest.JasmineTest

object JasmineLibTest extends JasmineTest {
  describe("Lib") {
    it("should provide sq") {
      expect(Lib.sq(2)).toEqual(4)
      expect(Lib.sq(4)).toEqual(16)
    }
  }
}
