package sbttest.noDOM

import org.scalajs.jasminetest.JasmineTest

object LibTest extends JasmineTest {

  describe("Dummy Library") {
    it("should provide `foo`") {
      expect(Lib.foo("")).toEqual("foo")
      expect(Lib.foo("a")).toEqual("afoo")
    }

    it("should provide `sq`") {
      expect(Lib.sq(0)).toEqual(0)
      expect(Lib.sq(10)).toEqual(100)
    }
  }

}
