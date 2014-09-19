package sbttest.withDOM

import scala.scalajs.js

import org.scalajs.jasminetest.JasmineTest

object LibTest extends JasmineTest {

  describe("Dummy Library") {

    it("should provide jQuery") {
      expect(Lib.jQuery).toBeDefined
    }

    it("should append an element") {
      def count = Lib.jQuery("p").length.asInstanceOf[Int]
      val oldCount = count
      Lib.appendDocument("foo")
      expect(count - oldCount).toEqual(1)
    }

  }

}
