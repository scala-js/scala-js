import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js.Dynamic.global

object WindowTest extends JasmineTest {

  describe("window") {

    it("should initialize 'window'") {
      expect(global.window).toBeDefined
    }
  }
}
