import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js.Dynamic.global

object JQueryTest extends JasmineTest {

  describe("jQuery") {

    it("should initialize 'jQuery'") {
      expect(global.jQuery).toBeDefined
      expect(global.window.jQuery).toBeDefined
    }

    it("should initialize '$'") {
      expect(global.$).toBeDefined
      expect(global.window.$).toBeDefined
    }
  }
}
