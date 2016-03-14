import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js.Dynamic.global

object BootstrapTest extends JasmineTest {

  describe("document") {

    it("should initialize 'document'") {
      expect(global.document).toBeDefined
      expect(global.document.nodeName).toEqual("#document")
    }

    it("should initialize 'document.body'") {
      expect(global.document.body).toBeDefined
    }
  }

  describe("window") {
    it("should initialize 'window'") {
      expect(global.window).toBeDefined
    }
  }

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
