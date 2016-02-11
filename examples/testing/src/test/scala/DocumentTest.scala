import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js.Dynamic.global

object DocumentTest extends JasmineTest {

  describe("document") {

    it("should initialize 'document'") {
      expect(global.document).toBeDefined
      expect(global.document.nodeName).toEqual("#document")
    }

    it("should initialize 'document.body'") {
      expect(global.document.body).toBeDefined
    }
  }
}
