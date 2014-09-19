import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

import org.scalajs.jasminetest.JasmineTest

object ElementCreatorTest extends JasmineTest {

  describe("ElementCreator") {

    it("should be able to create an element in the body") {
      // create the element
      ElementCreator.create()

      // jquery would make this easier, but I wanted to
      // only use pure html in the test itself
      val body = global.document.getElementsByTagName("body")
        .asInstanceOf[js.Array[js.Dynamic]].head

      // the Scala.js DOM API would make this easier
      expect(body.lastChild.tagName.toString == "H1").toBeTruthy
      expect(body.lastChild.innerHTML.toString == "Test").toBeTruthy
    }
  }
}
