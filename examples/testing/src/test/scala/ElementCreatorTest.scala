import scala.scalajs.js.Dynamic.global
import scala.scalajs.js
import scala.scalajs.test.JasmineTest

object ElementCreatorTest extends JasmineTest {

  // ElementCreator expects jquery to be present
  global.importScripts("jquery.js")

  describe("ElementCreator") {

    it("should be able to create an element in the body") {

      // create the element
      ElementCreator.create()

      // jquery would make this easier, but I wanted to
      // only use pure html in the test itself
      val body = global.document.getElementsByTagName("body")
        .asInstanceOf[js.Array[js.Dynamic]].head

      // the Scala.js DOM API would make this easier
      expect(body.firstChild.tagName.toString == "H1").toBeTruthy
      expect(body.firstChild.innerHTML.toString == "Test").toBeTruthy
    }
  }
}
