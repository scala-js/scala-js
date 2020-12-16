import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

import org.junit.Test
import org.junit.Assert._

class ElementCreatorTest {

  @Test def elementCreatorCreateAnElementInBody(): Unit = {
    // Create the element
    ElementCreator.create()

    // Test that it was correctly created
    val body = global.document.body
    assertEquals("H1", body.lastChild.tagName.toString)
    assertEquals("Test", body.lastChild.innerHTML.toString)
  }
}
