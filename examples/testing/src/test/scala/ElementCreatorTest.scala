import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

import org.junit.Test
import org.junit.Assert._

class ElementCreatorTest {

  @Test
  def element_creator_create_an_element_in_body(): Unit = {
    // create the element
    ElementCreator.create()

    // jquery would make this easier, but I wanted to
    // only use pure html in the test itself
    val body = global.document.getElementsByTagName("body")
      .asInstanceOf[js.Array[js.Dynamic]].head

    // the Scala.js DOM API would make this easier
    assertEquals("H1", body.lastChild.tagName.toString)
    assertEquals("Test", body.lastChild.innerHTML.toString)
  }
}
