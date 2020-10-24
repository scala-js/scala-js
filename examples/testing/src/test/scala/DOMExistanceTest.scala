import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

import org.junit.Test
import org.junit.Assert._

class DOMExistenceTest {

  @Test def initializeDocument(): Unit = {
    assertFalse(js.isUndefined(global.document))
    assertEquals("#document", global.document.nodeName)
  }

  @Test def initializeDocumentBody(): Unit = {
    assertFalse(js.isUndefined(global.document.body))
  }

  @Test def initializeWindow(): Unit = {
    assertFalse(js.isUndefined(global.window))
  }
}
