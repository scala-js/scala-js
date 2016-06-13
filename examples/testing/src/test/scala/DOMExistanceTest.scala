import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

import org.junit.Test
import org.junit.Assert._

class DOMExistenceTest {

  @Test
  def should_initialize_document(): Unit = {
    assertFalse(js.isUndefined(global.document))
    assertEquals("#document", global.document.nodeName)
  }

  @Test
  def should_initialize_document_body(): Unit = {
    assertFalse(js.isUndefined(global.document.body))
  }

  @Test
  def should_initialize_windod(): Unit = {
    assertFalse(js.isUndefined(global.window))
  }

  @Test
  def should_initialize_jQuery(): Unit = {
    assertFalse(js.isUndefined(global.jQuery))
    assertFalse(js.isUndefined(global.window.jQuery))
  }

  @Test
  def should_initialize_dollar(): Unit = {
    assertFalse(js.isUndefined(global.$))
    assertFalse(js.isUndefined(global.window.$))
  }
}
