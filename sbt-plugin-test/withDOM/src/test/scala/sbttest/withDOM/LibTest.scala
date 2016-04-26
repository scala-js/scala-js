package sbttest.withDOM

import scala.scalajs.js

import org.junit.Test
import org.junit.Assert._

class LibTest {
  @Test def dummy_library_should_provide_jQuery(): Unit = {
    assertFalse(js.isUndefined(Lib.jQuery))
  }

  @Test def dummy_library_should_append_an_element(): Unit = {
    def count = Lib.jQuery("p").length.asInstanceOf[Int]
    val oldCount = count
    Lib.appendDocument("foo")
    assertEquals(1, count - oldCount)
  }
}
