package sbttest.noDOM

import org.junit.Test
import org.junit.Assert._

class LibTest {
  @Test def dummy_library_should_provide_foo(): Unit = {
    assertEquals("foo", Lib.foo(""))
    assertEquals("afoo", Lib.foo("a"))
  }

  @Test def dummy_library_should_provide_sq(): Unit = {
    assertEquals(0, Lib.sq(0))
    assertEquals(100, Lib.sq(10))
  }
}
