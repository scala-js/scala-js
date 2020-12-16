package sbttest

import org.junit.Test
import org.junit.Assert._

class LibTest {
  @Test def dummyLibraryHasFoo(): Unit = {
    assertEquals("foo", Lib.foo(""))
    assertEquals("afoo", Lib.foo("a"))
  }

  @Test def dummyLibraryHasSq(): Unit = {
    assertEquals(0, Lib.sq(0))
    assertEquals(100, Lib.sq(10))
  }
}
