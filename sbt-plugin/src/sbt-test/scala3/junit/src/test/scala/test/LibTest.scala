package test

import org.junit.Assert._
import org.junit.Test

class LibTest:
  @Test
  def square(): Unit =
    assertEquals(9, Lib.square(3))

  @Test
  def color(): Unit =
    val c: Lib.Color = Lib.Color.Red
    assertEquals("Red", c.toString())
end LibTest
