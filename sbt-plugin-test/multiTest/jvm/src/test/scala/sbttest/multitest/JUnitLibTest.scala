package sbttest.multitest.test

import sbttest.multitest.Lib

import org.junit.Test
import org.junit.Assert._

class JUnitLibTest {
  @Test
  def libTest: Unit = {
    assertEquals(Lib.sq(2), 4)
    assertEquals(Lib.sq(4), 16)
  }
}
