package org.scalajs.testsuite.javalib.util.logging

import java.util.logging.Level

import org.junit.Assert._
import org.junit.Test

class LevelTest {

  @Test def test_static(): Unit = {
    assertEquals("OFF", Level.OFF.getName)
    assertEquals(Int.MaxValue, Level.OFF.intValue())

    assertEquals("SEVERE", Level.SEVERE.getName)
    assertEquals(1000, Level.SEVERE.intValue())

    assertEquals("WARNING", Level.WARNING.getName)
    assertEquals(900, Level.WARNING.intValue())

    assertEquals("INFO", Level.INFO.getName)
    assertEquals(800, Level.INFO.intValue())

    assertEquals("CONFIG", Level.CONFIG.getName)
    assertEquals(700, Level.CONFIG.intValue())

    assertEquals("FINE", Level.FINE.getName)
    assertEquals(500, Level.FINE.intValue())

    assertEquals("FINER", Level.FINER.getName)
    assertEquals(400, Level.FINER.intValue())

    assertEquals("FINEST", Level.FINEST.getName)
    assertEquals(300, Level.FINEST.intValue())

    assertEquals("ALL", Level.ALL.getName)
    assertEquals(Int.MinValue, Level.ALL.intValue())
  }

  @Test def test_equals_hash_code(): Unit = {
    assertEquals(Level.SEVERE, Level.SEVERE)
    assertNotEquals(Level.SEVERE, Level.WARNING)
    assertEquals(Level.SEVERE.hashCode(), Level.SEVERE.hashCode())
  }
}
