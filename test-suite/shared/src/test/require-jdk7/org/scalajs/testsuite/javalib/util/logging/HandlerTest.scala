package org.scalajs.testsuite.javalib.util.logging

import java.util.logging._

import org.junit.Test
import org.junit.Assert._

class HandlerTest {
  class TestHandler extends Handler {
    override def flush(): Unit = {}

    override def publish(record: LogRecord): Unit = {}

    override def close(): Unit = {}
  }

  class TestFilter extends Filter {
    override def isLoggable(record: LogRecord): Boolean = true
  }

  @Test def test_constructor(): Unit = {
    val h = new TestHandler()
    assertNull(h.getFormatter)
    assertNull(h.getFilter)
    assertEquals(Level.ALL, h.getLevel)

    h.setFormatter(new SimpleFormatter())
    assertNotNull(h.getFormatter)

    h.setFilter(new TestFilter())
    assertNotNull(h.getFilter)
  }

  @Test def test_handler_loggable(): Unit = {
    val h = new TestHandler()
    assertTrue(h.isLoggable(new LogRecord(Level.INFO, "message")))
    h.setLevel(Level.SEVERE)
    assertFalse(h.isLoggable(new LogRecord(Level.INFO, "message")))
  }
}
