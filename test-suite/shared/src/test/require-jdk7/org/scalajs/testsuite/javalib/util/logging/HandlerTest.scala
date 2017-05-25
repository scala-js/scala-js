package org.scalajs.testsuite.javalib.util.logging

import java.util.logging._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.Platform
import org.scalajs.testsuite.utils.AssertThrows._

class HandlerTest {
  class TestHandler extends Handler {
    override def flush(): Unit = {}

    override def publish(record: LogRecord): Unit = {}

    override def close(): Unit = {}

    override def reportError(msg: String, ex: Exception, code: Int): Unit =
      super.reportError(msg, ex, code)
  }

  class TestFilter(allow: Boolean) extends Filter {
    override def isLoggable(record: LogRecord): Boolean = allow
  }

  class TestErrorManager extends ErrorManager {
    var callParams: Option[(String, Exception, Int)] = None

    override def error(msg: String, ex: Exception, code: Int): Unit =
      callParams = Some((msg, ex, code))
  }

  @Test def test_constructor(): Unit = {
    val h = new TestHandler()
    assertNull(h.getFormatter)
    assertNull(h.getEncoding)
    assertNull(h.getFilter)
    assertNotNull(h.getErrorManager)
    assertEquals(Level.ALL, h.getLevel)

    h.setFormatter(new SimpleFormatter())
    assertNotNull(h.getFormatter)
    // The javadocs indicate this is allowed but at runtime on
    // the JVM an NPE is thrown
    if (!Platform.executingInJVM) {
      h.setFormatter(null)
      assertNull(h.getFormatter)
    }

    h.setFilter(new TestFilter(true))
    assertNotNull(h.getFilter)
    h.setFilter(null)
    assertNull(h.getFilter)

    h.setEncoding("UTF-16")
    assertNotNull(h.getEncoding)
    h.setEncoding(null)
    assertNull(h.getEncoding)

    expectThrows(classOf[NullPointerException], h.setErrorManager(null))

    h.setLevel(Level.FINE)
    assertEquals(Level.FINE, h.getLevel)
    expectThrows(classOf[NullPointerException], h.setLevel(null))
  }

  @Test def test_report_error(): Unit = {
    // Error manager is quite opaque, to test we'll use a mock version
    // to record the params
    val h = new TestHandler()
    val e = new TestErrorManager()
    h.setErrorManager(e)
    val ex = new RuntimeException()
    h.reportError("msg", ex, 24)
    assertEquals(Some(("msg", ex, 24)), e.callParams)
  }

  @Test def test_handler_is_loggable(): Unit = {
    val h1 = new TestHandler()

    def checkLogAllLevels: Unit = {
      assertTrue(h1.isLoggable(new LogRecord(Level.SEVERE, "message")))
      assertTrue(h1.isLoggable(new LogRecord(Level.WARNING, "message")))
      assertTrue(h1.isLoggable(new LogRecord(Level.INFO, "message")))
      assertTrue(h1.isLoggable(new LogRecord(Level.CONFIG, "message")))
      assertTrue(h1.isLoggable(new LogRecord(Level.FINE, "message")))
      assertTrue(h1.isLoggable(new LogRecord(Level.FINER, "message")))
      assertTrue(h1.isLoggable(new LogRecord(Level.FINEST, "message")))
      assertTrue(h1.isLoggable(new LogRecord(Level.OFF, "message")))
      assertTrue(h1.isLoggable(new LogRecord(Level.ALL, "message")))
    }

    // Level is all at start
    checkLogAllLevels

    // Check filter
    h1.setFilter(new TestFilter(true))
    checkLogAllLevels

    // Check filter
    h1.setFilter(new TestFilter(false))
    assertFalse(h1.isLoggable(new LogRecord(Level.SEVERE, "message")))
    assertFalse(h1.isLoggable(new LogRecord(Level.WARNING, "message")))
    assertFalse(h1.isLoggable(new LogRecord(Level.INFO, "message")))
    assertFalse(h1.isLoggable(new LogRecord(Level.CONFIG, "message")))
    assertFalse(h1.isLoggable(new LogRecord(Level.FINE, "message")))
    assertFalse(h1.isLoggable(new LogRecord(Level.FINER, "message")))
    assertFalse(h1.isLoggable(new LogRecord(Level.FINEST, "message")))
    assertFalse(h1.isLoggable(new LogRecord(Level.OFF, "message")))
    assertFalse(h1.isLoggable(new LogRecord(Level.ALL, "message")))

    // set it at INFO
    val h2 = new TestHandler()
    h2.setLevel(Level.INFO)
    assertTrue(h2.isLoggable(new LogRecord(Level.SEVERE, "message")))
    assertTrue(h2.isLoggable(new LogRecord(Level.WARNING, "message")))
    assertTrue(h2.isLoggable(new LogRecord(Level.INFO, "message")))
    assertFalse(h2.isLoggable(new LogRecord(Level.CONFIG, "message")))
    assertFalse(h2.isLoggable(new LogRecord(Level.FINE, "message")))
    assertFalse(h2.isLoggable(new LogRecord(Level.FINER, "message")))
    assertFalse(h2.isLoggable(new LogRecord(Level.FINEST, "message")))
    assertTrue(h2.isLoggable(new LogRecord(Level.OFF, "message")))
    assertFalse(h2.isLoggable(new LogRecord(Level.ALL, "message")))
  }
}
