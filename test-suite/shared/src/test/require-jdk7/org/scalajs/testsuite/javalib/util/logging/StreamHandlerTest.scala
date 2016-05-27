package org.scalajs.testsuite.javalib.util.logging

import java.io.ByteArrayOutputStream
import java.util.logging._

import org.junit.Test
import org.junit.Assert._

class StreamHandlerTest {
  object TestFormatter extends SimpleFormatter {
    override def getHead(h: Handler): String = "header"

    override def getTail(h: Handler): String = "footer"
  }

  @Test def test_logging():Unit = {
    val o = new ByteArrayOutputStream()
    val sh = new StreamHandler(o, new SimpleFormatter())
    sh.publish(new LogRecord(Level.INFO, "message"))
    sh.flush()
    assertTrue(o.toString.contains("message"))
  }

  @Test def test_default_level():Unit = {
    val o = new ByteArrayOutputStream()
    val sh = new StreamHandler(o, new SimpleFormatter())
    // Defaults to level INFO
    sh.publish(new LogRecord(Level.FINER, "message"))
    sh.flush()
    assertFalse(o.toString.contains("message"))
  }

  @Test def test_default_config():Unit = {
    val o = new ByteArrayOutputStream()
    val sh = new StreamHandler(o, new SimpleFormatter())
    assertNull(sh.getEncoding)
    assertNull(sh.getFilter)
    assertNotNull(sh.getFormatter)
    assertNotNull(sh.getErrorManager)
  }

  @Test def test_default_constructor_config():Unit = {
    val sh = new StreamHandler()
    assertNull(sh.getEncoding)
    assertNull(sh.getFilter)
    assertNotNull(sh.getFormatter)
    assertNotNull(sh.getErrorManager)
  }

  @Test def test_no_logging_for_level():Unit = {
    val o = new ByteArrayOutputStream()
    val sh = new StreamHandler(o, new SimpleFormatter())
    sh.setLevel(Level.WARNING)
    sh.publish(new LogRecord(Level.INFO, "message"))
    sh.flush()
    // No output under the given level
    assertTrue(o.toString.isEmpty)
  }

  @Test def test_no_errors_if_no_stream():Unit = {
    val sh = new StreamHandler()
    sh.publish(new LogRecord(Level.INFO, "message"))
    sh.flush()
  }

  @Test def test_print_head():Unit = {
    val o = new ByteArrayOutputStream()
    val sh = new StreamHandler(o, TestFormatter)
    assertTrue(o.toString.isEmpty)
    sh.publish(new LogRecord(Level.INFO, "message"))
    sh.flush()
    assertTrue(o.toString.contains("header"))
    assertTrue(!o.toString.contains("footer"))
  }

  @Test def test_print_tail():Unit = {
    val o = new ByteArrayOutputStream()
    val sh = new StreamHandler(o, TestFormatter)
    assertTrue(o.toString.isEmpty)
    sh.close()
    assertTrue(o.toString.contains("header"))
    assertTrue(o.toString.contains("footer"))
  }
}
