package org.scalajs.testsuite.javalib.util.logging

import java.util.logging.{Level, LogRecord, SimpleFormatter}

import org.junit.{Before, Test}
import org.junit.Assert._

import org.scalajs.testsuite.utils.Platform

class SimpleFormatterTest {
  @Before def clearProperties():Unit = {
    System.clearProperty("java.util.logging.SimpleFormatter.format")
  }

  @Test def test_default_format(): Unit = {
    val f = new SimpleFormatter()
    val r = new LogRecord(Level.INFO, "message")
    r.setLoggerName("logger")
    assertTrue(f.format(r).contains("message"))
    assertTrue(f.format(r).contains("logger"))
  }

  @Test def test_format_property(): Unit = {
    System.setProperty("java.util.logging.SimpleFormatter.format", "%3$s - %5$s")
    val f = new SimpleFormatter()
    val r = new LogRecord(Level.INFO, "message")
    r.setLoggerName("logger")
    if (!Platform.executingInJVM) {
      assertEquals("logger - message", f.format(r))
    }
  }
}
