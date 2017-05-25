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

  @Test def test_format_with_params(): Unit = {
    val f = new SimpleFormatter()
    val msg = "message with params {0} {1}"
    val r = new LogRecord(Level.INFO, msg)
    assertTrue(f.format(r).contains(msg))

    val r1 = new LogRecord(Level.INFO, msg)
    r1.setParameters(Array("param1"))
    assertTrue(f.format(r1).contains("message with params param1 {1}"))

    val r2 = new LogRecord(Level.INFO, msg)
    r2.setParameters(Array("param1", new java.lang.Integer(20)))
    assertTrue(f.format(r2).contains("message with params param1 20"))

    // Bogus cases
    val r3 = new LogRecord(Level.INFO, "message with params {0} {abc}")
    r3.setParameters(Array("param1", "test"))
    assertTrue(f.format(r3).contains("message with params {0} {abc}"))

    val r4 = new LogRecord(Level.INFO, "message with params {0} {{1}}")
    r4.setParameters(Array("param1", "test"))
    assertTrue(f.format(r4).contains("message with params {0} {{1}}"))

    val r5 = new LogRecord(Level.INFO, "message with params {0} {{1}")
    r5.setParameters(Array("param1", "test"))
    assertTrue(f.format(r5).contains("message with params {0} {{1}"))

    val r6 = new LogRecord(Level.INFO, "message with params {0} {-1}")
    r6.setParameters(Array("param1", "test"))
    assertTrue(f.format(r6).contains("message with params {0} {-1}"))

    val r7 = new LogRecord(Level.INFO, "message with params {0} {1")
    r7.setParameters(Array("param1", "test"))
    assertTrue(f.format(r7).contains("message with params {0} {1"))
  }

  @Test def test_format_property(): Unit = {
    System.setProperty("java.util.logging.SimpleFormatter.format", "%3$s - %5$s")
    val f = new SimpleFormatter()
    val r = new LogRecord(Level.INFO, "message")
    r.setLoggerName("logger")
    // The JVM has a different logic for formatting though the javadocs
    // indicate that the property above should be used
    if (!Platform.executingInJVM)
      assertEquals("logger - message", f.format(r))
  }
}
