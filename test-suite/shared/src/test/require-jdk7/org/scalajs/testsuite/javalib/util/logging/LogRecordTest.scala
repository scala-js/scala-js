package org.scalajs.testsuite.javalib.util.logging

import java.util.logging.{Level, LogRecord}

import org.junit.Test
import org.junit.Assert._

class LogRecordTest {
  @Test def test_constructor(): Unit = {
    val record = new LogRecord(Level.INFO, "msg")

    assertEquals(Level.INFO, record.getLevel)
    record.setLevel(Level.FINE)
    assertEquals(Level.FINE, record.getLevel)

    assertTrue(record.getSequenceNumber >= 0)
    record.setSequenceNumber(256L)
    assertEquals(256L, record.getSequenceNumber)

    assertNull(record.getLoggerName)
    record.setLoggerName("logger")
    assertEquals("logger", record.getLoggerName)
    record.setLoggerName(null)
    assertNull(record.getLoggerName)

    assertNull(record.getSourceClassName)
    record.setSourceClassName("MyClass")
    assertEquals("MyClass", record.getSourceClassName)
    record.setSourceClassName(null)
    assertNull(record.getSourceClassName)

    assertNull(record.getSourceMethodName)
    record.setSourceMethodName("method")
    assertEquals("method", record.getSourceMethodName)
    record.setSourceMethodName(null)
    assertNull(record.getSourceMethodName)

    assertEquals("msg", record.getMessage)
    record.setMessage("newmsg")
    assertEquals("newmsg", record.getMessage)
    record.setMessage(null)
    assertNull(record.getMessage)

    assertNull(record.getParameters)
    record.setParameters(Array[AnyRef]("value"))
    assertArrayEquals(Array[AnyRef]("value"), record.getParameters)
    record.setParameters(null)
    assertNull(record.getParameters)

    assertEquals(Thread.currentThread().getId, record.getThreadID.toLong)
    record.setThreadID(5)
    assertEquals(5L, record.getThreadID.toLong)

    assertTrue(record.getMillis <= System.currentTimeMillis())
    record.setMillis(1)
    assertEquals(1L, record.getMillis)

    assertNull(record.getThrown)
    record.setThrown(new RuntimeException("testmsg"))
    assertEquals("testmsg", record.getThrown.getMessage)
    record.setThrown(null)
    assertNull(record.getThrown)
  }


  @Test def test_parameter_reference(): Unit = {
    val params = Array[AnyRef]("abc", "cde")
    val record = new LogRecord(Level.INFO, "msg")
    record.setParameters(params)

    // Change the params reference
    params(0) = "101"
    assertEquals("101", record.getParameters()(0))
  }

}
