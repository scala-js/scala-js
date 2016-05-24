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

    assertNull(record.getSourceClassName)
    record.setSourceClassName("MyClass")
    assertEquals("MyClass", record.getSourceClassName)

    assertNull(record.getSourceMethodName)
    record.setSourceMethodName("method")
    assertEquals("method", record.getSourceMethodName)

    assertEquals("msg", record.getMessage)
    record.setMessage("newmsg")
    assertEquals("newmsg", record.getMessage)
    // You can set message to null
    record.setMessage(null)
    assertNull(record.getMessage)

    assertNull(record.getParameters)
    record.setParameters(Array[AnyRef]("value"))
    assertArrayEquals(Array[AnyRef]("value"), record.getParameters)

    assertEquals(Thread.currentThread().getId, record.getThreadID.toLong)
    record.setThreadID(5)
    assertEquals(5L, record.getThreadID.toLong)

    assertTrue(record.getMillis <= System.currentTimeMillis())
    record.setMillis(1)
    assertEquals(1L, record.getMillis)

    assertNull(record.getThrown)
    record.setThrown(new RuntimeException("testmsg"))
    assertEquals("testmsg", record.getThrown.getMessage)
  }
}
