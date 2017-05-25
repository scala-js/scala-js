package org.scalajs.testsuite.javalib.util.logging

import java.util.logging._

import org.junit.{Ignore, Test}
import org.junit.Assert._

class LoggerTest {
  // We add a prefix to all loggers. This avoids some errors when running tests
  // more than once as the loggers are global
  val prefix = System.currentTimeMillis().toString

  val levels = List(Level.SEVERE, Level.WARNING, Level.INFO, Level.CONFIG,
      Level.FINE, Level.FINER, Level.FINEST)

  class TestFilter extends Filter {
    override def isLoggable(record: LogRecord): Boolean = true
  }

  class TestHandler extends Handler {
    var lastRecord: LogRecord = null

    override def flush(): Unit = {}

    override def publish(record: LogRecord): Unit = lastRecord = record

    override def close(): Unit = {}
  }

  @Test def test_global_logger(): Unit = {
    val logger = Logger.getGlobal
    assertEquals("global", Logger.GLOBAL_LOGGER_NAME)
    assertEquals("global", logger.getName)
    assertTrue(logger.getUseParentHandlers)
    assertNotNull(logger.getParent)
  }

  @Test def test_static(): Unit = {
    assertEquals("global", Logger.GLOBAL_LOGGER_NAME)
  }

  @Test def test_get_logger(): Unit = {
    assertEquals(s"$prefix.TestLogger",
        Logger.getLogger(s"$prefix.TestLogger").getName)
    // Level comes from the default log
    assertNull(Logger.getLogger(s"$prefix.TestLogger").getLevel)
    assertTrue(Logger.getLogger(s"$prefix.TestLogger").getUseParentHandlers)
    assertNotNull(Logger.getLogger(s"$prefix.TestLogger").getParent)
  }

  @Test def test_get_anonymous_logger(): Unit = {
    assertEquals(null, Logger.getAnonymousLogger().getName)
    // Level comes from the default log
    assertNull(Logger.getAnonymousLogger().getLevel)
    assertTrue(Logger.getAnonymousLogger().getUseParentHandlers)
    assertNotNull(Logger.getAnonymousLogger().getParent)
  }

  @Test def test_properties(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger2")
    logger.setUseParentHandlers(true)
    assertTrue(logger.getUseParentHandlers)
    logger.setUseParentHandlers(false)
    assertFalse(logger.getUseParentHandlers)
    assertNull(logger.getResourceBundleName)

    val filter = new TestFilter()
    logger.setFilter(filter)
    assertEquals(filter, logger.getFilter)

    logger.setLevel(Level.WARNING)
    assertEquals(Level.WARNING, logger.getLevel)
  }

  @Test def test_is_loggable(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger3")
    logger.setLevel(Level.ALL)
    assertTrue(logger.isLoggable(logger.getLevel))

    assertTrue(levels.forall(logger.isLoggable))
    logger.setLevel(Level.OFF)
    assertFalse(levels.exists(logger.isLoggable))
  }

  @Test def test_is_loggable_inheritance(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger4")
    logger.setLevel(null)
    logger.getParent.setLevel(Level.ALL)

    assertTrue(levels.forall(logger.isLoggable))
    logger.getParent.setLevel(Level.OFF)
    assertFalse(levels.exists(logger.isLoggable))
  }

  @Test def test_add_remove_handler(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger5")
    assertTrue(logger.getHandlers.isEmpty)
    val testHandler = new TestHandler
    logger.addHandler(testHandler)
    assertTrue(logger.getHandlers.contains(testHandler))
    logger.removeHandler(testHandler)
    assertFalse(logger.getHandlers.contains(testHandler))
  }

  @Test def test_log_record_no_parents_all(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger6")
    val testHandler = new TestHandler
    logger.setLevel(Level.ALL)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    val lrFine = new LogRecord(Level.FINE, "Log")
    logger.log(lrFine)
    assertEquals(Level.FINE, testHandler.lastRecord.getLevel)
    assertEquals("Log", testHandler.lastRecord.getMessage)
  }

  @Test def test_log_record_no_parents_level(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger7")
    val testHandler = new TestHandler
    logger.setLevel(Level.INFO)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    val lrFine = new LogRecord(Level.FINE, "Log")
    logger.log(lrFine)
    assertNull(testHandler.lastRecord)
  }

  @Test def test_log_record_use_parents_all(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger8")
    val testHandler = new TestHandler
    logger.getParent.setLevel(Level.ALL)
    logger.getParent.addHandler(testHandler)
    logger.setUseParentHandlers(true)

    val lrFine = new LogRecord(Level.FINE, "Log")
    logger.log(lrFine)
    assertEquals(Level.FINE, testHandler.lastRecord.getLevel)
    assertEquals("Log", testHandler.lastRecord.getMessage)
  }

  @Test def test_log_record_use_parents_local_off(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger9")
    val testHandler = new TestHandler
    logger.setLevel(Level.OFF)
    logger.getParent.setLevel(Level.ALL)
    logger.getParent.addHandler(testHandler)
    logger.setUseParentHandlers(true)

    val lrFine = new LogRecord(Level.FINE, "Log")
    logger.log(lrFine)
    assertNull(testHandler.lastRecord)
  }

  @Test def test_log_record_use_parents_level(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger10")
    val testHandler = new TestHandler
    logger.setLevel(Level.OFF)
    logger.getParent.setLevel(Level.INFO)
    logger.getParent.addHandler(testHandler)
    logger.setUseParentHandlers(true)

    val lrFine = new LogRecord(Level.FINE, "Log")
    logger.log(lrFine)
    assertNull(testHandler.lastRecord)
  }

  @Test def test_log_variants(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger11")
    val testHandler = new TestHandler
    logger.setLevel(Level.ALL)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    logger.log(Level.FINE, "Log")
    assertEquals(Level.FINE, testHandler.lastRecord.getLevel)
    assertEquals("Log", testHandler.lastRecord.getMessage)

    logger.log(Level.FINE, "Log", "param")
    assertEquals(Level.FINE, testHandler.lastRecord.getLevel)
    assertEquals("Log", testHandler.lastRecord.getMessage)
    assertArrayEquals(Array[AnyRef]("param"),
        testHandler.lastRecord.getParameters)

    logger.log(Level.FINE, "Log", Array[AnyRef]("param1", "param2"))
    assertEquals(Level.FINE, testHandler.lastRecord.getLevel)
    assertEquals("Log", testHandler.lastRecord.getMessage)
    assertArrayEquals(Array[AnyRef]("param1", "param2"),
        testHandler.lastRecord.getParameters)

    logger.log(Level.FINE, "Log", new RuntimeException())
    assertEquals(Level.FINE, testHandler.lastRecord.getLevel)
    assertEquals("Log", testHandler.lastRecord.getMessage)
    assertNotNull(testHandler.lastRecord.getThrown)
  }

  @Test def test_logp_variants(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger11")
    val testHandler = new TestHandler
    logger.setLevel(Level.ALL)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    logger.logp(Level.FINE, "cls", "method", "Log")
    assertEquals(Level.FINE, testHandler.lastRecord.getLevel)
    assertEquals("Log", testHandler.lastRecord.getMessage)
    assertEquals("cls", testHandler.lastRecord.getSourceClassName)
    assertEquals("method", testHandler.lastRecord.getSourceMethodName)

    logger.logp(Level.FINE, "cls", "method", "Log", "param")
    assertEquals(Level.FINE, testHandler.lastRecord.getLevel)
    assertEquals("Log", testHandler.lastRecord.getMessage)
    assertArrayEquals(Array[AnyRef]("param"),
        testHandler.lastRecord.getParameters)
    assertEquals("cls", testHandler.lastRecord.getSourceClassName)
    assertEquals("method", testHandler.lastRecord.getSourceMethodName)

    logger.logp(Level.FINE, "cls", "method", "Log",
        Array[AnyRef]("param1", "param2"))
    assertEquals(Level.FINE, testHandler.lastRecord.getLevel)
    assertEquals("Log", testHandler.lastRecord.getMessage)
    assertArrayEquals(Array[AnyRef]("param1", "param2"),
        testHandler.lastRecord.getParameters)
    assertEquals("cls", testHandler.lastRecord.getSourceClassName)
    assertEquals("method", testHandler.lastRecord.getSourceMethodName)

    logger.logp(Level.FINE, "cls", "method", "Log", new RuntimeException())
    assertEquals(Level.FINE, testHandler.lastRecord.getLevel)
    assertEquals("Log", testHandler.lastRecord.getMessage)
    assertNotNull(testHandler.lastRecord.getThrown)
    assertEquals("cls", testHandler.lastRecord.getSourceClassName)
    assertEquals("method", testHandler.lastRecord.getSourceMethodName)
  }

  @Test def test_entering_variants(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger11")
    val testHandler = new TestHandler
    logger.setLevel(Level.ALL)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    logger.entering("cls", "method")
    assertEquals(Level.FINER, testHandler.lastRecord.getLevel)
    assertEquals("ENTRY", testHandler.lastRecord.getMessage)
    assertEquals("cls", testHandler.lastRecord.getSourceClassName)
    assertEquals("method", testHandler.lastRecord.getSourceMethodName)

    logger.entering("cls", "method", "param")
    assertEquals(Level.FINER, testHandler.lastRecord.getLevel)
    assertEquals("ENTRY {0}", testHandler.lastRecord.getMessage)
    assertEquals("cls", testHandler.lastRecord.getSourceClassName)
    assertEquals("method", testHandler.lastRecord.getSourceMethodName)
    assertArrayEquals(Array[AnyRef]("param"),
        testHandler.lastRecord.getParameters)

    logger.entering("cls", "method", Array[AnyRef]("param1", "param2"))
    assertEquals(Level.FINER, testHandler.lastRecord.getLevel)
    assertEquals("ENTRY {0} {1}", testHandler.lastRecord.getMessage)
    assertEquals("cls", testHandler.lastRecord.getSourceClassName)
    assertEquals("method", testHandler.lastRecord.getSourceMethodName)
    assertArrayEquals(Array[AnyRef]("param1", "param2"),
        testHandler.lastRecord.getParameters)
  }

  @Test def test_exiting_variants(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger12")
    val testHandler = new TestHandler
    logger.setLevel(Level.ALL)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    logger.exiting("cls", "method")
    assertEquals(Level.FINER, testHandler.lastRecord.getLevel)
    assertEquals("RETURN", testHandler.lastRecord.getMessage)
    assertEquals("cls", testHandler.lastRecord.getSourceClassName)
    assertEquals("method", testHandler.lastRecord.getSourceMethodName)

    logger.exiting("cls", "method", "param")
    assertEquals(Level.FINER, testHandler.lastRecord.getLevel)
    assertEquals("RETURN {0}", testHandler.lastRecord.getMessage)
    assertEquals("cls", testHandler.lastRecord.getSourceClassName)
    assertEquals("method", testHandler.lastRecord.getSourceMethodName)
    assertArrayEquals(Array[AnyRef]("param"),
        testHandler.lastRecord.getParameters)
  }

  @Test def test_throwing(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger13")
    val testHandler = new TestHandler
    logger.setLevel(Level.ALL)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    logger.throwing("cls", "method", new RuntimeException)
    assertEquals(Level.FINER, testHandler.lastRecord.getLevel)
    assertEquals("THROW", testHandler.lastRecord.getMessage)
    assertEquals("cls", testHandler.lastRecord.getSourceClassName)
    assertEquals("method", testHandler.lastRecord.getSourceMethodName)
    assertNotNull(testHandler.lastRecord.getThrown)
  }

  @Test def test_named_levels(): Unit = {
    val logger = Logger.getLogger(s"$prefix.TestLogger13")
    val testHandler = new TestHandler
    logger.setLevel(Level.ALL)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    logger.severe("severe_msg")
    assertEquals(Level.SEVERE, testHandler.lastRecord.getLevel)
    assertEquals("severe_msg", testHandler.lastRecord.getMessage)

    logger.warning("warning_msg")
    assertEquals(Level.WARNING, testHandler.lastRecord.getLevel)
    assertEquals("warning_msg", testHandler.lastRecord.getMessage)

    logger.info("info_msg")
    assertEquals(Level.INFO, testHandler.lastRecord.getLevel)
    assertEquals("info_msg", testHandler.lastRecord.getMessage)

    logger.config("config_msg")
    assertEquals(Level.CONFIG, testHandler.lastRecord.getLevel)
    assertEquals("config_msg", testHandler.lastRecord.getMessage)

    logger.fine("fine_msg")
    assertEquals(Level.FINE, testHandler.lastRecord.getLevel)
    assertEquals("fine_msg", testHandler.lastRecord.getMessage)

    logger.finer("finer_msg")
    assertEquals(Level.FINER, testHandler.lastRecord.getLevel)
    assertEquals("finer_msg", testHandler.lastRecord.getMessage)

    logger.finest("finest_msg")
    assertEquals(Level.FINEST, testHandler.lastRecord.getLevel)
    assertEquals("finest_msg", testHandler.lastRecord.getMessage)
  }

  @Test def test_logger_parents_by_name(): Unit = {
    val l1 = Logger.getLogger(s"$prefix.a.b.c.d")
    assertEquals("", l1.getParent.getName)

    val l2 = Logger.getLogger(s"$prefix.a.b")
    assertEquals(l2.getName, l1.getParent.getName)
  }
}
