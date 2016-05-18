package org.scalajs.testsuite.javalib.util.logging

import java.util.logging._

import org.junit.Test
import org.junit.Assert._

class LoggerTest {
  class TestFilter extends Filter {
    override def isLoggable(record: LogRecord): Boolean = true
  }

  class TestHandler extends Handler {
    var lastRecord: Option[LogRecord] = None

    override def flush(): Unit = {}

    override def publish(record: LogRecord): Unit = {
      lastRecord = Some(record)
    }

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
    assertEquals("TestLogger", Logger.getLogger("TestLogger").getName)
    // Level comes from the default log
    assertNull(Logger.getLogger("TestLogger").getLevel)
    assertTrue(Logger.getLogger("TestLogger").getUseParentHandlers)
    assertNotNull(Logger.getLogger("TestLogger").getParent)
  }

  @Test def test_get_anonymous_logger(): Unit = {
    assertEquals(null, Logger.getAnonymousLogger().getName)
    // Level comes from the default log
    assertNull(Logger.getAnonymousLogger().getLevel)
    assertTrue(Logger.getAnonymousLogger().getUseParentHandlers)
    assertNotNull(Logger.getAnonymousLogger().getParent)
  }

  @Test def test_properties(): Unit = {
    val logger = Logger.getLogger("TestLogger2")
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
    val logger = Logger.getLogger("TestLogger3")
    logger.setLevel(Level.ALL)
    assertTrue(logger.isLoggable(logger.getLevel))

    val levels = List(Level.SEVERE, Level.WARNING, Level.INFO, Level.CONFIG,
      Level.FINE, Level.FINER, Level.FINEST)

    assertTrue(levels.forall(logger.isLoggable))
    logger.setLevel(Level.OFF)
    assertFalse(levels.exists(logger.isLoggable))
  }

  @Test def test_is_loggable_inheritance(): Unit = {
    val logger = Logger.getLogger("TestLogger4")
    logger.setLevel(null)
    logger.getParent.setLevel(Level.ALL)

    val levels = List(Level.SEVERE, Level.WARNING, Level.INFO, Level.CONFIG,
      Level.FINE, Level.FINER, Level.FINEST)

    assertTrue(levels.forall(logger.isLoggable))
    logger.getParent.setLevel(Level.OFF)
    assertFalse(levels.exists(logger.isLoggable))
  }

  @Test def test_add_remove_handler(): Unit = {
    val logger = Logger.getLogger("TestLogger5")
    assertTrue(logger.getHandlers.isEmpty)
    val testHandler = new TestHandler
    logger.addHandler(testHandler)
    assertTrue(logger.getHandlers.contains(testHandler))
    logger.removeHandler(testHandler)
    assertFalse(logger.getHandlers.contains(testHandler))
  }

  @Test def test_log_record_no_parents_all(): Unit = {
    val logger = Logger.getLogger("TestLogger6")
    val testHandler = new TestHandler
    logger.setLevel(Level.ALL)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    val lrFine = new LogRecord(Level.FINE, "Log")
    logger.log(lrFine)
    assertEquals(Some(Level.FINE), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("Log"), testHandler.lastRecord.map(_.getMessage))
  }

  @Test def test_log_record_no_parents_level(): Unit = {
    val logger = Logger.getLogger("TestLogger7")
    val testHandler = new TestHandler
    logger.setLevel(Level.INFO)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    val lrFine = new LogRecord(Level.FINE, "Log")
    logger.log(lrFine)
    assertEquals(None, testHandler.lastRecord)
  }

  @Test def test_log_record_use_parents_all(): Unit = {
    val logger = Logger.getLogger("TestLogger8")
    val testHandler = new TestHandler
    logger.getParent.setLevel(Level.ALL)
    logger.getParent.addHandler(testHandler)
    logger.setUseParentHandlers(true)

    val lrFine = new LogRecord(Level.FINE, "Log")
    logger.log(lrFine)
    assertEquals(Some(Level.FINE), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("Log"), testHandler.lastRecord.map(_.getMessage))
  }

  @Test def test_log_record_use_parents_local_off(): Unit = {
    val logger = Logger.getLogger("TestLogger9")
    val testHandler = new TestHandler
    logger.setLevel(Level.OFF)
    logger.getParent.setLevel(Level.ALL)
    logger.getParent.addHandler(testHandler)
    logger.setUseParentHandlers(true)

    val lrFine = new LogRecord(Level.FINE, "Log")
    logger.log(lrFine)
    assertEquals(None, testHandler.lastRecord)
  }

  @Test def test_log_record_use_parents_level(): Unit = {
    val logger = Logger.getLogger("TestLogger10")
    val testHandler = new TestHandler
    logger.setLevel(Level.OFF)
    logger.getParent.setLevel(Level.INFO)
    logger.getParent.addHandler(testHandler)
    logger.setUseParentHandlers(true)

    val lrFine = new LogRecord(Level.FINE, "Log")
    logger.log(lrFine)
    assertEquals(None, testHandler.lastRecord)
  }
}
