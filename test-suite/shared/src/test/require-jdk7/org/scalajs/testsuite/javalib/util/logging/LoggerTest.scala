package org.scalajs.testsuite.javalib.util.logging

import java.util.logging._

import org.junit.{Ignore, Test}
import org.junit.Assert._

class LoggerTest {
  // We add a prefix to all loggers. This avoids some errors when running tests
  // more than once as the loggers are global
  val Prefix = System.currentTimeMillis().toString

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
    assertEquals(s"$Prefix.TestLogger",
      Logger.getLogger(s"$Prefix.TestLogger").getName)
    // Level comes from the default log
    assertNull(Logger.getLogger(s"$Prefix.TestLogger").getLevel)
    assertTrue(Logger.getLogger(s"$Prefix.TestLogger").getUseParentHandlers)
    assertNotNull(Logger.getLogger(s"$Prefix.TestLogger").getParent)
  }

  @Test def test_get_anonymous_logger(): Unit = {
    assertEquals(null, Logger.getAnonymousLogger().getName)
    // Level comes from the default log
    assertNull(Logger.getAnonymousLogger().getLevel)
    assertTrue(Logger.getAnonymousLogger().getUseParentHandlers)
    assertNotNull(Logger.getAnonymousLogger().getParent)
  }

  @Test def test_properties(): Unit = {
    val logger = Logger.getLogger(s"$Prefix.TestLogger2")
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
    val logger = Logger.getLogger(s"$Prefix.TestLogger3")
    logger.setLevel(Level.ALL)
    assertTrue(logger.isLoggable(logger.getLevel))

    val levels = List(Level.SEVERE, Level.WARNING, Level.INFO, Level.CONFIG,
      Level.FINE, Level.FINER, Level.FINEST)

    assertTrue(levels.forall(logger.isLoggable))
    logger.setLevel(Level.OFF)
    assertFalse(levels.exists(logger.isLoggable))
  }

  @Test def test_is_loggable_inheritance(): Unit = {
    val logger = Logger.getLogger(s"$Prefix.TestLogger4")
    logger.setLevel(null)
    logger.getParent.setLevel(Level.ALL)

    val levels = List(Level.SEVERE, Level.WARNING, Level.INFO, Level.CONFIG,
      Level.FINE, Level.FINER, Level.FINEST)

    assertTrue(levels.forall(logger.isLoggable))
    logger.getParent.setLevel(Level.OFF)
    assertFalse(levels.exists(logger.isLoggable))
  }

  @Test def test_add_remove_handler(): Unit = {
    val logger = Logger.getLogger(s"$Prefix.TestLogger5")
    assertTrue(logger.getHandlers.isEmpty)
    val testHandler = new TestHandler
    logger.addHandler(testHandler)
    assertTrue(logger.getHandlers.contains(testHandler))
    logger.removeHandler(testHandler)
    assertFalse(logger.getHandlers.contains(testHandler))
  }

  @Test def test_log_record_no_parents_all(): Unit = {
    val logger = Logger.getLogger(s"$Prefix.TestLogger6")
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
    val logger = Logger.getLogger(s"$Prefix.TestLogger7")
    val testHandler = new TestHandler
    logger.setLevel(Level.INFO)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    val lrFine = new LogRecord(Level.FINE, "Log")
    logger.log(lrFine)
    assertEquals(None, testHandler.lastRecord)
  }

  @Test def test_log_record_use_parents_all(): Unit = {
    val logger = Logger.getLogger(s"$Prefix.TestLogger8")
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
    val logger = Logger.getLogger(s"$Prefix.TestLogger9")
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
    val logger = Logger.getLogger(s"$Prefix.TestLogger10")
    val testHandler = new TestHandler
    logger.setLevel(Level.OFF)
    logger.getParent.setLevel(Level.INFO)
    logger.getParent.addHandler(testHandler)
    logger.setUseParentHandlers(true)

    val lrFine = new LogRecord(Level.FINE, "Log")
    logger.log(lrFine)
    assertEquals(None, testHandler.lastRecord)
  }

  @Test def test_log_variants(): Unit = {
    val logger = Logger.getLogger(s"$Prefix.TestLogger11")
    val testHandler = new TestHandler
    logger.setLevel(Level.ALL)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    logger.log(Level.FINE, "Log")
    assertEquals(Some(Level.FINE), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("Log"), testHandler.lastRecord.map(_.getMessage))

    logger.log(Level.FINE, "Log", "param")
    assertEquals(Some(Level.FINE), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("Log"), testHandler.lastRecord.map(_.getMessage))
    assertArrayEquals(Array[AnyRef]("param"),
      testHandler.lastRecord.map(_.getParameters).getOrElse(Array.empty))

    logger.log(Level.FINE, "Log", Array[AnyRef]("param1", "param2"))
    assertEquals(Some(Level.FINE), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("Log"), testHandler.lastRecord.map(_.getMessage))
    assertArrayEquals(Array[AnyRef]("param1", "param2"),
      testHandler.lastRecord.map(_.getParameters).getOrElse(Array.empty))

    logger.log(Level.FINE, "Log", new RuntimeException())
    assertEquals(Some(Level.FINE), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("Log"), testHandler.lastRecord.map(_.getMessage))
    assertTrue(testHandler.lastRecord.map(_.getThrown).isDefined)
  }

  @Test def test_logp_variants(): Unit = {
    val logger = Logger.getLogger(s"$Prefix.TestLogger11")
    val testHandler = new TestHandler
    logger.setLevel(Level.ALL)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    logger.logp(Level.FINE, "cls", "method", "Log")
    assertEquals(Some(Level.FINE), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("Log"), testHandler.lastRecord.map(_.getMessage))
    assertEquals(Some("cls"), testHandler.lastRecord.map(_.getSourceClassName))
    assertEquals(Some("method"),
      testHandler.lastRecord.map(_.getSourceMethodName))

    logger.logp(Level.FINE, "cls", "method", "Log", "param")
    assertEquals(Some(Level.FINE), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("Log"), testHandler.lastRecord.map(_.getMessage))
    assertArrayEquals(Array[AnyRef]("param"),
      testHandler.lastRecord.map(_.getParameters).getOrElse(Array.empty))
    assertEquals(Some("cls"), testHandler.lastRecord.map(_.getSourceClassName))
    assertEquals(Some("method"),
      testHandler.lastRecord.map(_.getSourceMethodName))

    logger.logp(Level.FINE, "cls", "method", "Log",
      Array[AnyRef]("param1", "param2"))
    assertEquals(Some(Level.FINE), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("Log"), testHandler.lastRecord.map(_.getMessage))
    assertArrayEquals(Array[AnyRef]("param1", "param2"),
      testHandler.lastRecord.map(_.getParameters).getOrElse(Array.empty))
    assertEquals(Some("cls"), testHandler.lastRecord.map(_.getSourceClassName))
    assertEquals(Some("method"),
      testHandler.lastRecord.map(_.getSourceMethodName))

    logger.logp(Level.FINE, "cls", "method", "Log", new RuntimeException())
    assertEquals(Some(Level.FINE), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("Log"), testHandler.lastRecord.map(_.getMessage))
    assertTrue(testHandler.lastRecord.map(_.getThrown).isDefined)
    assertEquals(Some("cls"), testHandler.lastRecord.map(_.getSourceClassName))
    assertEquals(Some("method"),
      testHandler.lastRecord.map(_.getSourceMethodName))
  }

  @Test def test_entering_variants(): Unit = {
    val logger = Logger.getLogger(s"$Prefix.TestLogger11")
    val testHandler = new TestHandler
    logger.setLevel(Level.ALL)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    logger.entering("cls", "method")
    assertEquals(Some(Level.FINER), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("ENTRY"), testHandler.lastRecord.map(_.getMessage))
    assertEquals(Some("cls"), testHandler.lastRecord.map(_.getSourceClassName))
    assertEquals(Some("method"),
      testHandler.lastRecord.map(_.getSourceMethodName))

    logger.entering("cls", "method", "param")
    assertEquals(Some(Level.FINER), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("ENTRY {0}"), testHandler.lastRecord.map(_.getMessage))
    assertEquals(Some("cls"), testHandler.lastRecord.map(_.getSourceClassName))
    assertEquals(Some("method"),
      testHandler.lastRecord.map(_.getSourceMethodName))
    assertArrayEquals(Array[AnyRef]("param"),
      testHandler.lastRecord.map(_.getParameters).getOrElse(Array.empty))

    logger.entering("cls", "method", Array[AnyRef]("param1", "param2"))
    assertEquals(Some(Level.FINER), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("ENTRY {0} {1}"), testHandler.lastRecord.map(_.getMessage))
    assertEquals(Some("cls"), testHandler.lastRecord.map(_.getSourceClassName))
    assertEquals(Some("method"),
      testHandler.lastRecord.map(_.getSourceMethodName))
    assertArrayEquals(Array[AnyRef]("param1", "param2"),
      testHandler.lastRecord.map(_.getParameters).getOrElse(Array.empty))
  }

  @Test def test_exiting_variants(): Unit = {
    val logger = Logger.getLogger(s"$Prefix.TestLogger12")
    val testHandler = new TestHandler
    logger.setLevel(Level.ALL)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    logger.exiting("cls", "method")
    assertEquals(Some(Level.FINER), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("RETURN"), testHandler.lastRecord.map(_.getMessage))
    assertEquals(Some("cls"), testHandler.lastRecord.map(_.getSourceClassName))
    assertEquals(Some("method"),
      testHandler.lastRecord.map(_.getSourceMethodName))

    logger.exiting("cls", "method", "param")
    assertEquals(Some(Level.FINER), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("RETURN {0}"), testHandler.lastRecord.map(_.getMessage))
    assertEquals(Some("cls"), testHandler.lastRecord.map(_.getSourceClassName))
    assertEquals(Some("method"),
      testHandler.lastRecord.map(_.getSourceMethodName))
    assertArrayEquals(Array[AnyRef]("param"),
      testHandler.lastRecord.map(_.getParameters).getOrElse(Array.empty))
  }

  @Test def test_throwing(): Unit = {
    val logger = Logger.getLogger(s"$Prefix.TestLogger13")
    val testHandler = new TestHandler
    logger.setLevel(Level.ALL)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    logger.throwing("cls", "method", new RuntimeException)
    assertEquals(Some(Level.FINER), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("THROW"), testHandler.lastRecord.map(_.getMessage))
    assertEquals(Some("cls"), testHandler.lastRecord.map(_.getSourceClassName))
    assertEquals(Some("method"),
      testHandler.lastRecord.map(_.getSourceMethodName))
    assertTrue(testHandler.lastRecord.map(_.getThrown).isDefined)
  }

  @Test def test_named_levels(): Unit = {
    val logger = Logger.getLogger(s"$Prefix.TestLogger13")
    val testHandler = new TestHandler
    logger.setLevel(Level.ALL)
    logger.addHandler(testHandler)
    logger.setUseParentHandlers(false)

    logger.severe("severe_msg")
    assertEquals(Some(Level.SEVERE), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("severe_msg"), testHandler.lastRecord.map(_.getMessage))

    logger.warning("warning_msg")
    assertEquals(Some(Level.WARNING), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("warning_msg"), testHandler.lastRecord.map(_.getMessage))

    logger.info("info_msg")
    assertEquals(Some(Level.INFO), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("info_msg"), testHandler.lastRecord.map(_.getMessage))

    logger.config("config_msg")
    assertEquals(Some(Level.CONFIG), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("config_msg"), testHandler.lastRecord.map(_.getMessage))

    logger.fine("fine_msg")
    assertEquals(Some(Level.FINE), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("fine_msg"), testHandler.lastRecord.map(_.getMessage))

    logger.finer("finer_msg")
    assertEquals(Some(Level.FINER), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("finer_msg"), testHandler.lastRecord.map(_.getMessage))

    logger.finest("finest_msg")
    assertEquals(Some(Level.FINEST), testHandler.lastRecord.map(_.getLevel))
    assertEquals(Some("finest_msg"), testHandler.lastRecord.map(_.getMessage))
  }

  @Test def test_logger_parents_by_name(): Unit = {
    val l1 = Logger.getLogger(s"$Prefix.a.b.c.d")
    assertEquals("", l1.getParent.getName)

    val l2 = Logger.getLogger(s"$Prefix.a.b")
    assertEquals(l2.getName, l1.getParent.getName)
  }
}
