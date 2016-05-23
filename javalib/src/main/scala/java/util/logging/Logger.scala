package java.util.logging

import scala.annotation.tailrec
import scala.collection.mutable

object Logger {
  val GLOBAL_LOGGER_NAME: String =  "global"

  // Not implemented, deprecated on JDK 1.8
  //val global: Logger

  private val defaultLogLevel = Level.ALL

  private val loggers: mutable.Map[String, Logger] = mutable.Map.empty

  // Root is not visible to the outside but gives defaults
  private[this] val rootLogger =
    new Logger("", None, Some(defaultLogLevel), false, None)

  private[this] val globalLogger =
    new Logger(GLOBAL_LOGGER_NAME, None, None, true, Some(rootLogger))

  def getGlobal():Logger = globalLogger

  def getLogger(name: String): Logger = {
    if (name == null)
      throw new NullPointerException("Logger name cannot be null")
    loggers.getOrElseUpdate(name,
      new Logger(name, None, None, true, Some(rootLogger)))
  }

  // Not implemented, no resource bundle in scala.js
  //def getLogger(name: String, resourceBundle: String): Logger

  protected def findParent(logger: Logger): Option[Logger] = {
    def go(s: List[String]): Option[Logger] = s match {
      case Nil                                    =>
        None
      case b if loggers.contains(b.mkString(".")) =>
        loggers.get(b.mkString("."))
      case b                                      =>
        go(b.dropRight(1))
    }
    go(Option(logger.getName).getOrElse("").split("\\.").toList.dropRight(1))
  }

  def getAnonymousLogger():Logger =
    // No references to anonymous loggers are kept
    new Logger(null, None, None, true, Some(rootLogger))

  // Not implemented, no resource bundle in scala.js
  //def getAnonymousLogger(resourceBundle: String):Logger
}

class Logger private (protected val name: String,
    protected val resourceBundle: Option[String],
    protected var level: Option[Level],
    protected var useParentsHandlers: Boolean,
    protected var parent: Option[Logger] = None,
    protected var filter: Option[Filter] = None) {

  private[this] var handlers: Array[Handler] = Array.empty

  protected def this(name: String, resourceBundle: String) =
    this(name, Option(resourceBundle), None, true)

  // Not implemented, no resource bundle
  //def getResourceBundle():ResourceBundle = ???

  // Find the effective level
  private def levelR: Option[Level] = {
    @tailrec
    def go(logger: Logger): Option[Level] = logger.level match {
      case l @ Some(_)                 => l
      case None if getParent() != null => go(logger.getParent)
      case None                        => None
    }

    go(this)
  }

  def getResourceBundleName():String = resourceBundle.orNull

  def setFilter(filter: Filter): Unit = this.filter = Option(filter)

  def getFilter(): Filter = filter.orNull

  def log(record: LogRecord): Unit = if (isLoggable(record.getLevel)) {
    if (useParentsHandlers) {
      Logger.findParent(this).orElse(parent).foreach(_.log(record))
    }
    handlers.foreach(_.publish(record))
  }

  def log(level: Level, msg: String): Unit =
    log(new LogRecord(level, msg, loggerName = Option(name)))

  def log(level: Level, msg: String, param: AnyRef): Unit =
    log(new LogRecord(level, msg, params = List(param),
      loggerName = Option(name)))

  def log(level: Level, msg: String, params: Array[AnyRef]): Unit =
    log(new LogRecord(level, msg, params = params.toList,
      loggerName = Option(name)))

  def log(level: Level, msg: String, thrown: Throwable): Unit =
    log(new LogRecord(level, msg, thrown = Option(thrown),
      loggerName = Option(name)))

  def logp(level: Level, sourceClass: String, sourceMethod: String,
      msg: String): Unit =
    log(new LogRecord(level, msg,
      sourceClassName = Option(sourceClass),
      sourceMethodName = Option(sourceMethod),
      loggerName = Option(name)))

  def logp(level: Level, sourceClass: String, sourceMethod: String,
      msg: String, param: AnyRef): Unit =
    log(new LogRecord(level, msg,
      sourceClassName = Option(sourceClass),
      sourceMethodName = Option(sourceMethod),
      params = List(param),
      loggerName = Option(name)))

  def logp(level: Level, sourceClass: String, sourceMethod: String,
      msg: String, params: Array[AnyRef]): Unit =
    log(new LogRecord(level, msg,
      sourceClassName = Option(sourceClass),
      sourceMethodName = Option(sourceMethod),
      params = params.toList,
      loggerName = Option(name)))

  def logp(level: Level, sourceClass: String, sourceMethod: String,
      msg: String, thrown: Throwable): Unit =
    log(new LogRecord(level, msg,
      sourceClassName = Option(sourceClass),
      sourceMethodName = Option(sourceMethod),
      thrown = Option(thrown),
      loggerName = Option(name)))

  // Not implemented, no resource bundle
  //def logrb(level: Level, sourceClass: String, sourceMethod: String,
  //    bundleName: String, msg: String): Unit = ???

  // Not implemented, no resource bundle
  //def logrb(level: Level, sourceClass: String, sourceMethod: String,
  //    bundleName: String, msg: String, param: AnyRef): Unit = ???

  // Not implemented, no resource bundle
  //def logrb(level: Level, sourceClass: String, sourceMethod: String,
  //    bundleName: String, msg: String, params: Array[AnyRef]): Unit = ???

  // Not implemented, no resource bundle
  //def logrb(level: Level, sourceClass: String, sourceMethod: String,
  //    bundleName: String, msg: String, thrown: Throwable): Unit = ???

  def entering(sourceClass: String, sourceMethod: String): Unit =
    log(new LogRecord(Level.FINER,
      "ENTRY",
      sourceClassName = Option(sourceClass),
      sourceMethodName = Option(sourceMethod),
      loggerName = Option(name)))

  def entering(sourceClass: String, sourceMethod: String,
      param: AnyRef): Unit =
    log(new LogRecord(Level.FINER,
      "ENTRY {0}",
      sourceClassName = Option(sourceClass),
      sourceMethodName = Option(sourceMethod),
      params = List(param),
      loggerName = Option(name)))

  private def paramsString(i: Int): String =
    (0 until i).map(i => s"{$i}").mkString(" ")

  def entering(sourceClass: String, sourceMethod: String,
      params: Array[AnyRef]): Unit =
    log(new LogRecord(Level.FINER,
      s"ENTRY ${paramsString(params.length)}",
      sourceClassName = Option(sourceClass),
      sourceMethodName = Option(sourceMethod),
      params = params.toList))

  def exiting(sourceClass: String, sourceMethod: String): Unit =
    log(new LogRecord(Level.FINER,
      "RETURN",
      sourceClassName = Option(sourceClass),
      sourceMethodName = Option(sourceMethod)))

  def exiting(sourceClass: String, sourceMethod: String,
      result: AnyRef): Unit =
    log(new LogRecord(Level.FINER,
      "RETURN {0}",
      sourceClassName = Option(sourceClass),
      sourceMethodName = Option(sourceMethod),
      params = List(result)))

  def throwing(sourceClass: String, sourceMethod: String,
      thrown: Throwable): Unit =
    log(new LogRecord(Level.FINER,
      "THROW",
      sourceClassName = Option(sourceClass),
      sourceMethodName = Option(sourceMethod),
      thrown = Some(thrown)))

  def severe(msg: String): Unit =
    log(new LogRecord(Level.SEVERE, msg, loggerName = Option(name)))

  def warning(msg: String): Unit =
    log(new LogRecord(Level.WARNING, msg, loggerName = Option(name)))

  def info(msg: String): Unit =
    log(new LogRecord(Level.INFO, msg, loggerName = Option(name)))

  def config(msg: String): Unit =
    log(new LogRecord(Level.CONFIG, msg, loggerName = Option(name)))

  def fine(msg: String): Unit =
    log(new LogRecord(Level.FINE, msg, loggerName = Option(name)))

  def finer(msg: String): Unit =
    log(new LogRecord(Level.FINER, msg, loggerName = Option(name)))

  def finest(msg: String): Unit =
    log(new LogRecord(Level.FINEST, msg, loggerName = Option(name)))

  def setLevel(newLevel: Level): Unit = this.level = Option(newLevel)

  def getLevel(): Level = level.orNull

  def getName(): String = name

  def isLoggable(level: Level): Boolean =
    levelR.forall(_.intValue() <= level.intValue())

  def addHandler(handler: Handler): Unit = handlers = handlers :+ handler

  def removeHandler(handler: Handler): Unit =
    handlers = handlers.filterNot(_ == handler)

  def getHandlers(): Array[Handler] = handlers

  def setUseParentHandlers(useParentHandlers: Boolean): Unit =
    this.useParentsHandlers = useParentHandlers

  def getUseParentHandlers(): Boolean = useParentsHandlers

  def getParent(): Logger =
    Logger.findParent(this).orElse(parent).orNull

  def setParent(parent: Logger): Unit = this.parent = Some(parent)
}
