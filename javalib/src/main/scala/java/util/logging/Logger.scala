package java.util.logging

import scala.annotation.tailrec

object Logger {
  val GLOBAL_LOGGER_NAME: String =  "global"

  // Not implemented, deprecated on JDK 1.8
  //val global: Logger

  private val defaultLogLevel = Level.ALL

  private val loggers: Map[String, Logger] = Map.empty

  // Root is not visible to the outside but gives defaults
  private[this] val rootLogger =
    new Logger("root", None, Some(defaultLogLevel), false, None, None)

  private[this] val globalLogger =
    new Logger(GLOBAL_LOGGER_NAME, None, None, true, Some(rootLogger), None)

  def getGlobal():Logger = globalLogger

  def getLogger(name: String): Logger = {
    if (name == null)
      throw new NullPointerException("Logger name cannot be null")
    loggers.getOrElse(name,
      new Logger(name, None, None, true, Some(globalLogger), None))
  }

  def getLogger(name: String, resourceBundle: String): Logger = {
    if (name == null)
      throw new NullPointerException("Logger name cannot be null")
    loggers.getOrElse(name,
      new Logger(name, Some(resourceBundle), None, true, Some(globalLogger), None))
  }

  def getAnonymousLogger():Logger =
    // No references to anonymous loggers are kept
    new Logger(null, None, None, true, Some(rootLogger))

  def getAnonymousLogger(resourceBundle: String):Logger =
    new Logger(null, Some(resourceBundle), None, true, Some(rootLogger))
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

  // Not implemented, no locale in Scala.js
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
    if (useParentsHandlers && parent.isDefined) {
      parent.foreach(_.log(record))
    }
    handlers.foreach(_.publish(record))
  }

  def log(level: Level, msg: String): Unit = ???

  def log(level: Level, msg: String, param: AnyRef): Unit = ???

  def log(level: Level, msg: String, params: Array[AnyRef]): Unit = ???

  def log(level: Level, msg: String, thrown: Throwable): Unit = ???

  def logp(level: Level, sourceClass: String, sourceMethod: String,
      msg: String): Unit = ???

  def logp(level: Level, sourceClass: String, sourceMethod: String,
      msg: String, param: AnyRef): Unit = ???

  def logp(level: Level, sourceClass: String, sourceMethod: String,
      msg: String, params: Array[AnyRef]): Unit = ???

  def logp(level: Level, sourceClass: String, sourceMethod: String,
      msg: String, thrown: Throwable): Unit = ???

  def logrb(level: Level, sourceClass: String, sourceMethod: String,
      bundleName: String, msg: String): Unit = ???

  def logrb(level: Level, sourceClass: String, sourceMethod: String,
      bundleName: String, msg: String, param: AnyRef): Unit = ???

  def logrb(level: Level, sourceClass: String, sourceMethod: String,
      bundleName: String, msg: String, params: Array[AnyRef]): Unit = ???

  def logrb(level: Level, sourceClass: String, sourceMethod: String,
      bundleName: String, msg: String, thrown: Throwable): Unit = ???

  def entering(sourceClass: String, sourceMethod: String): Unit = ???

  def entering(sourceClass: String, sourceMethod: String,
      param: AnyRef): Unit = ???

  def entering(sourceClass: String, sourceMethod: String,
      params: Array[AnyRef]): Unit = ???

  def exiting(sourceClass: String, sourceMethod: String): Unit = ???

  def exiting(sourceClass: String, sourceMethod: String,
      result: AnyRef): Unit = ???

  def throwing(sourceClass: String, sourceMethod: String,
      thrown: Throwable): Unit = ???

  def severe(msg: String): Unit = ???

  def warning(msg: String): Unit = ???

  def info(msg: String): Unit = ???

  def config(msg: String): Unit = ???

  def fine(msg: String): Unit = ???

  def finer(msg: String): Unit = ???

  def finest(msg: String): Unit = ???

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

  def getParent(): Logger = parent.orNull

  def setParent(parent: Logger): Unit = this.parent = Some(parent)
}
