package java.util.logging

import java.nio.charset.{Charset, UnsupportedCharsetException}

abstract class Handler protected () {
  private[this] var level:Option[Level] = Some(Level.ALL)
  private[this] var filter:Option[Filter] = None
  private[this] var formatter:Option[Formatter] = None
  private[this] var encoding:Option[String] = None
  private[this] var errorManager:Option[ErrorManager] = None

  def publish(record:LogRecord):Unit

  def flush():Unit

  def close():Unit

  def setFormatter(formatter: Formatter):Unit =
    this.formatter = Option(formatter)

  def getFormatter():Formatter = formatter.orNull

  def setEncoding(encoding: String):Unit =
    if (Charset.isSupported(encoding)) this.encoding = Option(encoding)
    else throw new UnsupportedCharsetException(s"$encoding not supported")

  def getEncoding():String = encoding.orNull

  def setFilter(filter: Filter):Unit =
    this.filter = Option(filter)

  def getFilter():Filter = filter.orNull

  def setErrorManager(errorManager: ErrorManager):Unit =
    this.errorManager = Option(errorManager)

  def getErrorManager():ErrorManager = errorManager.orNull

  def reportError(msg: String, ex: Exception, code: Int):Unit =
    errorManager.foreach(_.error(msg, ex, code))

  def setLevel(level: Level):Unit =
    this.level = Option(level)

  def getLevel():Level = level.orNull

  def isLoggable(record:LogRecord):Boolean =
    level.forall(_.intValue() <= record.getLevel.intValue()) &&
      filter.forall(_.isLoggable(record))

  protected def encodingOrDefault:Option[Charset] =
    encoding
      .fold(Option(Charset.defaultCharset()))(e => Option(Charset.forName(e)))
}
