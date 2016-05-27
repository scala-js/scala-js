package java.util.logging

import java.nio.charset.{Charset, UnsupportedCharsetException}

abstract class Handler protected () {
  private[this] var level: Level = Level.ALL
  private[this] var filter: Filter = null
  private[this] var formatter: Formatter = null
  private[this] var encoding: String = null
  private[this] var errorManager: ErrorManager = new ErrorManager()

  def publish(record:LogRecord): Unit

  def flush(): Unit

  def close(): Unit

  def setFormatter(formatter: Formatter): Unit =
    this.formatter = formatter

  def getFormatter(): Formatter = formatter

  def setEncoding(encoding: String): Unit = {
    if (encoding == null) this.encoding = null
    else if (Charset.isSupported(encoding)) this.encoding = encoding
    else throw new UnsupportedCharsetException(s"$encoding not supported")
  }

  def getEncoding(): String = encoding

  def setFilter(filter: Filter): Unit =
    this.filter = filter

  def getFilter(): Filter = filter

  def setErrorManager(errorManager: ErrorManager): Unit =
    if (errorManager == null) throw new NullPointerException()
    else this.errorManager = errorManager

  def getErrorManager(): ErrorManager = errorManager

  protected def reportError(msg: String, ex: Exception, code: Int): Unit =
    errorManager.error(msg, ex, code)

  def setLevel(level: Level): Unit =
    if (level == null) throw new NullPointerException()
    else this.level = level

  def getLevel(): Level = level

  def isLoggable(record:LogRecord): Boolean = {
    level.intValue() <= record.getLevel.intValue() &&
    (filter == null || filter.isLoggable(record))
  }

}
