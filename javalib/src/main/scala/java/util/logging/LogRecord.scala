package java.util.logging

object LogRecord {
  protected[logging] var sequence: Long = 0L
}

class LogRecord(private[this] var level: Level, private[this] var msg: String) {

  private[this] var sourceClassName: String = null
  private[this] var sourceMethodName: String = null
  private[this] var params: Array[AnyRef] = null
  private[this] var thrown: Throwable = null
  private[this] var loggerName: String = null
  private[this] var millis: Long = System.currentTimeMillis()
  private[this] var threadId: Long = Thread.currentThread().getId

  private[this] var sequenceNumber: Long = {
    LogRecord.sequence = LogRecord.sequence + 1
    LogRecord.sequence
  }

  def getLoggerName(): String = loggerName

  def setLoggerName(loggerName: String): Unit =
    this.loggerName = loggerName

  // Not implemented, no locale in Scala.js
  //def getResourceBundle():ResourceBundle = ???

  // Not implemented, no locale in Scala.js
  //def setResourceBundle(bundle: ResourceBundle):Unit = ???

  // Message is not localizable, return null
  def getResourceBundleName(): String = null

  // Message is not localizable, no-op
  def setResourceBundleName(name: String): Unit = {}

  def getLevel(): Level = level

  def setLevel(level: Level): Unit = this.level = level

  def getSequenceNumber(): Long = sequenceNumber

  def setSequenceNumber(seq: Long): Unit = sequenceNumber = seq

  def getSourceClassName(): String = sourceClassName

  def setSourceClassName(sourceClassName: String): Unit =
    this.sourceClassName = sourceClassName

  def getSourceMethodName(): String = sourceMethodName

  def setSourceMethodName(sourceClassName: String): Unit =
    this.sourceMethodName = sourceClassName

  def getMessage(): String = msg

  def setMessage(message: String): Unit = msg = message

  def getParameters(): Array[AnyRef] = params

  def setParameters(parameters: Array[AnyRef]): Unit = this.params = parameters

  def getThreadID(): Int = threadId.toInt

  def setThreadID(threadID: Int): Unit = this.threadId = threadID

  def getMillis(): Long = millis

  def setMillis(millis: Long): Unit = this.millis = millis

  def getThrown(): Throwable = thrown

  def setThrown(thrown: Throwable): Unit = this.thrown = thrown
}
