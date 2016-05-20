package java.util.logging

import java.util.concurrent.atomic.AtomicLong

object LogRecord {
  protected[logging] var sequence:AtomicLong = new AtomicLong(0)
}

class LogRecord protected[logging] (private[this] var level: Level,
    private[this] var msg: String,
    private[this] var sourceClassName: Option[String] = None,
    private[this] var sourceMethodName: Option[String] = None,
    private[this] var params: List[AnyRef] = Nil,
    private[this] var thrown: Option[Throwable] = None,
    private[this] var loggerName: Option[String] = None) {

  def this(level: Level, msg: String) = this(level, msg, None)

  private[this] var millis: Long = System.currentTimeMillis()
  private[this] var threadId: Long = Thread.currentThread().getId

  private[this] var sequenceNumber:Long =
    LogRecord.sequence.getAndIncrement

  def getLoggerName():String = loggerName.orNull

  def setLoggerName(loggerName: String): Unit =
    this.loggerName = Option(loggerName)

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

  def getSourceClassName(): String = sourceClassName.orNull

  def setSourceClassName(sourceClassName: String): Unit =
    this.sourceClassName = Option(sourceClassName)

  def getSourceMethodName(): String = sourceMethodName.orNull

  def setSourceMethodName(sourceClassName: String): Unit =
    this.sourceMethodName = Option(sourceClassName)

  def getMessage(): String = msg

  def setMessage(message: String): Unit = msg = message

  def getParameters(): Array[AnyRef] =
    if (params.isEmpty) null
    else params.toArray

  def setParameters(parameters: Array[AnyRef]): Unit =
    if (parameters == null) this.params = Nil
    else this.params = parameters.toList

  def getThreadID(): Int = threadId.toInt

  def setThreadID(threadID: Int): Unit = this.threadId = threadID

  def getMillis(): Long = millis

  def setMillis(millis: Long): Unit = this.millis = millis

  def getThrown(): Throwable = thrown.orNull

  def setThrown(thrown: Throwable): Unit = this.thrown = Option(thrown)
}
