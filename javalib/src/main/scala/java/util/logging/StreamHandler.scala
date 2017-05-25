package java.util.logging

import java.io.OutputStream
import java.nio.charset.Charset

class StreamHandler(private[this] var out: OutputStream,
    private[this] val formatter: Formatter) extends Handler {

  // Defaults defined on javadocs
  setLevel(Level.INFO)
  setFilter(null)

  if (formatter == null) setFormatter(new SimpleFormatter())
  else setFormatter(formatter)

  // Required by javadoc but it is unspecified what to do if formatter is null
  def this() = this(null, null)

  private[this] var headWritten: Boolean = false

  private def encodingOrDefault: Charset =
    if (getEncoding == null) Charset.defaultCharset()
    else Charset.forName(getEncoding)

  protected def setOutputStream(out: OutputStream): Unit = {
    // Required by javadocs
    if (out != null && formatter != null) {
      out.write(formatter.getTail(this).getBytes(encodingOrDefault))
      flush()
      out.close()
    }
    this.headWritten = false
    this.out = out
  }

  // Mentioned as part of StreamHandler javadocs but it doesn't specify behavior
  override def setEncoding(encoding: String): Unit = super.setEncoding(encoding)

  private def write(c: Formatter => String): Unit = {
    // The javadocs don't specify what to do if the formatter is null
    if (out != null && formatter != null) {
      out.write(c(formatter).getBytes(encodingOrDefault))
      flush()
    }
  }

  private[logging] def writeHeader(): Unit = {
    if (!headWritten) {
      write(_.getHead(this))
      headWritten = true
    }
  }

  private[logging] def writeTail(): Unit = write(_.getTail(this))

  override def publish(record: LogRecord): Unit = {
    writeHeader()
    // The javadocs don't specify what to do if the formatter is null
    if (out != null && formatter != null && isLoggable(record)) {
      out.write(formatter.format(record).getBytes(encodingOrDefault))
      flush()
    }
  }

  override def isLoggable(record:LogRecord): Boolean =
    out != null && record != null && super.isLoggable(record)

  override def flush(): Unit = if (out != null) out.flush()

  override def close(): Unit = {
    if (out != null) {
      // Required by javadocs
      writeHeader()
      writeTail()
      flush()
      out.close()
    }
  }
}
